/*
 * XMA1/XMA2 encoder, written by Claude using FFMPEG decoder as a reference
 *
 * XMA is the WMA Pro bitstream, cut into 512-sample frames which are packed
 * into 2048-byte packets. The bitstream syntax here mirrors the decoder in
 * FFmpeg's libavcodec/wmaprodec.c. The encoder only uses the subset of
 * features seen in official Xbox 360 XMA files (from both Microsoft's and
 * FMOD's encoders), so that the result is safe for the hardware decoder:
 *
 * - every frame is split into four 128-sample subframes, shared by both channels
 * - stereo is coded either as L/R or as M/S for all bands
 * - scale factor step is 2 dB
 * - no extended headers, post-processing, or dynamic range compression
 * - the first frame trims 512 samples (encoder delay), the last trims the tail
 *
 * Quantization is driven by a simple masking model: each frequency bin's
 * allowed noise comes from the spread energy of nearby bins, lowered by a
 * quality-dependent offset, and limited below by the absolute threshold of
 * hearing. It is computed per frame (so scale factors rarely need to change
 * between subframes), with limits to avoid pre-echo before attacks.
 */

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "xma_encode.h"
#include "xma_tables.h"

#define SPF        512   /* samples per frame */
#define SUB        128   /* samples per subframe */
#define NSUB       4     /* subframes per frame */
#define WIN        256   /* MDCT window length */
#define MAX_BANDS  29
#define FFT_N      (SUB / 2)

#define PACKET_BYTES     2048
#define PAYLOAD_BYTES    (PACKET_BYTES - 4)
#define PAYLOAD_BITS     (PAYLOAD_BYTES * 8)

/* Frames are kept small enough to span at most two packets. */
#define MAX_FRAME_BITS   16000
#define FRAME_BUF_BYTES  8192

/*
 * Alignment of encoded audio. The first MDCT window of frame 0 starts 192
 * samples into the decoder's output stream, and decoders (Microsoft's
 * xma2encode, FFmpeg, vgmstream) skip trim_start + 64 = 576 samples of
 * that stream at the start. At the end, the decoded stream has frames * 512
 * + 192 samples, minus trim_end. These numbers reproduce the trim values in
 * official files and in xma2encode output.
 */
#define DECODER_SKIP     (SPF + 64)
/* Input samples (zeros) before the first MDCT window */
#define PREROLL          (DECODER_SKIP - 192)
/* Samples needed in the input buffer to encode one frame */
#define FRAME_INPUT      (SPF + WIN - SUB)
/* Decoded stream length is frames * SPF + TAIL - trim_end - DECODER_SKIP */
#define TAIL             192

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

typedef struct {
    uint32_t code;
    int      len;
} Vlc;

struct XmaEncoder {
    int channels;
    int rate;
    int quality;

    /* scale factor bands for 128-sample subframes */
    int num_bands;
    int band_start[MAX_BANDS + 1];

    /* masking model */
    float spread[SUB][SUB];       /* masking from bin j onto bin k */
    float ath[SUB];               /* minimum allowed noise power per coefficient */
    float mask_offset;            /* allowed noise relative to masking level */
    float transient_margin;       /* how far a subframe's own masking can exceed the frame's */
    int   sf_tolerance;           /* largest scale factor tolerance to try */
    float ms_noise;               /* M/S noise relative to the quieter channel's allowance */
    float pre_echo_rise;          /* max increase of allowed noise from one subframe to the next */
    float prev_noise[2][MAX_BANDS];

    /* MDCT */
    float window[WIN];
    float pre_re[FFT_N], pre_im[FFT_N];
    float post_re[FFT_N], post_im[FFT_N];
    float fft_re[FFT_N / 2], fft_im[FFT_N / 2];
    int   bitrev[FFT_N];

    /* VLC encoding tables */
    Vlc   sf_vlc[HUFF_SCALE_SIZE];          /* by delta + 60 */
    Vlc   sfrl_vlc[HUFF_SCALE_RL_SIZE];     /* by index */
    int   sfrl_index[32][16];               /* [run][level] -> index, or -1 */
    Vlc   coef_vlc[2][HUFF_COEF0_SIZE];     /* by index */
    int   coef_index[2][160][64];           /* [run][level] -> index, or -1 */
    int   coef_max_run[2], coef_max_level[2];
    Vlc   vec4_esc, vec2_esc;
    Vlc   vec2_vlc[256];                    /* by (a << 4) | b, len 0 = missing */
    Vlc   vec1_vlc[HUFF_VEC1_SIZE];
    int16_t *vec4_index;                    /* 65536 entries: key -> table position, -1 if missing */
    Vlc   vec4_vlc[HUFF_VEC4_SIZE];

    /* input buffer */
    float  *pcm[2];
    int     pcm_count;
    int64_t samples_in;
    int     finished;

    /* encoded frames, concatenated bitwise */
    uint8_t  *fdata;
    size_t    fdata_cap;    /* bytes */
    int64_t   fdata_bits;
    uint16_t *flen;
    int64_t   nframes;
    int       trim_end;     /* trim_end of the last frame */
    size_t    flen_cap;
};

/* ------------------------------------------------------------------------- */
/* Bit writer (MSB first). With buf == NULL it only counts bits.             */

typedef struct {
    uint8_t *buf;
    int64_t  pos;
    int64_t  cap;   /* in bits */
} BitWriter;

static void put_bits(BitWriter *bw, uint32_t value, int n)
{
    if (bw->buf) {
        for (int i = n - 1; i >= 0; i--) {
            if (bw->pos < bw->cap && ((value >> i) & 1))
                bw->buf[bw->pos >> 3] |= 0x80 >> (bw->pos & 7);
            bw->pos++;
        }
    } else {
        bw->pos += n;
    }
}

static void put_vlc(BitWriter *bw, Vlc v)
{
    put_bits(bw, v.code, v.len);
}

static int large_val_bits(uint32_t v)
{
    if (v < (1u << 8))  return 1 + 8;
    if (v < (1u << 16)) return 2 + 16;
    if (v < (1u << 24)) return 3 + 24;
    return 3 + 31;
}

/* Inverse of ff_wma_get_large_val */
static void put_large_val(BitWriter *bw, uint32_t v)
{
    if (v < (1u << 8)) {
        put_bits(bw, 0, 1);
        put_bits(bw, v, 8);
    } else if (v < (1u << 16)) {
        put_bits(bw, 2, 2);
        put_bits(bw, v, 16);
    } else if (v < (1u << 24)) {
        put_bits(bw, 6, 3);
        put_bits(bw, v, 24);
    } else {
        put_bits(bw, 7, 3);
        put_bits(bw, v & 0x7FFFFFFF, 31);
    }
}

/* ------------------------------------------------------------------------- */
/* Table setup                                                               */

/* Assign codes the same way as FFmpeg's ff_vlc_init_from_lengths. */
static void assign_codes(int n, const uint8_t *lens, int len_stride,
                         uint32_t *codes)
{
    uint64_t code = 0;
    for (int i = 0; i < n; i++) {
        int len = lens[i * len_stride];
        codes[i] = (uint32_t)(code >> (32 - len));
        code += 1ull << (32 - len);
    }
}

static void init_tables(XmaEncoder *e)
{
    uint32_t codes[300];

    assign_codes(HUFF_SCALE_SIZE, &scale_table[0][1], 2, codes);
    for (int i = 0; i < HUFF_SCALE_SIZE; i++) {
        Vlc v = { codes[i], scale_table[i][1] };
        e->sf_vlc[scale_table[i][0]] = v;
    }

    memset(e->sfrl_index, 0xFF, sizeof(e->sfrl_index));
    assign_codes(HUFF_SCALE_RL_SIZE, &scale_rl_table[0][1], 2, codes);
    for (int i = 0; i < HUFF_SCALE_RL_SIZE; i++) {
        Vlc v = { codes[i], scale_rl_table[i][1] };
        e->sfrl_vlc[scale_rl_table[i][0]] = v;
    }
    for (int idx = 2; idx < HUFF_SCALE_RL_SIZE; idx++)
        e->sfrl_index[scale_rl_run[idx]][scale_rl_level[idx]] = idx;

    memset(e->coef_index, 0xFF, sizeof(e->coef_index));
    assign_codes(HUFF_COEF0_SIZE, coef0_lens, 1, codes);
    for (int i = 0; i < HUFF_COEF0_SIZE; i++) {
        Vlc v = { codes[i], coef0_lens[i] };
        e->coef_vlc[0][coef0_syms[i]] = v;
    }
    assign_codes(HUFF_COEF1_SIZE, &coef1_table[0][1], 2, codes);
    for (int i = 0; i < HUFF_COEF1_SIZE; i++) {
        Vlc v = { codes[i], coef1_table[i][1] };
        e->coef_vlc[1][coef1_table[i][0]] = v;
    }
    for (int t = 0; t < 2; t++) {
        int size = t ? HUFF_COEF1_SIZE : HUFF_COEF0_SIZE;
        const uint16_t *run = t ? coef1_run : coef0_run;
        const float *level = t ? coef1_level : coef0_level;
        e->coef_max_run[t] = 0;
        e->coef_max_level[t] = 0;
        for (int idx = 2; idx < size; idx++) {
            int r = run[idx], l = (int)level[idx];
            if (r < 160 && l < 64) {
                /* keep the shortest code if a pair appears twice */
                int old = e->coef_index[t][r][l];
                if (old < 0 || e->coef_vlc[t][idx].len < e->coef_vlc[t][old].len)
                    e->coef_index[t][r][l] = idx;
            }
            if (r > e->coef_max_run[t]) e->coef_max_run[t] = r;
            if (l > e->coef_max_level[t]) e->coef_max_level[t] = l;
        }
    }

    for (int i = 0; i < 65536; i++) e->vec4_index[i] = -1;
    assign_codes(HUFF_VEC4_SIZE, vec4_lens, 1, codes);
    for (int i = 0; i < HUFF_VEC4_SIZE; i++) {
        Vlc v = { codes[i], vec4_lens[i] };
        if (vec4_syms[i] == 0) {
            e->vec4_esc = v;
        } else {
            e->vec4_vlc[i] = v;
            e->vec4_index[vec4_syms[i] - 1] = (int16_t)i;
        }
    }

    memset(e->vec2_vlc, 0, sizeof(e->vec2_vlc));
    assign_codes(HUFF_VEC2_SIZE, &vec2_table[0][1], 2, codes);
    for (int i = 0; i < HUFF_VEC2_SIZE; i++) {
        Vlc v = { codes[i], vec2_table[i][1] };
        if (vec2_table[i][0] == 0) e->vec2_esc = v;
        else e->vec2_vlc[vec2_table[i][0] - 1] = v;
    }

    assign_codes(HUFF_VEC1_SIZE, &vec1_table[0][1], 2, codes);
    for (int i = 0; i < HUFF_VEC1_SIZE; i++) {
        Vlc v = { codes[i], vec1_table[i][1] };
        e->vec1_vlc[vec1_table[i][0]] = v;
    }
}

/* Same band layout as the decoder (decode_init in wmaprodec.c). */
static void init_bands(XmaEncoder *e)
{
    int rate;
    if (e->rate > 44100) rate = 48000;
    else if (e->rate > 32000) rate = 44100;
    else if (e->rate > 24000) rate = 32000;
    else rate = 24000;

    int band = 1;
    e->band_start[0] = 0;
    for (int x = 0; x < MAX_BANDS - 1 && e->band_start[band - 1] < SUB; x++) {
        int offset = (SUB * 2 * critical_freq[x]) / rate + 2;
        offset &= ~3;
        if (offset > e->band_start[band - 1])
            e->band_start[band++] = offset;
        if (offset >= SUB)
            break;
    }
    e->band_start[band - 1] = SUB;
    e->num_bands = band - 1;
}

static double bark(double f)
{
    return 13.0 * atan(0.00076 * f) + 3.5 * atan((f / 7500.0) * (f / 7500.0));
}

/* Absolute threshold of hearing in dB SPL (Terhardt) */
static double ath_db(double f)
{
    double k = f / 1000.0;
    if (k < 0.02) k = 0.02;
    return 3.64 * pow(k, -0.8) - 6.5 * exp(-0.6 * (k - 3.3) * (k - 3.3)) + 0.001 * k * k * k * k;
}

static void mdct(const XmaEncoder *e, const float *in, float *out);

static void init_psy(XmaEncoder *e)
{
    /*
     * Tuning, chosen by comparing against xma2encode at the same quality
     * setting (noise per frequency band, bitrate, and pre-echo before drum
     * hits) on GH3/GHWT/GH5 audio.
     */
    const double offset_db = 21.0 + 0.25 * (e->quality - 60); /* noise below masking level */
    const double margin_db = 8.0;   /* see transient_margin */
    const double rise_db = 6.0;     /* see pre_echo_rise */
    const double ath_cap = 14000.0; /* don't let the hearing threshold remove everything above this */

    double z[SUB];
    double bin_hz = (double)e->rate / 2.0 / SUB;

    e->mask_offset = (float)pow(10.0, -offset_db / 10.0);
    e->transient_margin = (float)pow(10.0, margin_db / 10.0);
    e->pre_echo_rise = (float)pow(10.0, rise_db / 10.0);
    e->sf_tolerance = 3;
    e->ms_noise = 0.5f;
    for (int c = 0; c < 2; c++)
        for (int b = 0; b < MAX_BANDS; b++)
            e->prev_noise[c][b] = 1e30f;

    for (int k = 0; k < SUB; k++)
        z[k] = bark((k + 0.5) * bin_hz);
    /* Schroeder spreading function */
    for (int k = 0; k < SUB; k++) {
        for (int j = 0; j < SUB; j++) {
            double dz = z[k] - z[j];
            double sf = 15.81 + 7.5 * (dz + 0.474) - 17.5 * sqrt(1.0 + (dz + 0.474) * (dz + 0.474));
            e->spread[k][j] = sf < -80 ? 0.0f : (float)pow(10.0, sf / 10.0);
        }
    }

    /* Power of one MDCT coefficient for a full scale sine, which we treat as 96 dB SPL */
    float in[WIN], out[SUB];
    double peak = 0;
    for (int n = 0; n < WIN; n++)
        in[n] = (float)sin(2 * M_PI * (10.5 * bin_hz) * n / e->rate);
    mdct(e, in, out);
    for (int k = 0; k < SUB; k++)
        if (out[k] * out[k] > peak) peak = out[k] * out[k];

    for (int k = 0; k < SUB; k++) {
        double f = (k + 0.5) * bin_hz;
        double a = ath_db(f < ath_cap ? f : ath_cap);
        e->ath[k] = (float)(peak * pow(10.0, (a - 96.0) / 10.0));
    }
}

static void init_mdct(XmaEncoder *e)
{
    for (int n = 0; n < WIN; n++)
        e->window[n] = (float)sin(M_PI * (n + 0.5) / WIN);
    for (int n = 0; n < FFT_N; n++) {
        /* pre-twiddle exp(-i pi n / SUB), post-twiddle exp(-i pi (k + 1/4) / SUB) */
        e->pre_re[n] = (float)cos(-M_PI * n / SUB);
        e->pre_im[n] = (float)sin(-M_PI * n / SUB);
        e->post_re[n] = (float)cos(-M_PI * (n + 0.25) / SUB);
        e->post_im[n] = (float)sin(-M_PI * (n + 0.25) / SUB);
    }
    for (int k = 0; k < FFT_N / 2; k++) {
        e->fft_re[k] = (float)cos(-2 * M_PI * k / FFT_N);
        e->fft_im[k] = (float)sin(-2 * M_PI * k / FFT_N);
    }
    int bits = 0;
    while ((1 << bits) < FFT_N) bits++;
    for (int i = 0; i < FFT_N; i++) {
        int r = 0;
        for (int b = 0; b < bits; b++)
            if (i & (1 << b)) r |= 1 << (bits - 1 - b);
        e->bitrev[i] = r;
    }
}

/* ------------------------------------------------------------------------- */
/* MDCT: X[k] = sum_n w[n] x[n] cos(pi/SUB (n + 1/2 + SUB/2)(k + 1/2))      */

static void fft(const XmaEncoder *e, float *re, float *im)
{
    for (int i = 0; i < FFT_N; i++) {
        int j = e->bitrev[i];
        if (j > i) {
            float t = re[i]; re[i] = re[j]; re[j] = t;
            t = im[i]; im[i] = im[j]; im[j] = t;
        }
    }
    for (int size = 2; size <= FFT_N; size <<= 1) {
        int half = size >> 1, step = FFT_N / size;
        for (int start = 0; start < FFT_N; start += size) {
            for (int k = 0; k < half; k++) {
                float wr = e->fft_re[k * step], wi = e->fft_im[k * step];
                int a = start + k, b = a + half;
                float tr = re[b] * wr - im[b] * wi;
                float ti = re[b] * wi + im[b] * wr;
                re[b] = re[a] - tr; im[b] = im[a] - ti;
                re[a] += tr;        im[a] += ti;
            }
        }
    }
}

static void mdct(const XmaEncoder *e, const float *in, float *out)
{
    float z[WIN], u[SUB], re[FFT_N], im[FFT_N];
    const int h = SUB / 2;
    for (int n = 0; n < WIN; n++)
        z[n] = in[n] * e->window[n];
    /* fold into a DCT-IV input */
    for (int n = 0; n < h; n++) {
        u[n]     = -z[3 * h - 1 - n] - z[3 * h + n];
        u[n + h] =  z[n] - z[SUB - 1 - n];
    }
    /* DCT-IV through a half-length complex FFT */
    for (int n = 0; n < FFT_N; n++) {
        float vr = u[2 * n], vi = u[SUB - 1 - 2 * n];
        re[n] = vr * e->pre_re[n] - vi * e->pre_im[n];
        im[n] = vr * e->pre_im[n] + vi * e->pre_re[n];
    }
    fft(e, re, im);
    for (int k = 0; k < FFT_N; k++) {
        float zr = re[k] * e->post_re[k] - im[k] * e->post_im[k];
        float zi = re[k] * e->post_im[k] + im[k] * e->post_re[k];
        out[2 * k] = zr;
        out[SUB - 1 - 2 * k] = -zi;
    }
}

/* ------------------------------------------------------------------------- */
/* Coefficient coding                                                        */

static int vec1_bits(const XmaEncoder *e, int v)
{
    if (v < HUFF_VEC1_SIZE - 1) return e->vec1_vlc[v].len;
    return e->vec1_vlc[HUFF_VEC1_SIZE - 1].len + large_val_bits(v - (HUFF_VEC1_SIZE - 1));
}

static void put_vec1(const XmaEncoder *e, BitWriter *bw, int v)
{
    if (v < HUFF_VEC1_SIZE - 1) {
        put_vlc(bw, e->vec1_vlc[v]);
    } else {
        put_vlc(bw, e->vec1_vlc[HUFF_VEC1_SIZE - 1]);
        put_large_val(bw, v - (HUFF_VEC1_SIZE - 1));
    }
}

static int pair_bits(const XmaEncoder *e, int a, int b)
{
    if (a < 16 && b < 16 && e->vec2_vlc[(a << 4) | b].len)
        return e->vec2_vlc[(a << 4) | b].len;
    return e->vec2_esc.len + vec1_bits(e, a) + vec1_bits(e, b);
}

static void put_pair(const XmaEncoder *e, BitWriter *bw, int a, int b)
{
    if (a < 16 && b < 16 && e->vec2_vlc[(a << 4) | b].len) {
        put_vlc(bw, e->vec2_vlc[(a << 4) | b]);
    } else {
        put_vlc(bw, e->vec2_esc);
        put_vec1(e, bw, a);
        put_vec1(e, bw, b);
    }
}

static void put_quad(const XmaEncoder *e, BitWriter *bw, const int *a)
{
    int direct = -1;
    if (a[0] < 16 && a[1] < 16 && a[2] < 16 && a[3] < 16)
        direct = e->vec4_index[(a[0] << 12) | (a[1] << 8) | (a[2] << 4) | a[3]];
    int esc_bits = e->vec4_esc.len + pair_bits(e, a[0], a[1]) + pair_bits(e, a[2], a[3]);
    if (direct >= 0 && e->vec4_vlc[direct].len <= esc_bits) {
        put_vlc(bw, e->vec4_vlc[direct]);
    } else {
        put_vlc(bw, e->vec4_esc);
        put_pair(e, bw, a[0], a[1]);
        put_pair(e, bw, a[2], a[3]);
    }
}

#define ESC_RUN_BITS 7 /* av_log2(SUB - 1) + 1 */

static int esc_run_bits(int run)
{
    if (run == 0) return 1;
    if (run <= 4) return 2 + 2;
    return 3 + ESC_RUN_BITS;
}

static void put_esc_run(BitWriter *bw, int run)
{
    if (run == 0) {
        put_bits(bw, 0, 1);
    } else if (run <= 4) {
        put_bits(bw, 2, 2);
        put_bits(bw, run - 1, 2);
    } else {
        put_bits(bw, 6, 3);
        put_bits(bw, run - 4, ESC_RUN_BITS);
    }
}

static void put_run_level(const XmaEncoder *e, BitWriter *bw, int t, int run, int level)
{
    int idx = -1;
    if (run < 160 && level < 64)
        idx = e->coef_index[t][run][level];
    int esc = e->coef_vlc[t][0].len + large_val_bits(level) + esc_run_bits(run);
    if (idx >= 0 && e->coef_vlc[t][idx].len <= esc) {
        put_vlc(bw, e->coef_vlc[t][idx]);
    } else {
        put_vlc(bw, e->coef_vlc[t][0]);
        put_large_val(bw, level);
        put_esc_run(bw, run);
    }
}

/*
 * Code one channel's quantized coefficients with the given run-level table.
 * Mirrors decode_coeffs: vector coding (groups of 4) continues until a group
 * contains a zero (for 128-sample subframes), then run-level coding follows.
 */
static void put_coefs_table(const XmaEncoder *e, BitWriter *bw, const int *q, int t)
{
    int cur = 0, rl_mode = 0;
    put_bits(bw, t, 1);
    while (!rl_mode && cur + 3 < SUB) {
        int a[4];
        for (int i = 0; i < 4; i++) a[i] = abs(q[cur + i]);
        put_quad(e, bw, a);
        for (int i = 0; i < 4; i++) {
            if (a[i]) put_bits(bw, q[cur + i] > 0, 1);
            else rl_mode = 1; /* more than (SUB >> 8) = 0 zeros in a row */
        }
        cur += 4;
    }
    if (cur < SUB) {
        int offset = cur;
        for (int k = cur; k < SUB; k++) {
            if (q[k]) {
                put_run_level(e, bw, t, k - offset, abs(q[k]));
                put_bits(bw, q[k] > 0, 1);
                offset = k + 1;
            }
        }
        if (offset < SUB)
            put_vlc(bw, e->coef_vlc[t][1]); /* end of block */
    }
}

static int coefs_bits_table(const XmaEncoder *e, const int *q, int t)
{
    BitWriter bw = { NULL, 0, 0 };
    put_coefs_table(e, &bw, q, t);
    return (int)bw.pos;
}

/* Returns the cheaper table, and its cost in *bits. */
static int choose_coef_table(const XmaEncoder *e, const int *q, int *bits)
{
    int b0 = coefs_bits_table(e, q, 0), b1 = coefs_bits_table(e, q, 1);
    if (bits) *bits = b0 <= b1 ? b0 : b1;
    return b0 <= b1 ? 0 : 1;
}

/* ------------------------------------------------------------------------- */
/* Scale factor coding                                                       */

#define SF_STEP   2   /* dB per scale factor unit, as in official files */
#define SF_START  (45 / SF_STEP)

static int clamp_int(int v, int lo, int hi)
{
    return v < lo ? lo : v > hi ? hi : v;
}

static void put_sf_dpcm(const XmaEncoder *e, BitWriter *bw, const int *sf)
{
    int prev = SF_START;
    put_bits(bw, SF_STEP - 1, 2);
    for (int b = 0; b < e->num_bands; b++) {
        put_vlc(bw, e->sf_vlc[sf[b] - prev + 60]);
        prev = sf[b];
    }
}

static void put_sf_rl(const XmaEncoder *e, BitWriter *bw, const int *old_sf, const int *sf)
{
    int i = 0;
    for (int b = 0; b < e->num_bands; b++) {
        int diff = sf[b] - old_sf[b];
        if (!diff) continue;
        int run = b - i, level = abs(diff);
        int idx = (run < 32 && level < 16) ? e->sfrl_index[run][level] : -1;
        if (idx >= 0) {
            put_vlc(bw, e->sfrl_vlc[idx]);
            put_bits(bw, diff > 0, 1);
        } else {
            put_vlc(bw, e->sfrl_vlc[0]);
            put_bits(bw, ((uint32_t)level << 6) | ((uint32_t)run << 1) | (diff > 0), 14);
        }
        i = b + 1;
    }
    if (i < e->num_bands)
        put_vlc(bw, e->sfrl_vlc[1]); /* end */
}

static int sf_rl_bits(const XmaEncoder *e, const int *old_sf, const int *sf)
{
    BitWriter bw = { NULL, 0, 0 };
    put_sf_rl(e, &bw, old_sf, sf);
    return (int)bw.pos;
}

/* ------------------------------------------------------------------------- */
/* Quantization                                                              */

/* Decoder gain: an MDCT coefficient x (from mdct() above, with input samples
   in -1..1) is reconstructed from q * 10^(exp / 20) * XMA_GAIN. */
#define XMA_GAIN (1.0f / 32768.0f)

static float step_size(int exp)
{
    return XMA_GAIN * powf(10.0f, exp / 20.0f);
}

static void quantize(const XmaEncoder *e, const float *y, const int *exps, int *q)
{
    for (int b = 0; b < e->num_bands; b++) {
        float inv = 1.0f / step_size(exps[b]);
        for (int k = e->band_start[b]; k < e->band_start[b + 1]; k++) {
            float v = fabsf(y[k]) * inv + 0.4054f;
            int m = v > 2.0e9f ? 2000000000 : (int)v;
            q[k] = y[k] < 0 ? -m : m;
        }
    }
}

/* Smallest scale (in SF_STEP units) at which a band quantizes to all zeros. */
static int zero_scale(const XmaEncoder *e, const float *y, int b)
{
    float peak = 0;
    for (int k = e->band_start[b]; k < e->band_start[b + 1]; k++)
        if (fabsf(y[k]) > peak) peak = fabsf(y[k]);
    if (peak <= 0) return -1000;
    /* need peak / step + 0.4054 < 1 */
    double exp_db = 20.0 * log10(peak / (0.5946 * XMA_GAIN));
    return (int)floor(exp_db / SF_STEP) + 1;
}

/* ------------------------------------------------------------------------- */
/* Frame encoding                                                            */

typedef struct {
    int reuse_sf;              /* decoder has scale factors from this frame */
    int sf[MAX_BANDS];         /* scale factors as the decoder sees them */
} ChannelState;

/*
 * Quantization target for one coded channel, or for both channels of an M/S
 * pair. (M/S channels must share step sizes, because the decoder applies the
 * M/S matrix to the quantized values and then scales each output channel by
 * its own step sizes.) Scales are in SF_STEP units: the step size of a band
 * is step_size(scale * SF_STEP).
 */
typedef struct {
    const float *y[2];         /* spectra quantized with these scales */
    int   ny;
    int   want[MAX_BANDS];     /* desired scale per band */
    int   zmin[MAX_BANDS];     /* smallest scale where the band quantizes to all zeros */
    int   nonzero;             /* some band is nonzero at the desired scale */
} Target;

static void make_target(const XmaEncoder *e, const float *noise,
                        const float *y0, const float *y1, Target *t)
{
    t->y[0] = y0;
    t->y[1] = y1;
    t->ny = y1 ? 2 : 1;
    t->nonzero = 0;
    for (int b = 0; b < e->num_bands; b++) {
        /* uniform quantizer noise is step^2 / 12 */
        double step = sqrt(12.0 * noise[b]);
        t->want[b] = (int)floor(20.0 * log10(step / XMA_GAIN) / SF_STEP);
        t->zmin[b] = zero_scale(e, y0, b);
        if (y1) {
            int z = zero_scale(e, y1, b);
            if (z > t->zmin[b]) t->zmin[b] = z;
        }
        if (t->want[b] < t->zmin[b]) t->nonzero = 1;
    }
}

static int is_zero_band(const Target *t, int b)
{
    return t->want[b] >= t->zmin[b];
}

/* Allowed noise power per coefficient for each band of one channel */
static void masking(const XmaEncoder *e, const float *power, float *noise, float extra)
{
    float mask[SUB];
    for (int k = 0; k < SUB; k++) {
        float m = 0;
        for (int j = 0; j < SUB; j++)
            m += power[j] * e->spread[k][j];
        m *= e->mask_offset * extra;
        mask[k] = m < e->ath[k] ? e->ath[k] : m;
    }
    /* Low bands are much wider than critical bands, so use the band's
       lowest masking level. */
    for (int b = 0; b < e->num_bands; b++) {
        float lowest = mask[e->band_start[b]];
        for (int k = e->band_start[b] + 1; k < e->band_start[b + 1]; k++)
            if (mask[k] < lowest) lowest = mask[k];
        noise[b] = lowest;
    }
}

/* Bits to code the target's spectra with the given exponents */
static int coef_cost(const XmaEncoder *e, const Target *t, const int *exps)
{
    int bits = 0, q[SUB];
    for (int i = 0; i < t->ny; i++) {
        int any = 0, b;
        quantize(e, t->y[i], exps, q);
        for (int k = 0; k < SUB && !any; k++) any = q[k] != 0;
        if (!any) continue;
        choose_coef_table(e, q, &b);
        bits += b;
    }
    return bits;
}

/* Scales for a fresh (DPCM coded) set: the wanted scales on nonzero bands.
   A zero band copies its neighbor when that keeps it zero, which is cheap
   to code. */
static void fresh_scales(const XmaEncoder *e, const Target *t, int tolerance, int *u)
{
    int first = 0;
    while (first < e->num_bands && is_zero_band(t, first)) first++;
    int ref = first < e->num_bands ? t->want[first] : 0;
    for (int b = 0; b < e->num_bands; b++) {
        int prev = b == 0 ? ref : u[b - 1];
        if (!is_zero_band(t, b)) {
            /* keep the previous band's scale if it's up to `tolerance` finer */
            int ok = prev <= t->want[b] && prev >= t->want[b] - tolerance;
            u[b] = ok ? prev : t->want[b];
        } else {
            u[b] = prev > t->zmin[b] ? prev : t->zmin[b];
        }
    }
}

static int dpcm_bits(const XmaEncoder *e, const int *u)
{
    int bits = 2, prev = u[0];
    for (int b = 0; b < e->num_bands; b++) {
        int d = clamp_int(u[b] - prev, -60, 60);
        bits += e->sf_vlc[d + 60].len;
        prev += d;
    }
    return bits;
}

/* Estimated bits to code a target from scratch */
static int target_cost(const XmaEncoder *e, const Target *t)
{
    if (!t->nonzero) return 0;
    int u[MAX_BANDS], exps[MAX_BANDS];
    fresh_scales(e, t, 0, u);
    for (int b = 0; b < e->num_bands; b++) exps[b] = u[b] * SF_STEP;
    return dpcm_bits(e, u) + coef_cost(e, t, exps);
}

/* Exponents for scale factors sf, where scale = sf - base */
static void sf_exps(const XmaEncoder *e, const int *sf, int base, int *exps)
{
    for (int b = 0; b < e->num_bands; b++)
        exps[b] = (sf[b] - base) * SF_STEP;
}

/* DPCM coded scale factors for scales u, starting from SF_START */
static void dpcm_sf(const XmaEncoder *e, const int *u, int *sf, int *base)
{
    *base = SF_START - u[0];
    int prev = SF_START;
    for (int b = 0; b < e->num_bands; b++) {
        sf[b] = clamp_int(u[b] + *base, prev - 60, prev + 60);
        prev = sf[b];
    }
}

static int sf_dpcm_bits(const XmaEncoder *e, const int *sf)
{
    BitWriter bw = { NULL, 0, 0 };
    put_sf_dpcm(e, &bw, sf);
    return (int)bw.pos;
}

/*
 * Choose a channel's scale factors for a target with nonzero bands.
 * Sets *resend, sf (the values the decoder will have afterwards), and *base
 * (so that scale = sf - base).
 *
 * Changing scale factors costs bits, so a band may keep a scale that is up
 * to `tolerance` units finer than wanted (costing more coefficient bits, but
 * never adding noise). Each tolerance is tried and the cheapest result wins.
 */
static void choose_sf(const XmaEncoder *e, const Target *t, const ChannelState *s,
                      int *resend, int *sf, int *base)
{
    int u[MAX_BANDS], exps[MAX_BANDS], cand[MAX_BANDS], cand_base, bits;
    int best = INT_MAX;
    const int nb = e->num_bands;

    if (!s->reuse_sf) {
        /* first scale factors in this frame: DPCM across bands */
        for (int tol = 0; tol <= e->sf_tolerance; tol++) {
            fresh_scales(e, t, tol, u);
            dpcm_sf(e, u, cand, &cand_base);
            sf_exps(e, cand, cand_base, exps);
            bits = sf_dpcm_bits(e, cand) + coef_cost(e, t, exps);
            if (bits < best) {
                best = bits;
                memcpy(sf, cand, sizeof(cand));
                *base = cand_base;
            }
        }
        *resend = 1;
        return;
    }

    /* Keep the previous scale factors, at the coarsest level where every
       nonzero band is at least as fine as wanted. */
    int offset = INT_MAX;
    for (int b = 0; b < nb; b++)
        if (!is_zero_band(t, b) && t->want[b] - s->sf[b] < offset)
            offset = t->want[b] - s->sf[b];
    sf_exps(e, s->sf, -offset, exps);
    best = coef_cost(e, t, exps);
    *resend = 0;
    memcpy(sf, s->sf, MAX_BANDS * sizeof(int));
    *base = -offset;

    /* Or send differences from the previous scale factors. Only differences
       between bands matter (the quant step sets the level), so pick the
       alignment where the most bands can stay unchanged. */
    for (int tol = 0; tol <= e->sf_tolerance; tol++) {
        int best_count = -1, base1 = 0;
        for (int i = 0; i < nb; i++) {
            if (is_zero_band(t, i)) continue;
            int bc = s->sf[i] - t->want[i], count = 0;
            for (int b = 0; b < nb; b++) {
                int old = s->sf[b] - bc;
                if (is_zero_band(t, b)) count += old >= t->zmin[b];
                else count += old <= t->want[b] && old >= t->want[b] - tol;
            }
            if (count > best_count) {
                best_count = count;
                base1 = bc;
            }
        }
        for (int b = 0; b < nb; b++) {
            int old = s->sf[b] - base1, v;
            if (is_zero_band(t, b))
                v = old >= t->zmin[b] ? s->sf[b] : t->zmin[b] + base1;
            else
                v = (old <= t->want[b] && old >= t->want[b] - tol) ? s->sf[b] : t->want[b] + base1;
            cand[b] = clamp_int(v, s->sf[b] - 255, s->sf[b] + 255);
        }
        sf_exps(e, cand, base1, exps);
        bits = coef_cost(e, t, exps) + sf_rl_bits(e, s->sf, cand);
        if (bits < best) {
            best = bits;
            *resend = 1;
            memcpy(sf, cand, sizeof(cand));
            *base = base1;
        }
    }
}

/* Scale factors for the second channel of an M/S pair, giving the same
   exponents as the first channel (sf0, base0). */
static void link_sf(const XmaEncoder *e, const int *sf0, int base0, const ChannelState *s,
                    int *resend, int *sf, int *base)
{
    int d;
    if (s->reuse_sf) {
        int lo = INT_MAX, hi = INT_MIN;
        for (int b = 0; b < e->num_bands; b++) {
            int diff = s->sf[b] - sf0[b];
            if (diff < lo) lo = diff;
            if (diff > hi) hi = diff;
        }
        if (lo == hi) {
            /* same shape already */
            *resend = 0;
            memcpy(sf, s->sf, MAX_BANDS * sizeof(int));
            *base = base0 + lo;
            return;
        }
        d = (lo + hi) / 2;
    } else {
        d = SF_START - sf0[0];
    }
    *resend = 1;
    for (int b = 0; b < e->num_bands; b++)
        sf[b] = sf0[b] + d;
    *base = base0 + d;
}

static int max_sf(const XmaEncoder *e, const int *sf)
{
    int m = sf[0];
    for (int b = 1; b < e->num_bands; b++)
        if (sf[b] > m) m = sf[b];
    return m;
}

static void put_quant_step(BitWriter *bw, int qs)
{
    int delta = qs - 90;
    if (delta > -32 && delta < 31) {
        put_bits(bw, (uint32_t)delta & 0x3F, 6);
        return;
    }
    int rest;
    if (delta >= 31) {
        put_bits(bw, 31, 6);
        rest = delta - 31;
    } else {
        put_bits(bw, 0x20, 6); /* -32 */
        rest = -32 - delta;
    }
    while (rest >= 31) {
        put_bits(bw, 31, 5);
        rest -= 31;
    }
    put_bits(bw, rest, 5);
}

static int bit_length(int v)
{
    int n = 0;
    while (v) { n++; v >>= 1; }
    return n;
}

/*
 * Encode one subframe. x[c] are the MDCT coefficients of the input channels.
 */
static void encode_subframe(const XmaEncoder *e, BitWriter *bw, float *const x[2],
                            float (*const noise)[MAX_BANDS], int sub_index, ChannelState *st)
{
    const int nch = e->channels;
    float m[SUB], s[SUB];
    Target tgt[2];
    int ms = 0;

    make_target(e, noise[0], x[0], NULL, &tgt[0]);

    if (nch == 2) {
        /* Compare L/R against M/S. The decoder computes L = M - S, R = M + S.
           Noise in M and S adds up in both L and R. */
        Target ms_tgt;
        float ms_noise[MAX_BANDS];
        for (int k = 0; k < SUB; k++) {
            m[k] = (x[0][k] + x[1][k]) * 0.5f;
            s[k] = (x[1][k] - x[0][k]) * 0.5f;
        }
        for (int b = 0; b < e->num_bands; b++)
            ms_noise[b] = e->ms_noise * (noise[0][b] < noise[1][b] ? noise[0][b] : noise[1][b]);
        make_target(e, noise[1], x[1], NULL, &tgt[1]);
        make_target(e, ms_noise, m, s, &ms_tgt);
        int lr_cost = target_cost(e, &tgt[0]) + target_cost(e, &tgt[1]);
        int ms_cost = target_cost(e, &ms_tgt);
        if (ms_cost <= lr_cost) {
            ms = 1;
            tgt[0] = ms_tgt;
        }
    }

    /* scale factors and exponents for each coded channel */
    int resend[2] = { 0, 0 }, sf[2][MAX_BANDS], base[2] = { 0, 0 }, qs[2] = { 0, 0 };
    int active[2] = { 0, 0 }; /* channel's step sizes matter */
    if (ms) {
        if (tgt[0].nonzero) {
            choose_sf(e, &tgt[0], &st[0], &resend[0], sf[0], &base[0]);
            link_sf(e, sf[0], base[0], &st[1], &resend[1], sf[1], &base[1]);
            active[0] = active[1] = 1;
        }
    } else {
        for (int c = 0; c < nch; c++) {
            if (tgt[c].nonzero) {
                choose_sf(e, &tgt[c], &st[c], &resend[c], sf[c], &base[c]);
                active[c] = 1;
            } else {
                /* nothing to code; keep or send the cheapest scale factors */
                resend[c] = !st[c].reuse_sf;
                for (int b = 0; b < e->num_bands; b++)
                    sf[c][b] = st[c].reuse_sf ? st[c].sf[b] : SF_START;
            }
        }
    }

    /* shared quantization step, plus a modifier per channel */
    int qs_common = INT_MAX;
    for (int c = 0; c < nch; c++) {
        if (!active[c]) continue;
        qs[c] = (max_sf(e, sf[c]) - base[c]) * SF_STEP;
        if (qs[c] < qs_common) qs_common = qs[c];
    }
    int mod[2] = { 0, 0 };
    for (int c = 0; c < nch; c++) {
        if (!active[c]) continue;
        mod[c] = qs[c] - qs_common;
        if (mod[c] > 128) mod[c] = 128; /* finer than wanted, but codable */
        qs[c] = qs_common + mod[c];
    }

    /* quantize with the exponents the decoder will use */
    int q[2][SUB], transmit[2] = { 0, 0 }, any = 0;
    for (int c = 0; c < nch; c++) {
        if (!active[c]) continue;
        int exps[MAX_BANDS], mx = max_sf(e, sf[c]);
        for (int b = 0; b < e->num_bands; b++)
            exps[b] = qs[c] - (mx - sf[c][b]) * SF_STEP;
        quantize(e, ms ? (c ? s : m) : x[c], exps, q[c]);
        for (int k = 0; k < SUB && !transmit[c]; k++) transmit[c] = q[c][k] != 0;
        any |= transmit[c];
    }

    /* subframe header */
    put_bits(bw, 0, 1); /* no extended header */
    put_bits(bw, 0, 1); /* reserved */
    if (nch == 2) {
        put_bits(bw, 0, 1); /* no multichannel transform */
        if (ms) {
            put_bits(bw, 0, 1); /* M/S */
            put_bits(bw, 1, 1); /* on for all bands */
        } else {
            put_bits(bw, 1, 1); /* no transform */
            put_bits(bw, 0, 1);
        }
    }
    for (int c = 0; c < nch; c++)
        put_bits(bw, transmit[c], 1);
    if (!any)
        return;

    put_bits(bw, 0, 1); /* number of vector coded coefficients not transmitted */
    put_quant_step(bw, qs_common);
    if (nch == 2) {
        int max_mod = mod[0] > mod[1] ? mod[0] : mod[1];
        int mod_len = max_mod > 1 ? bit_length(max_mod - 1) : 0;
        put_bits(bw, mod_len, 3);
        for (int c = 0; c < nch; c++) {
            put_bits(bw, mod[c] > 0, 1);
            if (mod[c] > 0 && mod_len)
                put_bits(bw, mod[c] - 1, mod_len);
        }
    }

    for (int c = 0; c < nch; c++) {
        if (sub_index > 0)
            put_bits(bw, resend[c], 1);
        if (resend[c]) {
            if (!st[c].reuse_sf) put_sf_dpcm(e, bw, sf[c]);
            else put_sf_rl(e, bw, st[c].sf, sf[c]);
            memcpy(st[c].sf, sf[c], sizeof(st[c].sf));
            st[c].reuse_sf = 1;
        }
    }

    for (int c = 0; c < nch; c++) {
        if (!transmit[c]) continue;
        int t = choose_coef_table(e, q[c], NULL);
        put_coefs_table(e, bw, q[c], t);
    }
}

/* prev_noise: allowed noise of the previous subframe (updated to this frame's last) */
static int encode_frame_bits(const XmaEncoder *e, uint8_t *buf, int trim_start, int trim_end,
                             float extra, float (*prev_noise)[MAX_BANDS])
{
    BitWriter bw = { buf, 0, FRAME_BUF_BYTES * 8 };
    float x[2][NSUB][SUB];
    ChannelState st[2];

    memset(buf, 0, FRAME_BUF_BYTES);
    memset(st, 0, sizeof(st));
    /* The decoder's inverse MDCT is the negative of the standard one
       (FFmpeg av_tx), so negate to get back the input signal. */
    for (int c = 0; c < e->channels; c++) {
        for (int s = 0; s < NSUB; s++) {
            mdct(e, e->pcm[c] + s * SUB, x[c][s]);
            for (int k = 0; k < SUB; k++) x[c][s][k] = -x[c][s][k];
        }
    }

    put_bits(&bw, 0, 15); /* length, filled in below */
    /* tile header: same layout for all channels, four 128-sample subframes */
    put_bits(&bw, 1, 1);
    for (int s = 0; s < NSUB - 1; s++)
        put_bits(&bw, 3, 2);
    if (e->channels > 1)
        put_bits(&bw, 0, 1); /* no post-processing transform */
    put_bits(&bw, 0, 8); /* DRC gain */
    if (trim_start || trim_end) {
        put_bits(&bw, 1, 1);
        put_bits(&bw, trim_start ? 1 : 0, 1);
        if (trim_start) put_bits(&bw, trim_start, 10);
        put_bits(&bw, trim_end ? 1 : 0, 1);
        if (trim_end) put_bits(&bw, trim_end, 10);
    } else {
        put_bits(&bw, 0, 1);
    }

    /* Masking is computed over the whole frame, so that step sizes (and
       scale factors) stay stable across subframes. A subframe much quieter
       than the frame, such as just before an attack, gets its own lower
       level to avoid pre-echo. */
    float noise[NSUB][2][MAX_BANDS];
    for (int c = 0; c < e->channels; c++) {
        float frame_power[SUB] = { 0 }, power[SUB], frame_noise[MAX_BANDS];
        for (int s = 0; s < NSUB; s++) {
            for (int k = 0; k < SUB; k++) {
                power[k] = x[c][s][k] * x[c][s][k];
                frame_power[k] += power[k] / NSUB;
            }
            masking(e, power, noise[s][c], extra);
        }
        masking(e, frame_power, frame_noise, extra);
        for (int s = 0; s < NSUB; s++) {
            for (int b = 0; b < e->num_bands; b++) {
                float n = noise[s][c][b] * e->transient_margin;
                if (frame_noise[b] < n) n = frame_noise[b];
                /* pre-echo control: allowed noise can only rise gradually,
                   so attacks are coded finely */
                float limit = prev_noise[c][b] * e->pre_echo_rise;
                if (limit < n) n = limit;
                noise[s][c][b] = n;
                prev_noise[c][b] = n;
            }
        }
    }

    for (int s = 0; s < NSUB; s++) {
        float *xs[2] = { x[0][s], x[1][s] };
        encode_subframe(e, &bw, xs, noise[s], s, st);
    }

    put_bits(&bw, 1, 1); /* always 1 in official files */
    put_bits(&bw, 0, 1); /* trailer: set later when packing */

    int len = (int)bw.pos;
    if (len >= (1 << 15))
        return len;
    for (int i = 0; i < 15; i++) {
        if ((len >> (14 - i)) & 1) buf[i >> 3] |= 0x80 >> (i & 7);
    }
    return len;
}

static int append_frame(XmaEncoder *e, const uint8_t *buf, int bits)
{
    size_t need = (size_t)((e->fdata_bits + bits + 7) / 8) + 1;
    if (need > e->fdata_cap) {
        size_t cap = e->fdata_cap * 2;
        if (cap < need) cap = need;
        uint8_t *p = realloc(e->fdata, cap);
        if (!p) return -1;
        memset(p + e->fdata_cap, 0, cap - e->fdata_cap);
        e->fdata = p;
        e->fdata_cap = cap;
    }
    if ((size_t)e->nframes >= e->flen_cap) {
        size_t cap = e->flen_cap * 2;
        uint16_t *p = realloc(e->flen, cap * sizeof(uint16_t));
        if (!p) return -1;
        e->flen = p;
        e->flen_cap = cap;
    }
    for (int i = 0; i < bits; i++) {
        if ((buf[i >> 3] >> (7 - (i & 7))) & 1) {
            int64_t pos = e->fdata_bits + i;
            e->fdata[pos >> 3] |= 0x80 >> (pos & 7);
        }
    }
    e->fdata_bits += bits;
    e->flen[e->nframes++] = (uint16_t)bits;
    return 0;
}

static int encode_frame(XmaEncoder *e, int trim_start, int trim_end)
{
    uint8_t buf[FRAME_BUF_BYTES];
    float extra = 1.0f, prev_noise[2][MAX_BANDS];
    int bits;
    for (;;) {
        memcpy(prev_noise, e->prev_noise, sizeof(prev_noise));
        bits = encode_frame_bits(e, buf, trim_start, trim_end, extra, prev_noise);
        if (bits <= MAX_FRAME_BITS) break;
        extra *= 2.0f; /* rare: allow more noise until the frame fits */
    }
    memcpy(e->prev_noise, prev_noise, sizeof(prev_noise));
    if (append_frame(e, buf, bits)) return -1;
    /* drop the oldest frame's worth of input */
    for (int c = 0; c < e->channels; c++)
        memmove(e->pcm[c], e->pcm[c] + SPF, (e->pcm_count - SPF) * sizeof(float));
    e->pcm_count -= SPF;
    return 0;
}

/* ------------------------------------------------------------------------- */
/* Public encoder API                                                        */

#define PCM_CAP (FRAME_INPUT + SPF)

XmaEncoder *xma_encoder_new(int channels, int sample_rate, int quality)
{
    if (channels < 1 || channels > 2 || sample_rate <= 0) return NULL;
    XmaEncoder *e = calloc(1, sizeof(XmaEncoder));
    if (!e) return NULL;
    e->channels = channels;
    e->rate = sample_rate;
    e->quality = clamp_int(quality, 1, 100);
    e->vec4_index = malloc(65536 * sizeof(int16_t));
    e->pcm[0] = calloc(PCM_CAP, sizeof(float));
    e->pcm[1] = calloc(PCM_CAP, sizeof(float));
    e->fdata_cap = 1 << 16;
    e->fdata = calloc(e->fdata_cap, 1);
    e->flen_cap = 1024;
    e->flen = malloc(e->flen_cap * sizeof(uint16_t));
    if (!e->vec4_index || !e->pcm[0] || !e->pcm[1] || !e->fdata || !e->flen) {
        xma_encoder_free(e);
        return NULL;
    }
    init_tables(e);
    init_bands(e);
    init_mdct(e);
    init_psy(e);
    e->pcm_count = PREROLL;
    return e;
}

void xma_encoder_free(XmaEncoder *e)
{
    if (!e) return;
    free(e->vec4_index);
    free(e->pcm[0]);
    free(e->pcm[1]);
    free(e->fdata);
    free(e->flen);
    free(e);
}

int xma_encoder_push(XmaEncoder *e, const float *samples, int count)
{
    if (e->finished) return -1;
    int i = 0;
    while (i < count) {
        int n = count - i;
        if (n > PCM_CAP - e->pcm_count) n = PCM_CAP - e->pcm_count;
        for (int j = 0; j < n; j++)
            for (int c = 0; c < e->channels; c++)
                e->pcm[c][e->pcm_count + j] = samples[(size_t)(i + j) * e->channels + c];
        e->pcm_count += n;
        i += n;
        e->samples_in += n;
        while (e->pcm_count >= FRAME_INPUT)
            if (encode_frame(e, e->nframes == 0 ? SPF : 0, 0)) return -1;
    }
    return 0;
}

int xma_encoder_finish(XmaEncoder *e)
{
    if (e->finished) return -1;
    e->finished = 1;
    /* fewest frames that decode to at least n samples */
    int64_t n = e->samples_in;
    int64_t extra = DECODER_SKIP - TAIL;
    int64_t total = (n + extra + SPF - 1) / SPF;
    while (e->nframes < total) {
        for (int c = 0; c < e->channels; c++)
            memset(e->pcm[c] + e->pcm_count, 0, (FRAME_INPUT - e->pcm_count) * sizeof(float));
        e->pcm_count = FRAME_INPUT;
        int last = e->nframes == total - 1;
        int trim_end = last ? (int)(total * SPF - extra - n) : 0;
        if (last) e->trim_end = trim_end;
        if (encode_frame(e, e->nframes == 0 ? SPF : 0, trim_end)) return -1;
    }
    return 0;
}

int64_t xma_encoder_samples(const XmaEncoder *e) { return e->samples_in; }
int64_t xma_encoder_frames(const XmaEncoder *e) { return e->nframes; }
int xma_encoder_trim_end(const XmaEncoder *e) { return e->trim_end; }

void xma_free(void *p) { free(p); }

/* ------------------------------------------------------------------------- */
/* Packing frames into packets                                               */

typedef struct {
    int64_t packet;   /* packet index (relative) where the frame starts */
    int     bit;      /* bit offset within that packet's payload */
} FramePos;

/*
 * Lay out frames [f0, f1) of one stream into packets starting at packet 0,
 * allowing at most max_frames frame starts per packet. Fills pos (if not NULL)
 * and returns the number of packets used.
 */
static int64_t layout_frames(const XmaEncoder *e, int64_t f0, int64_t f1,
                             int max_frames, FramePos *pos)
{
    int64_t packet = 0;
    int64_t bit = 0;
    int started = 0;
    for (int64_t f = f0; f < f1; f++) {
        if (started == max_frames || bit >= PAYLOAD_BITS) {
            packet++;
            bit = 0;
            started = 0;
        }
        if (pos) {
            pos[f - f0].packet = packet;
            pos[f - f0].bit = (int)bit;
        }
        started++;
        bit += e->flen[f];
        while (bit > PAYLOAD_BITS) {
            packet++;
            bit -= PAYLOAD_BITS;
            started = 0;
        }
    }
    return packet + 1;
}

/* Bit offset of each frame within the encoder's frame data */
static int64_t *frame_offsets(const XmaEncoder *e)
{
    int64_t *off = malloc((size_t)(e->nframes + 1) * sizeof(int64_t));
    if (!off) return NULL;
    off[0] = 0;
    for (int64_t f = 0; f < e->nframes; f++)
        off[f + 1] = off[f] + e->flen[f];
    return off;
}

typedef struct {
    int      stream;
    int64_t  key;            /* first frame with data in this packet */
    int      frame_count;
    int      first_offset;   /* 0x7FFF if no frame starts here */
    uint8_t  payload[PAYLOAD_BYTES];
} Packet;

static void copy_bits(uint8_t *dst, int64_t dst_bit, const uint8_t *src, int64_t src_bit, int64_t n,
                      int set_last, int last_value)
{
    for (int64_t i = 0; i < n; i++) {
        int v;
        if (set_last && i == n - 1) v = last_value;
        else v = (src[(src_bit + i) >> 3] >> (7 - ((src_bit + i) & 7))) & 1;
        int64_t d = dst_bit + i;
        if (v) dst[d >> 3] |= 0x80 >> (d & 7);
        else dst[d >> 3] &= ~(0x80 >> (d & 7));
    }
}

/*
 * Write frames [f0, f1) of one stream into packets (already allocated and
 * filled with padding). Trailer bit of each frame is 1 if the next frame
 * starts in the same packet as this one.
 */
static void write_frames(const XmaEncoder *e, const int64_t *offsets, int64_t f0, int64_t f1,
                         const FramePos *pos, Packet *packets, int64_t npackets)
{
    for (int64_t i = 0; i < npackets; i++) {
        packets[i].frame_count = 0;
        packets[i].first_offset = 0x7FFF;
        packets[i].key = -1;
    }
    for (int64_t f = f0; f < f1; f++) {
        const FramePos *p = &pos[f - f0];
        int trailer = (f + 1 < f1) && pos[f + 1 - f0].packet == p->packet;
        Packet *pk = &packets[p->packet];
        if (pk->frame_count == 0) pk->first_offset = p->bit;
        pk->frame_count++;
        if (pk->key < 0) pk->key = f - f0;

        int64_t remaining = e->flen[f], src = offsets[f];
        int64_t packet = p->packet, bit = p->bit;
        while (remaining > 0) {
            int64_t n = PAYLOAD_BITS - bit;
            if (n > remaining) n = remaining;
            copy_bits(packets[packet].payload, bit, e->fdata, src, n,
                      n == remaining, trailer);
            if (packets[packet].key < 0) packets[packet].key = f - f0;
            remaining -= n;
            src += n;
            packet++;
            bit = 0;
        }
    }
}

static int grow(uint8_t **buf, size_t *cap, size_t need)
{
    if (need <= *cap) return 0;
    size_t c = *cap ? *cap * 2 : 1 << 20;
    while (c < need) c *= 2;
    uint8_t *p = realloc(*buf, c);
    if (!p) return -1;
    *buf = p;
    *cap = c;
    return 0;
}

static int compare_packets(const void *a, const void *b)
{
    const Packet *x = *(Packet *const *)a, *y = *(Packet *const *)b;
    if (x->key != y->key) return x->key < y->key ? -1 : 1;
    if (x->stream != y->stream) return x->stream - y->stream;
    return x < y ? -1 : x > y; /* keep each stream's packets in order */
}

#define XMA2_MAX_FRAMES_PER_PACKET 63
#define XMA1_MAX_FRAMES_PER_PACKET 171

int xma2_pack(XmaEncoder *const *encs, int num_streams, int packets_per_block,
              uint8_t **out, size_t *out_len, uint32_t **seek, size_t *seek_len)
{
    *out = NULL; *out_len = 0; *seek = NULL; *seek_len = 0;
    if (num_streams < 1 || packets_per_block < num_streams) return -1;
    int64_t nframes = encs[0]->nframes;
    for (int s = 1; s < num_streams; s++)
        if (encs[s]->nframes != nframes) return -1;

    int ret = -1;
    size_t out_cap = 0, seek_cap = 0;
    int64_t **offsets = calloc(num_streams, sizeof(int64_t *));
    FramePos **pos = calloc(num_streams, sizeof(FramePos *));
    Packet *packets = malloc((size_t)packets_per_block * sizeof(Packet));
    Packet **order = malloc((size_t)packets_per_block * sizeof(Packet *));
    int64_t *stream_packets = calloc(num_streams, sizeof(int64_t));
    if (!offsets || !pos || !packets || !order || !stream_packets) goto done;
    for (int s = 0; s < num_streams; s++) {
        offsets[s] = frame_offsets(encs[s]);
        pos[s] = malloc((size_t)(nframes + 1) * sizeof(FramePos));
        if (!offsets[s] || !pos[s]) goto done;
    }

    int64_t f0 = 0;
    while (f0 < nframes) {
        /* find how many frames (per stream) fit in this block */
        int64_t n = 0;
        while (f0 + n < nframes) {
            int64_t total = 0;
            for (int s = 0; s < num_streams; s++)
                total += layout_frames(encs[s], f0, f0 + n + 1, XMA2_MAX_FRAMES_PER_PACKET, NULL);
            if (total > packets_per_block) break;
            n++;
        }
        if (n == 0) goto done;

        /* lay out each stream's packets */
        int64_t count = 0;
        for (int s = 0; s < num_streams; s++) {
            int64_t np = layout_frames(encs[s], f0, f0 + n, XMA2_MAX_FRAMES_PER_PACKET, pos[s]);
            for (int64_t i = 0; i < np; i++) {
                packets[count + i].stream = s;
                memset(packets[count + i].payload, 0xFF, PAYLOAD_BYTES);
            }
            write_frames(encs[s], offsets[s], f0, f0 + n, pos[s], &packets[count], np);
            stream_packets[s] = np;
            count += np;
        }

        /* first packet of each stream comes first, in stream order; the rest
           are ordered by when the decoder needs them */
        int64_t idx = 0;
        for (int s = 0; s < num_streams; s++) {
            order[s] = &packets[idx];
            idx += stream_packets[s];
        }
        int64_t m = num_streams;
        idx = 0;
        for (int s = 0; s < num_streams; s++) {
            for (int64_t i = 1; i < stream_packets[s]; i++)
                order[m++] = &packets[idx + i];
            idx += stream_packets[s];
        }
        qsort(order + num_streams, (size_t)(count - num_streams), sizeof(Packet *), compare_packets);

        /* Every block but the last is padded to full size with all-zero
           packets, as in official files. */
        f0 += n;
        int64_t block_size = f0 < nframes ? packets_per_block : count;

        size_t base = *out_len;
        if (grow(out, &out_cap, base + (size_t)block_size * PACKET_BYTES)) goto done;
        memset(*out + base, 0, (size_t)block_size * PACKET_BYTES);
        for (int64_t i = 0; i < count; i++) {
            Packet *p = order[i];
            /* skip = packets of other streams before this stream's next packet;
               the next block starts with each stream's packet in order */
            int64_t next = block_size + p->stream;
            for (int64_t j = i + 1; j < count; j++)
                if (order[j]->stream == p->stream) { next = j; break; }
            int64_t skip = next - i - 1;
            if (skip > 255) goto done;
            /* metadata is 1, or 0 in packets where no frame starts */
            uint32_t header = ((uint32_t)p->frame_count << 26)
                            | ((uint32_t)p->first_offset << 11)
                            | ((p->frame_count ? 1u : 0u) << 8)
                            | (uint32_t)skip;
            uint8_t *dst = *out + base + (size_t)i * PACKET_BYTES;
            dst[0] = header >> 24; dst[1] = header >> 16; dst[2] = header >> 8; dst[3] = header;
            memcpy(dst + 4, p->payload, PAYLOAD_BYTES);
        }
        *out_len = base + (size_t)block_size * PACKET_BYTES;

        if (*seek_len + 1 > seek_cap) {
            size_t c = seek_cap ? seek_cap * 2 : 256;
            uint32_t *p = realloc(*seek, c * sizeof(uint32_t));
            if (!p) goto done;
            *seek = p;
            seek_cap = c;
        }
        (*seek)[(*seek_len)++] = (uint32_t)(f0 * SPF);
    }
    ret = 0;

done:
    if (offsets) for (int s = 0; s < num_streams; s++) free(offsets[s]);
    if (pos) for (int s = 0; s < num_streams; s++) free(pos[s]);
    free(offsets);
    free(pos);
    free(packets);
    free(order);
    free(stream_packets);
    if (ret) {
        free(*out); *out = NULL; *out_len = 0;
        free(*seek); *seek = NULL; *seek_len = 0;
    }
    return ret;
}

int xma1_pack(const XmaEncoder *e, uint8_t **out, size_t *out_len,
              uint32_t **seek, size_t *seek_len, uint32_t *last_frame_bit)
{
    *out = NULL; *out_len = 0; *seek = NULL; *seek_len = 0; *last_frame_bit = 0;
    int64_t nframes = e->nframes;
    int ret = -1;
    int64_t *offsets = frame_offsets(e);
    FramePos *pos = malloc((size_t)(nframes + 1) * sizeof(FramePos));
    Packet *packets = NULL;
    if (!offsets || !pos) goto done;

    int64_t np = layout_frames(e, 0, nframes, XMA1_MAX_FRAMES_PER_PACKET, pos);
    packets = malloc((size_t)np * sizeof(Packet));
    *out = malloc((size_t)np * PACKET_BYTES);
    *seek = malloc((size_t)np * sizeof(uint32_t));
    if (!packets || !*out || !*seek) goto done;
    for (int64_t i = 0; i < np; i++) {
        packets[i].stream = 0;
        memset(packets[i].payload, 0, PAYLOAD_BYTES);
    }
    write_frames(e, offsets, 0, nframes, pos, packets, np);

    int64_t frames_before = 0;
    for (int64_t i = 0; i < np; i++) {
        uint32_t header = ((uint32_t)(i & 15) << 28)
                        | (2u << 26)
                        | ((uint32_t)packets[i].first_offset << 11);
        uint8_t *dst = *out + (size_t)i * PACKET_BYTES;
        dst[0] = header >> 24; dst[1] = header >> 16; dst[2] = header >> 8; dst[3] = header;
        memcpy(dst + 4, packets[i].payload, PAYLOAD_BYTES);
        (*seek)[i] = (uint32_t)(frames_before * SPF);
        frames_before += packets[i].frame_count;
    }
    *out_len = (size_t)np * PACKET_BYTES;
    *seek_len = (size_t)np;
    if (nframes > 0) {
        const FramePos *p = &pos[nframes - 1];
        *last_frame_bit = (uint32_t)(p->packet * PACKET_BYTES * 8 + 32 + p->bit);
    }
    ret = 0;

done:
    free(offsets);
    free(pos);
    free(packets);
    if (ret) {
        free(*out); *out = NULL; *out_len = 0;
        free(*seek); *seek = NULL; *seek_len = 0;
    }
    return ret;
}
