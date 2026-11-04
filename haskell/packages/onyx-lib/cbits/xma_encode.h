#ifndef ONYX_XMA_ENCODE_H
#define ONYX_XMA_ENCODE_H

#include <stddef.h>
#include <stdint.h>

/*
 * Encoder for XMA1/XMA2 audio (the Xbox 360 hardware codec, which uses the
 * WMA Pro bitstream). Each encoder handles one mono or stereo stream.
 * Multichannel audio is made by running one encoder per stereo pair and then
 * interleaving their packets with xma2_pack.
 */

typedef struct XmaEncoder XmaEncoder;

/* channels: 1 or 2. quality: 1 to 100 (like xma2encode, 60 is a good default).
   Returns NULL on invalid arguments or allocation failure. */
XmaEncoder *xma_encoder_new(int channels, int sample_rate, int quality);
void xma_encoder_free(XmaEncoder *enc);

/* Add samples (interleaved, nominal range -1 to 1). count is per channel.
   Returns 0 on success, nonzero on failure. */
int xma_encoder_push(XmaEncoder *enc, const float *samples, int count);

/* Encode the remaining audio. Must be called once, after all samples are pushed. */
int xma_encoder_finish(XmaEncoder *enc);

/* Number of input samples (per channel) pushed so far. */
int64_t xma_encoder_samples(const XmaEncoder *enc);
/* Number of XMA frames (512 samples each) encoded so far. */
int64_t xma_encoder_frames(const XmaEncoder *enc);
/* Samples trimmed from the end of the last frame (after finishing). */
int xma_encoder_trim_end(const XmaEncoder *enc);

/*
 * Pack finished encoders into XMA2 packets, in blocks of packets_per_block
 * 2048-byte packets (16 = 32 KB blocks as used by FSB4). All encoders must
 * have the same frame count. On success, *out holds the packet data and
 * *seek holds one entry per block: the number of samples (frames * 512)
 * decoded by the end of that block. Free both with xma_free.
 */
int xma2_pack(XmaEncoder *const *encs, int num_streams, int packets_per_block,
              uint8_t **out, size_t *out_len, uint32_t **seek, size_t *seek_len);

/*
 * Pack a finished encoder into XMA1 packets. *seek holds one entry per packet:
 * the number of samples (frames * 512) decoded before that packet.
 * *last_frame_bit is the bit position of the last frame relative to the
 * start of the data (counting packet headers).
 */
int xma1_pack(const XmaEncoder *enc, uint8_t **out, size_t *out_len,
              uint32_t **seek, size_t *seek_len, uint32_t *last_frame_bit);

void xma_free(void *p);

#endif
