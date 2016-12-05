/*
Taken from Guitar Wizard 0.22 by Jim Halliday <malictus@malictus.net>
http://www.scorehero.com/forum/viewtopic.php?t=17309
*/

#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include "encode_vag.h"

double filter[5][2] =  {
    { 0.0, 0.0 },
    {  -60.0 / 64.0, 0.0 },
    { -115.0 / 64.0, 52.0 / 64.0 },
    {  -98.0 / 64.0, 55.0 / 64.0 },
    { -122.0 / 64.0, 60.0 / 64.0 }
};

void encodeVAGBlock(int16_t *in, unsigned char *out, unsigned char flagcheck, double *state1, double *state2, double *state1a, double *state2a) {

    out[1] = flagcheck;

    int32_t i, j;
    double buffer[28][5] = {0};
    double min = 1e10;
    double max[5] = {0};
    double ds;
    int32_t min2;
    int32_t shift_mask;
    double s_0, s_1, s_2;
    double stat[2] = {0};
    int32_t predict_nr = 0;
    double d_samples[28] = {0};

    stat[0] = *state1;
    stat[1] = *state2;

    s_1 = 0;
    s_2 = 0;
    for ( i = 0; i < 5; i++ ) {
        max[i] = 0.0;
        s_1 = stat[0];
        s_2 = stat[1];
        for ( j = 0; j < 28; j ++ ) {
            s_0 = (double) in[j];
            if ( s_0 > 30719.0 )
                s_0 = 30719.0;
            if ( s_0 < - 30720.0 )
                s_0 = -30720.0;
            ds = s_0 + s_1 * filter[i][0] + s_2 * filter[i][1];
            buffer[j][i] = ds;
            if ( fabs( ds ) > max[i] ) {
                max[i] = fabs( ds );
            }
            s_2 = s_1;
            s_1 = s_0;
        }

        if ( max[i] < min ) {
            min = max[i];
            predict_nr = i;
        }
        if ( min <= 7 ) {
            predict_nr = 0;
            break;
        }
    }
    stat[0] = s_1;
    stat[1] = s_2;

    *state1 = stat[0];
    *state2 = stat[1];

    for ( i = 0; i < 28; i++ ) {
        d_samples[i] = buffer[i][predict_nr];
    }

    min2 = ( int32_t ) min;
    shift_mask = 0x4000;
    int32_t shift_factor = 0;

    while( shift_factor < 12 ) {
        if ( ( shift_mask  & ( min2 + ( shift_mask >> 3 ) ) ) != 0 ) {
            break;
        }
        (shift_factor)++;
        shift_mask = shift_mask >> 1;
    }

    //pack
    int32_t di;
    double s_0a;
    double statA[2] = {0};

    statA[0] = *state1a;
    statA[1] = *state2a;

    int16_t four_bit[28] = {0};

    for ( i = 0; i < 28; i++ ) {
        s_0a = d_samples[i] + statA[0] * filter[predict_nr][0] + statA[1] * filter[predict_nr][1];
        ds = s_0a * (double) ( 1 << shift_factor );

        di = ( (int32_t) ds + 0x800 ) & 0xfffff000;

        if ( di > 32767 )
            di = 32767;
        if ( di < -32768 )
            di = -32768;

        four_bit[i] = (int16_t) di;

        di = di >> shift_factor;
        statA[1] = statA[0];
        statA[0] = (double) di - s_0a;

    }

    *state1a = statA[0];
    *state2a = statA[1];

    out[0] = (unsigned char)(( predict_nr << 4 ) | shift_factor);
    for ( int32_t k = 0; k < 28; k += 2 ) {
        int32_t amt = ( ( four_bit[k+1] >> 8 ) & 0xf0 ) | ( ( four_bit[k] >> 12 ) & 0xf );
        out[(k/2) + 2] = (unsigned char)((amt & 0x000000FFL));
    }

    return;
}
