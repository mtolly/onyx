/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: itaiji.c,v 1.3 2007-03-12 06:43:05 knok Exp $
 * Copyright (C) 1992
 * Hironobu Takahashi (takahasi@tiny.or.jp)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either versions 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with KAKASI, see the file COPYING.  If not, write to the Free
 * Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include "kakasi.h"

#define BUFSIZE 128

#ifndef ITAIJIDICT
#define ITAIJIDICT "./itaijidict"
#endif

/* The change table of ITAIJI is made. */

static unsigned char ittbl1[0x80][0x80], ittbl2[0x80][0x80];
static int itaijitbl_made=0;

void
mkitaijitbl()
{
    FILE *fp;
    int i, j;
    char buffer[BUFSIZE];
    unsigned char n1, n2, o1, o2;
    char *itaijidictpath;
    
    itaijidictpath = (char*)getenv("ITAIJIDICTPATH");
    if (itaijidictpath == (char*)NULL)
	itaijidictpath = (char*)getenv("ITAIJIDICT");
    if (itaijidictpath == (char*)NULL)
	itaijidictpath = ITAIJIDICT;

    if ((fp = fopen(itaijidictpath, "rb")) == NULL) {
	fprintf(stderr, "Can't open Kanji itaijidict file ");
	perror(itaijidictpath);
	exit(0);
    }
    for (i = 0; i < 0x80; ++ i) {
	for (j = 0; j < 0x80; ++ j) {
	    ittbl1[i][j] = i | 0x80;
	    ittbl2[i][j] = j | 0x80;
	}
    }
    while(fgets(buffer, BUFSIZE, fp) != NULL) {
	n1 = buffer[0];
	n2 = buffer[1];
	o1 = buffer[2];
	o2 = buffer[3];
	n1 &= 0x7f;
	n2 &= 0x7f;
	ittbl1[n1][n2] = o1;
	ittbl2[n1][n2] = o2;
    }
    fclose(fp);
}

/* The change of ITAIJI */

void
itaijiknj(c1, c2)
     int *c1;
     int *c2;
{
    int b1, b2;

    if (itaijitbl_made == 0) {
	mkitaijitbl();
	itaijitbl_made =1;
    }

    b1 = *c1 & 0x7f;
    b2 = *c2 & 0x7f;
    *c1 = ittbl1[b1][b2];
    *c2 = ittbl2[b1][b2];
}
