/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: rdic-conv.c,v 1.3 2007-10-23 05:25:56 knok Exp $
 * Copyright (C) 1992 1994
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
#include "conv-util.h"

static void
jis2ujis(buffer)
     unsigned char *buffer;
{
    unsigned char *p, *q;
    int kanji=0;

    p = q = buffer;
    while(*p != '\0') {
	if (*p == '\033') {
	    if ((p[1] == '$') &&
		((p[2] == '@') || (p[2] == 'B'))) {
		kanji = 1;
		p += 2;
	    } else if ((p[1] == '(') &&
		       ((p[2] == 'B') || (p[2] == 'J'))) {
		kanji = 0;
		p += 2;
	    } else {
		*(q ++) = *p;
	    }
	} else {
	    if (kanji) {
		*(q ++) = *(p ++) | 0x80;
		*(q ++) = *p | 0x80;
	    } else {
		*(q ++) = *p;
	    }
	}
	++ p;
    }
    *q = '\0';
}

static void
extract(file_name)
     char *file_name;
{
    FILE *fp;
    unsigned char buffer[1024];
    unsigned char f1[1024], f2[1024], f3[1024];

    if ((fp = fopen(file_name, "r")) == NULL) {
	perror(file_name);
	return;
    }

    while(fgets((char *)buffer, 1024, fp) != NULL) {
	if ((buffer[0] == '\0') || (buffer[0] == '#')) continue;
	jis2ujis(buffer);
	if (sscanf((const char *)buffer, "%s%s%s", f1, f2, f3) != 3) continue;
	if (isallkana(f2) == 0) continue;
	if (isallzenkaku(f3) == 0) continue;
	if (includekanji(f3) == 0) continue;
	printf("%s %s\n", f2, f3);
    }

    fclose(fp);
}

int
main(argc, argv)
     int argc;
     char **argv;
{
    int i;
    for(i = 1; i < argc; ++ i) {
	extract(argv[i]);
    }
    return 0;
}
