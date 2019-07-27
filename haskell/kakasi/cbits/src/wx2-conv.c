/*
 * wx2-conv.c  --  WX2 -> KAKASI dictionary converter
 * $Id: wx2-conv.c,v 1.4 2007-10-23 05:25:56 knok Exp $
 * Copyright(c) 1996, Hajime BABA,
 *          Department of Astronomy, Kyoto University,
 *          KYOTO, Japan, 606-01.
 * E-mail: < baba@kusastro.kyoto-u.ac.jp >
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

/*
 *  [IKKATUTOUROKU:TOUROKU file] format of 
 *                                  WX2 series.
 *  <YOMI><tab>"<KANJI>":<HINSHI><FYKUKAI>
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#ifdef HAVE_STRING_H
# include <string.h>
#else
# include <strings.h>
#endif
#include "conv-util.h"

static void
sjis2ujis(sjis, ujis)
     unsigned char *sjis;
     unsigned char *ujis;
{
    unsigned char *p, *q;
    int c1, c2, o1, o2;
    static unsigned char k2H_table[64][3] = {
	"\241\241", "\241\243", "\241\326", "\241\327", "\241\242", "\241\245", "\244\362", "\244\241",
	"\244\243", "\244\245", "\244\247", "\244\251", "\244\343", "\244\345", "\244\347", "\244\303",
	"\241\274", "\244\242", "\244\244", "\244\246", "\244\250", "\244\252", "\244\253", "\244\255",
	"\244\257", "\244\261", "\244\263", "\244\265", "\244\267", "\244\271", "\244\273", "\244\275",
	"\244\277", "\244\301", "\244\304", "\244\306", "\244\310", "\244\312", "\244\313", "\244\314",
	"\244\315", "\244\316", "\244\317", "\244\322", "\244\325", "\244\330", "\244\333", "\244\336",
	"\244\337", "\244\340", "\244\341", "\244\342", "\244\344", "\244\346", "\244\350", "\244\351",
	"\244\352", "\244\353", "\244\354", "\244\355", "\244\357", "\244\363", "\241\253", "\241\254" };
    static unsigned char k2H_dtable[64][3] = {
	"",   "",   "",   "",   "",   "",   "",   "",
	"",   "",   "",   "",   "",   "",   "",   "",
	"",   "",   "",   "",   "",   "",   "\244\254", "\244\256",
	"\244\260", "\244\262", "\244\264", "\244\266", "\244\270", "\244\272", "\244\274", "\244\276",
	"\244\300", "\244\302", "\244\305", "\244\307", "\244\311", "",   "",   "",
	"",   "",   "\244\320", "\244\323", "\244\326", "\244\331", "\244\334", "",
	"",   "",   "",   "",   "",   "",   "",   "",
	"",   "",   "",   "",   "",   "",   "",   "" };
    static unsigned char k2H_htable[64][3] = {
	"",   "",   "",   "",   "",   "",   "",   "",
	"",   "",   "",   "",   "",   "",   "",   "",
	"",   "",   "",   "",   "",   "",   "",   "",
	"",   "",   "",   "",   "",   "",   ""  , "",
	"",   "",   "",   "",   "",   "",   "",   "",
	"",   "",   "\244\321", "\244\324", "\244\327", "\244\332", "\244\335", "",
	"",   "",   "",   "",   "",   "",   "",   "",
	"",   "",   "",   "",   "",   "",   "",   "" };

    p = sjis;
    q = ujis;
    while((c1 = *p) != '\0') {
	if (c1 == ',') {
	    *(q ++) = ' ';
	} else if (c1 < 0x80) {
	    *(q ++) = c1;
	} else if ((0xa0 <= c1) && (c1  < 0xe0)) {
	    c2 = p[1];
	    if ((c2==0xde) && (k2H_dtable[c1-0xa0][0] != '\0')) {
		*(q ++) = k2H_dtable[c1-0xa0][0];
		*(q ++) = k2H_dtable[c1-0xa0][1];
		++ p;
	    } else if ((c2==0xdf) && (k2H_htable[c1-0xa0][0] != '\0')) {
		*(q ++) = k2H_htable[c1-0xa0][0];
		*(q ++) = k2H_htable[c1-0xa0][1];
		++ p;
	    } else {
		*(q ++) = k2H_table[c1-0xa0][0];
		*(q ++) = k2H_table[c1-0xa0][1];
	    }
	} else {
	    c2 = p[1];
	    if (c2 >= 0x9f) {
		if (c1 >= 0xe0) o1 = c1*2 - 0xe0;
		else o1 = c1*2 - 0x60;
		o2 = c2 + 2;
	    } else {
		if (c1 >= 0xe0) o1 = c1*2 - 0xe1;
		else o1 = c1*2 - 0x61;
		if (c2 >= 0x7f) o2 = c2 + 0x60;
		else o2 = c2 +  0x61;
	    }
	    *(q ++) = o1;
	    *(q ++) = o2;
	    ++ p;
	}
	++ p;
    }
    *q = '\0';
}

static void
getkanji(kanji, s)
     char *kanji;
     char *s;
{
    s++;			 /* skip first '"' */
    while (*s) {
	if (*s == '"') {	 /* detect second '"' */
	    *kanji = '\0';
	    break;
	}
	*kanji = *s;
	kanji++;
	s++;
    }
}

static void
extract(file_name)
     char *file_name;
{
    FILE *fp;
    unsigned char sjis[1024], ujis[1024];
    unsigned char f1[1024], f2[1024];
    unsigned char tmp[1024];

    if ((fp = fopen(file_name, "r")) == NULL) {
	perror(file_name);
	return;
    }

    while(fgets((char *)sjis, 1024, fp) != NULL) {
	if ((sjis[0] == '\0') || (sjis[0] == '#')) continue;
	sjis2ujis(sjis, ujis);
	if (sscanf((const char *)ujis, "%s%s", f1, tmp) != 2) continue;
	getkanji(f2, tmp);
	if (isallkana(f1) == 0) continue;
	if (isallzenkaku(f2) == 0) continue;
	if (includekanji(f2) == 0) continue;
	printf("%s %s\n", f1, f2);
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
