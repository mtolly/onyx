/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: jj2.c,v 1.7 2001-04-12 05:57:34 rug Exp $
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#ifdef HAVE_STRING_H
# include <string.h>
#else
# include <strings.h>
#endif
#include "kakasi.h"
#include <assert.h>

#define J2HBUF 256
#define J2READ 10

static int
J2_cletter(l, c1, c2)
     unsigned int l;
     unsigned int c1;
     unsigned int c2;
{
    static char *cl_table[96] = {
	"", "aiueow", "aiueow", "aiueow", "aiueow", "aiueow", "aiueow", "aiueow",
	"aiueow", "aiueow", "aiueow", "k", "g", "k", "g", "k", "g", "k", "g", "k",
	"g", "s", "zj", "s", "zj", "s", "zj", "s", "zj", "s", "zj", "t", "d", "tc",
	"d", "aiueokstchgzjfdbpw", "t", "d", "t", "d", "t", "d", "n", "n", "n", "n",
	"n", "h", "b", "p", "h", "b", "p", "hf", "b", "p", "h", "b", "p", "h", "b",
	"p", "m", "m", "m", "m", "m", "y", "y", "y", "y", "y", "y", "rl", "rl",
	"rl", "rl", "rl", "wiueo", "wiueo", "wiueo", "wiueo", "w", "n", "v", "k",
	"k", "", "", "", "", "", "", "", "", ""};

    char *p;

    if ((c1 == 0xa4) && (0xa0 <= c2 ) && (c2 < 0xff)) {
	for (p = cl_table[c2 - 0xa0]; *p != '\0'; ++ p) {
	    if (*p == l)
		return 0;
	}
    }
    return 1;
}

static void
J2append(n, str)
     Character *n;
     unsigned char *str;
{
    int i, j;

    j = 0;
    for (i = 0; str[i] != '\0'; ++ i, ++j) {
	if (str[i] > 0xa0) {
	    n[j].type = JIS83;
	    n[j].c1 = str[i];
	    n[j].c2 = str[i+1];
	    ++ i;
	} else {
	    n[j].type = ASCII;
	    n[j].c1 = str[i];
	}
    }
    n[j].type = OTHER;
    n[j].c1 = 0;
    n[j].c2 = 0;
}

int
J2H(c, n)
     Character *c;
     Character *n;
{
    int c1, c2;
    unsigned char Jstr[J2HBUF], Hstr[J2READ][J2HBUF], Dstr[J2HBUF], *h_point;
    int max_len, length, match_more, n_read, i, clen;
    struct kanji_yomi *ptr;

    max_len = 0;
    match_more = 0;
    n_read = 0;

    for (i = 0; i * 2 < J2HBUF - 2 && c[i].c1 != 0; ++ i) { /* FIXME: chop down incoming string (ad-hoc solution)*/
	c1 = c[i].c1;
	c2 = c[i].c2;
	if ((c[i].type == JIS83) || (c[i].type == JIS78)) {
	    itaijiknj(&c1, &c2);
	    Jstr[i*2]   = c1;
	    Jstr[i*2+1] = c2;
	} else {
	    Jstr[i*2]   = 0;
	    Jstr[i*2+1] = 0;
	}
    }
    assert(i*2 < J2HBUF);
    Jstr[i*2] = '\0';
    clen = i*2;

    add_kanwa((int)Jstr[0], (int)Jstr[1]);

    for (ptr = jisyo_table[Jstr[0]&0x7f][Jstr[1]&0x7f];
	 ptr != NULL;
	 ptr = ptr->next) {
	length = ptr->length;
	if (clen >= length) {
	    if (strncmp((char *)Jstr+2, (char *)(ptr->kanji),
			(length & 1) ? length-3 : length-2))
		continue;
	    if (length & 1)
		if (J2_cletter(ptr->tail,Jstr[length-1],Jstr[length]))
		    continue;
	    if (max_len < length) {
		if (length & 1) {
		    sprintf((char *)Hstr[0], "%s%c%c", ptr->yomi,
			    Jstr[length-1],Jstr[length]);
		} else {
		    strcpy((char *)Hstr[0], (const char *)(ptr->yomi));
		}
		max_len = length;
		n_read = 1;
	    } else if (max_len == length) {
		if ((heiki_mode) && (n_read < J2READ)) {
		    if (length & 1) {
			sprintf((char *)Hstr[n_read], "%s%c%c", ptr->yomi,
				Jstr[length-1],Jstr[length]);
		    } else {
			strcpy((char *)Hstr[n_read], (const char *)(ptr->yomi));
		    }
		    for (i = 0; i < n_read; ++ i) {
			if (strcmp((const char *)Hstr[i], (const char *)Hstr[n_read]) == 0) goto next;
		    }
		    n_read ++;
		  next:;
		}
	    }
	} else {
	    if (clen == 2)
		match_more = 1;
	    else if (strncmp((char *)Jstr+2, (char *)(ptr->kanji), clen-2) == 0)
		match_more = 1;
	}
    }

    if (max_len == 0) {
	n[0].type = OTHER;
	n[0].c1 = 0;
	n[0].c2 = 0;
	return 1;
    }

    h_point = Jstr+((max_len-1) & 0xfffe);
    if (strncmp((const char *)h_point, "\244\303", 2) == 0) {
	if (clen <= max_len+1)
	    match_more = 1;
	else {
	    max_len += 2;
	    for (i = 0; i < n_read; ++ i) {
		sprintf((char *)Hstr[i], "%s%c%c", Hstr[i], h_point[2], h_point[3]);
	    }
	}
    }

    if (n_read > 1) {
	strcpy((char *)Dstr, "{");
	for (i = 0; i < n_read; ++ i) {
	    strcat((char *)Dstr, (const char *)Hstr[i]);
	    if (n_read - i == 1)
		strcat((char *)Dstr, "}");
	    else
		strcat((char *)Dstr, "|");
	}
	J2append(n, Dstr);
    } else {
	J2append(n, Hstr[0]);
    }
    return (match_more == 0) ? (max_len+1)/2 : -(max_len+1)/2;
}

static void
J2convert(m, n, proc)
     Character *m;
     Character *n;
     int (*proc)();
{
    int mp=0, np=0;
    int ret;

    while(m[mp].c1 != 0) {
	if (m[mp].type != JIS83) {
	    n[np].type = m[mp].type;
	    n[np].c1 = m[mp].c1;
	    n[np].c2 = m[mp].c2;
	    ++ np;
	    ++ mp;
	} else {
	    ret = (* proc)(m+mp, n+np);
	    if (ret == 0) ret = 1;
	    mp += (ret < 0) ? -ret : ret;
	    for (; n[np].c1 != 0; ++ np) ;
	}
    }
    n[np].type = OTHER;
    n[np].c1 = 0;
    n[np].c2 = 0;
}

int
J2a(c, n)
     Character *c;
     Character *n;
{
    Character m[256];
    
    int ret;
    ret = J2H(c, m);
    J2convert(m, n, H2a);
    return ret;
}

int
J2j(c, n)
     Character *c;
     Character *n;
{
    Character m[256];
    
    int ret;
    ret = J2H(c, m);
    J2convert(m, n, H2j);
    return ret;
}

int
J2k(c, n)
     Character *c;
     Character *n;
{
    Character m[256];
    
    int ret;
    ret = J2H(c, m);
    J2convert(m, n, H2k);
    return ret;
}

int
J2K(c, n)
     Character *c;
     Character *n;
{
    Character m[256];
    
    int ret;
    ret = J2H(c, m);
    J2convert(m, n, H2K);
    return ret;
}
