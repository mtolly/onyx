/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: kanjiio.c,v 1.18 2014-02-19 07:47:17 knok Exp $
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
/*
  Modified by NOKUBI Takatsugu
  1999/03/04
       Rename PERLMOD macro to LIBRARY
  1999/01/08
       Add PERLMOD macro.
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
#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif
#include <stdlib.h>
#include "kakasi.h"
#ifdef LIBRARY
# include "libkakasi.h"
#endif

#ifdef KAKASI_SUPPORT_UTF8
#include <iconv.h>
extern iconv_t fromutf8;
extern iconv_t toutf8;
int utf8converted = 0;
void pututf8(int f, int s);

/* 
   The macro UTF8_COMPUTE was imported from gutf8.c in glib.
   You can get the original source code from ftp://ftp.gtk.org.
 */
#define UTF8_COMPUTE(Char, Mask, Len)					      \
  if (Char < 128)							      \
    {									      \
      Len = 1;								      \
      Mask = 0x7f;							      \
    }									      \
  else if ((Char & 0xe0) == 0xc0)					      \
    {									      \
      Len = 2;								      \
      Mask = 0x1f;							      \
    }									      \
  else if ((Char & 0xf0) == 0xe0)					      \
    {									      \
      Len = 3;								      \
      Mask = 0x0f;							      \
    }									      \
  else if ((Char & 0xf8) == 0xf0)					      \
    {									      \
      Len = 4;								      \
      Mask = 0x07;							      \
    }									      \
  else if ((Char & 0xfc) == 0xf8)					      \
    {									      \
      Len = 5;								      \
      Mask = 0x03;							      \
    }									      \
  else if ((Char & 0xfe) == 0xfc)					      \
    {									      \
      Len = 6;								      \
      Mask = 0x01;							      \
    }									      \
  else									      \
    Len = -1;

#endif /* KAKASI_SUPPORT_UTF8 */


#if ! defined HAVE_MEMMOVE && ! defined memmove
# define memmove(d, s, n) bcopy ((s), (d), (n))
#endif

int input_term_type = UNKNOWN;

int input_GL = SETG0;
int input_GR = SETG3;
int input_G[5] = {ASCII, KATAKANA, KATAKANA, JIS83, SJKANA};

int output_term_type = UNKNOWN;

int output_GL = SETG0;
int output_GR = SETG3;
int output_G[5] = {ASCII, KATAKANA, KATAKANA, JIS83, SJKANA};

#ifdef LIBRARY
unsigned char *getcharbuffer;
#endif /* LIBRARY */

/* One character buffer */

static Character kanji_buf={OTHER, 0, 0};
static int kanji_buf_set = 0;

void
ungetkanji(c)
     Character *c;
{
    kanji_buf.type = c->type;
    kanji_buf.c1 = c->c1;
    kanji_buf.c2 = c->c2;
    kanji_buf_set = 1;
}

/* One input of a character */

static unsigned char input_stack[1024];
static int input_stack_depth = 0;

#ifdef LIBRARY

int pbuf_error = 0;

#define PBSTRSIZE (4096)
#ifdef putchar
#undef putchar
#endif
#define putchar(x) putcharpbuf(x)

typedef struct pbuf_t {
    char *str;
    long size;
    struct pbuf_t *next;
} pbuf;

pbuf pcbuf = {
    NULL, -1, NULL
};

pbuf *pcbuf_tail = &pcbuf;

void
setcharbuffer(s)
     unsigned char *s;
{
    getcharbuffer = s;
}

void
putcharpbuf(c)
     int c;
{
    pbuf *pb = pcbuf_tail;

    if (pb->size == PBSTRSIZE) {
	pb = pb->next;
	pcbuf_tail = pb;
    }
    if (pb->size < 0) {
	pbuf *npb;
	if ((pb->str = (char *) malloc(PBSTRSIZE)) == NULL) {
	    pbuf_error = 1;
	    return;
	}
	pb->size = 0;
	if ((npb = (void *) malloc(sizeof(pbuf))) == NULL) {
	    pbuf_error = 1;
	    return;
	}
	npb->str = NULL;
	npb->size = -1;
	npb->next = NULL;
	pb->next = npb;
    }
    pb->str[pb->size] = (char) c;
    pb->size ++;
}

char *
getpbstr() {
    char *ret, *tmp;
    long tsize = 0;
    pbuf *pb = &pcbuf;
    while (pb->next != NULL) {
	tsize += pb->size;
	pb = pb->next;
    }
    if (tsize <= 0)
	return NULL;
    pb = &pcbuf;
    tmp = ret = (char *) malloc(tsize + 1);
    if (ret == NULL)
	return NULL;
    while (pb->next != NULL) {
	memmove(tmp, pb->str, pb->size);
	tmp += pb->size;
	pb = pb->next;
    }
    ret[tsize] = '\0';
    pb = &pcbuf;
    free(pb->str);
    pb = pb->next;
    if (pb->next != NULL) {
	pbuf *opb = pb;
	pb = pb->next;
	while (pb != NULL) {
	    free(opb->str);
	    free(opb);
	    opb = pb;
	    pb = pb->next;
	}
    } else {
	free(pb);
    }
    pcbuf.str = NULL;
    pcbuf.size = -1;
    pcbuf.next = NULL;
    pcbuf_tail = &pcbuf;
    return ret;
}
#endif /* LIBRARY */

static int
get1byte()
{
    if (input_stack_depth == 0)
#ifdef LIBRARY
    {
	if (*getcharbuffer == '\0') return EOF;
	return (int) *getcharbuffer ++;
    }
#else
	return getchar();
#endif /* LIBRARY */
    else
	return input_stack[-- input_stack_depth];
}

static void
unget1byte(c)
     int c;
{
    input_stack[input_stack_depth ++] = c;
}

static int
getc0set1(gn)
     int gn;
{
    int c3;
    int set;

    switch(c3 = get1byte()) {
      case 'B':
	set = ASCII; break;
      case 'J':
	set = JISROMAN; break;
      case 'O':
	set = GRAPHIC; break;
      case 'I':
	set = KATAKANA; break;
      default:
	unget1byte(c3); return -1;
    }
    input_G[gn] = set;
    return 0;
}

static void
set_terms(type, term_type, GL, GR, G)
     int type;
     int *term_type;
     int *GL;
     int *GR;
     int *G;
{
    *term_type = type;

    switch(type) {
      case OLDJIS:
	*GL=SETG0, *GR=SETG1,
	G[0]=JISROMAN, G[1]=KATAKANA, G[2]=KATAKANA, G[3]=KATAKANA;
	break;
      case NEWJIS:
	*GL=SETG0, *GR=SETG1,
	G[0]=ASCII, G[1]=KATAKANA, G[2]=KATAKANA, G[3]=KATAKANA;
	break;
      case DEC:
	*GL=SETG0, *GR=SETG3,
	G[0]=ASCII, G[1]=GRAPHIC, G[2]=KATAKANA, G[3]=JIS83;
	break;
      case EUC:
	*GL=SETG0, *GR=SETG3,
	G[0]=ASCII, G[1]=KATAKANA, G[2]=KATAKANA, G[3]=JIS83;
	break;
      case MSKANJI:
	*GL=SETG0, *GR=SJKANA,
	G[0]=ASCII, G[1]=KATAKANA, G[2]=KATAKANA, G[3]=KATAKANA;
	break;
    }
}

void
set_input_term(type)
     int type;
{
    set_terms(type, &input_term_type, &input_GL, &input_GR, input_G);
}

void
set_output_term(type)
     int type;
{
    set_terms(type, &output_term_type, &output_GL, &output_GR, output_G);
}

static int
getc0set2(gn)
     int gn;
{
    int c4;
    int set;

    switch(c4 = get1byte()) {
      case '@':
	set = JIS78;
	if (input_term_type == UNKNOWN)
	    set_input_term(OLDJIS);
	if (output_term_type == UNKNOWN)
	    set_output_term(OLDJIS);
	break;
      case 'B':
	set = JIS83;
	if (input_term_type == UNKNOWN)
	    set_input_term(NEWJIS);
	if (output_term_type == UNKNOWN)
	    set_output_term(NEWJIS);
	break;
      default:
	unget1byte(c4); return -1;
    }
    input_G[gn] = set;
    return 0;
}

static void
getc0(c, c1)
     Character *c;
     int c1;
{
    int c2, c3;
    int GL_save, GR_save;

    switch(c1) {
      case '\033':
	switch(c2 = get1byte()) {
	  case '(':
	    if (getc0set1(SETG0) != 0) {
		unget1byte(c2);	c->type = OTHER; c->c1 = c1; return;
	    }
	    break;
	  case ')':
	    if (getc0set1(SETG1) != 0) {
		unget1byte(c2);	c->type = OTHER; c->c1 = c1; return;
	    }
	    break;
	  case '*':
	    if (getc0set1(SETG2) != 0) {
		unget1byte(c2);	c->type = OTHER; c->c1 = c1; return;
	    }
	    break;
	  case '+':
	    if (getc0set1(SETG3) != 0) {
		unget1byte(c2);	c->type = OTHER; c->c1 = c1; return;
	    }
	    break;
	  case '$':
	    switch(c3 = get1byte()) {
	      case '@':
		if (input_term_type == UNKNOWN)
		    set_input_term(OLDJIS);
		if (output_term_type == UNKNOWN)
		    set_output_term(OLDJIS);
		input_G[SETG0] = JIS78;
		break;
	      case 'B':
		if (input_term_type == UNKNOWN)
		    set_input_term(NEWJIS);
		if (output_term_type == UNKNOWN)
		    set_output_term(NEWJIS);
		input_G[SETG0] = JIS83;
		break;
	      case '(':
		if (getc0set2(SETG0) != 0) {
		    unget1byte(c3); unget1byte(c2);
		    c->type = OTHER; c->c1 = c1; return;
		}
		break;
	      case ')':
		if (getc0set2(SETG1) != 0) {
		    unget1byte(c3); unget1byte(c2);
		    c->type = OTHER; c->c1 = c1; return;
		}
		break;
	      case '*':
		if (getc0set2(SETG2) != 0) {
		    unget1byte(c3); unget1byte(c2);
		    c->type = OTHER; c->c1 = c1; return;
		}
		break;
	      case '+':
		if (getc0set2(SETG3) != 0) {
		    unget1byte(c3); unget1byte(c2);
		    c->type = OTHER; c->c1 = c1; return;
		}
		break;
	      default:
		unget1byte(c3);
		unget1byte(c2);
		c->type = OTHER; c->c1 = c1; return;
	    }
	    break;
	  case 'n':
	    input_GL = SETG2;
	    break;
	  case 'o':
	    input_GL = SETG3;
	    break;
	  case '~':
	    input_GR = SETG1;
	    break;
	  case '}':
	    input_GR = SETG2;
	    break;
	  case '|':
	    input_GR = SETG3;
	    break;
	  case 'N':
	    GL_save = input_GL;
	    GR_save = input_GR;
	    input_GL = SETG2;
	    input_GR = SETG2;
	    getkanji(c);
	    input_GL = GL_save;
	    input_GR = GR_save;
	    return;
	  case 'O':
	    GL_save = input_GL;
	    GR_save = input_GR;
	    input_GL = SETG3;
	    input_GR = SETG3;
	    getkanji(c);
	    input_GL = GL_save;
	    input_GR = GR_save;
	    return;
	  default:
	    unget1byte(c2);
	    c->type = OTHER; c->c1 = c1; return;
	}
	break;
      case 0xe:
	input_GL = SETG1;
	break;
      case 0xf:
	input_GL = SETG0;
	break;
      case EOF:
	c->type = OTHER; c->c1 = 0xff; return;
      default:
	c->type = OTHER; c->c1 = c1; return;
    }
    getkanji(c);
}

static void
getc1(c, c1)
     Character *c;
     int c1;
{
    int GL_save, GR_save;

    switch(c1) {
      case 0x8e:
	GL_save = input_GL;
	GR_save = input_GR;
	input_GL = SETG2;
	input_GR = SETG2;
	getkanji(c);
	input_GL = GL_save;
	input_GR = GR_save;
	return;
      case 0x8f:
	GL_save = input_GL;
	GR_save = input_GR;
	input_GL = SETG3;
	input_GR = SETG3;
	getkanji(c);
	input_GL = GL_save;
	input_GR = GR_save;
	return;
      default:
	c->type = OTHER; c->c1 = c1; return;
    }
}

void
getkanji(c)
     Character *c;
{
    int c1;

    if (kanji_buf_set) {
	c->type = kanji_buf.type;
	c->c1 = kanji_buf.c1;
	c->c2 = kanji_buf.c2;
	kanji_buf_set = 0;
	return;
    }

    c1 = get1byte();
#ifdef KAKASI_SUPPORT_UTF8
	if (!utf8converted && input_term_type == UTF8) {
	    char utf8[6], eucj[3];
	    char *from = utf8, *to = eucj;
	    size_t fromlen = 0, tolen = 3;
	    int i, len, mask;
	    utf8converted = 1;
	    UTF8_COMPUTE(c1, mask, len);
	    (void) mask;
	    if (len <= 1) {
		unget1byte(c1);
		getkanji(c);
		return;
	    }
	    utf8[0] = c1;
	    for (i = 1; i < len; i ++) {
		utf8[i] = getchar();
	    }
	    fromlen = len;
	    if (fromutf8 == (iconv_t) -1)
		fromutf8 = iconv_open("EUC-JP", "UTF-8");
	    iconv(fromutf8, &from, &fromlen, &to, &tolen);
	    if (tolen == 1) {
		unget1byte(eucj[1]);
		unget1byte(eucj[0]);
	    } else {
		unget1byte(eucj[2]);
		unget1byte(eucj[1]);
		unget1byte(eucj[0]);
	    }
	    getkanji(c);
	    return;
	} else {
	    utf8converted = 0;
	}
#endif /* KAKASI_SUPPORT_UTF8 */
    if (c1 < 0x20) { /* C0 */
	getc0(c, c1);
    } else if (c1 < 0x7f) { /* GL */
	c->type = input_G[input_GL];
	switch(c->type) {
	  case JIS78:
	    c->c1 = c1|0x80; c->c2 = get1byte()|0x80;
	    exc78_83(c);
	    break;
	  case JIS83:
	    c->c1 = c1|0x80; c->c2 = get1byte()|0x80;
	    break;
	  default:
	    c->c1 = c1;
	}
    } else if (c1 == 0x7f) { /* C0 */
	c->type = OTHER; c->c1 = c1;
    } else { /* 0x80 - 0xff */
	if (input_term_type == UNKNOWN) {
	    int c2, term_type;
	    
	    c2 = get1byte(); unget1byte(c2);
	    if ((c1 <= 0x9f) && (c1 >= 0x81) &&
		(c2 >= 0x40) && (c2 <= 0xfc) && (c2 != 0x7f))
		term_type = MSKANJI;
	    else if ((c1 <= 0xe9) && (c1 >= 0xe0) &&
		     (c2 >= 0x40) && (c2 <= 0xfc) && (c2 != 0x7f))
		term_type = MSKANJI;
	    else if ((c1 == 0xea) &&
		     (c2 >= 0x40) && (c2 <= 0x0a5) && (c2 != 0x7f))
		term_type = MSKANJI;
	    else if ((c1 <= 0xf4) && (c1 >= 0xa1) &&
		     (c2 >= 0xa1) && (c2 <= 0xfe))
		term_type = DEC;
	    else
		term_type = NEWJIS;
	    set_input_term(term_type);
	    if (output_term_type == UNKNOWN) {
		set_output_term(term_type);
	    }
	}

	if (input_term_type == MSKANJI) {
	    if ((0xa0 <= c1) && (c1 <= 0xdf)) {
		c->type=KATAKANA; c->c1 = c1&0x7f;
	    } else if ((0x81 <= c1) && (c1 <= 0xea)) {
		int o1, o2, c2;
		
		c2 = get1byte();
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
		c->type=JIS83;
		c->c1 = o1;
		c->c2 = o2;
	    } else {
		c->type=OTHER; c->c1 = c1;
	    }
	} else {
	    if (c1 < 0xa0) { /* C1 */
		getc1(c, c1);
	    } else if (c1 < 0xff) { /* GR */
		c->type = input_G[input_GR];
		switch(c->type) {
		  case JIS78:
		    c->c1 = c1; c->c2 = get1byte()|0x80;
		    exc78_83(c);
		  case JIS83:
		    c->c1 = c1; c->c2 = get1byte()|0x80;
		    break;
		  default:
		    c->c1 = c1 & 0x7f;
		}
		if (c->c2 == 0xff) /* get1byte() == EOF */
		    c->type = OTHER;
	    } else if (c1 == 0xff) { /* C1 */
		c->type = OTHER; c->c1 = c1;
	    }
	}
    }
}

static void
separator_proc(c)
     Character *c;
{
    int i;
    int flag = 1;

    switch(c->type) {
      case OTHER:
      case ASCII:
      case JISROMAN:
	switch(c->c1) {
	  case ' ':
	  case '\011':
	  case '\015':
	    separator_out = 0;
	    return;
	}

	/* check whether Character c is equal to separator or not */
	for (i=0; i<KAKASIBUF && separator[i].c1 != 0; i++) {
	    if ((c+i)->c1 != separator[i].c1 || 
		(c+i)->c2 != separator[i].c2) {
		flag = 0;
		break;
	    }
	}
	if (flag) { /* In case of c === separator */
	    separator_out = 0;
	    return;
	}
    }

    if (separator_out != 2) {
	separator_out = 1;
	return;
    }

    /* output separator */
    for (i=0; i<KAKASIBUF && separator[i].c1 != 0; i++) {
	putkanji(&separator[i]);
    }

    separator_out = 1;
}

/* One character output */

void
putkanji(c)
     Character *c;
{
    if (bunkatu_mode) {
	separator_proc(c);
    }

    switch(output_term_type) {
      case UNKNOWN:
	switch(c->type) {
	  case OTHER:
	  case ASCII:
	  case JISROMAN:
	    if ((output_G[0] != ASCII) && (output_G[0] != JISROMAN)) {
		putchar('\033');putchar('(');putchar('J');
		output_G[0] = JISROMAN;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case KATAKANA:
	    if (output_G[0] != KATAKANA) {
		putchar('\033');putchar('(');putchar('I');
		output_G[0] = KATAKANA;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case JIS83:
	  case JIS78:
	    if ((output_G[0] != JIS78) && (output_G[0] != JIS83)) {
		putchar('\033');putchar('$');putchar('@');
		output_G[0] = JIS78;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    putchar((c->c2)&0x7f);
	    break;
	}
	break;
      case OLDJIS:
	switch(c->type) {
	  case OTHER:
	    if ((output_G[0] != ASCII) && (output_G[0] != JISROMAN)) {
		putchar('\033');putchar('(');putchar('J');
		output_G[0] = JISROMAN;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case ASCII:
	    if (output_G[0] != ASCII) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case JISROMAN:
	    if (output_G[0] != JISROMAN) {
		putchar('\033');putchar('(');putchar('J');
		output_G[0] = JISROMAN;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case KATAKANA:
	    if (output_G[0] != KATAKANA) {
		putchar('\033');putchar('(');putchar('I');
		output_G[0] = KATAKANA;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case JIS83:
	    exc78_83(c);
	  case JIS78:
	    if (output_G[0] != JIS78) {
		putchar('\033');putchar('$');putchar('@');
		output_G[0] = JIS78;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    putchar((c->c2)&0x7f);
	    break;
	}
	break;
      case NEWJIS:
	switch(c->type) {
	  case OTHER:
	    if ((output_G[0] != ASCII) && (output_G[0] != JISROMAN)) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case ASCII:
	    if (output_G[0] != ASCII) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case JISROMAN:
	    if (output_G[0] != JISROMAN) {
		putchar('\033');putchar('(');putchar('J');
		output_G[0] = JISROMAN;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case KATAKANA:
	    if (output_G[0] != KATAKANA) {
		putchar('\033');putchar('(');putchar('I');
		output_G[0] = KATAKANA;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case JIS78:
	    exc78_83(c);
	  case JIS83:
	    if (output_G[0] != JIS83) {
		putchar('\033');putchar('$');putchar('B');
		output_G[0] = JIS83;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    putchar((c->c2)&0x7f);
	    break;
	}
	break;
      case DEC:
	switch(c->type) {
	  case OTHER:
	    if ((output_G[0] != ASCII) && (output_G[0] != JISROMAN)) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case ASCII:
	    if (output_G[0] != ASCII) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case JISROMAN:
	    if (output_G[0] != JISROMAN) {
		putchar('\033');putchar('(');putchar('J');
		output_G[0] = JISROMAN;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case KATAKANA:
	    if (output_G[2] != KATAKANA) {
		putchar('\033');putchar('*');putchar('I');
		output_G[2] = KATAKANA;}
	    if (output_GR != SETG2) {
		putchar('\033');putchar('}');output_GR=SETG2;}
	    putchar((c->c1)|0x80);
	    break;
	  case GRAPHIC:
	    if (output_G[1] != GRAPHIC) {
		putchar('\033');putchar(')');putchar('0');
		output_G[2] = GRAPHIC;}
	    if (output_GR != SETG1) {
		putchar('\033');putchar('~');output_GR=SETG1;}
	    putchar((c->c1)|0x80);
	    break;
	  case JIS78:
	    exc78_83(c);
	  case JIS83:
	    if (output_G[3] != JIS83) {
		putchar('\033');putchar('$');putchar('+');putchar('B');
		output_G[3] = JIS83;}
	    if (output_GR != SETG3) {
		putchar('\033'); putchar('|'); output_GR = SETG3;}
	    putchar((c->c1)|0x80);
	    putchar((c->c2)|0x80);
	    break;
	}
	break;
      case EUC:
	switch(c->type) {
	  case OTHER:
	    if ((output_G[0] != ASCII) && (output_G[0] != JISROMAN)) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case ASCII:
	    if (output_G[0] != ASCII) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case JISROMAN:
	    if (output_G[0] != JISROMAN) {
		putchar('\033');putchar('(');putchar('J');
		output_G[0] = JISROMAN;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case KATAKANA:
	    if (output_G[2] != KATAKANA) {
		putchar('\033');putchar('*');putchar('I');
		output_G[2] = KATAKANA;}
	    putchar(0x8e);
	    putchar((c->c1)|0x80);
	    break;
	  case JIS78:
	    exc78_83(c);
	  case JIS83:
	    if (output_G[3] != JIS83) {
		putchar('\033');putchar('$');putchar('+');putchar('B');
		output_G[3] = JIS83;}
	    if (output_GR != SETG3) {
		putchar('\033'); putchar('|'); output_GR = SETG3;}
	    putchar((c->c1)|0x80);
	    putchar((c->c2)|0x80);
	    break;
	}
	break;
      case MSKANJI:
	switch(c->type) {
	  case OTHER:
	    if ((output_G[0] != ASCII) && (output_G[0] != JISROMAN)) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case ASCII:
	  case JISROMAN:
	    putchar((c->c1)&0x7f);
	    break;
	  case KATAKANA:
	    putchar((c->c1)|0x80);
	    break;
	  case JIS78:
	    exc78_83(c);
	  case JIS83:
	    {
		int o1, o2;

		if ((c->c1) & 1) {
		    o1 = c->c1/2 + ((c->c1 < 0xdf) ? 0x31 : 0x71);
		    o2 = c->c2 - ((c->c2 >= 0xe0) ? 0x60 : 0x61);
		} else {
		    o1 = c->c1/2 + ((c->c1 < 0xdf) ? 0x30 : 0x70);
		    o2 = c->c2 - 2;
		}
		putchar(o1);
		putchar(o2);
		break;
	    }
	}
	break;
#ifdef KAKASI_SUPPORT_UTF8
      case UTF8:
	switch(c->type) {
	  case OTHER:
	    if ((output_G[0] != ASCII) && (output_G[0] != JISROMAN)) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar(c->c1);
	    break;
	  case ASCII:
	    if (output_G[0] != ASCII) {
		putchar('\033');putchar('(');putchar('B');
		output_G[0] = ASCII;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case JISROMAN:
	    if (output_G[0] != JISROMAN) {
		putchar('\033');putchar('(');putchar('J');
		output_G[0] = JISROMAN;}
	    if (output_GL != SETG0) {
		putchar(0xf); output_GL = SETG0;}
	    putchar((c->c1)&0x7f);
	    break;
	  case KATAKANA:
	    if (output_G[2] != KATAKANA) {
		putchar('\033');putchar('*');putchar('I');
		output_G[2] = KATAKANA;}
	    pututf8(0x8e, (c->c1)|0x80);
	    break;
	  case JIS78:
	    exc78_83(c);
	  case JIS83:
	    if (output_G[3] != JIS83) {
		putchar('\033');putchar('$');putchar('+');putchar('B');
		output_G[3] = JIS83;}
	    if (output_GR != SETG3) {
		putchar('\033'); putchar('|'); output_GR = SETG3;}
	    pututf8((c->c1)|0x80, (c->c2)|0x80);
	    break;
	}
	break;
#endif /* KAKASI_SUPPORT_UTF8 */
    }
}

int
term_type_str(str)
     char *str;
{
    if ((strncmp(str, "oldjis", 6) == 0) ||
	(strncmp(str, "jisold", 6) == 0))
	return OLDJIS;
    if (strncmp(str, "dec", 6) == 0)
	return DEC;
    if ((strncmp(str, "euc", 6) == 0) ||
	(strncmp(str, "att", 6) == 0))
	return EUC;
    if ((strncmp(str, "sjis", 6) == 0) ||
	(strncmp(str, "msjis", 6) == 0) ||
	(strncmp(str, "shiftjis", 6) == 0))
	return MSKANJI;
#ifdef KAKASI_SUPPORT_UTF8
    if ((strncmp(str, "utf8", 6) == 0) ||
	(strncmp(str, "utf-8", 6) == 0))
	return UTF8;
#endif /* KAKASI_SUPPORT_UTF8 */

    return NEWJIS;
}

#ifdef KAKASI_SUPPORT_UTF8
void
pututf8(f, s)
    int f, s;
{
    char fromstr[1024], tostr[1024];
    char *from = fromstr, *to = tostr;
    size_t fromlen = 2, tolen = 6;
    int i;
    fromstr[0] = (char) f;
    fromstr[1] = (char) s;
    if (toutf8 == (iconv_t) -1)
	toutf8 = iconv_open("UTF-8", "EUC-JP");
    iconv(toutf8, &from, &fromlen, &to, &tolen);
    if (tolen >= 6 || tolen < 0)
	return;
    for (i = 0; i < (6 - tolen); i ++) {
	putchar(tostr[i]);
    }
}

void 
close_iconv()
{
    if (fromutf8 != (iconv_t) -1) {
	iconv_close(fromutf8);
	fromutf8 = (iconv_t) -1;
    }
    if (toutf8 != (iconv_t) -1) {
	iconv_close(toutf8);
	toutf8 = (iconv_t) -1;
    }
}
#endif /* KAKASI_SUPPORT_UTF8 */
