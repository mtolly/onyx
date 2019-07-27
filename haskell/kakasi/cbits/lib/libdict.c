/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: dict.c,v 1.13 2014-02-13 07:30:35 knok Exp $
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
  Modified by NOKUBI Takatsugu <knok@daionet.gr.jp>
  1999/03/04
      Rename PERLMOD macro to LIBRARY
  1999/01/11
      Add PERLMOD macro.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
# include <string.h>
#else
# include <strings.h>
#endif
#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif
#include <stdlib.h>
#ifdef HAVE_STDINT_H
# include <stdint.h>
#else
typedef int uintptr_t;
#endif
#include "kakasi.h"

#define BUFLEN 1024

#define IALLOCSIZE (1024*100)
#define CELLALLOC  5000

#ifndef KANWADICT
#define KANWADICT "./kanwadict"
#endif

/* variables for memory management */
void ** ary_charalloc = NULL;
void ** ary_cellalloc = NULL;
size_t ary_size_charalloc = -1;
size_t ary_cur_charalloc = -1;
size_t ary_size_cellalloc = -1;
size_t ary_cur_cellalloc = -1;
int point_charalloc = 0;
unsigned char *ptr_charalloc = NULL;
int point_cellalloc = 0;
struct kanji_yomi *ptr_cellalloc = NULL;

struct kanji_yomi *jisyo_table[0x80][0x80]; /* hash table */
int kanwa_load[0x80][0x80];                 /* Is kanwadict put? */
struct kanwa_entry kanwa[0x60][0x60];       /* ujis onl 0xa0 is shift to code. */

static void
add_ary_charalloc(ptr)
     void * ptr;
{
    ary_cur_charalloc ++;
    if (ary_charalloc == NULL || ary_cur_charalloc > ary_size_charalloc) {
	ary_size_charalloc += CELLALLOC;
	ary_charalloc = realloc(ary_charalloc, 
				sizeof(void *) * ary_size_charalloc +1);
    }
    ary_charalloc[ary_cur_charalloc] = ptr;
}

static void
add_ary_cellalloc(ptr)
     void * ptr;
{
    ary_cur_cellalloc ++;
    if (ary_cellalloc == NULL || ary_cur_cellalloc > ary_size_cellalloc) {
	ary_size_cellalloc += CELLALLOC;
	ary_cellalloc = realloc(ary_cellalloc, 
				sizeof(void *) * ary_size_cellalloc +1);
    }
    ary_cellalloc[ary_cur_cellalloc] = ptr;
}

static unsigned char *
charalloc(length)
     int length;
{
    unsigned char *ret;

    if ((ptr_charalloc == NULL) || (point_charalloc+length >= IALLOCSIZE)) {
	ptr_charalloc = (unsigned char *)malloc(IALLOCSIZE);
	add_ary_charalloc(ptr_charalloc);
	point_charalloc = 0;
    }
    ret = ptr_charalloc+point_charalloc;
    point_charalloc += length;
    return ret;
}

static struct kanji_yomi *
cellalloc()
{

    if ((ptr_cellalloc == NULL) || (point_cellalloc >= CELLALLOC)) {
	char *cptr;
	cptr = malloc((CELLALLOC+1)*sizeof(struct kanji_yomi));
	add_ary_cellalloc(cptr);
	if ((uintptr_t)cptr & 7) cptr += 8 - ((uintptr_t)cptr & 7);
	ptr_cellalloc = (struct kanji_yomi *) cptr;
	point_cellalloc = 0;
    }
    ++ point_cellalloc;
    return ptr_cellalloc ++;
}

void init_jisyo()
{
    int c1, c2;

    for(c1 = 0; c1 < 0x80; c1 ++)
	for(c2 = 0; c2 < 0x80; c2 ++)
	    jisyo_table[c1][c2] = NULL;
}

static void jis2ujis_jisyo(buffer)
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

static void add_item(yomi, kanji, tail)
     unsigned char *yomi;
     unsigned char *kanji;
     int tail;
{
    unsigned char *q, *ptr_kanji, *ptr_yomi;
    struct kanji_yomi *ptr_kanji_yomi, **ptr;
    int length, c1, c2;

    /* Is the head a kanji? */
    if (kanji[0] < 0xb0) return;

    /* Isn't a HANKAKU character contained? Convert ITAIJI. */
    for (q = kanji;; q += 2) {
	c1 = q[0]; c2 = q[1];
	if ((c1 == '\0') || (c2 == '\0')) break;
	if ((c1 <= 0xa0) || (c2 <= 0xa0)) return;
	itaijiknj(&c1, &c2);
	q[0] = c1; q[1] = c2;
    }

    /* Isn't the one except for the KANA contained? A KATAKANA changes into the HIRAGANA. */
    for (q = yomi; (q[0] != '\0') && (q[1] != '\0'); q += 2) {
	if (*q < 0xa1) return;
	if (*q == 0xa5) {
	    *q = 0xa4;
	    if (q[1] >= 0xf4 && q[1] <= 0xf6) /* skip when input is "vu", small "ka", small "ke"  */
		return;
	}
	if ((*q != 0xa4) &&
	    ((q[0] != 0xa1) || (q[1] != 0xbc)) && /* Prolonged sound */ 
	    ((q[0] != 0xa1) || (q[1] != 0xab)) && /* Sonant */
	    ((q[0] != 0xa1) || (q[1] != 0xac)))   /* Half-sonant */
	    return;
    }

    /* A cell because of the reading is made. */

    length = strlen((const char *)kanji);
    ptr_kanji =	charalloc(length-1);
    strcpy((char *)ptr_kanji, (const char *)(kanji+2));
    ptr_yomi = charalloc(strlen((const char *)yomi)+1);
    strcpy((char *)ptr_yomi, (const char *)yomi);

    ptr_kanji_yomi = cellalloc();
    ptr_kanji_yomi->next = NULL;
    ptr_kanji_yomi->length = tail ? length+1 : length;
    ptr_kanji_yomi->kanji = ptr_kanji;
    ptr_kanji_yomi->yomi = ptr_yomi;
    ptr_kanji_yomi->tail = tail;

    /* It is connected in search of the end of the link of the internal dictionary. */
    for (ptr = &(jisyo_table[kanji[0]-0x80][kanji[1]-0x80]);
	 *ptr != NULL;
	 ptr = &((*ptr)->next));
    *ptr = ptr_kanji_yomi;
}

void add_jisyo(filename)
     char *filename;
{
    FILE *jisyo_fp;
    unsigned char buffer[BUFLEN];
    unsigned char *p;
    unsigned char *yomi, *kanji;
    int tail;
    extern char *ialloc();

    if ((jisyo_fp = fopen(filename, "rb")) == NULL) {
	perror(filename);
	exit(0);
    }
    while(fgets((char *)buffer, BUFLEN, jisyo_fp)) {
	/* If there is the one except for the KANA at the head, to the next */
	if ((buffer[0] < 0xa0) && (buffer[0] != '\033')) continue;

	/* A line is changed into ujis. */
	jis2ujis_jisyo(buffer);

	yomi = buffer;
	/* The next ward is looked for. */
	for (p = buffer; (*p != ' ') && (*p != '\011') && (*p != ','); ++ p) {
	    if ((*p == '\0') || (*p == '\n')) goto next_line;
	}

	if (isalpha(p[-1])) { /* An OKURIGANA is given if the last character is an alphabet. */
	    tail = p[-1];
	    p[-1] = '\0';
	} else {
	    tail = 0;
	    p[0] = '\0';
	}

	/* The next ward is looked for. */
	for (++ p; (*p == ' ') || (*p == '\011') || (*p == ','); ++ p) {
	    if ((*p == '\0') || (*p == '\n')) goto next_line;
	}

	if (*p == '/') { /* It seems to be the dictionary of SKK. */
	    for (;;) {
		kanji = p+1;
		/* The next ward is looked for. */
		for (++ p; (*p != '/'); ++ p) {
		    if ((*p == '\0')||(*p == '\n')||(*p == '[')) goto next_line;
		}
		*p = '\0';
		add_item(yomi, kanji, tail);
	    }
	} else { /* It seems to be a standard dictionary. */
	    kanji = p;
	    /* The next ward is looked for. */
	    for (++ p; 
		 (*p != ' ') && (*p != '\n') && (*p != '\011') &&
		 (*p != '\0') && (*p != ',')
		 ; ++ p) {
		;
	    }
	    *p = '\0';
	    add_item(yomi, kanji, tail);
	}
      next_line:;
    }
    fclose(jisyo_fp);
}

/* The initialization of kanwa is done. Reading kanwa_load is actually cleared in 
   kanwa the part at the head. */

#ifdef LIBRARY
FILE *kanwadict = NULL;
unsigned short dict_endian_mark;
#else
static FILE *kanwadict;
static unsigned short dict_endian_mark;
#endif

static void
fix_dict_endian_int(int *v)
{
    unsigned char *p;
    int i;
    if (dict_endian_mark == 0xFEFF) {
	/* native endian */
	return;
    }
    i = 0;
    p = (unsigned char *)v;
    i = (p[3]<<24) | (p[2]<<16) | (p[1]<<8) | p[0];
    *v = i;
}

void init_kanwa()
{
    int i, j;
    
    char *kanwadictpath;
    char magic[6];
    int kanwa_offset;

    kanwadictpath = (char*)getenv("KANWADICTPATH");
    if (kanwadictpath == (char*)NULL)
	kanwadictpath = (char*)getenv("KANWADICT");
    if (kanwadictpath == (char*)NULL)
	kanwadictpath = KANWADICT;

    if ((kanwadict = fopen(kanwadictpath,"rb")) == NULL) {
	perror(kanwadictpath);
	exit(2);
    }

    fread(magic, 6, 1, kanwadict);
    if (memcmp(magic, "KAKASI", 6) == 0) {
	fread(&dict_endian_mark, 2, 1, kanwadict);
	fread(&kanwa_offset, sizeof(int), 1, kanwadict);
	fix_dict_endian_int(&kanwa_offset);
	/* XXX: ignore options */
	fseek(kanwadict, kanwa_offset, SEEK_SET);
    } else {
	dict_endian_mark = 0;
    }

    if (fread((char *)kanwa, sizeof kanwa, 1, kanwadict) != 1) {
	perror(kanwadictpath);
    }

    if (dict_endian_mark) {
	for (i = 0x20; i < 0x7f; i++) {
	    for (j = 0x20; j < 0x7f; j++) {
		fix_dict_endian_int(&kanwa[i-0x20][j-0x20].index);
		fix_dict_endian_int(&kanwa[i-0x20][j-0x20].entry);
	    }
	}
    }

    for (i = 0; i < 0x80; ++ i)
	for (j = 0; j < 0x80; ++ j)
	    kanwa_load[i][j] = 0;
}

/* An applicable part from kanwa if necessary is drawn. */

void add_kanwa(c1, c2)
     int c1;
     int c2;
{
    unsigned char *ptr_yomi, *ptr_kanji;
    struct kanji_yomi *ptr_kanji_yomi, **ptr;
    int i;
    unsigned char tail, length;

    c1 &= 0x7f;
    c2 &= 0x7f;

    if (kanwa_load[c1][c2]) return;
    kanwa_load[c1][c2] = 1;

    /* It is finished when there is no description in the dictionary just in case. */
    if (kanwa[c1-0x20][c2-0x20].entry == 0) return;
    /* It is moved to the fixed position of kanwadict. */
    fseek(kanwadict, (long)(kanwa[c1-0x20][c2-0x20].index), 0L);

    /* The end of the link of the internal dictionary is looked for. */
    for (ptr = &(jisyo_table[c1][c2]);
	 *ptr != NULL;
	 ptr = &((*ptr)->next));

    for (i = 0; i < kanwa[c1-0x20][c2-0x20].entry; ++ i) {
	ptr_kanji_yomi = cellalloc();

	fread(&tail, 1, 1, kanwadict);
	ptr_kanji_yomi->tail = tail;

	fread(&length, 1, 1, kanwadict);
	ptr_kanji = charalloc(length+1);
	fread(ptr_kanji, (int)length, 1, kanwadict);
	ptr_kanji[length] = '\0';
	ptr_kanji_yomi->kanji = ptr_kanji;

	ptr_kanji_yomi->length = length + ((tail == 0) ? 2 : 3);

	fread(&length, 1, 1, kanwadict);
	ptr_yomi = charalloc(length+1);
	fread(ptr_yomi, (int)length, 1, kanwadict);
	ptr_yomi[length] = '\0';
	ptr_kanji_yomi->yomi = ptr_yomi;

	ptr_kanji_yomi->next = NULL;

	*ptr = ptr_kanji_yomi;
	ptr = &(ptr_kanji_yomi->next);
    }
}
