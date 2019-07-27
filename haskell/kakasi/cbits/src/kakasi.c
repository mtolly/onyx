/*
 * $Id: kakasi.c,v 1.41 2013-02-06 06:05:02 knok Exp $
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
  1999/05/09
     Fix kakasi_do returning no values.
  1999/04/14
     Add more valuables initialize routine.
  1999/04/12
     Add initialize routine for some valuables to funtion kakasi_getopt_argv.
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
extern void close_iconv();

iconv_t fromutf8 = (iconv_t) -1;
iconv_t toutf8 = (iconv_t) -1;
#endif /* KAKASI_SUPPORT_UTF8 */

/* FIXME: this macro should be removed future. */
#ifdef LIBRARY
#define KAKASI_ATTR
#else /* !LIBRARY */
#define KAKASI_ATTR static
#endif /* !LIBRARY */

int romaji_type = HEPBURN;
int use_old_romaji_table = 0;
int romaji_capitalize = 0;
int romaji_upcase = 0;
int heiki_mode = 0;
int bunkatu_mode = 0;
int furigana_mode = 0;
int cr_eat_mode = 0;
int flush_mode = 0;
#ifdef WAKATIGAKI
int wakatigaki_mode = 0;
int terminate_done = 0;
int wo_mode = 0;
#endif /* WAKATIGAKI */
int level_hiragana_mode = 0;
int level_furigana_mode = 0;
#ifdef EACH_YOMI
int eachyomi_mode = 0;
void output_yomi_eachkanji(Character *, int);
void putkanjis(Character *);
#endif /* EACH_YOMI */

int kanji_digest;
int separator_out;
Character separator[KAKASIBUF];
char cr_eat_string[KAKASIBUF];
Character n[KAKASIBUF];
Character left_paren[KAKASIBUF];
Character right_paren[KAKASIBUF];

#ifdef LIBRARY
extern FILE *kanwadict;
static int (*proc[8])()={NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
/* ASCII, JISROMAN, KATAKANA, GRAPHIC, ZENKAKU-KIGOU, ZENKAKU-KATAKANA, ZENKAKU-HIRAGANA, KANJI, */
#endif

extern int input_term_type;
extern int output_term_type;

/* variables for memory management */
extern void ** ary_charalloc;
extern void ** ary_cellalloc;
extern size_t ary_size_charalloc;
extern size_t ary_cur_charalloc;
extern size_t ary_size_cellalloc;
extern size_t ary_cur_cellalloc;
extern int point_charalloc;
extern unsigned char *ptr_charalloc;
extern int point_cellalloc;
extern struct kanji_yomi *ptr_cellalloc;

/* forward decls */
KAKASI_ATTR void digest_start_copy PARAMS((Character *c, Character *r));
KAKASI_ATTR void put_separator PARAMS((void));
KAKASI_ATTR void putchars PARAMS((Character *results));
KAKASI_ATTR void digest_out PARAMS((Character *c, int ret));
KAKASI_ATTR int digest PARAMS((Character *c, int clen, Character *r, int rlen, int type, int (*proc)(void)));
KAKASI_ATTR void digest_shift PARAMS((Character *c, int s));
#ifdef LIBRARY
static void free_jisyo PARAMS((void));
#endif /* LIBRARY */

KAKASI_ATTR void
digest_start_copy(c, r)
     Character *c;
     Character *r;
{
    for(;;) {
	r->type = c->type;
	r->c1 = c->c1;
	r->c2 = c->c2;
	if ((r->type == OTHER) && (r->c1 == 0)) return;
	++r, ++c;
    }
}

KAKASI_ATTR void
put_separator()
{
#ifdef WAKATIGAKI
    if (bunkatu_mode) {
        if(! terminate_done) {
	    if (separator_out == 1)
	        separator_out = 2;
        }
    }
#else
    if (bunkatu_mode)
	if (separator_out == 1)
	    separator_out = 2;
#endif /* WAKATIGAKI */
}

KAKASI_ATTR void
putchars(results)
     Character *results;
{
    while(results->c1 != '\0') {
	putkanji(results);
	++ results;
    }
}

KAKASI_ATTR void
digest_out(c, ret)
     Character *c;
     int ret;
{
    Character *ptr;
    int i;
 
    if (kanji_digest) {
	put_separator();
	if (romaji_capitalize) {
	    if ((n[0].type == ASCII) || (n[0].type == JISROMAN))
		if (('a' <= n[0].c1) && (n[0].c1 <= 'z'))
		    n[0].c1 = n[0].c1 - 0x20;
	} else if (romaji_upcase) {
	    for (ptr = n; ptr->c1 != '\0'; ++ptr) {
		if ((ptr->type == ASCII) || (ptr->type == JISROMAN))
		    if (('a' <= ptr->c1) && (ptr->c1 <= 'z'))
			ptr->c1 = ptr->c1 - 0x20;
	    }
	}
    }

    if ((kanji_digest) && (furigana_mode)) {
	for (i = 0; i < ret; ++ i)
	    putkanji(c+i);
	/* put parentheses around furigana (a.k.a. ruby) */
	for (i=0; i<KAKASIBUF && separator[i].c1 != 0; i++) {
	    putchars(&left_paren[i]);
	}
	putchars(n);
	for (i=0; i<KAKASIBUF && separator[i].c1 != 0; i++) {
	    putchars(&right_paren[i]);
	}
#ifdef WAKATIGAKI
    } else if ((kanji_digest) && (wakatigaki_mode)) {
	for (i = 0; i < ret; ++ i)
	    putkanji(c+i);
#endif /* WAKATIGAKI */
    } else if ((kanji_digest) && (level_hiragana_mode)) {
	if (check_kanji_level(c, ret, level_hiragana_mode)) {
            for (i = 0; i < ret; i++)
		putkanji(c+i);
	} else {
	    putchars(n);
	}
    } else if ((kanji_digest) && (level_furigana_mode)) {
	for (i = 0; i < ret; ++ i)
	    putkanji(c+i);
	if (! check_kanji_level(c, ret, level_furigana_mode)) {
	    /* put parentheses around furigana (a.k.a. ruby) */
	    for (i=0; i<KAKASIBUF && separator[i].c1 != 0; i++) {
		putchars(&left_paren[i]);
	    }
	    putchars(n);
	    for (i=0; i<KAKASIBUF && separator[i].c1 != 0; i++) {
		putchars(&right_paren[i]);
	    }
	}
    } else {
	putchars(n);
    }
    if (flush_mode) fflush(stdout);
}

KAKASI_ATTR int
digest(c, clen, r, rlen, type, proc)
     Character *c;
     int clen;
     Character *r;
     int rlen;
     int type;
     int (*proc)();
{
    int ret, i, j, k;
    Character new;
    char *p;

    ret = (* proc)(c, n);
    if (ret == 0) ret = 1;

    if ((ret < 0) && (rlen < KAKASIBUF)) {
	getkanji(&new);
	if(new.type == type) {
	    r[rlen].type = c[clen].type = type;
	    r[rlen].c1 = c[clen].c1 = new.c1;
	    r[rlen].c2 = c[clen].c2 = new.c2;
	    r[rlen+1].type = c[clen+1].type = OTHER;
	    r[rlen+1].c1 = c[clen+1].c1 = '\0';
	    return digest(c, clen+1, r, rlen+1, type, proc);
	} else if (cr_eat_mode) {
	    if ((rlen < KAKASIBUF -1) && /* keep in check a buffer overflow */
		((new.type == ASCII) || (new.type == JISROMAN) || (new.type == OTHER))) {
		for (p = cr_eat_string; *p != '\0'; ++ p) {
		    if ((unsigned)(*p) == new.c1) {
			r[rlen].type = new.type;
			r[rlen].c1 = new.c1;
			r[rlen].c2 = new.c2;
			r[rlen+1].type = OTHER;
			r[rlen+1].c1 = '\0';
			return digest(c, clen, r, rlen+1, type, proc);
		    }
		}
	    }
	}
	ungetkanji(&new);
	ret = -ret;
    }

    digest_out(c, ret);

#ifdef EACH_YOMI
    if (eachyomi_mode) {
	output_yomi_eachkanji(c, ret);
    }
#endif /* EACH_YOMI */

    k = ret;
    j = 0;
    for (i = 0;; ++ i) {
	if ((r[i].type == type) && (k > 0)) {
	    -- k;
	} else {
	    c[j].type = r[i].type;
	    c[j].c1 = r[i].c1;
	    c[j].c2 = r[i].c2;
	    if (c[j].c1 == '\0')
		break;
	    ++ j;
	}
    }
    return rlen - ret;
}

KAKASI_ATTR void
digest_shift(c, s)
     Character *c;
     int s;
{
    int i;

    for (i = 0;; ++ i) { /* Yes, I know following lines can be written in
			    1 line, but I have doubts of compatibilities.. */
	c[i].type = c[i+s].type;
	c[i].c1 = c[i+s].c1;
	c[i].c2 = c[i+s].c2;
	if (c[i+s].c1 == '\0')
	    break;
    }
}

#ifndef LIBRARY
int
main(argc, argv)
     int argc;
     char **argv;
#else
int
kakasi_getopt_argv(argc, argv)
     int argc;
     char **argv;
#endif
{
#ifdef LIBRARY
  int retval = 0;
#endif
#ifndef LIBRARY
    Character c[KAKASIBUF], r[KAKASIBUF];
    int clen, ptype, pctype;
    static int (*proc[8])()={NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
    /* ASCII, JISROMAN, KATAKANA, GRAPHIC, ZENKAKU-KIGOU, ZENKAKU-KATAKANA, ZENKAKU-HIRAGANA, KANJI, */
#endif

    int i;
#ifdef LIBRARY
    /* Initialize some valuables. */
    for (i = 0; i < 8; i ++) {
      proc[i] = NULL;
    }
    romaji_type = HEPBURN;
    heiki_mode = 0;
    bunkatu_mode = 0;
    furigana_mode = 0;
    cr_eat_mode = 0;
    romaji_capitalize = 0;
    romaji_upcase = 0;
    flush_mode = 0;
#ifdef WAKATIGAKI
    wakatigaki_mode = 0;
    terminate_done = 0;
    wo_mode = 0;
#endif /* WAKATIGAKI*/
    input_term_type = UNKNOWN;
    output_term_type = UNKNOWN;
#endif
    /* Initialize separator */
    separator[0].type = OTHER;
    separator[0].c1 = ' ';
    separator[0].c2 = 0;
    for (i=1; i<KAKASIBUF; i++)
	separator[i].c1 = separator[i].c2 = 0;
    /* Initialize parentheses around furigana (a.k.a. ruby) */
    left_paren[0].type = OTHER;
    left_paren[0].c1 = '[';
    left_paren[0].c2 = 0;
    for (i=1; i<KAKASIBUF; i++)
	left_paren[i].c1 = left_paren[i].c2 = 0;
    right_paren[0].type = OTHER;
    right_paren[0].c1 = ']';
    right_paren[0].c2 = 0;
    for (i=1; i<KAKASIBUF; i++)
	right_paren[i].c1 = right_paren[i].c2 = 0;

    while(--argc > 0) {
	++ argv;
	if ((*argv)[0] != '-') break;
	switch((*argv)[1]) {
	  case 'a':
	    switch((*argv)[2]) {
	      case 'j':	proc[0] = a2j; break;
	      case 'E':	proc[0] = a2E; break;
	      default:  proc[0] = NULL;
	    }
	    break;
	  case 'j':
	    switch((*argv)[2]) {
	      case 'a':	proc[1] = j2a; break;
	      case 'E':	proc[1] = j2E; break;
	      default:  proc[1] = NULL;
	    }
	    break;
	  case 'g':
	    switch((*argv)[2]) {
	      case 'a':	proc[2] = g2a; break;
	      case 'j':	proc[2] = g2j; break;
	      case 'E':	proc[2] = g2E; break;
	      default:  proc[2] = NULL;
	    }
	    break;
	  case 'k':
	    switch((*argv)[2]) {
	      case 'a':	proc[3] = k2a; break;
	      case 'j':	proc[3] = k2j; break;
	      case 'K':	proc[3] = k2K; break;
	      case 'H':	proc[3] = k2H; break;
	      default:  proc[3] = NULL;
	    }
	    break;
	  case 'E':
	    switch((*argv)[2]) {
	      case 'a':	proc[4] = E2a; break;
	      case 'j':	proc[4] = E2j; break;
	      default:  proc[4] = NULL;
	    }
	    break;
	  case 'K':
	    switch((*argv)[2]) {
	      case 'a':	proc[5] = K2a; break;
	      case 'j':	proc[5] = K2j; break;
	      case 'k':	proc[5] = K2k; break;
	      case 'H':	proc[5] = K2H; break;
	      default:  proc[5] = NULL;
	    }
	    break;
	  case 'H':
	    switch((*argv)[2]) {
	      case 'a':	proc[6] = H2a; break;
	      case 'j':	proc[6] = H2j; break;
	      case 'k':	proc[6] = H2k; break;
	      case 'K':	proc[6] = H2K; break;
	      case 'H':	proc[6] = H2H; break;
	      default:  proc[6] = NULL;
	    }
	    break;
	  case 'J':
	    switch((*argv)[2]) {
	      case 'a':	proc[7] = J2a; break;
	      case 'j':	proc[7] = J2j; break;
	      case 'k':	proc[7] = J2k; break;
	      case 'K':	proc[7] = J2K; break;
	      case 'H':	proc[7] = J2H; break;
	      default:  proc[7] = NULL;
	    }
	    break;
	  case 'i':
	    if ((*argv)[2] != '\0')
		set_input_term(term_type_str((*argv)+2));
	    else
		if (argc > 1) {
		    -- argc;
		    set_input_term(term_type_str(*(++ argv)));
		}
	    break;
	  case 'o':
	    if ((*argv)[2] != '\0')
		set_output_term(term_type_str((*argv)+2));
	    else
		if (argc > 1) {
		    -- argc;
		    set_output_term(term_type_str(*(++ argv)));
		}
	    break;
	  case 'r':
	    if ((*argv)[2] == 'k')
		romaji_type = KUNREI;
	    break;
	  case 'p':
	    heiki_mode = 1;
	    break;
	  case 's':
	    bunkatu_mode = 1;
	    break;
	  case 'S':
	    separator[0].type = OTHER;
	    for (i=0; i<KAKASIBUF && *(*(argv)+2+i) != 0; i++) {
		separator[i].c1 = *((*argv)+2+i);
		separator[i].c2 = 0;
	    }
	    break;
	  case 'f':
	    furigana_mode = 1;
	    break;
	  case 'F':
	    switch((*argv)[2]) {
	      case 'l':
		left_paren[0].type = OTHER;
		for (i=0; i<KAKASIBUF && *(*(argv)+3+i) != 0; i++) {
		    left_paren[i].c1 = *((*argv)+3+i);
		    left_paren[i].c2 = 0;
		}
		break;
	      case 'r':
		right_paren[0].type = OTHER;
		for (i=0; i<KAKASIBUF && *(*(argv)+3+i) != 0; i++) {
		    right_paren[i].c1 = *((*argv)+3+i);
		    right_paren[i].c2 = 0;
		}
		break;
	      default:
		break;
	    }
	    break;
	  case 'c':
	    cr_eat_mode = 1;
	    sprintf(cr_eat_string, "\011\012\015 %s", (*argv)+2);
	    break;
	  case 'C':
	    romaji_capitalize = 1;
	    break;
	  case 'U':
	    romaji_upcase = 1;
	    break;
	  case 'u':
	    flush_mode = 1;
	    break;
	  case 't':
	    use_old_romaji_table = 1;
#ifdef WAKATIGAKI
	  case 'w':
	    wakatigaki_mode = 1;
	    bunkatu_mode = 1;
	    cr_eat_mode = 1;
	    sprintf(cr_eat_string, "\011\012\015 %s", (*argv)+2);
	    proc[5] = K2K;
	    proc[6] = H2H;
	    proc[7] = J2H;
	    break;
#endif /* WAKATIGAKI */
	  case 'l':
	    switch((*argv)[2]) {
	      case '0': case '1': case '2': case '3': case '4': case '5':
	      case '6': case '7': case '8': case '9': case 'j': case 'n':
		level_hiragana_mode = (*argv)[2]; break;
	    }
	    cr_eat_mode = 1;
	    sprintf(cr_eat_string, "\011\012\015");
	    proc[5] = K2K;
	    proc[6] = H2H;
	    proc[7] = J2H;
	    break;
	  case 'L':
	    switch((*argv)[2]) {
	      case '0': case '1': case '2': case '3': case '4': case '5':
	      case '6': case '7': case '8': case '9': case 'j': case 'n':
		level_furigana_mode = (*argv)[2]; break;
	    }
	    cr_eat_mode = 1;
	    sprintf(cr_eat_string, "\011\012\015");
	    proc[5] = K2K;
	    proc[6] = H2H;
	    proc[7] = J2H;
	    break;
#ifdef EACH_YOMI
	  case 'y':
	    eachyomi_mode = 1;
	    break;
#endif /* EACH_YOMI */
	  case '?':
	  default:
#ifndef LIBRARY
	    fprintf(stderr, "KAKASI - Kanji Kana Simple Inverter  Version %s\n", VERSION);
	    fprintf(stderr, "Copyright (C) 1992-1999 Hironobu Takahashi. All rights reserved.\n");
	    fprintf(stderr, "\n");
	    fprintf(stderr, "Usage: kakasi -a[jE] -j[aE] -g[ajE] -k[ajKH] -E[aj] -K[ajkH] -H[ajkKH] -J[ajkKH]\n");
#ifdef KAKASI_SUPPORT_UTF8
	    fprintf(stderr, "              -i{oldjis,newjis,dec,euc,sjis,utf8} -o{oldjis,newjis,dec,euc,sjis,utf8}\n");
#else
	    fprintf(stderr, "              -i{oldjis,newjis,dec,euc,sjis} -o{oldjis,newjis,dec,euc,sjis}\n");
#endif /* KAKASI_SUPPORT_UTF8 */
	    fprintf(stderr, "              -r{hepburn,kunrei} -p -s -f -c\"chars\"  [jisyo1, jisyo2,,,]\n");
	    fprintf(stderr, "\n");
	    fprintf(stderr, "      Character Sets:\n");
	    fprintf(stderr, "       a: ascii  j: jisroman  g: graphic  k: kana (j,k     defined in jisx0201)\n");
	    fprintf(stderr, "       E: kigou  K: katakana  H: hiragana J: kanji(E,K,H,J defined in jisx0208)\n");
	    fprintf(stderr, "\n");
	    fprintf(stderr, "      Options:\n");
	    fprintf(stderr, "      -i: input coding system    -o: output coding system\n");
	    fprintf(stderr, "      -r: romaji conversion system\n");
	    fprintf(stderr, "      -p: list all readings (with -J option)\n");
	    fprintf(stderr, "      -s: insert separate characters (with -J option)  -S\"chars\": set separator\n");
	    fprintf(stderr, "      -f: furigana mode (with -J option)\n");
	    fprintf(stderr, "      -F[rl]\"chars\": set parentheses around furigana\n");
	    fprintf(stderr, "      -c: skip chars within jukugo (with -J option: default TAB CR LF BLANK)\n");
	    fprintf(stderr, "      -C: romaji Capitalize (with -Ja or -Jj option)\n");
	    fprintf(stderr, "      -U: romaji Upcase     (with -Ja or -Jj option)\n");
	    fprintf(stderr, "      -u: call fflush() after 1 character output\n");
	    fprintf(stderr, "      -t: use old romaji table\n");
#ifdef WAKATIGAKI
	    fprintf(stderr, "      -w: wakatigaki mode\n");
#endif /* WAKATIGAKI */
	    fprintf(stderr, "      -{l,L}: level {hiragana,furigana} mode (-{l,L}[123456jn])\n");
#ifdef EACH_YOMI
	    fprintf(stderr, "      -y: display yomi of each kanji characters\n");
#endif /* EACH_YOMI */
	    fprintf(stderr, "\n");
	    fprintf(stderr, "Report bugs to <bug-kakasi@namazu.org>.\n");
	    exit(1);
#else /* LIBRARY */
	    retval = 1;
#endif
	}
    }

    if ((input_term_type != UNKNOWN) && (output_term_type == UNKNOWN))
	set_output_term(input_term_type);

#ifdef LIBRARY
    free_jisyo();
    kakasi_close_kanwadict();
#ifdef KAKASI_SUPPORT_UTF8
    close_iconv();
#endif /* KAKASI_SUPPORT_UTF8 */

#endif /* LIBRARY */
    init_jisyo();
    init_kanwa();
    if (proc[7] != NULL) {
	for (; argc > 0; -- argc)
	    add_jisyo(*(argv ++));
    }

#ifdef LIBRARY
    return retval;
}

char *
kakasi_do(str)
     char *str;
{
    Character c[KAKASIBUF], r[KAKASIBUF];
    int clen, ptype, pctype;

    setcharbuffer((unsigned char *)str);
#endif

    ptype = pctype = OTHER;
    separator_out = 0;
    for(;;) {
	getkanji(c);
	if ((c[0].type == OTHER) && (c[0].c1 == 0xff)) break;
	c[1].type = OTHER;
	c[1].c1 = '\0';
	clen = 1;
	while (clen > 0) {
	    kanji_digest = 0;
	    switch (c[0].type) {
	      case ASCII:
	      case JISROMAN:
	      case GRAPHIC:
	      case KATAKANA:
		if ((c[0].type != OTHER) && (c[0].type != pctype)) {
		    put_separator();
		    pctype = c[0].type;
		}
		if ((*proc[(int)(c[0].type)]) == NULL) {
		    putkanji(c); digest_shift(c, 1); -- clen;
		    if (flush_mode) fflush(stdout);
		} else {
		    digest_start_copy(c, r);
		    clen = digest(c, clen, r, clen, (int)(c[0].type), *proc[(int)(c[0].type)]);
		}
#ifdef WAKATIGAKI
		terminate_done = 0;
#endif /* WAKATIGAKI */
		break;
	      case JIS83:
		if (c[0].c1 >= 0xb0) {
		    ptype = 7;
		    kanji_digest = 1;
#ifdef WAKATIGAKI
		} else if ((c[0].c1 == 0xa1) && /* charcter code(\241\270),charcter code(\241\271),charcter code(\241\272) */
			   (c[0].c2 >= 0xb8 && c[0].c2 <= 0xba)) {
		    ptype = 7;
		    kanji_digest = 1;
		} else if ((c[0].c1 == 0xa5) && /* charcter code(\245\365),charcter code(\245\366) */
			   (c[0].c2 >= 0xf5 && c[0].c2 <= 0xf6)) {
		    ptype = 7;
		    kanji_digest = 1;
#endif /* WAKATIGAKI */
    		} else if (c[0].c1 == 0xa4) {
		    ptype = 6;
#ifdef WAKATIGAKI
		} else if ((c[0].c1 == 0xa1)  && /* charcter code(\241\263),charcter code(\241\264),charcter code(\241\265),charcter code(\241\266) */
			   (c[0].c2 >= 0xb3 && c[0].c2 <= 0xb6)) {
		    if (c[0].c2 == 0xb3 || c[0].c2 == 0xb4) {
			ptype = 5;
		    } else if (c[0].c2 == 0xb5 || c[0].c2 <= 0xb6) {
			ptype = 6;
		    }
#endif /* WAKATIGAKI */
		} else if (c[0].c1 == 0xa5) {
		    ptype = 5;
		} else if ((c[0].c1 == 0xa1) && (c[0].c2 == 0xbc)) {
		    if (pctype == 5) {
			ptype = 5;
		    } else if (pctype == 6) {
			ptype = 6;
		    } else {
			ptype = 5;
		    }
		} else {
		    ptype = 4;
		}
		if (ptype != pctype) {
		    put_separator();
		    pctype = ptype;
		}
		if ((*proc[ptype]) == NULL) {
		    putkanji(c); digest_shift(c, 1); -- clen;
		    if (flush_mode) fflush(stdout);
		} else {
		    digest_start_copy(c, r);
		    clen = digest(c, clen, r, clen, JIS83, *proc[ptype]);
		}
#ifdef WAKATIGAKI
		terminate_done = 0;
#endif /* WAKATIGAKI */
		break;
	      default:
#ifdef WAKATIGAKI
		terminate_done = 1;
#endif /* WAKATIGAKI */
		putkanji(c); digest_shift(c, 1); -- clen;
#ifndef LIBRARY
		if (flush_mode) fflush(stdout);
#endif
	    }
	}
    }
#ifndef LIBRARY
    return 0;
#else /* LIBRARY */
    {
	char *ret = getpbstr();
	if (ret == NULL)
	    return strdup("");
	return ret;
    }
#endif
}

#ifdef LIBRARY
int
kakasi_close_kanwadict()
{
    if (kanwadict != NULL) {
	fclose(kanwadict);
	kanwadict = NULL;
	return 0;
    }
    return 1;
}

static void
free_jisyo()
{
    size_t x;

    if (ary_charalloc) {
	for (x = 0; x <= ary_cur_charalloc; x ++) {
	    free(ary_charalloc[x]);
	}
    }

    if (ary_cellalloc) {
	for (x = 0; x <= ary_cur_cellalloc; x ++) {
	    free(ary_cellalloc[x]);
	}
    }

    free(ary_charalloc);
    free(ary_cellalloc);
    
    ary_charalloc = NULL;
    ary_cellalloc = NULL;
    ary_size_charalloc = -1;
    ary_cur_charalloc = -1;
    ary_size_cellalloc = -1;
    ary_cur_cellalloc = -1;
    point_charalloc = 0;
    ptr_charalloc = NULL;
    point_cellalloc = 0;
    ptr_cellalloc = NULL;
}

int
kakasi_free(char *p)
{
    if (p) {
	free(p);
	return 1;
    }
    return 0;
}
#endif /* LIBRARY */

#ifdef EACH_YOMI

/*
  each_yomi output
 */

Character ek_bc[] = {{ASCII, '[', 0}, {OTHER, 0, 0}};
Character ek_ec[] = {{ASCII, ']', 0}, {OTHER, 0, 0}};
Character ek_kysep[] = {{ASCII, ':', 0}, {OTHER, 0, 0}};
Character ek_kksep[] = {{ASCII, ',', 0}, {OTHER, 0, 0}};

void
putkanjis(c)
    Character *c;
{
    while (c->type != OTHER && c->c1 != 0) {
        putkanji(c);
        c ++;
    }
}

void
output_yomi_eachkanji(Character *c, int len)
{
    int old_hy, i;
    Character cbuf[KAKASIBUF], rbuf[KAKASIBUF];
    old_hy = heiki_mode;
    heiki_mode = 1;

    if (! (c[0].c1 >= 0xb0 ||
	((c[0].c1 == 0xa1) && (c[0].c2 >= 0xb8 && c[0].c2 <= 0xba)) ||
	((c[0].c1 == 0xa5) && (c[0].c2 >= 0xf5 && c[0].c2 <= 0xf6))))
	return;
    
    putkanji(ek_bc);
    for (i = 0; i < len; i ++) {
	if (c[i].c1 >= 0xb0 ||
	    ((c[i].c1 == 0xa1) && (c[i].c2 >= 0xb8 && c[i].c2 <= 0xba)) ||
	    ((c[i].c1 == 0xa5) && (c[i].c2 >= 0xf5 && c[i].c2 <= 0xf6))) {
	    if (i > 0) putkanji(ek_kksep);
	    memcpy(cbuf, &c[i], sizeof(Character));
	    cbuf[1].type = OTHER;
	    cbuf[1].c1 = 0;
	    cbuf[1].c2 = 0;
	    putkanji(cbuf);
	    putkanji(ek_kysep);
	    J2H(cbuf, rbuf);
	    putkanjis(rbuf);
	}
    }
    putkanji(ek_ec);

    heiki_mode = old_hy;
}

#endif /* EACH_YOMI */
