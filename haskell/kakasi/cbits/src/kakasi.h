/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: kakasi.h,v 1.13 2004-03-01 05:17:15 knok Exp $
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

#ifndef _KAKASI_H
#define _KAKASI_H

#ifndef PARAMS
# if __STDC__ || defined __cplusplus
# define PARAMS(args) args
# else
# define PARAMS(args) ()
# endif
#endif


/*
 * macro definitions
 */

/* character set */
#define ASCII    0
#define JISROMAN 1
#define GRAPHIC  2
#define KATAKANA 3
#define JIS78    4
#define JIS83    5
#define OTHER  127

/* assign */
#define SETG0  0
#define SETG1  1
#define SETG2  2
#define SETG3  3
#define SJKANA 4

/* terminal type */
#define UNKNOWN 0
#define OLDJIS  1
#define NEWJIS  2
#define DEC     3
#define EUC     4
#define MSKANJI 5
#define UTF8    6

/* options */
#define UNINITIALIZED -1
#define HEPBURN 0
#define KUNREI  1

#define MAXJISYO 10

#define KAKASIBUF 256


/*
 * data structures
 */

typedef struct character {
    char type;
    unsigned char c1, c2;
} Character;

/* The internal dictionary format of KAKASI. */

extern struct kanji_yomi *jisyo_table[0x80][0x80]; /* hash table */
extern int kanwa_load[0x80][0x80];                 /* Is kanwadict put? */

struct kanji_yomi {
    struct kanji_yomi *next;
    int  length;
    unsigned char *kanji;
    unsigned char *yomi;
    unsigned char tail;
};

/* The dictionary format of kanwadict. */

/*
 * new dict format - 2003/03/11 ukai
 *
 * 0x0000      |  K  A  K  A  S  I <endian mark> | magic
 * 0x0008      |  nn nn nn nn | <kanwa offset>
 *             |  00 00 00 00 | <option length> = 0
 *             |  00 00 00 00 | <option type>   = DICT_OPT_END(0)
 * 0+<kanwa offset> 
 *             |  kanwa_entry |
 * ...  rest is same as old version
 *
 *  <endian mark>
 *    FF FE = little endian (x86)
 *    FE FF = big endian
 * 
 *  we assume 'int' has 4 bytes.
 *  <kanwa offset> and <option length> must be a multiple of 4
 *  to avoid alignment issues.
 *
 *  option may be used to specify encoding or something like that.
 *
 * old dict format - endian dependent
 *
 * 0000        | kanwa_entry |
 *  :   <0x60 * 0x60 * sizoef(int)>
 * <index>     | tail         |
 * <index>+1   | kanji_length |
 * <index>+2   | kanji[]      |
 * <index>+2+kl| yomi_length  |
 * <index>+3+kl| yomi[]       |
 * <index>.... | <next entry> |
 *
 */
/* option type */
#define KAKASI_DICT_OPT_END		0
#define KAKASI_DICT_OPT_ENCODING	1
   /* A part at the head of kakasi, load does only this at the time of the start. */
struct kanwa_entry {
    int index; /* It points at the offset in the file. */
    int entry; /* The registered number of words. */
};
extern struct kanwa_entry kanwa[0x60][0x60];

   /* The part of the body.
unsigned char tail;         If there is an okurigana, if it is not here, '0'.
unsigned char kanji_length; The entire length of the part of the kanji, a character at the head, '\0' of the end aren't contained.
unsigned char kanji[];      The body of the kanji.
unsigned char yomi_length;  The entire length of the part of the reading, '\0' of the end aren't contained.
unsigned char yomi[];       The body of the reading.
*/


/*
 * global variables.
 */

extern int input_term_type;

extern int input_GL;
extern int input_GR;
extern int input_G[5];

extern int output_term_type;

extern int output_GL;
extern int output_GR;
extern int output_G[5];

/* options */
extern int romaji_type;
extern int use_old_romaji_table;
extern int romaji_capitalize;
extern int romaji_upcase;
extern int bunkatu_mode;
extern int heiki_mode;
extern int furigana_mode;
extern int cr_eat_mode;
extern int separator_out;
#ifdef WAKATIGAKI
extern int wo_mode;
extern int terminate_done;
extern Character separator[KAKASIBUF];
#endif /* WAKATIGAKI */


/*
 * prototype declarations
 */

/* a2.c */
extern int a2j PARAMS((Character *c, Character *n));
extern int a2E PARAMS((Character *c, Character *n));

/* g2.c */
extern int g2a PARAMS((Character *c, Character *n));
extern int g2j PARAMS((Character *c, Character *n));
extern int g2E PARAMS((Character *c, Character *n));

/* j2.c */
extern int j2a PARAMS((Character *c, Character *n));
extern int j2E PARAMS((Character *c, Character *n));

/* k2.c */
extern int k2a PARAMS((Character *c, Character *n));
extern int k2j PARAMS((Character *c, Character *n));
extern int k2K PARAMS((Character *c, Character *n));
extern int k2H PARAMS((Character *c, Character *n));

/* ee2.c */
extern int E2a PARAMS((Character *c, Character *n));
extern int E2j PARAMS((Character *c, Character *n));

/* hh2.c */
extern int H2a PARAMS((Character *c, Character *n));
extern int H2j PARAMS((Character *c, Character *n));
extern int H2K PARAMS((Character *c, Character *n));
extern int H2k PARAMS((Character *c, Character *n));
#ifdef WAKATIGAKI
extern int H2H PARAMS((Character *c, Character *n));
#endif /* WAKATIGAKI */

/* jj2.c */
extern int J2H PARAMS((Character *c, Character *n));
extern int J2a PARAMS((Character *c, Character *n));
extern int J2j PARAMS((Character *c, Character *n));
extern int J2k PARAMS((Character *c, Character *n));
extern int J2K PARAMS((Character *c, Character *n));

/* kk2.c */
extern int K2a PARAMS((Character *c, Character *n));
extern int K2j PARAMS((Character *c, Character *n));
extern int K2H PARAMS((Character *c, Character *n));
extern int K2k PARAMS((Character *c, Character *n));
#ifdef WAKATIGAKI
extern int K2K PARAMS((Character *c, Character *n));
#endif /* WAKATIGAKI */

/* Other prototypes */

/* kanjiio.c */
extern void getkanji PARAMS((Character *c));
extern void putkanji PARAMS((Character *c));
extern void ungetkanji PARAMS((Character *c));
extern int term_type_str PARAMS((char *str));
extern void set_input_term PARAMS((int type));
extern void set_output_term PARAMS((int type));
#ifdef LIBRARY
extern void putcharpbuf PARAMS((int c));
#endif /* LIBRARY */

/* 78_83.c */
extern void exc78_83 PARAMS((Character *c));

/* itaiji.c */
extern void mkitaijitbl PARAMS((void));
extern void itaijiknj PARAMS((int *c1, int *c2));

/* dict.c */
extern void init_jisyo PARAMS((void));
extern void add_jisyo PARAMS((char *filename));
extern void init_kanwa PARAMS((void));
extern void add_kanwa PARAMS((int c1, int c2));

/* hh2.c */
extern int H2rom PARAMS((Character *c, Character *n, int type));
/* kk2.c */
extern int K2rom PARAMS((Character *c, Character *n, int type));

/* level.c */
extern int check_kanji_level PARAMS((Character *c, int ret, int level));

#endif /* _KAKASI_H */
