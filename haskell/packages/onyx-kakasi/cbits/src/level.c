/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: level.c,v 1.1 2002-12-06 13:45:56 baba Exp $
 * Copyright (C) 2002 Hajime BABA  All rights reserved.
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
#include "kakasi.h"
#include "level.h"


/*
 * Return 1 if the character is learned in the level.
 */
static int
is_learned_char(c, level)
     Character c;
     int level;
{
    int i,n;

    /* in case of okurigana (= hiragana,katakana: learned character) */
    if (c.c1 == 0xA4 || c.c1 == 0xA5)
	return 1;
    if ((c.c1 == 0xA1) &&
	((c.c2 >= 0xB3 && c.c2 <= 0xB6) ||
	 (c.c2 >= 0xB8 && c.c2 <= 0xBA) || (c.c2 == 0xBC)))
	return 1;

    n = c.c1 - 0xA0;
    switch (level) {
    case 'n':
	for (i=leveln_index[n-1]; i<leveln_index[n]; i++) {
	    if (c.c2 == leveln_table[i][1])
		return 1;
	}
    case 'j':
    case '7':
    case '8':
    case '9':
	for (i=levelj_index[n-1]; i<levelj_index[n]; i++) {
	    if (c.c2 == levelj_table[i][1])
		return 1;
	}
    case '6':
	for (i=level6_index[n-1]; i<level6_index[n]; i++) {
	  if (c.c2 == level6_table[i][1])
		return 1;
	}
    case '5':
	for (i=level5_index[n-1]; i<level5_index[n]; i++) {
	    if (c.c2 == level5_table[i][1])
		return 1;
	}
    case '4':
	for (i=level4_index[n-1]; i<level4_index[n]; i++) {
	    if (c.c2 == level4_table[i][1])
		return 1;
	}
    case '3':
	for (i=level3_index[n-1]; i<level3_index[n]; i++) {
	    if (c.c2 == level3_table[i][1])
		return 1;
	}
    case '2':
	for (i=level2_index[n-1]; i<level2_index[n]; i++) {
	    if (c.c2 == level2_table[i][1])
		return 1;
	}
    case '1':
	for (i=level1_index[n-1]; i<level1_index[n]; i++) {
	    if (c.c2 == level1_table[i][1])
		return 1;
	}
    case '0':
    default:
	break;
    }
    return 0;
}

/*
 * Return 1 if all characters of the word are learned in the level.
 * Return 0 if any character of the word is unlearned in the level.
 */
int
check_kanji_level(c, ret, level)
     Character *c;
     int ret;
     int level;
{
    int i;
    int flag=0;

    for (i=0; i<ret; i++) {
	if (is_learned_char(*(c+i), level)) {
	    flag++;
	}
    }
    if (flag == ret)
	return 1;
    else
	return 0;
}
