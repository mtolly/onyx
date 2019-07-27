/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: conv-util.c,v 1.2 2001-01-16 07:51:47 rug Exp $
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
#include "conv-util.h"

int
isallkana(str)
     unsigned char *str;
{
    while(*str) {
	if (str[0] <= 0xa0) return 0;
	if (str[1] <= 0xa0) return 0;
	if ((str[0] != 0xa4) && (str[0] != 0xa5)) return 0;
	str += 2;
    }
    return 1;
}

int
isallzenkaku(str)
     unsigned char *str;
{
    while(*str) {
	if (str[0] <= 0xa0) return 0;
	if (str[1] <= 0xa0) return 0;
	str += 2;
    }
    return 1;
}

int
includekanji(str)
     unsigned char *str;
{
    while(*str) {
	if (str[0] >= 0xb0) return 1;
	str += 2;
    }
    return 0;
}
