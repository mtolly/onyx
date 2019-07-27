/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: j2.c,v 1.2 2001-01-16 07:51:47 rug Exp $
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

#include "kakasi.h"

int
j2a(c, n)
     Character *c;
     Character *n;
{
    n[0].type=ASCII;
    n[0].c1 = c->c1;
    n[1].type=OTHER;
    n[1].c1 = '\0';
    return 1;
}

int
j2E(c, n)
     Character *c;
     Character *n;
{
    int c1;
    static unsigned char j2E_table[95][3] = {
	"\241\241", "\241\252", "\241\311", "\241\364", "\241\360", "\241\363", "\241\365", "\241\307",
	"\241\312", "\241\313", "\241\366", "\241\334", "\241\244", "\241\335", "\241\245", "\241\277",
	"\243\260", "\243\261", "\243\262", "\243\263", "\243\264", "\243\265", "\243\266", "\243\267",
	"\243\270", "\243\271", "\241\247", "\241\250", "\241\343", "\241\341", "\241\344", "\241\251",
	"\241\367", "\243\301", "\243\302", "\243\303", "\243\304", "\243\305", "\243\306", "\243\307",
	"\243\310", "\243\311", "\243\312", "\243\313", "\243\314", "\243\315", "\243\316", "\243\317",
	"\243\320", "\243\321", "\243\322", "\243\323", "\243\324", "\243\325", "\243\326", "\243\327",
	"\243\330", "\243\331", "\243\332", "\241\316", "\241\357", "\241\317", "\241\260", "\241\262",
	"\241\256", "\243\341", "\243\342", "\243\343", "\243\344", "\243\345", "\243\346", "\243\347",
	"\243\350", "\243\351", "\243\352", "\243\353", "\243\354", "\243\355", "\243\356", "\243\357",
	"\243\360", "\243\361", "\243\362", "\243\363", "\243\364", "\243\365", "\243\366", "\243\367",
	"\243\370", "\243\371", "\243\372", "\241\320", "\241\303", "\241\321", "\241\261" };

    c1 = c -> c1;
    n[0].type=JIS83;
    n[0].c1 = j2E_table[c1-0x20][0];
    n[0].c2 = j2E_table[c1-0x20][1];
    n[1].type=OTHER;
    n[1].c1 = '\0';
    return 1;
}

