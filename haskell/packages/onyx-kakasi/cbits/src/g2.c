/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: g2.c,v 1.3 2013-01-31 06:49:43 knok Exp $
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
g2a(c, n)
     Character *c;
     Character *n;
{
    static unsigned char g2a_table[0x1f] = {
	'O','=','h','f','c','l','O','+','n','v','+','+','+','+','+','-',
	'-','-','-','-','+','+','+','+','|','<','>','p','!','$','.'};

    n[0].type = ASCII;
    if ((c->c1 >= 0x60) && (c->c1 < 0x7f)) {
	n[0].c1 = g2a_table[c->c1 - 0x60];
    }
    n[1].type=OTHER;
    n[1].c1 = '\0';
    return 1;
}

int
g2j(c, n)
     Character *c;
     Character *n;
{
    static unsigned char g2j_table[0x1f] = {
	'O','=','h','f','c','l','O','+','n','v','+','+','+','+','+','-',
	'-','-','-','-','+','+','+','+','|','<','>','p','!','$','.'};

    c->type = ASCII;
    if ((c->c1 >= 0x60) && (c->c1 < 0x7f)) {
	c->c1 = g2j_table[c->c1 - 0x60];
    }
    n[1].type=OTHER;
    n[1].c1 = '\0';
    return 1;
}

int
g2E(c, n)
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
	"\243\330", "\243\331", "\243\332", "\241\316", "\241\357", "\241\317", "\241\260", "\241\241",
	"\242\241", "\242\243", "\243\350", "\243\346", "\243\343", "\243\354", "\243\260", "\241\336",
	"\243\356", "\243\366", "\250\245", "\250\244", "\250\243", "\250\246", "\250\253", "\250\241",
	"\250\241", "\250\241", "\250\241", "\250\241", "\250\247", "\250\251", "\250\252", "\250\250",
	"\250\242", "\241\345", "\241\346", "\246\320", "\241\342", "\241\362", "\241\246" };

    c1 = c -> c1;
    n[0].type=JIS83;
    n[0].c1 = j2E_table[c1-0x20][0];
    n[0].c2 = j2E_table[c1-0x20][1];
    n[1].type=OTHER;
    n[1].c1 = '\0';
    return 1;
}

