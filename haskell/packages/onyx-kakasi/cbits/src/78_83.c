/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: 78_83.c,v 1.2 2001-01-16 07:51:47 rug Exp $
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

void
exc78_83(c)
     Character *c;
{
    int i;
    static unsigned char table78_83[22][2][3] = {
	{"\260\263", "\362\315"},	{"\262\251", "\362\364"},	{"\263\302", "\351\332"},	{"\263\311", "\331\370"},
	{"\263\366", "\343\336"},	{"\264\303", "\336\365"},	{"\264\322", "\353\335"},	{"\267\333", "\360\364"},
	{"\271\334", "\342\350"},	{"\274\311", "\351\242"},	{"\277\331", "\360\327"},	{"\301\250", "\354\315"},
	{"\304\333", "\324\344"},	{"\305\327", "\342\352"},	{"\305\356", "\333\355"},	{"\305\363", "\336\271"},
	{"\306\366", "\355\356"},	{"\307\350", "\352\244"},	{"\311\260", "\333\330"},	{"\313\371", "\320\326"},
	{"\314\371", "\351\256"},	{"\317\266", "\344\306"}};

    switch(c->type) {
      case JIS78:
	c->type = JIS83;
	break;
      case JIS83:
	c->type = JIS78;
	break;
      default:
	return;
    }
    for(i = 0; i < 22; ++ i) {
	if ((c->c1 == table78_83[i][0][0]) && (c->c2 == table78_83[i][0][1])) {
	    c->c1 = table78_83[i][1][0];
	    c->c2 = table78_83[i][1][1];
	    return;
	}
	if ((c->c1 == table78_83[i][1][0]) && (c->c2 == table78_83[i][1][1])) {
	    c->c1 = table78_83[i][0][0];
	    c->c2 = table78_83[i][0][1];
	    return;
	}
    }
}
