/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: ee2.c,v 1.4 2003-03-12 13:00:12 knok Exp $
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

#include "kakasi.h"

static void
E2alphabet_copy(n, str, type)
     Character *n;
     char *str;
     int type;
{
    int i;

    for (i = 0; str[i] != '\0'; ++ i) {
	n[i].type = type;
	n[i].c1 = str[i];
    }
    n[i].type = OTHER;
    n[i].c1 = 0;
    n[i].c2 = 0;
}

static int
E2alphabet(c, n, type)
     Character *c;
     Character *n;
     int type;
{
    static char E2alphabet_a1table[94][12] = {
	" ",",",".",",",".",".",":",";","?","!","\"","(maru)","'","`","..","~",
	"~","_","(kurikaesi)","(kurikaesi)","(kurikaesi)","(kurikaesi)","(kurikaesi)",
	"(kurikaesi)","(kurikaesi)","sime","(maru)","^","-","-","/","\\","~","||",
	"|","...","..","`","'","\"","\"","(",")","[","]","[","]","{","}","<",">",
	"<<",">>","(",")","(",")","(",")","+","-","+-","X","/","=","!=","<",">",
	"<=",">=","(kigou)","...","(osu)","(mesu)","(do)","'","\"","(Sessi)","\\",
	"$","(cent)","(pound)","%","#","&","*","@","(setu)","(hosi)","(hosi)","(maru)",
	"(maru)","(maru)","(diamond)" };
    static char E2greek_table[56][8] = {
	"Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta",
	"Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho",
	"Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega", 
	"", "", "", "", "", "", "", "", 
	"alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
	"iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho",
	"sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"};

    switch(c[0].c1) { /* Management is divided into every classification. */
      case 0xa1: /* It changes by using the table because sign/others are mixed in the first district. */
	if ((0xa1 <= c[0].c2) && (c[0].c2 < 0xa1 + 94)) {
	    E2alphabet_copy(n, E2alphabet_a1table[c[0].c2-0xa1], type);
	    return 1;
	} else {
	    E2alphabet_copy(n, "??", type);
	    return 1;
	}
      case 0xa2: /* A sign is contained in the second district. Because it is troublesome, (kigou) is completely taken. */
	E2alphabet_copy(n, "(kigou)", type);
	return 1;
      case 0xa3: /* Ascii and number are contained in the third district. */
	n[0].type = type;
	n[0].c1 = c[0].c2 & 0x7f; /* This is corner-cutting. The character which isn't defined is done somehow. */
	n[1].type = OTHER;
	n[2].c1 = '\0';
#ifdef WAKATIGAKI
	n[0].c2 = '\0';
	n[1].c1 = '\0';
	n[1].c2 = '\0';
	n[2].c2 = '\0';
#endif /* WAKATIGAKI */
	return 1;
      case 0xa4: /* HIRAGANA */
	return H2rom(c, n, type);
      case 0xa5: /* KATAKANA */
	return K2rom(c, n, type);
      case 0xa6: /* The sixth district is Greek. */
	if ((0xa1 <= c[0].c2) && (c[0].c2 < 0xa1 + 56)) {
	    E2alphabet_copy(n, E2greek_table[c[0].c2-0xa1], type);
	    return 1;
	} else {
	    E2alphabet_copy(n, "??", type);
	    return 1;
	}
      case 0xa7: /* The seventh district is Russian. */
	E2alphabet_copy(n, "(Russia)", type);
	return 1;
      default:   /* The eighth district line base settlement. You have only to read it except for that with what. */
	E2alphabet_copy(n, "??", type);
	return 1;
    }
}

int
E2a(c, n)
     Character *c;
     Character *n;
{
    return E2alphabet(c, n, ASCII);
}

int
E2j(c, n)
     Character *c;
     Character *n;
{
    return E2alphabet(c, n, JISROMAN);
}
