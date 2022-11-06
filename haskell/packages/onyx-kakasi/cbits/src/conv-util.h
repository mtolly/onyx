/*
 * KAKASI (Kanji Kana Simple inversion program)
 * $Id: conv-util.h,v 1.2 2000-12-29 18:39:11 rug Exp $
 * Copyright (C) 1994
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

#ifndef _CONV_UTIL_H
#define _CONV_UTIL_H

#ifndef PARAMS
# if __STDC__ || defined __cplusplus
# define PARAMS(args) args
# else
# define PARAMS(args) ()
# endif
#endif

extern int isallkana PARAMS((unsigned char *str));
extern int isallzenkaku PARAMS((unsigned char *str));
extern int includekanji PARAMS((unsigned char *str));

#endif /* _CONV_UTIL_H */
