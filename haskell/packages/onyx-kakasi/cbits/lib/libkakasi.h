/*
 * libkakasi.h
 * $Id: libkakasi.h,v 1.6 2001-08-27 02:11:56 rug Exp $
 * Copyright (C) 1999
 * NOKUBI Takatsugu <knok@daionet.gr.jp>
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
 * Software Foundation Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _LIBKAKASI_H
#define _LIBKAKASI_H

#ifndef PARAMS
# if __STDC__ || defined __cplusplus
# define PARAMS(args) args
# else
# define PARAMS(args) ()
# endif
#endif

int kakasi_getopt_argv PARAMS((int argc, char **argv));
char *kakasi_do PARAMS((char *str));
int kakasi_close_kanwadict PARAMS((void));
int kakasi_free PARAMS((char *p));

/*
 * NOTE: these are internal functions. Do not use.
 */
void digest_start_copy();
void put_separator();
void putchars();
void digest_out();
int digest();
void digest_shift();

void setcharbuffer PARAMS((unsigned char *s));
char *getpbstr PARAMS((void));

#endif /* _LIBKAKASI_H */
