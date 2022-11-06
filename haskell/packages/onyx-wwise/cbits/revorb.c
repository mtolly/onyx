/*
 * REVORB - Recomputes page granule positions in Ogg Vorbis files.
 *  version 0.4 (2018/02/11) - Naomi Ahmed <yukimono6@gmail.com> - library version
 *  version 0.3 (2015/09/03) - Jon Boydell <jonboydell@hotmail.com> - version for *NIX systems
 *  version 0.2 (2008/06/29)
 *
 * Copyright (c) 2008, Jiri Hruska <jiri.hruska@fud.cz>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*# INCLUDE=.\include #*/
/*# LIB=.\lib         #*/
/*# CFLAGS=/D_UNICODE #*/
/*# LFLAGS=/NODEFAULTLIB:MSVCRT /LTCG /OPT:REF /MANIFEST:NO #*/

#ifdef __APPLE__
# include <sys/uio.h>
#else
# include <stdio.h>
#endif

#include <fcntl.h>
#include <ogg/ogg.h>
#include <vorbis/codec.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "revorb.h"

size_t revorb_fread(void* buffer, size_t size, size_t n, REVORB_FILE* fp) {
  if (fp->size == -1) {
    return -1;
  }
  size_t sz = size * n;
  intptr_t offset = (intptr_t)(fp->cursor) - (intptr_t)(fp->start);
  if (offset + sz > fp->size) {
    sz = fp->size - offset;
  }
  memcpy(buffer, fp->cursor, sz);
  /* (intptr_t) */fp->cursor += sz;
  return sz;
}

size_t revorb_fwrite(void* buffer, size_t size, size_t n, REVORB_FILE* fp) {
  if (fp->size == -1) {
    return -1;
  }
  size_t sz = size * n;
  intptr_t offset = (intptr_t)(fp->cursor) - (intptr_t)(fp->start);
  if (fp->size - offset < sz) {
      intptr_t delta = sz - (fp->size - offset);
    fp->size += delta + 1;
    fp->start = (void*)realloc(fp->start, fp->size);
    fp->cursor = (void*)((intptr_t)fp->start + offset);
  }
  memcpy(fp->cursor, buffer, sz);
  /* (intptr_t) */fp->cursor += sz;
  return sz;
}
void revorb_fclose(REVORB_FILE* fp) {
  free(fp->start);
  fp->start = NULL;
  fp->cursor = NULL;
  fp->size = -1;
}

REVORB_RESULT copy_headers(REVORB_FILE* fi,
                           ogg_sync_state* si,
                           ogg_stream_state* is,
                           REVORB_FILE* fo,
                           ogg_sync_state* so,
                           ogg_stream_state* os,
                           vorbis_info* vi) {
  char* buffer = ogg_sync_buffer(si, 4096);
  size_t numread = revorb_fread(buffer, 1, 4096, fi);
  ogg_sync_wrote(si, numread);

  ogg_page page;
  if (ogg_sync_pageout(si, &page) != 1) {
    return REVORB_ERR_NOT_OGG;
  }

  ogg_stream_init(is, ogg_page_serialno(&page));
  ogg_stream_init(os, ogg_page_serialno(&page));

  if (ogg_stream_pagein(is, &page) < 0) {
    ogg_stream_clear(is);
    ogg_stream_clear(os);
    return REVORB_ERR_FIRST_PAGE;
  }

  ogg_packet packet;
  if (ogg_stream_packetout(is, &packet) != 1) {
    ogg_stream_clear(is);
    ogg_stream_clear(os);
    return REVORB_ERR_FIRST_PACKET;
  }

  vorbis_comment vc;
  vorbis_comment_init(&vc);
  if (vorbis_synthesis_headerin(vi, &vc, &packet) < 0) {
    vorbis_comment_clear(&vc);
    ogg_stream_clear(is);
    ogg_stream_clear(os);
    return REVORB_ERR_HEADER;
  }

  ogg_stream_packetin(os, &packet);

  int i = 0;
  while (i < 2) {
    int res = ogg_sync_pageout(si, &page);

    if (res == 0) {
      buffer = ogg_sync_buffer(si, 4096);
      numread = revorb_fread(buffer, 1, 4096, fi);
      if (numread == 0 && i < 2) {
        ogg_stream_clear(is);
        ogg_stream_clear(os);
        return REVORB_ERR_TRUNCATED;
      }
      ogg_sync_wrote(si, 4096);
      continue;
    }

    if (res == 1) {
      ogg_stream_pagein(is, &page);
      while (i < 2) {
        res = ogg_stream_packetout(is, &packet);
        if (res == 0)
          break;
        if (res < 0) {
          vorbis_comment_clear(&vc);
          ogg_stream_clear(is);
          ogg_stream_clear(os);
          return REVORB_ERR_SECONDARY_HEADER;
        }
        vorbis_synthesis_headerin(vi, &vc, &packet);
        ogg_stream_packetin(os, &packet);
        i++;
      }
    }
  }

  vorbis_comment_clear(&vc);

  while (ogg_stream_flush(os, &page)) {
    if (revorb_fwrite(page.header, 1, page.header_len, fo) != page.header_len ||
        revorb_fwrite(page.body, 1, page.body_len, fo) != page.body_len) {
      ogg_stream_clear(is);
      ogg_stream_clear(os);
      return REVORB_ERR_HEADER_WRITE;
    }
  }

  return REVORB_ERR_SUCCESS;
}

/* REVORBAPI */ REVORB_RESULT revorb(REVORB_FILE* fi, REVORB_FILE* fo) {
  int g_failed;

  ogg_sync_state sync_in, sync_out;
  ogg_sync_init(&sync_in);
  ogg_sync_init(&sync_out);

  ogg_stream_state stream_in, stream_out;
  vorbis_info vi;
  vorbis_info_init(&vi);

  ogg_packet packet;
  ogg_page page;

  REVORB_RESULT code =
      copy_headers(fi, &sync_in, &stream_in, fo, &sync_out, &stream_out, &vi);

  if (code == REVORB_ERR_SUCCESS) {
    ogg_int64_t granpos = 0, packetnum = 0;
    int lastbs = 0;

    while (1) {
      int eos = 0;
      while (!eos) {
        int res = ogg_sync_pageout(&sync_in, &page);
        if (res == 0) {
          char* buffer = ogg_sync_buffer(&sync_in, 4096);
          int numread = revorb_fread(buffer, 1, 4096, fi);
          if (numread > 0)
            ogg_sync_wrote(&sync_in, numread);
          else
            eos = 2;
          continue;
        }

        if (res < 0) {
          code = REVORB_ERR_CORRUPT;
          g_failed = 1;
        } else {
          if (ogg_page_eos(&page))
            eos = 1;
          ogg_stream_pagein(&stream_in, &page);

          while (1) {
            res = ogg_stream_packetout(&stream_in, &packet);
            if (res == 0)
              break;
            if (res < 0) {
              code = REVORB_ERR_BITSTREAM_CORRUPT;
              g_failed = 1;
              continue;
            }

            /*
            if (packet.granulepos >= 0) {
              granpos = packet.granulepos + logstream_startgran;
              packet.granulepos = granpos;
            }
            */
            int bs = vorbis_packet_blocksize(&vi, &packet);
            if (lastbs)
              granpos += (lastbs + bs) / 4;
            lastbs = bs;

            packet.granulepos = granpos;
            packet.packetno = packetnum++;
            if (!packet.e_o_s) {
              ogg_stream_packetin(&stream_out, &packet);

              ogg_page opage;
              while (ogg_stream_pageout(&stream_out, &opage)) {
                if (revorb_fwrite(opage.header, 1, opage.header_len, fo) !=
                        opage.header_len ||
                    revorb_fwrite(opage.body, 1, opage.body_len, fo) !=
                        opage.body_len) {
                  code = REVORB_ERR_WRITE_FAIL;
                  eos = 2;
                  g_failed = 1;
                  break;
                }
              }
            }
          }
        }
      }

      if (eos == 2)
        break;

      { 
        packet.e_o_s = 1;
        ogg_stream_packetin(&stream_out, &packet);
        ogg_page opage;
        while (ogg_stream_flush(&stream_out, &opage)) {
          if (revorb_fwrite(opage.header, 1, opage.header_len, fo) !=
                  opage.header_len ||
              revorb_fwrite(opage.body, 1, opage.body_len, fo) !=
                  opage.body_len) {
            code = REVORB_ERR_WRITE_FAIL2;
            g_failed = 1;
            break;
          }
        }
        ogg_stream_clear(&stream_in);
        break;
      }
    }

    ogg_stream_clear(&stream_out);
  } else {
    g_failed = 1;
  }

  vorbis_info_clear(&vi);

  ogg_sync_clear(&sync_in);
  ogg_sync_clear(&sync_out);

  return code;
}

REVORB_RESULT hs_revorb(void *start, int size, void **out, int *outlen) {
  REVORB_FILE input, output;
  input.start = start;
  input.cursor = start;
  input.size = size;
  void *p = malloc(size);
  if (!p) return REVORB_ERR_WRITE_FAIL;
  output.start = p;
  output.cursor = p;
  output.size = size;
  REVORB_RESULT res = revorb(&input, &output);
  if (res == REVORB_ERR_SUCCESS) {
    *out = output.start;
    *outlen = output.cursor - output.start;
  }
  return res;
}
