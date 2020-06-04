/*
 * REVORB - Recomputes page granule positions in Ogg Vorbis files.
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

#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ogg/ogg.h>
#include <vorbis/codec.h>

bool revorb_copy_headers(FILE *fi, ogg_sync_state *si, ogg_stream_state *is,
                  FILE *fo, ogg_sync_state *so, ogg_stream_state *os,
                  vorbis_info *vi)
{
    char *buffer = ogg_sync_buffer(si, 4096);
    int numread = fread(buffer, 1, 4096, fi);
    ogg_sync_wrote(si, numread);

    ogg_page page;
    if (ogg_sync_pageout(si, &page) != 1) {
        fprintf(stderr, "Input is not an Ogg.\n");
        return false;
    }

    ogg_stream_init(is, ogg_page_serialno(&page));
    ogg_stream_init(os, ogg_page_serialno(&page));

    if (ogg_stream_pagein(is,&page) < 0) {
        fprintf(stderr, "Error in the first page.\n");
        ogg_stream_clear(is);
        ogg_stream_clear(os);
        return false;
    }

    ogg_packet packet;
    if (ogg_stream_packetout(is,&packet) != 1) {
        fprintf(stderr, "Error in the first packet.\n");
        ogg_stream_clear(is);
        ogg_stream_clear(os);
        return false;
    }

    vorbis_comment vc;
    vorbis_comment_init(&vc);
        if (vorbis_synthesis_headerin(vi, &vc, &packet) < 0) {
        fprintf(stderr, "Error in header, probably not a Vorbis file.\n");
        vorbis_comment_clear(&vc);
        ogg_stream_clear(is);
        ogg_stream_clear(os);
        return false;
    }

    ogg_stream_packetin(os, &packet);

    int i = 0;
    while(i < 2) {
        int res = ogg_sync_pageout(si, &page);

        if (res == 0) {
            buffer = ogg_sync_buffer(si, 4096);
            numread = fread(buffer, 1, 4096, fi);
            if (numread == 0 && i < 2) {
                fprintf(stderr, "Headers are damaged, file is probably truncated.\n");
                ogg_stream_clear(is);
                ogg_stream_clear(os);
                return false;
            }
            ogg_sync_wrote(si, 4096);
            continue;
        }

        if (res == 1) {
            ogg_stream_pagein(is, &page);
            while(i < 2) {
                res = ogg_stream_packetout(is, &packet);
                if (res == 0)
                    break;
                if (res < 0) {
                    fprintf(stderr, "Secondary header is corrupted.\n");
                    vorbis_comment_clear(&vc);
                    ogg_stream_clear(is);
                    ogg_stream_clear(os);
                    return false;
                }
                vorbis_synthesis_headerin(vi, &vc, &packet);
                ogg_stream_packetin(os, &packet);
                i++;
            }
        }
    }

    vorbis_comment_clear(&vc);

    while(ogg_stream_flush(os,&page)) {
        if (fwrite(page.header, 1, page.header_len, fo) != page.header_len || fwrite(page.body, 1, page.body_len, fo) != page.body_len) {
            fprintf(stderr,"Cannot write headers to output.\n");
            ogg_stream_clear(is);
            ogg_stream_clear(os);
            return false;
        }
    }

    return true;
}

int revorb_main(int argc, const char **argv)
{
    bool g_failed = false;

    if (argc < 2) {
        fprintf(stderr, "-= REVORB - <yirkha@fud.cz> 2008/06/29 =-\n");
        fprintf(stderr, "Recomputes page granule positions in Ogg Vorbis files.\n");
        fprintf(stderr, "Usage:\n");
        fprintf(stderr, "  revorb <input.ogg> [output.ogg]\n");
        return 1;
    }

    FILE *fi;
    fi = fopen(argv[1], "r");

    if (!fi) {
        fprintf(stderr, "Could not open input file.\n");
        return 2;
    }

    char tmpName[400];
    FILE *fo;
    if (argc >= 3) {
        fo = fopen(argv[2], "-r");
        if (!fo) {
            fprintf(stderr, "Could not open output file.\n");
            fclose(fi);
            return 2;
        }
    } else {
        fprintf(stderr, "%s\n", argv[1]);

        strcpy(tmpName, argv[1]);
        strcat(tmpName, ".tmp");

        fprintf(stderr, "%s\n", tmpName);

        fo = fopen(tmpName, "w");
        if (!fo) {
            fprintf(stderr, "Could not open output file.\n");
            fclose(fi);
            return 2;
        }
        g_failed = false;
    }

  ogg_sync_state sync_in, sync_out;
  ogg_sync_init(&sync_in);
  ogg_sync_init(&sync_out);

  ogg_stream_state stream_in, stream_out;
  vorbis_info vi;
  vorbis_info_init(&vi);

  ogg_packet packet;
  ogg_page page;

  if (revorb_copy_headers(fi, &sync_in, &stream_in, fo, &sync_out, &stream_out, &vi)) {
      ogg_int64_t granpos = 0, packetnum = 0;
      int lastbs = 0;

    while(1) {

      int eos = 0;
      while(!eos) {
        int res = ogg_sync_pageout(&sync_in, &page);
        if (res == 0) {
          char *buffer = ogg_sync_buffer(&sync_in, 4096);
          int numread = fread(buffer, 1, 4096, fi);
          if (numread > 0)
            ogg_sync_wrote(&sync_in, numread);
          else
            eos = 2;
          continue;
        }

        if (res < 0) {
          fprintf(stderr, "Warning: Corrupted or missing data in bitstream.\n");
          g_failed = true;
        } else {
          if (ogg_page_eos(&page))
            eos = 1;
          ogg_stream_pagein(&stream_in,&page);

          while(1) {
            res = ogg_stream_packetout(&stream_in, &packet);
            if (res == 0)
              break;
            if (res < 0) {
              fprintf(stderr, "Warning: Bitstream error.\n");
              g_failed = true;
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
              granpos += (lastbs+bs) / 4;
            lastbs = bs;

            packet.granulepos = granpos;
            packet.packetno = packetnum++;
            if (!packet.e_o_s) {
              ogg_stream_packetin(&stream_out, &packet);

              ogg_page opage;
              while(ogg_stream_pageout(&stream_out, &opage)) {
                if (fwrite(opage.header, 1, opage.header_len, fo) != opage.header_len || fwrite(opage.body, 1, opage.body_len, fo) != opage.body_len) {
                  fprintf(stderr, "Unable to write page to output.\n");
                  eos = 2;
                  g_failed = true;
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
        while(ogg_stream_flush(&stream_out, &opage)) {
          if (fwrite(opage.header, 1, opage.header_len, fo) != opage.header_len || fwrite(opage.body, 1, opage.body_len, fo) != opage.body_len) {
            fprintf(stderr, "Unable to write page to output.\n");
            g_failed = true;
            break;
          }
        }
        ogg_stream_clear(&stream_in);
        break;
      }
    }

    ogg_stream_clear(&stream_out);
  } else {
    g_failed = true;
  }

  vorbis_info_clear(&vi);

  ogg_sync_clear(&sync_in);
  ogg_sync_clear(&sync_out);

  fclose(fi);
  fclose(fo);

    if (argc < 3) {
        if (g_failed) {
            remove(tmpName);
        } else {
            if (remove(argv[1]) || rename(tmpName, argv[1]))
                fprintf(stderr, "%s: Could not put the output file back in place.\n", tmpName);
        }
    }
    return 0;
}
