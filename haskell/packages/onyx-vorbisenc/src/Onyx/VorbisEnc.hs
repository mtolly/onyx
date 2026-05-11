{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
module Onyx.VorbisEnc (sinkOggVorbis) where

import           Control.Exception            (bracket, finally)
import           Control.Monad                (forM_, unless)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString              as B
import           Data.Conduit                 (runConduit, (.|))
import qualified Data.Conduit.Audio           as CA
import qualified Data.Conduit.List            as CL
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import qualified Data.Vector.Storable         as V
import           Foreign                      (Ptr, castPtr, copyArray,
                                               peekArray, toBool)
import           Foreign.C                    (CFloat)
import qualified Language.C.Inline            as C
import qualified Language.C.Inline.Context    as C
import qualified Language.C.Types             as C
import           System.IO                    (Handle, IOMode (WriteMode),
                                               withBinaryFile)

data Ogg_stream_state
data Ogg_page
data Ogg_packet
data Vorbis_info
data Vorbis_comment
data Vorbis_dsp_state
data Vorbis_block

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "ogg_stream_state" , [t| Ogg_stream_state |])
    , (C.TypeName "ogg_page"         , [t| Ogg_page         |])
    , (C.TypeName "ogg_packet"       , [t| Ogg_packet       |])
    , (C.TypeName "vorbis_info"      , [t| Vorbis_info      |])
    , (C.TypeName "vorbis_comment"   , [t| Vorbis_comment   |])
    , (C.TypeName "vorbis_dsp_state" , [t| Vorbis_dsp_state |])
    , (C.TypeName "vorbis_block"     , [t| Vorbis_block     |])
    ]
  }

C.include "stdlib.h"
C.include "time.h"
C.include "ogg/ogg.h"
C.include "vorbis/vorbisenc.h"

whileC :: (Integral a) => IO a -> IO () -> IO ()
whileC b f = (toBool <$> b) >>= \case
  True  -> f >> whileC b f
  False -> return ()

flushPages :: Handle -> Ptr Ogg_stream_state -> Ptr Ogg_page -> IO ()
flushPages h os og = whileC
  [C.exp| int { ogg_stream_flush($(ogg_stream_state* os), $(ogg_page* og)) } |]
  (writePage h og)

pagesOut :: Handle -> Ptr Ogg_stream_state -> Ptr Ogg_page -> IO ()
pagesOut h os og = whileC
  [C.exp| int { ogg_stream_pageout($(ogg_stream_state* os), $(ogg_page* og)) } |]
  (writePage h og)

writePage :: Handle -> Ptr Ogg_page -> IO ()
writePage h og = do
  header     <- [C.exp| unsigned char* { $(ogg_page* og)->header     } |]
  header_len <- [C.exp| long           { $(ogg_page* og)->header_len } |]
  body       <- [C.exp| unsigned char* { $(ogg_page* og)->body       } |]
  body_len   <- [C.exp| long           { $(ogg_page* og)->body_len   } |]
  B.packCStringLen (castPtr header, fromIntegral header_len) >>= B.hPut h
  B.packCStringLen (castPtr body  , fromIntegral body_len  ) >>= B.hPut h

sinkOggVorbis :: Maybe Double -> FilePath -> CA.AudioSource (ResourceT IO) Float -> IO ()
sinkOggVorbis optQuality fout src = do

  withBinaryFile fout WriteMode \h -> do

  -- calloc (init to zero) so we can call *_clear functions always
  let free :: Ptr a -> IO ()
      free p = let v = castPtr p in [C.block| void { free($(void* v)); } |]
  bracket [C.exp| ogg_stream_state* { calloc(1, sizeof(ogg_stream_state)) } |] free \os -> do
  bracket [C.exp| ogg_page        * { calloc(1, sizeof(ogg_page        )) } |] free \og -> do
  bracket [C.exp| ogg_packet      * { calloc(1, sizeof(ogg_packet      )) } |] free \op -> do
  bracket [C.exp| vorbis_info     * { calloc(1, sizeof(vorbis_info     )) } |] free \vi -> do
  bracket [C.exp| vorbis_comment  * { calloc(1, sizeof(vorbis_comment  )) } |] free \vc -> do
  bracket [C.exp| vorbis_dsp_state* { calloc(1, sizeof(vorbis_dsp_state)) } |] free \vd -> do
  bracket [C.exp| vorbis_block    * { calloc(1, sizeof(vorbis_block    )) } |] free \vb -> do

  flip finally [C.block| void {
    ogg_stream_clear    ($(ogg_stream_state* os));
    vorbis_block_clear  ($(vorbis_block    * vb));
    vorbis_dsp_clear    ($(vorbis_dsp_state* vd));
    vorbis_comment_clear($(vorbis_comment  * vc));
    vorbis_info_clear   ($(vorbis_info     * vi));
  } |] do

  let channels = fromIntegral src.channels
      rate = round src.rate
      quality = realToFrac $ fromMaybe 0.5 optQuality

  ok <- toBool <$> [C.block| int {

    ogg_stream_state* os = $(ogg_stream_state* os);
    vorbis_info     * vi = $(vorbis_info     * vi);
    vorbis_comment  * vc = $(vorbis_comment  * vc);
    vorbis_dsp_state* vd = $(vorbis_dsp_state* vd);
    vorbis_block    * vb = $(vorbis_block    * vb);

    vorbis_info_init(vi);
    if (vorbis_encode_init_vbr(vi, $(long channels), $(long rate), $(float quality)) != 0) {
      vorbis_info_clear(vi);
      return 0;
    }

    vorbis_comment_init(vc);
    vorbis_comment_add_tag(vc, "ENCODER", "Onyx (vorbisenc)");

    vorbis_analysis_init(vd, vi);
    vorbis_block_init(vd, vb);

    /* Random serial number for the Ogg stream */
    srand((unsigned)time(NULL));
    ogg_stream_init(os, rand());

    /* Three header packets: identification, comment, codebook */
    ogg_packet header, header_comm, header_code;
    vorbis_analysis_headerout(vd, vc, &header, &header_comm, &header_code);
    ogg_stream_packetin(os, &header);
    ogg_stream_packetin(os, &header_comm);
    ogg_stream_packetin(os, &header_code);

    return 1;

  } |]
  unless ok $ fail "Error in setting up Ogg Vorbis encoding"

  flushPages h os og

  runResourceT $ runConduit $ src.source .| CL.mapM_ \buf -> liftIO do
    let chans   = CA.deinterleave src.channels buf
        frames  = CA.vectorFrames buf src.channels
        cframes = fromIntegral frames

    pbuffers <- [C.exp| float** { vorbis_analysis_buffer($(vorbis_dsp_state* vd), $(int cframes)) } |]
    buffers <- peekArray src.channels pbuffers
    forM_ (zip chans buffers) \(inVector, outBuffer) -> do
      V.unsafeWith inVector \pchan -> do
        copyArray outBuffer ((castPtr :: Ptr Float -> Ptr CFloat) pchan) frames
    [C.block| void { vorbis_analysis_wrote($(vorbis_dsp_state* vd), $(int cframes)); } |]

    whileC [C.exp| int { vorbis_analysis_blockout($(vorbis_dsp_state* vd), $(vorbis_block* vb)) == 1 } |] do
      [C.block| void {
        vorbis_analysis($(vorbis_block* vb), NULL);
        vorbis_bitrate_addblock($(vorbis_block* vb));
      } |]
      whileC [C.exp| int { vorbis_bitrate_flushpacket($(vorbis_dsp_state* vd), $(ogg_packet* op)) } |] do
        [C.block| void { ogg_stream_packetin($(ogg_stream_state* os), $(ogg_packet* op)); } |]
        pagesOut h os og

  flushPages h os og
