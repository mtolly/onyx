{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Onyx.Harmonix.MOGG.Crypt where

import           Control.Exception        (bracket, onException)
import           Control.Monad            (guard)
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (fromForeignPtr0)
import qualified Data.ByteString.Lazy     as BL
import           Data.Functor             ((<&>))
import           Foreign
import           Foreign.C
import qualified GHC.IO.Handle            as H
import qualified Language.C.Inline.Cpp    as C
import           Onyx.Util.Handle
import           System.IO
import           System.Posix.Internals   (sEEK_CUR, sEEK_END, sEEK_SET)

data OVCallbacks
data VorbisReader
data VorbisEncrypter
data BinkReader

C.context $ C.cppCtx <> C.cppTypePairs
  [ ("ov_callbacks"   , [t| OVCallbacks     |])
  , ("VorbisReader"   , [t| VorbisReader    |])
  , ("VorbisEncrypter", [t| VorbisEncrypter |])
  , ("BinkReader"     , [t| BinkReader      |])
  ]

C.include "VorbisReader.h"
C.include "VorbisEncrypter.h"
C.include "BinkReader.h"

foreign import ccall "wrapper"
  makeOVRead
    ::            (Ptr () -> CSize -> CSize -> Ptr () -> IO CSize)
    -> IO (FunPtr (Ptr () -> CSize -> CSize -> Ptr () -> IO CSize))

foreign import ccall "wrapper"
  makeOVSeek
    ::            (Ptr () -> Int64 -> CInt -> IO CInt)
    -> IO (FunPtr (Ptr () -> Int64 -> CInt -> IO CInt))

foreign import ccall "wrapper"
  makeOVClose
    ::            (Ptr () -> IO CInt)
    -> IO (FunPtr (Ptr () -> IO CInt))

foreign import ccall "wrapper"
  makeOVTell
    ::            (Ptr () -> IO CLong)
    -> IO (FunPtr (Ptr () -> IO CLong))

ovCallbacksNew
  :: FunPtr (Ptr () -> CSize -> CSize -> Ptr () -> IO CSize)
  -> FunPtr (Ptr () -> Int64 -> CInt -> IO CInt)
  -> FunPtr (Ptr () -> IO CInt)
  -> FunPtr (Ptr () -> IO CLong)
  -> IO (Ptr OVCallbacks)
ovCallbacksNew read_func seek_func close_func tell_func =
  [C.block| ov_callbacks* {
    ov_callbacks* cb = new ov_callbacks();
    cb->read_func  = $(size_t(*read_func)  (void *ptr, size_t size, size_t nmemb, void *datasource));
    cb->seek_func  = $(int(*seek_func)  (void *datasource, int64_t offset, int whence));
    cb->close_func = $(int(*close_func) (void *datasource));
    cb->tell_func  = $(long(*tell_func)  (void *datasource));
    return cb;
  } |]

ovCallbacksDelete :: Ptr OVCallbacks -> IO ()
ovCallbacksDelete cb =
  [C.block| void { delete $(ov_callbacks* cb); } |]

vorbisReaderOpen :: Ptr a -> Ptr OVCallbacks -> IO (Maybe (Ptr VorbisReader))
vorbisReaderOpen (castPtr -> src) cb =
  [C.block| VorbisReader* {
    VorbisReader* vr = new VorbisReader();
    if (vr->Open($(void* src), *$(ov_callbacks* cb))) {
      delete vr;
      return nullptr;
    } else {
      return vr;
    }
  } |] <&> \p -> guard (p /= nullPtr) >> Just p

vorbisReaderDelete :: Ptr VorbisReader -> IO ()
vorbisReaderDelete vr =
  [C.block| void { delete $(VorbisReader* vr); } |]

vorbisReaderSeekRaw :: Ptr VorbisReader -> Int64 -> CInt -> IO CInt
vorbisReaderSeekRaw vr offset whence =
  [C.exp| int { $(VorbisReader* vr)->SeekRaw($(int64_t offset), $(int whence)) } |]

vorbisReaderTellRaw :: Ptr VorbisReader -> IO CSize
vorbisReaderTellRaw vr =
  [C.exp| size_t { $(VorbisReader* vr)->TellRaw() } |]

vorbisReaderSizeRaw :: Ptr VorbisReader -> IO CSize
vorbisReaderSizeRaw vr =
  [C.exp| size_t { $(VorbisReader* vr)->SizeRaw() } |]

vorbisReaderReadRaw :: Ptr VorbisReader -> Ptr a -> CSize -> CSize -> IO CSize
vorbisReaderReadRaw vr (castPtr -> buf) elementSize elements =
  [C.exp| size_t { $(VorbisReader* vr)->ReadRaw($(void* buf), $(size_t elementSize), $(size_t elements)) } |]

vorbisEncrypterOpen :: Ptr a -> Ptr OVCallbacks -> IO (Ptr VorbisEncrypter)
vorbisEncrypterOpen (castPtr -> src) cb =
  [C.exp| VorbisEncrypter* { new VorbisEncrypter($(void* src), *$(ov_callbacks* cb)) } |]

vorbisEncrypterDelete :: Ptr VorbisEncrypter -> IO ()
vorbisEncrypterDelete ve =
  [C.block| void { delete $(VorbisEncrypter* ve); } |]

vorbisEncrypterReadRaw :: Ptr VorbisEncrypter -> Ptr a -> CSize -> CSize -> IO CSize
vorbisEncrypterReadRaw ve (castPtr -> buf) elementSize elements =
  [C.exp| size_t { $(VorbisEncrypter* ve)->ReadRaw($(void* buf), $(size_t elementSize), $(size_t elements)) } |]

binkReaderOpen :: Ptr a -> Ptr OVCallbacks -> IO (Ptr BinkReader)
binkReaderOpen (castPtr -> src) cb =
  [C.exp| BinkReader* { new BinkReader($(void* src), *$(ov_callbacks* cb)) } |]

binkReaderDelete :: Ptr BinkReader -> IO ()
binkReaderDelete br =
  [C.block| void { delete $(BinkReader* br); } |]

binkReaderReadToArray :: Ptr BinkReader -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt
binkReaderReadToArray br buffer size = do
  [C.exp| int { $(BinkReader* br)->ReadToArray($(uint8_t** buffer), $(size_t* size)) } |]

handleOVCallbacks :: Handle -> IO (Ptr OVCallbacks, IO ())
handleOVCallbacks h = do
  let modeMap =
        [ (sEEK_END, SeekFromEnd )
        , (sEEK_CUR, RelativeSeek)
        , (sEEK_SET, AbsoluteSeek)
        ]
  fnRead <- makeOVRead $ \buf size count _ -> do
    n <- hGetBuf h buf $ fromIntegral size * fromIntegral count
    return $ quot (fromIntegral n) size
  fnSeek <- makeOVSeek $ \_ offset whence -> do
    mode <- case lookup whence modeMap of
      Nothing -> do
        putStrLn $ "Warning: unrecognized seek mode passed to ov_callbacks, " <> show whence
        return AbsoluteSeek
      Just mode -> return mode
    hSeek h mode $ fromIntegral offset
    return 0 -- TODO catch hSeek exceptions and return nonzero?
  fnTell <- makeOVTell $ \_ -> do
    H.HandlePosn _ n <- hGetPosn h
    return $ fromIntegral n
  cb <- ovCallbacksNew fnRead fnSeek nullFunPtr fnTell
  let cleanup = do
        ovCallbacksDelete cb
        freeHaskellFunPtr fnRead
        freeHaskellFunPtr fnSeek
        freeHaskellFunPtr fnTell
  return (cb, cleanup)

moggToOggHandles :: Handle -> IO SimpleHandle
moggToOggHandles h = do
  (cb, cleanup) <- handleOVCallbacks h
  vr <- vorbisReaderOpen nullPtr cb >>= \case
    Just vr -> return vr
    Nothing -> do
      cleanup
      fail "Failed to decrypt .mogg file"
  size <- vorbisReaderSizeRaw vr
  return SimpleHandle
    { shSize  = fromIntegral size
    , shSeek  = \n -> vorbisReaderSeekRaw vr (fromIntegral n) sEEK_SET >>= \case
      0 -> return ()
      c -> fail $ "decrypted mogg seek returned error code " <> show c
    , shTell  = fromIntegral <$> vorbisReaderTellRaw vr
    , shClose = do
      vorbisReaderDelete vr
      hClose h
      cleanup
    , shRead  = \n -> allocaBytes (fromIntegral n) $ \buf -> do
      bytesRead <- vorbisReaderReadRaw vr (castPtr buf) 1 $ fromIntegral n
      B.packCStringLen (buf, fromIntegral bytesRead)
    }

moggToOgg :: Readable -> Readable
moggToOgg r = makeHandle "decrypted mogg" do
  h <- rOpen r
  moggToOggHandles h `onException` hClose h

-- 0x0A (unencrypted) -> 0x0B (RB1)
encryptMOGG :: (Monoid a) => Readable -> (B.ByteString -> IO a) -> IO a
encryptMOGG r eachChunk = useHandle r $ \h -> do
  bracket (handleOVCallbacks h) snd $ \(cb, _) -> do
    bracket (vorbisEncrypterOpen nullPtr cb) vorbisEncrypterDelete $ \ve -> do
      let bufSize = 8192
      allocaBytes (fromIntegral bufSize) $ \buf -> let
        go !cur = do
          bytesRead <- vorbisEncrypterReadRaw ve (castPtr buf) 1 bufSize
          if bytesRead == 0
            then return cur
            else do
              bs <- B.packCStringLen (buf, fromIntegral bytesRead)
              x <- eachChunk bs
              go (cur <> x)
        in go mempty

encryptMOGGFiles :: FilePath -> FilePath -> IO ()
encryptMOGGFiles fin fout = withBinaryFile fout WriteMode $ \hout -> do
  encryptMOGG (fileReadable fin) $ \bs -> do
    B.hPut hout bs

encryptMOGGToByteString :: Readable -> IO BL.ByteString
encryptMOGGToByteString r = encryptMOGG r $ return . BL.fromStrict

decryptBink :: Readable -> IO B.ByteString
decryptBink r = useHandle r $ \h -> do
  bracket (handleOVCallbacks h) snd $ \(cb, _) -> do
    bracket (binkReaderOpen nullPtr cb) binkReaderDelete $ \br -> do
      alloca $ \parray -> do
        alloca $ \psize -> do
          binkReaderReadToArray br parray psize >>= \case
            0 -> do
              -- array is malloc'd in ReadToArray, so use free when done
              fptr <- peek parray >>= newForeignPtr finalizerFree
              size <- fromIntegral <$> peek psize
              return $ fromForeignPtr0 fptr size
            _ -> fail "Failed to decrypt Bink file"
