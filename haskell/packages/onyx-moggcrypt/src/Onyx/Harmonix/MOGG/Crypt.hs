{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Onyx.Harmonix.MOGG.Crypt where

import           Control.Exception        (bracket)
import           Control.Monad            (when)
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (fromForeignPtr0)
import qualified Data.ByteString.Lazy     as BL
import           Foreign
import           Foreign.C
import qualified GHC.IO.Handle            as H
import           Onyx.Util.Handle
import           System.IO
import           System.Posix.Internals   (sEEK_CUR, sEEK_END, sEEK_SET)

-----------------------------------------------------------------------

newtype OVCallbacks     = OVCallbacks     (Ptr OVCallbacks    )
newtype VorbisReader    = VorbisReader    (Ptr VorbisReader   )
newtype VorbisEncrypter = VorbisEncrypter (Ptr VorbisEncrypter)
newtype BinkReader      = BinkReader      (Ptr BinkReader     )

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

foreign import ccall "onyx_new_ov_callbacks"
  onyx_new_ov_callbacks
    :: FunPtr (Ptr () -> CSize -> CSize -> Ptr () -> IO CSize)
    -> FunPtr (Ptr () -> Int64 -> CInt -> IO CInt)
    -> FunPtr (Ptr () -> IO CInt)
    -> FunPtr (Ptr () -> IO CLong)
    -> IO OVCallbacks

foreign import ccall "onyx_delete_ov_callbacks"
  onyx_delete_ov_callbacks :: OVCallbacks -> IO ()

foreign import ccall "onyx_VorbisReader_Open"
  onyx_VorbisReader_Open :: Ptr a -> OVCallbacks -> IO VorbisReader

foreign import ccall "onyx_delete_VorbisReader"
  onyx_delete_VorbisReader :: VorbisReader -> IO ()

foreign import ccall "onyx_VorbisReader_SeekRaw"
  onyx_VorbisReader_SeekRaw :: VorbisReader -> Int64 -> CInt -> IO CInt

foreign import ccall "onyx_VorbisReader_TellRaw"
  onyx_VorbisReader_TellRaw :: VorbisReader -> IO CSize

foreign import ccall "onyx_VorbisReader_SizeRaw"
  onyx_VorbisReader_SizeRaw :: VorbisReader -> IO CSize

foreign import ccall "onyx_VorbisReader_ReadRaw"
  onyx_VorbisReader_ReadRaw :: VorbisReader -> Ptr a -> CSize -> CSize -> IO CSize

foreign import ccall "onyx_VorbisEncrypter_Open"
  onyx_VorbisEncrypter_Open :: Ptr a -> OVCallbacks -> IO VorbisEncrypter

foreign import ccall "onyx_delete_VorbisEncrypter"
  onyx_delete_VorbisEncrypter :: VorbisEncrypter -> IO ()

foreign import ccall "onyx_VorbisEncrypter_ReadRaw"
  onyx_VorbisEncrypter_ReadRaw :: VorbisEncrypter -> Ptr a -> CSize -> CSize -> IO CSize

foreign import ccall "onyx_BinkReader_Open"
  onyx_BinkReader_Open :: Ptr a -> OVCallbacks -> IO BinkReader

foreign import ccall "onyx_delete_BinkReader"
  onyx_delete_BinkReader :: BinkReader -> IO ()

foreign import ccall "onyx_BinkReader_ReadToArray"
  onyx_BinkReader_ReadToArray :: BinkReader -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

handleOVCallbacks :: Handle -> IO (OVCallbacks, IO ())
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
  cb <- onyx_new_ov_callbacks fnRead fnSeek nullFunPtr fnTell
  let cleanup = do
        freeHaskellFunPtr fnRead
        freeHaskellFunPtr fnSeek
        freeHaskellFunPtr fnTell
        onyx_delete_ov_callbacks cb
  return (cb, cleanup)

moggToOggHandles :: Handle -> IO SimpleHandle
moggToOggHandles h = do
  (cb, cleanup) <- handleOVCallbacks h
  vr@(VorbisReader p) <- onyx_VorbisReader_Open nullPtr cb
  when (p == nullPtr) $ fail "Failed to decrypt .mogg file"
  size <- onyx_VorbisReader_SizeRaw vr
  return SimpleHandle
    { shSize  = fromIntegral size
    , shSeek  = \n -> onyx_VorbisReader_SeekRaw vr (fromIntegral n) sEEK_SET >>= \case
      0 -> return ()
      c -> fail $ "decrypted mogg seek returned error code " <> show c
    , shTell  = fromIntegral <$> onyx_VorbisReader_TellRaw vr
    , shClose = do
      onyx_delete_VorbisReader vr
      hClose h
      cleanup
    , shRead  = \n -> allocaBytes (fromIntegral n) $ \buf -> do
      bytesRead <- onyx_VorbisReader_ReadRaw vr (castPtr buf) 1 $ fromIntegral n
      B.packCStringLen (buf, fromIntegral bytesRead)
    }

moggToOgg :: Readable -> Readable
moggToOgg r = makeHandle "decrypted mogg" $ rOpen r >>= moggToOggHandles

-- 0x0A (unencrypted) -> 0x0B (RB1)
encryptMOGG :: (Monoid a) => Readable -> (B.ByteString -> IO a) -> IO a
encryptMOGG r eachChunk = useHandle r $ \h -> do
  bracket (handleOVCallbacks h) snd $ \(cb, _) -> do
    bracket (onyx_VorbisEncrypter_Open nullPtr cb) onyx_delete_VorbisEncrypter $ \ve -> do
      let bufSize = 8192
      allocaBytes (fromIntegral bufSize) $ \buf -> let
        go !cur = do
          bytesRead <- onyx_VorbisEncrypter_ReadRaw ve (castPtr buf) 1 bufSize
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
    bracket (onyx_BinkReader_Open nullPtr cb) onyx_delete_BinkReader $ \br -> do
      alloca $ \parray -> do
        alloca $ \psize -> do
          onyx_BinkReader_ReadToArray br parray psize >>= \case
            0 -> do
              -- array is malloc'd in ReadToArray, so use free when done
              fptr <- peek parray >>= newForeignPtr finalizerFree
              size <- fromIntegral <$> peek psize
              return $ fromForeignPtr0 fptr size
            _ -> fail "Failed to decrypt Bink file"
