{-# LANGUAGE LambdaCase #-}
module Onyx.Harmonix.MOGG.Crypt where

import           Control.Monad          (when)
import qualified Data.ByteString        as B
import           Foreign
import           Foreign.C
import qualified GHC.IO.Handle          as H
import           Onyx.Util.Handle
import           System.IO
import           System.Posix.Internals (sEEK_CUR, sEEK_END, sEEK_SET)

-----------------------------------------------------------------------

newtype OVCallbacks  = OVCallbacks  (Ptr OVCallbacks )
newtype VorbisReader = VorbisReader (Ptr VorbisReader)

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
  return (cb, cleanup)

decryptMOGG' :: Readable -> Readable
decryptMOGG' r = makeHandle "decrypted mogg" $ do
  h <- rOpen r
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
    , shClose = hClose h >> cleanup
    , shRead  = \n -> allocaBytes (fromIntegral n) $ \buf -> do
      bytesRead <- onyx_VorbisReader_ReadRaw vr (castPtr buf) 1 $ fromIntegral n
      B.packCStringLen (buf, fromIntegral bytesRead)
    }

-----------------------------------------------------------------------

foreign import ccall "saveOgg"
  c_saveOgg :: CString -> CString -> IO CInt

foreign import ccall "encryptOgg"
  c_encryptOgg :: CString -> CString -> IO CInt

foreign import ccall "saveBik"
  c_saveBik :: CString -> CString -> IO CInt

-- 0x0A (unencrypted) -> 0x0B (RB1)
encryptMOGG :: FilePath -> FilePath -> IO ()
encryptMOGG fin fout = do
  withCString fin $ \pin -> do
    withCString fout $ \pout -> do
      res <- c_encryptOgg pin pout
      when (res /= 0) $ ioError $ userError "Error in MOGG encryption"

-- mogg -> ogg
decryptMOGG :: FilePath -> FilePath -> IO ()
decryptMOGG fin fout = do
  withCString fin $ \pin -> do
    withCString fout $ \pout -> do
      res <- c_saveOgg pin pout
      when (res /= 0) $ ioError $ userError "Error in MOGG decryption"

decryptBink :: FilePath -> FilePath -> IO ()
decryptBink fin fout = do
  withCString fin $ \pin -> do
    withCString fout $ \pout -> do
      res <- c_saveBik pin pout
      when (res /= 0) $ ioError $ userError "Error in Bink decryption"
