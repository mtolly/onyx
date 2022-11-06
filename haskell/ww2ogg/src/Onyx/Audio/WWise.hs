{-# LANGUAGE LambdaCase #-}
module Onyx.Audio.WWise (WW2OggConfig(..), ww2ogg) where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as BU
import           Foreign
import           Foreign.C

data WW2OggConfig = WW2OggConfig
  { wwCodebook :: FilePath
  -- TODO other options
  }

ww2ogg :: WW2OggConfig -> B.ByteString -> IO (Maybe B.ByteString)
ww2ogg cfg inBytes = do
  BU.unsafeUseAsCStringLen inBytes $ \(p, len) -> do
    alloca $ \pStream -> do
      alloca $ \pOutLen -> do
        withCString (wwCodebook cfg) $ \cbook -> do
          n <- hs_ww2ogg p (fromIntegral len) pStream pOutLen cbook
          if n == 0
            then do
              stream <- peek pStream
              outLen <- peek pOutLen
              bs <- allocaBytes (fromIntegral outLen) $ \pout -> do
                hs_fill_stream stream pout
                hs_delete_stream stream
                B.packCStringLen (pout, fromIntegral outLen)
              revorb bs
            else return Nothing

revorb :: B.ByteString -> IO (Maybe B.ByteString)
revorb bs = BU.unsafeUseAsCStringLen bs $ \(p, inlen) -> do
  alloca $ \ppout -> do
    alloca $ \plen -> do
      res <- hs_revorb (castPtr p) (fromIntegral inlen) ppout plen
      if res == 0
        then do
          pout <- peek ppout
          outlen <- peek plen
          bs' <- B.packCStringLen (castPtr pout, fromIntegral outlen)
          free pout
          return $ Just bs'
        else return Nothing

foreign import ccall "hs_ww2ogg"
  hs_ww2ogg :: CString -> CSize -> Ptr (Ptr ()) -> Ptr CSize -> CString -> IO CInt

foreign import ccall "hs_fill_stream"
  hs_fill_stream :: Ptr () -> Ptr CChar -> IO ()

foreign import ccall "hs_delete_stream"
  hs_delete_stream :: Ptr () -> IO ()

-- If we don't run Revorb then my current ogg loading code hangs.
foreign import ccall "hs_revorb"
  hs_revorb :: Ptr () -> CInt -> Ptr (Ptr ()) -> Ptr CInt -> IO CInt
