module Kakasi where

import Foreign hiding (void)
import Foreign.C
import qualified Data.ByteString as B
import Control.Monad (void)

foreign import ccall "kakasi_getopt_argv"
  kakasi_getopt_argv :: CInt -> Ptr CString -> IO CInt

foreign import ccall "kakasi_do"
  kakasi_do :: CString -> IO CString

foreign import ccall "kakasi_close_kanwadict"
  kakasi_close_kanwadict :: IO CInt

foreign import ccall "kakasi_free"
  kakasi_free :: CString -> IO CInt

kakasiArgs :: [String] -> IO CInt
kakasiArgs args = withMany withCString args $ \ps -> do
  withArrayLen ps $ \len p -> do
    kakasi_getopt_argv (fromIntegral len) p

kakasiDo :: B.ByteString -> IO B.ByteString
kakasiDo bs = do
  cs <- B.useAsCString bs kakasi_do
  bs' <- B.packCString cs
  void $ kakasi_free cs
  return bs'
