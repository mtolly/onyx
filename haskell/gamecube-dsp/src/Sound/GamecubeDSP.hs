module Sound.GamecubeDSP where

import           Control.Exception
import           Foreign
import           Foreign.C

foreign import ccall unsafe "dsper_main"
  dsper_main :: CInt -> Ptr CString -> IO CInt

foreign import ccall unsafe "dkdsp_main"
  dkdsp_main :: CInt -> Ptr CString -> IO CInt

-- | Encode a mono WAV to a DSP.
dsper :: FilePath -> FilePath -> IO ()
dsper fin fout = do
  let args = ["hs-dsper", fin, fout]
  withMany withCString args $ \ps -> do
    withArrayLen ps $ \len p -> do
      res <- dsper_main (fromIntegral len) p
      case res of
        0 -> return ()
        n -> throwIO $ userError $ "dsper returned " <> show n

-- | Combine 2 WAVs into a Donkey Konga format stereo DSP.
dkdsp :: FilePath -> Maybe FilePath -> FilePath -> IO ()
dkdsp fin finR fout = do
  let args = ["hs-dkdsp", fin] <> maybe [] (\r -> ["-s", r]) finR <> [fout]
  withMany withCString args $ \ps -> do
    withArrayLen ps $ \len p -> do
      res <- dkdsp_main (fromIntegral len) p
      case res of
        0 -> return ()
        n -> throwIO $ userError $ "dkdsp returned " <> show n
