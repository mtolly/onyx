module Sound.WW2Ogg (WW2OggConfig(..), ww2ogg) where

import           Foreign.C

data WW2OggConfig = WW2OggConfig
  { wwCodebook :: FilePath
  -- TODO other options
  }

ww2ogg :: WW2OggConfig -> FilePath -> FilePath -> IO Bool
ww2ogg cfg fin fout = do
  withCString fin $ \fin' -> do
    withCString fout $ \fout' -> do
      withCString (wwCodebook cfg) $ \cbook -> do
        n <- hs_ww2ogg fin' fout' cbook
        return $ n == 0

foreign import ccall "hs_ww2ogg"
  hs_ww2ogg :: CString -> CString -> CString -> IO CInt
