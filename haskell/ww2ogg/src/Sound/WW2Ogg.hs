module Sound.WW2Ogg (WW2OggConfig(..), ww2ogg) where

import           Foreign
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
        if n == 0
          then do
            withCString "onyx_revorb" $ \prog -> do
              withArrayLen [prog, fout'] $ \len ptr -> do
                n' <- revorb_main (fromIntegral len) ptr
                return $ n' == 0
          else return False

foreign import ccall "hs_ww2ogg"
  hs_ww2ogg :: CString -> CString -> CString -> IO CInt

-- If we don't run Revorb then my current ogg loading code hangs.
-- TODO remove print statements inside revorb.c, strip down the main function
foreign import ccall "revorb_main"
  revorb_main :: CInt -> Ptr CString -> IO CInt
