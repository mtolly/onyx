module Sound.WW2Ogg (WW2OggConfig(..), ww2ogg) where

import           Foreign
import           Foreign.C
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

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
            bs <- B.readFile fout
            BU.unsafeUseAsCStringLen bs $ \(p, inlen) -> do
              alloca $ \ppout -> do
                alloca $ \plen -> do
                  res <- hs_revorb (castPtr p) (fromIntegral inlen) ppout plen
                  if res == 0
                    then do
                      pout <- peek ppout
                      outlen <- peek plen
                      bs' <- B.packCStringLen (castPtr pout, fromIntegral outlen)
                      free pout
                      B.writeFile fout bs'
                      return True
                    else return False
          else return False

foreign import ccall "hs_ww2ogg"
  hs_ww2ogg :: CString -> CString -> CString -> IO CInt

-- If we don't run Revorb then my current ogg loading code hangs.
foreign import ccall "hs_revorb"
  hs_revorb :: Ptr () -> CInt -> Ptr (Ptr ()) -> Ptr CInt -> IO CInt
