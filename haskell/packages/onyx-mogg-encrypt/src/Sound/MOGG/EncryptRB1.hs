module Sound.MOGG.EncryptRB1 where

import           Control.Monad (when)
import           Foreign.C

foreign import ccall "encryptOgg"
  c_encryptOgg :: CString -> CString -> IO CInt

encryptRB1 :: FilePath -> FilePath -> IO ()
encryptRB1 fin fout = do
  withCString fin $ \pin -> do
    withCString fout $ \pout -> do
      res <- c_encryptOgg pin pout
      when (res /= 0) $ ioError $ userError "Error in MOGG encryption"
