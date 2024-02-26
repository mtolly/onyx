module Onyx.Harmonix.MOGG.Crypt where

import           Control.Monad (when)
import           Foreign.C

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
