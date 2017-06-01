{-# LANGUAGE CPP #-}
-- | OS-specific functions to open and show files.
module OSFiles (osOpenFile) where

import           Control.Monad.IO.Class   (MonadIO (..))
#ifdef WINDOWS
import           Foreign                  (nullPtr, ptrToIntPtr)
import           Foreign.C                (withCWString)
import           Graphics.Win32.GDI.Types (HWND)
import           System.Win32.Types       (HINSTANCE, INT, LPCWSTR)
#else
import           System.Info              (os)
import           System.IO                (stderr, stdout)
import           System.IO.Silently       (hSilence)
import           System.Process           (callProcess)
#endif

osOpenFile :: (MonadIO m) => FilePath -> m ()

#ifdef WINDOWS

-- TODO this should be ccall on 64-bit, see Win32 package
foreign import stdcall unsafe "ShellExecuteW"
  c_ShellExecute :: HWND -> LPCWSTR -> LPCWSTR -> LPCWSTR -> LPCWSTR -> INT -> IO HINSTANCE

osOpenFile f = liftIO $ withCWString f $ \wstr -> do
  -- TODO do we need to open COM? I think SDL does it for us
  n <- c_ShellExecute nullPtr nullPtr wstr nullPtr nullPtr 5
  if ptrToIntPtr n > 32
    then return ()
    else error $ "osOpenFile: ShellExecuteW return code " ++ show n

#else

osOpenFile f = liftIO $ case os of
  -- "mingw32" -> void $ spawnCommand $ "\"" ++ f ++ "\""
  "darwin" -> callProcess "open" [f]
  "linux"  -> hSilence [stdout, stderr] $ callProcess "exo-open" [f]
  _        -> return ()

#endif
