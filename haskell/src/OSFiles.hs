{-# LANGUAGE CPP #-}
-- | OS-specific functions to open and show files.
module OSFiles (osOpenFile, osShowFiles, osShowFolder, useResultFiles) where

import           Control.Monad.IO.Class   (MonadIO (..))
import           System.Directory         (makeAbsolute)
import           System.FilePath          (takeDirectory, takeExtension)
#ifdef WINDOWS
import           Foreign                  (nullPtr, ptrToIntPtr)
import           Foreign.C                (withCWString)
import           Graphics.Win32.GDI.Types (HWND)
import           System.Win32.Types       (HINSTANCE, INT, LPCWSTR)
#else
import           System.Process           (callProcess)
#ifdef MACOSX
import           Foreign                  (Ptr, withArrayLen, withMany)
import           Foreign.C                (CInt (..), CString, withCString)
#else
import           System.Info              (os)
import           System.IO                (stderr, stdout)
import           System.IO.Silently       (hSilence)
#endif
#endif

-- | Does a sensible thing for the result files from a task or set of tasks.
useResultFiles :: (MonadIO m) => [FilePath] -> m ()
useResultFiles [] = return ()
useResultFiles [f] = case takeExtension f of
  ".html" -> osOpenFile f
  ".RPP"  -> osOpenFile f
  _       -> osShowFiles [f]
useResultFiles fs = do
  fs' <- liftIO $ mapM makeAbsolute fs
  case map takeDirectory fs' of
    dir : dirs | all (== dir) dirs -> osShowFiles fs'
    _          -> return ()

osOpenFile :: (MonadIO m) => FilePath -> m ()
osShowFiles :: (MonadIO m) => [FilePath] -> m ()
osShowFolder :: (MonadIO m) => FilePath -> m ()

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

osShowFiles _ = return () -- TODO

osShowFolder _ = return () -- TODO

#else

#ifdef MACOSX

osOpenFile f = liftIO $ callProcess "open" [f]

foreign import ccall unsafe "onyx_ShowFiles"
  c_ShowFiles :: Ptr CString -> CInt -> IO ()

osShowFiles files = liftIO $ withMany withCString files $ \cstrs -> do
  withArrayLen cstrs $ \len pcstrs -> do
    c_ShowFiles pcstrs $ fromIntegral len

osShowFolder dir = liftIO $ callProcess "open" [dir]

#else

osOpenFile f = liftIO $ case os of
  "linux" -> hSilence [stdout, stderr] $ callProcess "exo-open" [f]
  _       -> return ()

osShowFiles files = liftIO $ case os of
  "linux" -> callProcess "nautilus" files
  _       -> return ()

osShowFolder dir = liftIO $ case os of
  "linux" -> callProcess "nautilus" [dir]
  _       -> return ()

#endif

#endif
