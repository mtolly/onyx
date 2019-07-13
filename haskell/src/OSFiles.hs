{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
-- | OS-specific functions to open and show files.
module OSFiles (osOpenFile, osShowFolder, commonDir, fixFileCase) where

import           Control.Monad.IO.Class   (MonadIO (..))
import           System.Directory         (makeAbsolute)
import           System.FilePath          (takeDirectory)
#ifdef WINDOWS
import           Foreign                  (Ptr, nullPtr, ptrToIntPtr,
                                           withArrayLen, withMany)
import           Foreign.C                (CInt (..), CWString, withCWString)
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
import System.Directory (doesPathExist, listDirectory)
import Data.Maybe (fromMaybe)
import System.FilePath (splitFileName, dropTrailingPathSeparator, (</>))
import qualified Data.Text as T
#endif
#endif

osOpenFile :: (MonadIO m) => FilePath -> m ()
osShowFolder :: (MonadIO m) => FilePath -> [FilePath] -> m ()

commonDir :: [FilePath] -> IO (Maybe (FilePath, [FilePath]))
commonDir fs = do
  fs' <- liftIO $ mapM makeAbsolute fs
  return $ case map takeDirectory fs' of
    dir : dirs | all (== dir) dirs -> Just (dir, fs')
    _                              -> Nothing

-- | On case-sensitive systems, finds a file in a case-insensitive manner.
fixFileCase :: (MonadIO m) => FilePath -> m FilePath

#ifdef WINDOWS

-- TODO this should be ccall on 64-bit, see Win32 package
foreign import stdcall unsafe "ShellExecuteW"
  c_ShellExecute :: HWND -> LPCWSTR -> LPCWSTR -> LPCWSTR -> LPCWSTR -> INT -> IO HINSTANCE

foreign import ccall unsafe "onyx_ShowFiles"
  c_ShowFiles :: CWString -> Ptr CWString -> CInt -> IO ()

osOpenFile f = liftIO $ withCWString f $ \wstr -> do
  -- COM must be init'd before this. SDL does it for us
  n <- c_ShellExecute nullPtr nullPtr wstr nullPtr nullPtr 5
  if ptrToIntPtr n > 32
    then return ()
    else error $ "osOpenFile: ShellExecuteW return code " ++ show n

osShowFolder dir [] = osOpenFile dir
osShowFolder dir fs = liftIO $
  withCWString dir $ \cdir ->
  withMany withCWString fs $ \cfiles ->
  withArrayLen cfiles $ \len pcfiles ->
  c_ShowFiles cdir pcfiles $ fromIntegral len

fixFileCase = return

#else

#ifdef MACOSX

osOpenFile f = liftIO $ callProcess "open" [f]

foreign import ccall unsafe "onyx_ShowFiles"
  c_ShowFiles :: Ptr CString -> CInt -> IO ()

osShowFolder dir [] = osOpenFile dir
osShowFolder _   fs = do
  liftIO $ withMany withCString fs $ \cstrs -> do
    withArrayLen cstrs $ \len pcstrs -> do
      c_ShowFiles pcstrs $ fromIntegral len

fixFileCase = return

#else

osOpenFile f = liftIO $ case os of
  "linux" -> hSilence [stdout, stderr] $ callProcess "xdg-open" [f]
  _       -> return ()

osShowFolder dir _ = osOpenFile dir


fixFileCase f = fromMaybe f <$> fixFileCaseMaybe f

fixFileCaseMaybe :: (MonadIO m) => FilePath -> m (Maybe FilePath)
fixFileCaseMaybe (dropTrailingPathSeparator -> f) = liftIO $ do
  let (dropTrailingPathSeparator -> dir, entry) = splitFileName f
  doesPathExist f >>= \case
    True -> return $ Just f -- entry exists, no problem
    False -> if f == dir
      then return Nothing -- we're at root, drive, or cwd, and it doesn't exist
      else fixFileCaseMaybe dir >>= \case
        Nothing -> return Nothing -- dir doesn't exist
        Just dir' -> do
          -- dir exists, now we need to look for entry
          entries <- listDirectory dir'
          let compForm = T.toCaseFold . T.pack
              entry' = compForm entry
          case filter ((== entry') . compForm) entries of
            [] -> return Nothing
            e : _ -> return $ Just $ dir' </> e

#endif

#endif
