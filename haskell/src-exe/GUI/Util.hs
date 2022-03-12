{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module GUI.Util (askFolder) where

#ifdef WINDOWS
import           Foreign
import           Foreign.C
#else
import           Control.Monad                    (forM_)
import qualified Data.Text                        as T
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FL
#endif

askFolderResult :: Maybe FilePath -> IO (Maybe FilePath)

#ifdef WINDOWS

foreign import ccall unsafe "OnyxPickFolder"
  c_PickFolder :: CWString -> IO CWString

askFolderResult initial = maybe ($ nullPtr) withCWString initial $ \ws -> do
  res <- c_PickFolder ws
  if res == nullPtr
    then return Nothing
    else Just <$> peekCWString res

#else

askFolderResult initial = do
  picker <- FL.nativeFileChooserNew $ Just FL.BrowseDirectory
  FL.setTitle picker $ T.pack "Select folder"
  forM_ initial $ FL.setDirectory picker . T.pack
  FL.showWidget picker >>= \case
    FL.NativeFileChooserPicked -> fmap T.unpack <$> FL.getFilename picker
    _                          -> return Nothing

#endif

askFolder :: Maybe FilePath -> (FilePath -> IO ()) -> IO ()
askFolder initial fn = askFolderResult initial >>= mapM_ fn
