{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI.Core where

import           Control.Concurrent               (ThreadId)
import qualified Control.Exception                as Exc
import           Control.Monad.Extra              (forM, forM_, guard, unless,
                                                   void)
import           Control.Monad.IO.Class           (MonadIO (..), liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Control.Monad.Trans.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Resource     (ResourceT, resourceForkIO,
                                                   runResourceT)
import           Control.Monad.Trans.Writer       (WriterT, tell)
import qualified Data.ByteString                  as B
import           Data.Default.Class               (Default, def)
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import           Data.List.Extra                  (findIndex)
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (catMaybes, listToMaybe)
import           Data.Monoid                      (Endo (..))
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
-- import qualified Graphics.UI.FLTK.LowLevel.FL              as FLTK
-- import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
-- import           Graphics.UI.FLTK.LowLevel.FLTKHS          (Height (..),
--                                                             Position (..),
--                                                             Rectangle (..),
--                                                             Size (..),
--                                                             Width (..), X (..),
--                                                             Y (..))
-- import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
import           Numeric                          (readHex)
import           Onyx.Audio                       (Audio (..))
import           Onyx.Build                       (NameRule (..), hashRB3,
                                                   targetTitle, validFileName,
                                                   validFileNamePiece)
import           Onyx.Build.Common                (getTargetMetadata)
import           Onyx.GUI.Util                    (askFolder)
import           Onyx.Harmonix.Ark.GH2            (GH2InstallLocation (..))
import           Onyx.Import
import           Onyx.MIDI.Track.File             (PartName (..))
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.PlayStation.PKG             as PKG
import           Onyx.Preferences                 (Preferences (..),
                                                   readPreferences,
                                                   savePreferences)
import           Onyx.Project
import           Onyx.QuickConvert.FretsOnFire
import           Onyx.QuickConvert.RockBand
import           Onyx.QuickConvert.RockRevolution
import           Onyx.StackTrace
import qualified Onyx.Xbox.STFS                   as STFS
import           Optics                           ((.~))
import qualified System.Directory                 as Dir
import           System.FilePath                  (dropTrailingPathSeparator,
                                                   takeDirectory, takeFileName,
                                                   (</>))
import           Text.Read                        (readMaybe)

type Onyx = StackTraceT OnyxInner
type OnyxInner = QueueLog (ReaderT (Event -> IO ()) (ResourceT IO))

data Event
  = EventIO (IO ())
  | EventOnyx (Onyx ())
  | EventFail Message
  | EventMsg (MessageLevel, Message)

getEventSink :: Onyx (Event -> IO ())
getEventSink = lift $ lift ask

logToSink
  :: (MonadIO m)
  => (Event -> IO ())
  -> StackTraceT (QueueLog m) a
  -> m (Either Messages a)
logToSink sink = logIO $ sink . EventMsg

forkOnyx :: Onyx () -> Onyx ThreadId
forkOnyx act = do
  sink <- getEventSink
  chan <- lift $ lift ask
  lift $ lift $ lift $ resourceForkIO do
    runReaderT (logToSink sink act) chan >>= logErrors_ sink

embedOnyx :: (Event -> IO ()) -> Onyx a -> IO (Maybe a)
embedOnyx sink act = runResourceT $
  runReaderT (logToSink sink act) sink >>= logErrors sink

logErrors :: (MonadIO m) => (Event -> IO ()) -> Either Messages a -> m (Maybe a)
logErrors sink = \case
  Right x              -> return $ Just x
  Left (Messages msgs) -> do
    liftIO $ forM_ msgs $ sink . EventFail
    return Nothing

logErrors_ :: (MonadIO m) => (Event -> IO ()) -> Either Messages () -> m ()
logErrors_ sink = void . logErrors sink

safeOnyx :: Onyx () -> Onyx ()
safeOnyx f = getEventSink >>= \sink -> errorToEither f >>= logErrors_ sink

-- not translating _resizeWindow

data RB3Create
  = RB3CON FilePath
  | RB3PKG FilePath
  | RB3Magma FilePath
  | RB3LoosePS3 FilePath

data RB2Create
  = RB2CON FilePath
  | RB2PKG FilePath
  | RB2LoosePS3 FilePath

data PSCreate
  = PSDir FilePath
  | PSZip FilePath
  | PSSng FilePath

data GH1Create
  = GH1ARK FilePath
  | GH1DIYPS2 FilePath

data GH2Create
  = GH2LIVE FilePath
  | GH2ARK FilePath GH2InstallLocation
  | GH2DIYPS2 FilePath

data GH3Create
  = GH3LIVE FilePath
  | GH3PKG FilePath
  | GH3LoosePS3 FilePath

data GHWORCreate
  = GHWORLIVE FilePath
  | GHWORPKG FilePath
  | GHWORLoosePS3 FilePath

data RRCreate
  = RRLIVE FilePath
  | RRPKG FilePath

templateApplyInput :: Project -> Maybe (Target FilePath) -> T.Text -> T.Text
templateApplyInput proj mtgt txt = T.pack $ validFileName NameRulePC $ dropTrailingPathSeparator $ T.unpack $ foldr ($) txt
  [ T.intercalate (T.pack $ takeDirectory proj.projectTemplate) . T.splitOn "%input_dir%"
  , T.intercalate (validFileNamePiece NameRulePC $ T.pack $ takeFileName proj.projectTemplate) . T.splitOn "%input_base%"
  , T.intercalate (validFileNamePiece NameRulePC title) . T.splitOn "%title%"
  , T.intercalate (validFileNamePiece NameRulePC $ getArtist metadata) . T.splitOn "%artist%"
  , T.intercalate (validFileNamePiece NameRulePC $ getAlbum metadata) . T.splitOn "%album%"
  , T.intercalate (validFileNamePiece NameRulePC $ getAuthor metadata) . T.splitOn "%author%"
  , T.intercalate (validFileNamePiece NameRulePC songID) . T.splitOn "%song_id%"
  ] where
    metadata = case mtgt of
      Nothing  -> proj.projectSongYaml.metadata
      Just tgt -> getTargetMetadata proj.projectSongYaml tgt
    title = case mtgt of
      Nothing  -> getTitle metadata
      Just tgt -> targetTitle proj.projectSongYaml tgt
    songID = case mtgt of
      Just (RB3 rb3) -> case rb3.songID of
        SongIDInt    n -> T.pack $ show n
        SongIDSymbol s -> s
        _              -> T.pack $ show $ hashRB3 proj.projectSongYaml rb3
      _              -> ""

-- TODO many things

trimXbox
  :: Preferences
  -> FilePath
  -> FilePath
trimXbox prefs f = if prefs.prefTrimXbox
  then validFileName NameRuleXbox f
  else f

-- TODO many things

data STFSSpec = STFSSpec
  { stfsPath :: FilePath
  , stfsMeta :: STFS.Metadata
  }

data PKGSpec = PKGSpec
  { pkgPath      :: FilePath
  , pkgContentID :: B.ByteString
  }

type PackageSpec = Either STFSSpec PKGSpec

getSTFSSpec :: FilePath -> IO (Maybe STFSSpec)
getSTFSSpec f = Exc.try (STFS.withSTFSPackage f $ return . (.stfsMetadata)) >>= \case
  Left e -> let
    _ = e :: Exc.IOException
    in return Nothing
  Right meta -> return $ Just STFSSpec
    { stfsPath = f
    , stfsMeta = meta
    }

getPKGSpec :: FilePath -> IO (Maybe PKGSpec)
getPKGSpec f = Exc.try (PKG.loadPKG f) >>= \case
  Left e -> let
    _ = e :: Exc.IOException
    in return Nothing
  Right pkg -> return $ Just PKGSpec
    { pkgPath      = f
    , pkgContentID = pkg.pkgHeader.pkgContentID
    }

getPackageSpec :: FilePath -> IO (Maybe PackageSpec)
getPackageSpec f = getSTFSSpec f >>= \case
  Nothing   -> fmap Right <$> getPKGSpec f
  Just stfs -> return $ Just $ Left stfs

searchSTFS :: FilePath -> Onyx ([FilePath], [STFSSpec])
searchSTFS f = stackIO $ Dir.doesDirectoryExist f >>= \case
  True  -> (\fs -> (map (f </>) fs, [])) <$> Dir.listDirectory f
  False -> (\mspec -> ([], toList mspec)) <$> getSTFSSpec f

searchWoRCachable :: FilePath -> Onyx ([FilePath], [(FilePath, T.Text)])
searchWoRCachable f = stackIO $ Dir.doesDirectoryExist f >>= \case
  True  -> (\fs -> (map (f </>) fs, [])) <$> Dir.listDirectory f
  False -> (\mspec -> ([], toList mspec)) <$> isWoRCachable f

searchGH3Cachable :: FilePath -> Onyx ([FilePath], [(FilePath, T.Text)])
searchGH3Cachable f = stackIO $ Dir.doesDirectoryExist f >>= \case
  True  -> (\fs -> (map (f </>) fs, [])) <$> Dir.listDirectory f
  False -> (\mspec -> ([], toList mspec)) <$> isGH3Cachable f

searchQuickSongs :: FilePath -> Onyx ([FilePath], [QuickInput])
searchQuickSongs f = stackIO (Dir.doesDirectoryExist f) >>= \case
  True  -> stackIO $ (\fs -> (map (f </>) fs, [])) <$> Dir.listDirectory f
  False -> do
    res <- loadQuickInput f
    case res of
      Nothing     -> return ([], [])
      Just qinput -> return ([], [qinput])

searchQuickFoF :: FilePath -> Onyx ([FilePath], [QuickFoF])
searchQuickFoF f = stackIO (Dir.doesDirectoryExist f) >>= \case
  True  -> stackIO $ (\fs -> (map (f </>) fs, [])) <$> Dir.listDirectory f
  False -> do
    res <- loadQuickFoF f
    case res of
      Nothing   -> return ([], [])
      Just qfof -> return ([], [qfof])

searchRockRevolution :: FilePath -> Onyx ([FilePath], [(FilePath, [RRSong])])
searchRockRevolution f = stackIO (Dir.doesDirectoryExist f) >>= \case
  True  -> stackIO $ (\fs -> (map (f </>) fs, [])) <$> Dir.listDirectory f
  False -> do
    res <- loadRRSongs [f]
    return ([], res)

isWoRCachable :: FilePath -> IO (Maybe (FilePath, T.Text))
isWoRCachable f = if "_TEXT.PAK.PS3.EDAT" `T.isSuffixOf` T.toUpper (T.pack f)
  then return $ Just (f, "") -- could read content ID from EDAT if we wanted
  else getPackageSpec f >>= \case
    Just (Left stfs) -> return $ Just (f, T.concat $ take 1 stfs.stfsMeta.md_DisplayName)
    Just (Right pkg) -> return $ Just (f, TE.decodeLatin1 $ pkgContentID pkg)
    Nothing          -> return Nothing

isGH3Cachable :: FilePath -> IO (Maybe (FilePath, T.Text))
isGH3Cachable f = if "_TEXT.PAK.PS3.EDAT" `T.isSuffixOf` T.toUpper (T.pack f)
  then return $ Just (f, "")
  else getPackageSpec f >>= \case
    Just (Left stfs) -> return $ Just (f, T.concat $ take 1 stfs.stfsMeta.md_DisplayName)
    Just (Right pkg) -> return $ Just (f, TE.decodeLatin1 $ pkgContentID pkg)
    Nothing          -> return Nothing

-- TODO many things

mixDownStems :: SongYaml f -> SongYaml f
mixDownStems song = song
  { plans = song.plans <&> \case
    StandardPlan info -> StandardPlan info
      { parts = Parts HM.empty
      , song = let
        -- Shouldn't need to worry about mono/stereo due to Onyx.Audio.sameChannels
        allAudio = toList info.song <> (toList info.parts >>= toList)
        in case allAudio of
          []     -> Nothing
          [x]    -> Just x
          x : xs -> Just $ Mix $ x NE.:| xs
      , crowd = Nothing -- just drop? dunno what people would want
      }
    MoggPlan info -> MoggPlan $ info & #parts .~ Parts HM.empty
  }
