{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Foreign                          (Ptr)
import           Foreign.C                        (CInt, CUChar)
import qualified Language.C.Inline.Cpp            as C
import           Numeric                          (readHex)
import           Onyx.Audio                       (Audio (..))
import           Onyx.Build                       (NameRule (..), hashRB3,
                                                   targetTitle, validFileName,
                                                   validFileNamePiece)
import           Onyx.Build.Common                (getTargetMetadata)
import           Onyx.GUI.Binding
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

C.context flContext

C.include "FL/Fl.H"
C.include "FL/Fl_Group.H"
C.include "FL/fl_ask.H"

newtype X = X CInt deriving (Eq, Show, Ord)
newtype Y = Y CInt deriving (Eq, Show, Ord)
newtype Width = Width CInt deriving (Eq, Show, Ord)
newtype Height = Height CInt deriving (Eq, Show, Ord)
data Position = Position X Y deriving (Eq, Show, Ord)
data Size = Size Width Height deriving (Eq, Show, Ord)
data Rectangle = Rectangle { rectanglePosition :: Position , rectangleSize :: Size } deriving (Eq, Show, Ord)

fl_rgb_color :: CUChar -> CUChar -> CUChar -> Fl_Color
fl_rgb_color r g b =
  [C.pure| Fl_Color {
    fl_rgb_color($(unsigned char r), $(unsigned char g), $(unsigned char b))
  } |]

--------------------------------------------------------------------------------

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

chopRight :: CInt -> Rectangle -> (Rectangle, Rectangle)
chopRight n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width $ w - n) (Height h))
  , Rectangle (Position (X $ x + w - n) (Y y)) (Size (Width n) (Height h))
  )

chopLeft :: CInt -> Rectangle -> (Rectangle, Rectangle)
chopLeft n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width n) (Height h))
  , Rectangle (Position (X $ x + n) (Y y)) (Size (Width $ w - n) (Height h))
  )

chopTop :: CInt -> Rectangle -> (Rectangle, Rectangle)
chopTop n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width w) (Height n))
  , Rectangle (Position (X x) (Y $ y + n)) (Size (Width w) (Height $ h - n))
  )

chopBottom :: CInt -> Rectangle -> (Rectangle, Rectangle)
chopBottom n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width w) (Height $ h - n))
  , Rectangle (Position (X x) (Y $ y + h - n)) (Size (Width w) (Height n))
  )

splitHorizN :: CInt -> Rectangle -> [Rectangle]
splitHorizN 1 rect = [rect]
splitHorizN n rect@(Rectangle _ (Size (Width w) _)) = let
  (rectFirst, rectRest) = chopLeft (quot w n) rect
  in rectFirst : splitHorizN (n - 1) rectRest

splitVertN :: CInt -> Rectangle -> [Rectangle]
splitVertN 1 rect = [rect]
splitVertN n rect@(Rectangle _ (Size _ (Height h))) = let
  (rectFirst, rectRest) = chopTop (quot h n) rect
  in rectFirst : splitVertN (n - 1) rectRest

trimClock :: CInt -> CInt -> CInt -> CInt -> Rectangle -> Rectangle
trimClock t r b l rect = let
  (_, rect1) = chopTop    t rect
  (rect2, _) = chopRight  r rect1
  (rect3, _) = chopBottom b rect2
  (_, rect4) = chopLeft   l rect3
  in rect4

homeTabColor, functionTabColor, taskColor, globalLogColor, miscColor, loadSongColor, batchProcessColor, behindTabsColor, quickConvertColor, defaultColor :: Fl_Color
homeTabColor      = fl_rgb_color 209 177 224
functionTabColor  = fl_rgb_color 224 210 177
taskColor         = fl_rgb_color 179 221 187
globalLogColor    = fl_rgb_color 114 74 124
miscColor         = fl_rgb_color 177 173 244
loadSongColor     = fl_rgb_color 186 229 181
batchProcessColor = fl_rgb_color 237 173 193
behindTabsColor   = fl_rgb_color 94 94 94
quickConvertColor = fl_rgb_color 232 213 150
defaultColor      = fl_rgb_color 192 192 192

setTabColor :: Ptr Fl_Group -> Fl_Color -> IO ()
setTabColor tab color =
  [C.block| void {
    $(Fl_Group* tab)->color($(Fl_Color color));
    $(Fl_Group* tab)->selection_color($(Fl_Color color));
  } |]

-- TODO many things

trimXbox
  :: Preferences
  -> FilePath
  -> FilePath
trimXbox prefs f = if prefs.prefTrimXbox
  then validFileName NameRuleXbox f
  else f

warnCombineXboxGH2 :: (Event -> IO ()) -> IO () -> IO ()
warnCombineXboxGH2 sink go = sink $ EventOnyx do
  prefs <- readPreferences
  stackIO $ unless prefs.prefWarnedXboxGH2 do
    [C.block| void {
      fl_choice(
        "Note! When loading songs into Guitar Hero II for Xbox 360, you *must* combine them into packs (go to \"Quick convert\").\n"
        "Loading more than 16 packages will fail to load some songs, and will corrupt your save!",
        "OK", nullptr, nullptr
      );
    } |]
    savePreferences prefs { prefWarnedXboxGH2 = True }
  stackIO go

warnXboxGHWoR :: (Event -> IO ()) -> Onyx () -> IO ()
warnXboxGHWoR sink go = sink $ EventOnyx do
  prefs <- readPreferences
  stackIO $ unless prefs.prefWarnedXboxWoR do
    [C.block| void {
      fl_choice(
        "Please use the \"WoR Song Cache\" tab in Other Tools after conversion,\n"
        "to produce the extra file needed to load your songs.\n"
        "\n"
        "IMPORTANT: There may be an issue with loading too many custom songs\n"
        "into Guitar Hero: Warriors of Rock, that could corrupt your save.\n"
        "Please back up any save data you care about before loading custom songs!",
        "OK", nullptr, nullptr
      );
    } |]
    savePreferences prefs { prefWarnedXboxWoR = True }

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

type BuildYamlControl a = WriterT (IO (Endo a)) IO

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
