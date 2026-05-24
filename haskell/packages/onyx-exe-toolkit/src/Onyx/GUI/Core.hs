{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
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
import           Control.Monad                    (when)
import           Control.Monad.Extra              (forM, forM_, guard, unless,
                                                   void)
import           Control.Monad.IO.Class           (MonadIO (..), liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Resource     (ResourceT, resourceForkIO,
                                                   runResourceT)
import           Control.Monad.Trans.Writer       (WriterT, tell)
import qualified Data.ByteString                  as B
import           Data.Coerce                      (coerce)
import           Data.Default.Class               (Default, def)
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import           Data.List.Extra                  (findIndex)
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (listToMaybe)
import           Data.Monoid                      (Endo (..))
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Foreign                          (Ptr, nullPtr)
import           Foreign.C                        (CInt, CUChar, peekCString)
import qualified Language.C.Inline.Cpp            as C
import           Numeric                          (readHex)
import           Onyx.Audio                       (Audio (..))
import           Onyx.Build                       (NameRule (..), hashRB3,
                                                   targetTitle, validFileName,
                                                   validFileNamePiece)
import           Onyx.Build.Common                (getTargetMetadata)
import           Onyx.GUI.Binding
import           Onyx.GUI.Util                    (askFolder, checkValue,
                                                   peekUTF8, setCallback,
                                                   withNativeFileChooser,
                                                   withUTF8)
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
C.include "FL/Fl_Box.H"
C.include "FL/Fl_Button.H"
C.include "FL/Fl_Check_Button.H"
C.include "FL/Fl_Round_Button.H"
C.include "FL/Fl_Input.H"
C.include "FL/Fl_Counter.H"
C.include "FL/Fl_Choice.H"
C.include "FL/Fl_Menu_Button.H"
C.include "FL/Fl_Pack.H"
C.include "FL/Fl_Tree.H"
C.include "FL/Fl_Tree_Item.H"
C.include "FL/Fl_Native_File_Chooser.H"
C.include "FL/fl_ask.H"
C.include "Fl_Center.h"
C.include "hs_dnd_button.h"
C.include "hs_dnd_group.h"
C.include "hs_dnd_box.h"
C.include "hs_tree.h"

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

withRect :: Rectangle -> (CInt -> CInt -> CInt -> CInt -> a) -> a
withRect (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) f = f x y w h

withSize :: Size -> (CInt -> CInt -> a) -> a
withSize (Size (Width w) (Height h)) f = f w h

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

makeTemplateRunner :: (Event -> IO ()) -> T.Text -> T.Text -> (T.Text -> IO ()) -> IO ()
makeTemplateRunner sink buttonText defTemplate useTemplate = do
  padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect -> do
    input <- makeTemplateRunner' sink rect buttonText defTemplate useTemplate
    withUTF8 (T.unlines
      [ "Template for where to create new files."
      , "  %input_dir% - folder containing the input"
      , "  %input_base% - input filename by itself, extension removed"
      , "  %modifiers% - added distinguishing features e.g. speed modifier"
      , "  %title% - title from song's metadata (including modifiers)"
      , "  %artist% - artist from song's metadata"
      , "  %album% - album from song's metadata"
      , "  %author% - author from song's metadata"
      , "  %song_id% - unique song ID"
      ]) \tip ->
      [C.block| void { $(Fl_Input* input)->copy_tooltip($(char* tip)); } |]

makeTemplateRunner' :: (Event -> IO ()) -> Rectangle -> T.Text -> T.Text -> (T.Text -> IO ()) -> IO (Ptr Fl_Input)
makeTemplateRunner' sink rect buttonText defTemplate useTemplate = do
  let (buttonRect, notButton) = chopLeft 250 rect
      (_, rectA) = chopLeft 80 notButton
      (inputRect, rectB) = chopRight 100 rectA
      (_, rectC) = chopRight 90 rectB
      (browseRect, _) = chopLeft 40 rectC
      (_, rectD) = chopRight 50 rectC
      (_, resetRect) = chopRight 40 rectD
  button <- withRect buttonRect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
  withUTF8 buttonText \clabel ->
    [C.block| void { $(Fl_Button* button)->copy_label($(char* clabel)); } |]
  [C.block| void { $(Fl_Button* button)->color($(Fl_Color taskColor)); } |]
  input <- withRect inputRect \ix iy iw ih ->
    [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih), "Template") } |]
  [C.block| void {
    $(Fl_Input* input)->labelsize(13);
    $(Fl_Input* input)->align(FL_ALIGN_LEFT);
  } |]
  withUTF8 defTemplate \cdef ->
    [C.block| void { $(Fl_Input* input)->value($(char* cdef)); } |]
  setCallback button do
    val <- peekInputValue input
    useTemplate val
  browseButton <- withRect browseRect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@fileopen") } |]
  setCallback browseButton $ sink $ EventIO $ askFolder Nothing \dir -> do
    val <- peekInputValue input
    let newVal = dir </> takeFileName (T.unpack val)
    withUTF8 newVal \cnewval ->
      [C.block| void { $(Fl_Input* input)->value($(char* cnewval)); } |]
  resetButton <- withRect resetRect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@undo") } |]
  setCallback resetButton $ sink $ EventIO do
    withUTF8 defTemplate \cdef ->
      [C.block| void { $(Fl_Input* input)->value($(char* cdef)); } |]
  return input

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

padded
  :: (MonadIO m)
  => CInt -> CInt -> CInt -> CInt
  -> Size
  -> (Rectangle -> m a)
  -> m a
padded t r b l size@(Size (Width w) (Height h)) fn = do
  group <- liftIO do
    let gw = w + fromIntegral l + fromIntegral r
        gh = h + fromIntegral t + fromIntegral b
    [C.exp| Fl_Group* { new Fl_Group(0, 0, $(int gw), $(int gh)) } |]
  let rect = Rectangle (Position (X $ fromIntegral l) (Y $ fromIntegral t)) size
  box <- liftIO $ withRect rect \bx by bw bh ->
    [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
  liftIO [C.block| void {
    $(Fl_Group* group)->resizable($(Fl_Box* box));
  } |]
  x <- fn rect
  liftIO [C.block| void { $(Fl_Group* group)->end(); } |]
  return x

centerFixed :: Rectangle -> IO a -> IO a
centerFixed rect makeChildren = do
  center <- withRect rect \cx cy cw ch ->
    [C.exp| Fl_Center* { new Fl_Center($(int cx), $(int cy), $(int cw), $(int ch)) } |]
  x <- makeChildren
  [C.block| void { $(Fl_Center* center)->end(); } |]
  return x

makeTab :: Rectangle -> T.Text -> (Rectangle -> Ptr Fl_Group -> IO a) -> IO a
makeTab rect name fn = do
  let tabHeight = 25
      (_, innerRect) = chopTop tabHeight rect
  group <- withRect innerRect \gx gy gw gh ->
    [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
  withUTF8 name \cname ->
    [C.block| void { $(Fl_Group* group)->copy_label($(char* cname)); } |]
  res <- fn innerRect group
  [C.block| void { $(Fl_Group* group)->end(); } |]
  return res

horizRadio' :: Rectangle -> Maybe (IO ()) -> [(T.Text, a, Bool)] -> IO (IO (Maybe a))
horizRadio' _    _  []   = error "horizRadio: empty option list"
horizRadio' rect cb opts = do
  let rects = splitHorizN (fromIntegral $ length opts) rect
  btns <- forM (zip opts rects) \((label, _, b), rectButton) -> do
    btn <- withRect rectButton \bx by bw bh ->
      [C.exp| Fl_Round_Button* { new Fl_Round_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 label \clabel ->
      [C.block| void { $(Fl_Round_Button* btn)->copy_label($(char* clabel)); } |]
    let bval = if b then 1 else 0 :: CInt
    [C.block| void { $(Fl_Round_Button* btn)->value($(int bval)); } |]
    return btn
  forM_ btns \opt -> do
    setCallback opt do
      forM_ btns \opt' -> do
        let v = if opt == opt' then 1 else 0 :: CInt
        [C.block| void { $(Fl_Round_Button* opt')->value($(int v)); } |]
      sequence_ cb
  return do
    bools <- forM btns \btn -> do
      v <- [C.exp| int { $(Fl_Round_Button* btn)->value() } |]
      return (v /= 0)
    return $ fmap (\(_, (_, x, _)) -> x) $ listToMaybe $ filter fst $ zip bools opts

horizRadio :: Rectangle -> [(T.Text, a, Bool)] -> IO (IO (Maybe a))
horizRadio rect = horizRadio' rect Nothing

speedPercent' :: Bool -> Rectangle -> IO (IO Double, Ptr Fl_Counter)
speedPercent' isConverter rect = do
  let lbl :: T.Text
      lbl = if isConverter then "Speed (%)" else ""
  speed <- withRect rect \sx sy sw sh ->
    [C.exp| Fl_Counter* { new Fl_Counter($(int sx), $(int sy), $(int sw), $(int sh)) } |]
  withUTF8 lbl \clabel ->
    [C.block| void { $(Fl_Counter* speed)->copy_label($(char* clabel)); } |]
  [C.block| void {
    Fl_Counter* s = $(Fl_Counter* speed);
    s->labelsize(13);
    s->align(FL_ALIGN_LEFT);
    s->step(1);
    s->lstep(5);
    s->minimum(1);
    s->value(100);
  } |]
  if isConverter
    then do
      [C.block| void { $(Fl_Counter* speed)->tooltip(
        "Change the speed of the chart and its audio (without changing pitch). If importing from a CON, a non-100% value requires unencrypted audio."
      ); } |]
    else [C.block| void { $(Fl_Counter* speed)->tooltip("Speed (%)"); } |]
  let getSpeed = do
        v <- [C.exp| double { $(Fl_Counter* speed)->value() } |]
        return (coerce v / 100 :: Double)
  return (getSpeed, speed)

speedPercent :: Bool -> Rectangle -> IO (IO Double)
speedPercent isConverter rect = fst <$> speedPercent' isConverter rect

makePresetDropdown :: Rectangle -> [(T.Text, a)] -> IO (IO a)
makePresetDropdown rect opts = do
  let (initialLabel, initialOpt) = head opts
  menu <- withRect rect \mx my mw mh ->
    [C.exp| Fl_Menu_Button* { new Fl_Menu_Button($(int mx), $(int my), $(int mw), $(int mh)) } |]
  withUTF8 initialLabel \clabel ->
    [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* clabel)); } |]
  ref <- newIORef initialOpt
  forM_ opts \(label, opt) -> do
    -- need to escape in menu items (but not button label) to not make submenus
    let label' = T.replace "/" "\\/" label
    menuCb <- makeCallback \_ _ -> do
      writeIORef ref opt
      withUTF8 label \newLabel ->
        [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* newLabel)); } |]
    withUTF8 label' \clabel ->
      [C.block| void {
        $(Fl_Menu_Button* menu)->add($(char* clabel), 0, $(Fl_Callback menuCb));
      } |]
  return $ readIORef ref

makeDropdown :: Rectangle -> NE.NonEmpty (T.Text, a, Bool) -> IO (IO a)
makeDropdown rect opts = do
  let (initialLabel, initialOpt, _) = case NE.filter (\(_, _, b) -> b) opts of
        opt : _ -> opt
        []      -> NE.head opts
  menu <- withRect rect \mx my mw mh ->
    [C.exp| Fl_Menu_Button* { new Fl_Menu_Button($(int mx), $(int my), $(int mw), $(int mh)) } |]
  withUTF8 initialLabel \clabel ->
    [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* clabel)); } |]
  ref <- newIORef initialOpt
  forM_ opts \(label, opt, _) -> do
    -- need to escape in menu items (but not button label) to not make submenus
    let label' = T.replace "/" "\\/" label
    menuCb <- makeCallback \_ _ -> do
      writeIORef ref opt
      withUTF8 label \newLabel ->
        [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* newLabel)); } |]
    withUTF8 label' \clabel ->
      [C.block| void {
        $(Fl_Menu_Button* menu)->add($(char* clabel), 0, $(Fl_Callback menuCb));
      } |]
  return $ readIORef ref

trimXbox
  :: Preferences
  -> FilePath
  -> FilePath
trimXbox prefs f = if prefs.prefTrimXbox
  then validFileName NameRuleXbox f
  else f

gh2DeluxeSelector
  :: (Event -> IO ())
  -> Rectangle
  -> IO (IO (Maybe Bool)) -- Bool is True if 2x drums
gh2DeluxeSelector sink rect = do
  let (checkArea, kicksArea) = chopRight 150 rect
  check <- withRect checkArea \cx cy cw ch ->
    [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "GH2 Deluxe features") } |]
  kicks <- withRect kicksArea \kx ky kw kh ->
    [C.exp| Fl_Choice* { new Fl_Choice($(int kx), $(int ky), $(int kw), $(int kh)) } |]
  [C.block| void {
    $(Fl_Choice* kicks)->add("1x drums");
    $(Fl_Choice* kicks)->add("2x drums");
    $(Fl_Choice* kicks)->value(0);
  } |]
  let updateKicks = do
        enabled <- checkValue check
        if enabled
          then [C.block| void { $(Fl_Choice* kicks)->activate(); } |]
          else [C.block| void { $(Fl_Choice* kicks)->deactivate(); } |]
  setCallback check $ sink $ EventIO updateKicks
  updateKicks
  [C.block| void { $(Fl_Check_Button* check)->value(0); } |]
  return do
    enableDeluxe <- checkValue check
    if enableDeluxe
      then do
        i <- [C.exp| int { $(Fl_Choice* kicks)->value() } |]
        return $ Just $ i == 1
      else return Nothing

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
  go

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

fileLoadWindow
  :: Rectangle
  -> (Event -> IO ())
  -> T.Text -- ^ singular
  -> T.Text -- ^ plural
  -> (([a] -> IO [a]) -> IO ()) -- ^ read and/or modify the current list of files
  -> [FilePath] -- ^ initial paths to start searching
  -> (FilePath -> Onyx ([FilePath], [a])) -- ^ one step of file search process
  -> (a -> (T.Text, [T.Text])) -- ^ display an entry in the tree
  -> IO (Ptr Fl_Group)
fileLoadWindow rect sink single plural modifyFiles startFiles step display
  = fmap fst
  $ fileLoadWindow' rect sink single plural modifyFiles startFiles step display

fileLoadWindow'
  :: Rectangle
  -> (Event -> IO ())
  -> T.Text -- ^ singular
  -> T.Text -- ^ plural
  -> (([a] -> IO [a]) -> IO ()) -- ^ read and/or modify the current list of files
  -> [FilePath] -- ^ initial paths to start searching
  -> (FilePath -> Onyx ([FilePath], [a])) -- ^ one step of file search process
  -> (a -> (T.Text, [T.Text])) -- ^ display an entry in the tree
  -> IO (Ptr Fl_Group, IO ())
fileLoadWindow' rect sink single plural modifyFiles startFiles step display = mdo
  group <- withRect rect \gx gy gw gh ->
    [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
  let (labelRect, belowLabel) = chopTop 40 rect
      (treeRect, buttonsRect) = chopBottom 50 belowLabel
      labelRect' = trimClock 10 10 5 10 labelRect
      treeRect' = trimClock 5 10 5 10 treeRect
      buttonsRect' = trimClock 5 10 10 10 buttonsRect
      [btnRectA, btnRectB] = splitHorizN 2 buttonsRect'
      (btnRectA', _) = chopRight 5 btnRectA
      (_, btnRectB') = chopLeft 5 btnRectB
  label <- withRect labelRect' \lx ly lw lh ->
    [C.exp| Fl_Box* { new Fl_Box($(int lx), $(int ly), $(int lw), $(int lh)) } |]
  hsTree <- withRect treeRect' \tx ty tw th ->
    [C.exp| HsTree* { new HsTree($(int tx), $(int ty), $(int tw), $(int th)) } |]
  let tree = [C.pure| Fl_Tree* { (Fl_Tree*)$(HsTree* hsTree) } |]
  [C.block| void {
    $(HsTree* hsTree)->end();
    $(Fl_Group* group)->resizable($(HsTree* hsTree));
  } |]
  let updateLabel fs = do
        let msg = case length fs of
              1 -> "1 file loaded."
              n -> show n ++ " files loaded."
        withUTF8 msg \cmsg ->
          [C.block| void { $(Fl_Box* label)->copy_label($(char* cmsg)); } |]
      clearFiles = modifyFiles \_ -> do
        [C.block| void {
          $(Fl_Tree* tree)->clear();
          $(Fl_Tree* tree)->add("");
        } |]
        withUTF8 plural \cplural ->
          [C.block| void { $(Fl_Tree* tree)->root_label($(char* cplural)); } |]
        updateLabel ([] :: [a])
        [C.block| void { Fl::redraw(); } |]
        return []
      addFiles [] = return ()
      addFiles gs = modifyFiles \fs -> do
        root <- [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->root() } |]
        forM_ gs \imp -> do
          let (entry, sublines) = display imp
          withUTF8 entry \centry -> do
            item <- [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->add($(Fl_Tree_Item* root), $(char* centry)) } |]
            forM_ sublines \subline ->
              withUTF8 subline \csub ->
                [C.block| void { $(Fl_Tree* tree)->add($(Fl_Tree_Item* item), $(char* csub)); } |]
            [C.block| void { $(Fl_Tree_Item* item)->close(); } |]
        let fs' = fs ++ gs
        updateLabel fs'
        [C.block| void { Fl::redraw(); } |]
        return fs'
      searchSongs [] = return ()
      searchSongs (loc : locs) = do
        (children, imps) <- step loc
        stackIO $ sink $ EventIO $ addFiles imps
        searchSongs $ locs ++ children
      forkSearch = void . forkOnyx . searchSongs
      swapFiles index1 index2 = modifyFiles \fs -> do
        let fs' = zipWith (\i _ -> fs !! if i == index1 then index2 else if i == index2 then index1 else i) [0..] fs
        let ci1 = fromIntegral index1 :: CInt
            ci2 = fromIntegral index2 :: CInt
        [C.block| void {
          Fl_Tree_Item* root = $(Fl_Tree* tree)->root();
          if (root) root->swap_children($(int ci1), $(int ci2));
        } |]
        updateLabel fs'
        [C.block| void { Fl::redraw(); } |]
        return fs'
      removeFile index = modifyFiles \fs -> do
        let fs' = take index fs ++ drop (index + 1) fs
        -- Find and remove the tree item at this index
        let ci = fromIntegral index :: CInt
        [C.block| void {
          Fl_Tree_Item* root = $(Fl_Tree* tree)->root();
          if (root && $(int ci) < root->children()) {
            Fl_Tree_Item* child = root->child($(int ci));
            if (child) $(Fl_Tree* tree)->remove(child);
          }
        } |]
        updateLabel fs'
        [C.block| void { Fl::redraw(); } |]
        return fs'
  -- Handle Ctrl+Up, Ctrl+Down, Delete on the tree
  fpHandle <- makeTreeHandleCallback \_ event -> do
    if event /= [C.pure| int { FL_KEYDOWN } |] then return 0 else do
      key <- [C.exp| int { Fl::event_key() } |]
      cmd <- [C.exp| int { (Fl::event_state() & FL_COMMAND) ? 1 : 0 } |]
      -- Walk up from focused item to find depth-1 (top-level song) item
      let getSongIndex = do
            focused <- [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->get_item_focus() } |]
            if focused == nullPtr then return Nothing else do
              -- Walk up to depth 1
              let walkUp item = do
                    d <- [C.exp| int { $(Fl_Tree_Item* item)->depth() } |]
                    case compare d 1 of
                      LT -> return nullPtr
                      EQ -> return item
                      GT -> do
                        p <- [C.exp| Fl_Tree_Item* { $(Fl_Tree_Item* item)->parent() } |]
                        if p == nullPtr then return nullPtr else walkUp p
              songItem <- walkUp focused
              if songItem == nullPtr then return Nothing else do
                root <- [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->root() } |]
                ix <- [C.exp| int { $(Fl_Tree_Item* root)->find_child($(Fl_Tree_Item* songItem)) } |]
                if ix < 0 then return Nothing
                  else return $ Just (fromIntegral ix :: Int)
      if | key == [C.pure| int { FL_Up } |] && cmd == 1 -> do
             mIx <- getSongIndex
             case mIx of
               Just ix | ix > 0 -> do
                 sink $ EventIO $ swapFiles (ix - 1) ix
                 return 1
               _ -> return 0
         | key == [C.pure| int { FL_Down } |] && cmd == 1 -> do
             mIx <- getSongIndex
             case mIx of
               Just ix -> do
                 nChildren <- [C.exp| int {
                   $(Fl_Tree* tree)->root() ? $(Fl_Tree* tree)->root()->children() : 0
                 } |]
                 if ix < fromIntegral nChildren - 1
                   then do
                     sink $ EventIO $ swapFiles ix (ix + 1)
                     return 1
                   else return 0
               _ -> return 0
         | key == [C.pure| int { FL_Delete } |] -> do
             mIx <- getSongIndex
             case mIx of
               Just ix -> do
                 sink $ EventIO $ removeFile ix
                 return 1
               _ -> return 0
         | otherwise -> return 0
  [C.block| void {
    $(HsTree* hsTree)->set_handle_callback(
      $(int (*fpHandle)(void*, int)), NULL);
  } |]
  clearFiles
  sink $ EventOnyx $ forkSearch startFiles
  withRect btnRectA' \bx by bw bh -> do
    btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 ("Add " <> single) \clbl -> do
      [C.block| void { $(Fl_Button* btn)->copy_label($(char* clbl)); } |]
    setCallback btn $ sink $ EventIO $ withNativeFileChooser
      [C.pure| int { Fl_Native_File_Chooser::BROWSE_MULTI_FILE } |]
      \picker -> do
        withUTF8 ("Load " <> single) \ctitle ->
          [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* ctitle)); } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          n <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->count() } |]
          fs <- forM [0 .. n - 1] \i -> do
            cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename($(int i)) } |]
            peekUTF8 cstr
          sink $ EventOnyx $ forkSearch fs
  withRect btnRectB' \bx by bw bh -> do
    btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 ("Clear " <> plural) \clbl -> do
      [C.block| void { $(Fl_Button* btn)->copy_label($(char* clbl)); } |]
    setCallback btn $ sink $ EventIO $ void clearFiles
  -- Invisible box overlay on tree rect for drag-and-drop.
  -- Placed last so it's on top; returns 0 for non-DnD events so they pass through.
  dndBox <- withRect treeRect' \dx dy dw dh ->
    [C.exp| HsDndBox* { new HsDndBox($(int dx), $(int dy), $(int dw), $(int dh)) } |]
  setDndBoxCallback dndBox (sink . EventOnyx . forkSearch)
  [C.block| void { $(Fl_Group* group)->end(); } |]
  return (group, void clearFiles)

-- | Parse the text from an FLTK DnD paste event into file paths.
-- Linux uses URL encoding: file:///path/to/file with percent-encoded non-ASCII.
parseDndText :: String -> [String]
parseDndText str = map (T.unpack . removeProtocol) $ T.lines $ T.pack str
  where
    removeProtocol s = case T.stripPrefix "file://" s of
      Nothing -> s
      Just s' -> TE.decodeUtf8 $ percentDecode s'
    percentDecode :: T.Text -> B.ByteString
    percentDecode s = case T.uncons s of
      Just ('%', t) -> case readHex $ T.unpack $ T.take 2 t of
        [(n, "")] -> B.cons n $ percentDecode $ T.drop 2 t
        _         -> B.cons 37 {- % -} $ percentDecode t
      Just (h  , t) -> TE.encodeUtf8 (T.singleton h) <> percentDecode t
      Nothing       -> ""

-- | Set a DnD callback on an HsDndButton. The callback receives a list of file paths.
setDndButtonCallback :: Ptr HsDndButton -> ([String] -> IO ()) -> IO ()
setDndButtonCallback btn f = do
  fpDnd <- makeDndCallback \_ cstr -> do
    str <- peekCString cstr
    f $ parseDndText str
  [C.block| void {
    $(HsDndButton* btn)->set_dnd_callback(
      $(void (*fpDnd)(void*, const char*)), NULL);
  } |]

-- | Set a DnD callback on an HsDndGroup. The callback receives a list of file paths.
setDndGroupCallback :: Ptr HsDndGroup -> ([String] -> IO ()) -> IO ()
setDndGroupCallback grp f = do
  fpDnd <- makeDndCallback \_ cstr -> do
    str <- peekCString cstr
    f $ parseDndText str
  [C.block| void {
    $(HsDndGroup* grp)->set_dnd_callback(
      $(void (*fpDnd)(void*, const char*)), NULL);
  } |]

-- | Set a DnD callback on an HsDndBox. The callback receives a list of file paths.
setDndBoxCallback :: Ptr HsDndBox -> ([String] -> IO ()) -> IO ()
setDndBoxCallback box f = do
  fpDnd <- makeDndCallback \_ cstr -> do
    str <- peekCString cstr
    f $ parseDndText str
  [C.block| void {
    $(HsDndBox* box)->set_dnd_callback(
      $(void (*fpDnd)(void*, const char*)), NULL);
  } |]

type BuildYamlControl a = WriterT (IO (Endo a)) IO

customTitleSuffix
  :: (Event -> IO ())
  -> Rectangle
  -> IO T.Text
  -> (Maybe T.Text -> tgt -> tgt)
  -> BuildYamlControl tgt (IO ())
customTitleSuffix sink rect getSuffix setSuffix = do
  let (checkArea, inputArea) = chopLeft 200 rect
  check <- liftIO $ withRect checkArea \cx cy cw ch ->
    [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Custom Title Suffix") } |]
  input <- liftIO $ withRect inputArea \ix iy iw ih ->
    [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
  let controlInput = do
        b <- checkValue check
        if b
          then [C.block| void { $(Fl_Input* input)->activate(); } |]
          else do
            sfx <- getSuffix
            withUTF8 (T.strip sfx) \csfx ->
              [C.block| void { $(Fl_Input* input)->value($(char* csfx)); } |]
            [C.block| void { $(Fl_Input* input)->deactivate(); } |]
  liftIO $ sink $ EventIO controlInput -- have to delay; can't call now due to dependency loop!
  liftIO $ setCallback check controlInput
  tell do
    b <- checkValue check
    s <- peekInputValue input
    return $ Endo $ setSuffix $ guard b >> Just (T.strip s)
  return controlInput

songIDBox
  :: (?preferences :: Preferences)
  => Rectangle
  -> (RBSongID -> tgt -> tgt)
  -> BuildYamlControl tgt ()
songIDBox rect f = do
  let (checkArea, inputArea) = chopLeft 200 rect
  check <- liftIO $ withRect checkArea \cx cy cw ch ->
    [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Specific Song ID") } |]
  input <- liftIO $ withRect inputArea \ix iy iw ih ->
    [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
  let controlInput = do
        b <- checkValue check
        if b
          then [C.block| void { $(Fl_Input* input)->activate(); } |]
          else [C.block| void { $(Fl_Input* input)->deactivate(); } |]
  liftIO controlInput
  liftIO $ setCallback check controlInput
  tell do
    b <- checkValue check
    s <- peekInputValue input
    return $ Endo $ f if b && s /= ""
      then case readMaybe $ T.unpack s of
        Nothing -> SongIDSymbol s
        Just i  -> SongIDInt i
      else if ?preferences.prefRBNumberID
        then SongIDAutoInt
        else SongIDAutoSymbol

numberBox
  :: (?preferences :: Preferences)
  => Rectangle
  -> T.Text
  -> (Maybe Int -> tgt -> tgt)
  -> BuildYamlControl tgt ()
numberBox rect lbl f = do
  let (checkArea, inputArea) = chopLeft 250 rect
  check <- liftIO do
    c <- withRect checkArea \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    withUTF8 lbl \clbl ->
      [C.block| void { $(Fl_Check_Button* c)->copy_label($(char* clbl)); } |]
    return c
  input <- liftIO $ withRect inputArea \ix iy iw ih ->
    [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
  let controlInput = do
        b <- checkValue check
        if b
          then [C.block| void { $(Fl_Input* input)->activate(); } |]
          else [C.block| void { $(Fl_Input* input)->deactivate(); } |]
  liftIO controlInput
  liftIO $ setCallback check controlInput
  tell do
    b <- checkValue check
    s <- peekInputValue input
    return $ Endo $ f if b && s /= ""
      then readMaybe $ T.unpack s
      else Nothing

partSelectors
  :: (Default tgt)
  => Rectangle
  -> Project
  -> [(T.Text, tgt -> PartName, PartName -> tgt -> tgt, Part FilePath -> Bool)]
  -> BuildYamlControl tgt (Bool -> IO ())
partSelectors rect proj slots = let
  rects = splitHorizN (fromIntegral $ length slots) rect
  fparts = do
    (fpart, part) <- HM.toList proj.projectSongYaml.parts.getParts
    guard $ part /= emptyPart
    return (fpart, T.toTitle $ F.getPartName fpart, part)
  instSelector ((lbl, getter, setter, partFilter), r) = do
    let r' = trimClock 20 5 5 5 r
        fparts' = [ t | t@(_, _, part) <- fparts, partFilter part ]
    choice <- liftIO do
      choice <- withRect r' \cx cy cw ch ->
        [C.exp| Fl_Choice* { new Fl_Choice($(int cx), $(int cy), $(int cw), $(int ch)) } |]
      withUTF8 lbl \clbl ->
        [C.block| void { $(Fl_Choice* choice)->copy_label($(char* clbl)); } |]
      [C.block| void { $(Fl_Choice* choice)->align(FL_ALIGN_TOP); } |]
      [C.block| void { $(Fl_Choice* choice)->add("(none)"); } |]
      forM_ fparts' \(_, opt, _) ->
        withUTF8 opt \copt ->
          [C.block| void { $(Fl_Choice* choice)->add($(char* copt)); } |]
      let initVal = case findIndex (\(fpart, _, _) -> fpart == getter def) fparts' of
            Nothing -> 0 :: CInt -- (none)
            Just i  -> fromIntegral i + 1
      [C.block| void { $(Fl_Choice* choice)->value($(int initVal)); } |]
      return choice
    tell do
      i <- [C.exp| int { $(Fl_Choice* choice)->value() } |]
      return $ Endo $ setter case drop (fromIntegral i - 1) fparts' of
        (fpart, _, _) : _ | i /= 0 -> fpart
        _                          -> PartName "undefined"
    return \b -> if b
      then [C.block| void { $(Fl_Choice* choice)->activate(); } |]
      else [C.block| void { $(Fl_Choice* choice)->deactivate(); } |]
  in do
    controls <- mapM instSelector $ zip slots rects
    return \b -> mapM_ ($ b) controls

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

peekInputValue :: Ptr Fl_Input -> IO T.Text
peekInputValue input = do
  cstr <- [C.exp| const char* { $(Fl_Input* input)->value() } |]
  T.pack <$> peekUTF8 cstr

-- | Helper for save file chooser pattern used throughout song pages
saveFileChooser :: T.Text -> T.Text -> Maybe T.Text -> (FilePath -> IO ()) -> IO ()
saveFileChooser title preset mdir onPick = withNativeFileChooser
  [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |]
  \picker -> do
    withUTF8 title \ctitle ->
      [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* ctitle)); } |]
    withUTF8 preset \cpreset ->
      [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file($(char* cpreset)); } |]
    forM_ mdir \dir -> withUTF8 dir \cdir ->
      [C.block| void { $(Fl_Native_File_Chooser* picker)->directory($(char* cdir)); } |]
    result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
    when (result == 0) do
      cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
      f <- peekUTF8 cstr
      when (not $ null f) $ onPick f

-- | Helper for browsing a single file
browseFileChooser :: T.Text -> (FilePath -> IO ()) -> IO ()
browseFileChooser title onPick = withNativeFileChooser
  [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |]
  \picker -> do
    withUTF8 title \ctitle ->
      [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* ctitle)); } |]
    result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
    when (result == 0) do
      cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
      f <- peekUTF8 cstr
      when (not $ null f) $ onPick f
