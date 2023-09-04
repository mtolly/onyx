{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI.Core where

import           Control.Concurrent                        (ThreadId)
import qualified Control.Exception                         as Exc
import           Control.Monad.Extra                       (forM, forM_, guard,
                                                            unless, void)
import           Control.Monad.IO.Class                    (MonadIO (..),
                                                            liftIO)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Maybe                 (MaybeT (..))
import           Control.Monad.Trans.Reader                (ReaderT, ask,
                                                            runReaderT)
import           Control.Monad.Trans.Resource              (ResourceT,
                                                            resourceForkIO,
                                                            runResourceT)
import           Control.Monad.Trans.Writer                (WriterT, tell)
import qualified Data.ByteString                           as B
import           Data.Default.Class                        (Default, def)
import           Data.Foldable                             (toList)
import qualified Data.HashMap.Strict                       as HM
import           Data.IORef                                (newIORef, readIORef,
                                                            writeIORef)
import           Data.List.Extra                           (findIndex)
import qualified Data.List.NonEmpty                        as NE
import           Data.Maybe                                (catMaybes,
                                                            listToMaybe)
import           Data.Monoid                               (Endo (..))
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as TE
import qualified Graphics.UI.FLTK.LowLevel.FL              as FLTK
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
import           Graphics.UI.FLTK.LowLevel.FLTKHS          (Height (..),
                                                            Position (..),
                                                            Rectangle (..),
                                                            Size (..),
                                                            Width (..), X (..),
                                                            Y (..))
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
import           Numeric                                   (readHex)
import           Onyx.Audio                                (Audio (..))
import           Onyx.Build                                (NameRule (..),
                                                            hashRB3,
                                                            targetTitle,
                                                            validFileName,
                                                            validFileNamePiece)
import           Onyx.Build.Common                         (getTargetMetadata)
import           Onyx.FretsOnFire                          (stripTags)
import           Onyx.GUI.Util                             (askFolder)
import           Onyx.Harmonix.Ark.GH2                     (GH2InstallLocation (..))
import           Onyx.Import
import           Onyx.MIDI.Track.File                      (FlexPartName (..))
import qualified Onyx.MIDI.Track.File                      as F
import qualified Onyx.PlayStation.PKG                      as PKG
import           Onyx.Preferences                          (Preferences (..),
                                                            readPreferences,
                                                            savePreferences)
import           Onyx.Project
import           Onyx.QuickConvert
import           Onyx.StackTrace
import qualified Onyx.Xbox.STFS                            as STFS
import qualified System.Directory                          as Dir
import           System.FilePath                           (dropTrailingPathSeparator,
                                                            takeDirectory,
                                                            takeFileName, (</>))
import           Text.Read                                 (readMaybe)

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
  lift $ lift $ lift $ resourceForkIO $ do
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

_resizeWindow :: FL.Ref FL.Window -> Size -> IO ()
_resizeWindow window size = do
  x <- FL.getX window
  y <- FL.getY window
  FL.resize window $ Rectangle (Position x y) size

data RB3Create
  = RB3CON FilePath
  | RB3PKG FilePath
  | RB3Magma FilePath

data RB2Create
  = RB2CON FilePath
  | RB2PKG FilePath

data PSCreate
  = PSDir FilePath
  | PSZip FilePath

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

data GHWORCreate
  = GHWORLIVE FilePath
  | GHWORPKG FilePath

templateApplyInput :: Project -> Maybe (Target FilePath) -> T.Text -> T.Text
templateApplyInput proj mtgt txt = T.pack $ validFileName NameRulePC $ dropTrailingPathSeparator $ T.unpack $ foldr ($) txt
  [ T.intercalate (T.pack $ takeDirectory $ projectTemplate proj) . T.splitOn "%input_dir%"
  , T.intercalate (validFileNamePiece NameRulePC $ T.pack $ takeFileName $ projectTemplate proj) . T.splitOn "%input_base%"
  , T.intercalate (validFileNamePiece NameRulePC title) . T.splitOn "%title%"
  , T.intercalate (validFileNamePiece NameRulePC $ getArtist metadata) . T.splitOn "%artist%"
  , T.intercalate (validFileNamePiece NameRulePC $ getAlbum metadata) . T.splitOn "%album%"
  , T.intercalate (validFileNamePiece NameRulePC $ getAuthor metadata) . T.splitOn "%author%"
  , T.intercalate (validFileNamePiece NameRulePC songID) . T.splitOn "%song_id%"
  ] where
    metadata = case mtgt of
      Nothing  -> (projectSongYaml proj).metadata
      Just tgt -> getTargetMetadata (projectSongYaml proj) tgt
    title = case mtgt of
      Nothing  -> getTitle metadata
      Just tgt -> targetTitle (projectSongYaml proj) tgt
    songID = case mtgt of
      Just (RB3 rb3) -> case rb3.songID of
        SongIDInt    n -> T.pack $ show n
        SongIDSymbol s -> s
        _              -> T.pack $ show $ hashRB3 (projectSongYaml proj) rb3
      _              -> ""

makeTemplateRunner :: (Event -> IO ()) -> T.Text -> T.Text -> (T.Text -> IO ()) -> IO ()
makeTemplateRunner sink buttonText defTemplate useTemplate = do
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect -> do
    input <- makeTemplateRunner' sink rect buttonText defTemplate useTemplate
    FL.setTooltip input $ T.unlines
      [ "Template for where to create new files."
      , "  %input_dir% - folder containing the input"
      , "  %input_base% - input filename by itself, extension removed"
      , "  %modifiers% - added distinguishing features e.g. speed modifier"
      , "  %title% - title from song's metadata (including modifiers)"
      , "  %artist% - artist from song's metadata"
      , "  %album% - album from song's metadata"
      , "  %author% - author from song's metadata"
      , "  %song_id% - unique song ID"
      ]

makeTemplateRunner' :: (Event -> IO ()) -> Rectangle -> T.Text -> T.Text -> (T.Text -> IO ()) -> IO (FL.Ref FL.Input)
makeTemplateRunner' sink rect buttonText defTemplate useTemplate = do
  let (buttonRect, notButton) = chopLeft 250 rect
      (_, rectA) = chopLeft 80 notButton
      (inputRect, rectB) = chopRight 100 rectA
      (_, rectC) = chopRight 90 rectB
      (browseRect, _) = chopLeft 40 rectC
      (_, rectD) = chopRight 50 rectC
      (_, resetRect) = chopRight 40 rectD
  button <- FL.buttonNew buttonRect $ Just buttonText
  taskColor >>= FL.setColor button
  input <- FL.inputNew
    inputRect
    (Just "Template")
    (Just FL.FlNormalInput) -- required for labels to work
  FL.setLabelsize input $ FL.FontSize 13
  FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
  FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
  void $ FL.setValue input defTemplate
  FL.setCallback button $ \_ -> FL.getValue input >>= useTemplate
  browseButton <- FL.buttonNew browseRect $ Just "@fileopen"
  FL.setCallback browseButton $ \_ -> sink $ EventIO $ askFolder Nothing $ \dir -> do
    val <- FL.getValue input
    void $ FL.setValue input $ T.pack $ dir </> takeFileName (T.unpack val)
  resetButton <- FL.buttonNew resetRect $ Just "@undo"
  FL.setCallback resetButton $ \_ -> sink $ EventIO $ do
    void $ FL.setValue input defTemplate
  return input

chopRight :: Int -> Rectangle -> (Rectangle, Rectangle)
chopRight n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width $ w - n) (Height h))
  , Rectangle (Position (X $ x + w - n) (Y y)) (Size (Width n) (Height h))
  )

chopLeft :: Int -> Rectangle -> (Rectangle, Rectangle)
chopLeft n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width n) (Height h))
  , Rectangle (Position (X $ x + n) (Y y)) (Size (Width $ w - n) (Height h))
  )

chopTop :: Int -> Rectangle -> (Rectangle, Rectangle)
chopTop n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width w) (Height n))
  , Rectangle (Position (X x) (Y $ y + n)) (Size (Width w) (Height $ h - n))
  )

chopBottom :: Int -> Rectangle -> (Rectangle, Rectangle)
chopBottom n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width w) (Height $ h - n))
  , Rectangle (Position (X x) (Y $ y + h - n)) (Size (Width w) (Height n))
  )

splitHorizN :: Int -> Rectangle -> [Rectangle]
splitHorizN 1 rect = [rect]
splitHorizN n rect@(Rectangle _ (Size (Width w) _)) = let
  (rectFirst, rectRest) = chopLeft (quot w n) rect
  in rectFirst : splitHorizN (n - 1) rectRest

splitVertN :: Int -> Rectangle -> [Rectangle]
splitVertN 1 rect = [rect]
splitVertN n rect@(Rectangle _ (Size _ (Height h))) = let
  (rectFirst, rectRest) = chopTop (quot h n) rect
  in rectFirst : splitVertN (n - 1) rectRest

trimClock :: Int -> Int -> Int -> Int -> Rectangle -> Rectangle
trimClock t r b l rect = let
  (_, rect1) = chopTop    t rect
  (rect2, _) = chopRight  r rect1
  (rect3, _) = chopBottom b rect2
  (_, rect4) = chopLeft   l rect3
  in rect4

homeTabColor, functionTabColor, taskColor, globalLogColor, miscColor, loadSongColor, batchProcessColor, behindTabsColor, quickConvertColor, defaultColor :: IO FLE.Color
homeTabColor      = FLE.rgbColorWithRgb (209,177,224)
functionTabColor  = FLE.rgbColorWithRgb (224,210,177)
taskColor         = FLE.rgbColorWithRgb (179,221,187)
globalLogColor    = FLE.rgbColorWithRgb (114,74,124)
miscColor         = FLE.rgbColorWithRgb (177,173,244)
loadSongColor     = FLE.rgbColorWithRgb (186,229,181)
batchProcessColor = FLE.rgbColorWithRgb (237,173,193)
behindTabsColor   = FLE.rgbColorWithRgb (94,94,94) -- TODO report incorrect Char binding type for rgbColorWithGrayscale
quickConvertColor = FLE.rgbColorWithRgb (232,213,150)
defaultColor      = FLE.rgbColorWithRgb (192,192,192)

setTabColor :: FL.Ref FL.Group -> FLE.Color -> IO ()
setTabColor tab color = do
  FL.setColor tab color
  FL.setSelectionColor tab color

padded
  :: (MonadIO m)
  => Int -> Int -> Int -> Int
  -> Size
  -> (Rectangle -> m a)
  -> m a
padded t r b l size@(Size (Width w) (Height h)) fn = do
  group <- liftIO $ FL.groupNew
    (Rectangle
      (Position (X 0) (Y 0))
      (Size (Width (w + l + r)) (Height (h + t + b)))
    )
    Nothing
  let rect = Rectangle (Position (X l) (Y t)) size
  box <- liftIO $ FL.boxNew rect Nothing
  liftIO $ FL.setResizable group $ Just box
  x <- fn rect
  liftIO $ FL.end group
  return x

-- | Ported from Erco's Fl_Center class
centerFixed :: Rectangle -> IO a -> IO a
centerFixed rect makeChildren = do
  let centerX :: FL.Ref FL.Group -> FL.Ref FL.WidgetBase -> IO X
      centerX this wid = do
        X x <- FL.getX this
        Width w <- FL.getW this
        Width widW <- FL.getW wid
        return $ X $ x + quot w 2 - quot widW 2
      centerY :: FL.Ref FL.Group -> FL.Ref FL.WidgetBase -> IO Y
      centerY this wid = do
        Y y <- FL.getY this
        Height h <- FL.getH this
        Height widH <- FL.getH wid
        return $ Y $ y + quot h 2 - quot widH 2
      myResize :: FL.Ref FL.Group -> Rectangle -> IO ()
      myResize this rect' = do
        FL.resizeWidgetBase (FL.safeCast this) rect'
        children <- FL.children this
        forM_ [0 .. children - 1] $ \i -> do
          mcw <- FL.getChild this $ FL.AtIndex i
          forM_ mcw $ \cw -> do
            newX <- centerX this cw
            newY <- centerY this cw
            newW <- FL.getW cw
            newH <- FL.getH cw
            FL.resize cw $ Rectangle
              (Position newX newY)
              (Size newW newH)
  group <- FL.groupCustom rect Nothing Nothing FL.defaultCustomWidgetFuncs
    { FL.resizeCustom = Just myResize
    }
  x <- makeChildren
  FL.end group
  myResize group rect
  return x

makeTab :: Rectangle -> T.Text -> (Rectangle -> FL.Ref FL.Group -> IO a) -> IO a
makeTab rect name fn = do
  let tabHeight = 25
      (_, innerRect) = chopTop tabHeight rect
  group <- FL.groupNew innerRect (Just name)
  res <- fn innerRect group
  FL.end group
  return res

horizRadio' :: Rectangle -> Maybe (IO ()) -> [(T.Text, a, Bool)] -> IO (IO (Maybe a))
horizRadio' _    _  []   = error "horizRadio: empty option list"
horizRadio' rect cb opts = do
  let rects = splitHorizN (length opts) rect
  btns <- forM (zip opts rects) $ \((label, _, b), rectButton) -> do
    btn <- FL.roundButtonNew rectButton $ Just label
    void $ FL.setValue btn b
    return btn
  forM_ btns $ \opt -> FL.setCallback opt $ \_ -> do
    forM_ btns $ \opt' -> FL.setValue opt' $ opt == opt'
    sequence_ cb
  return $ do
    bools <- mapM FL.getValue btns
    return $ fmap (\(_, (_, x, _)) -> x) $ listToMaybe $ filter fst $ zip bools opts

horizRadio :: Rectangle -> [(T.Text, a, Bool)] -> IO (IO (Maybe a))
horizRadio rect = horizRadio' rect Nothing

speedPercent' :: Bool -> Rectangle -> IO (IO Double, FL.Ref FL.Counter)
speedPercent' isConverter rect = do
  speed <- FL.counterNew rect $ if isConverter
    then Just "Speed (%)"
    else Nothing
  FL.setLabelsize speed $ FL.FontSize 13
  FL.setLabeltype speed FLE.NormalLabelType FL.ResolveImageLabelDoNothing
  FL.setAlign speed $ FLE.Alignments [FLE.AlignTypeLeft]
  FL.setStep speed 1
  FL.setLstep speed 5
  FL.setMinimum speed 1
  void $ FL.setValue speed 100
  FL.setTooltip speed $ if isConverter
    then "Change the speed of the chart and its audio (without changing pitch). If importing from a CON, a non-100% value requires unencrypted audio."
    else "Speed (%)"
  return ((/ 100) <$> FL.getValue speed, speed)

speedPercent :: Bool -> Rectangle -> IO (IO Double)
speedPercent isConverter rect = fst <$> speedPercent' isConverter rect

makePresetDropdown :: Rectangle -> [(T.Text, a)] -> IO (IO a)
makePresetDropdown rect opts = do
  let (initialLabel, initialOpt) = head opts
  menu <- FL.menuButtonNew rect $ Just initialLabel
  ref <- newIORef initialOpt
  forM_ opts $ \(label, opt) -> do
    -- need to escape in menu items (but not button label) to not make submenus
    let label' = T.replace "/" "\\/" label
    FL.add menu label' Nothing
      ((Just $ \_ -> do
        writeIORef ref opt
        FL.setLabel menu label
      ) :: Maybe (FL.Ref FL.MenuItem -> IO ()))
      (FL.MenuItemFlags [])
  return $ readIORef ref

makeDropdown :: Rectangle -> NE.NonEmpty (T.Text, a, Bool) -> IO (IO a)
makeDropdown rect opts = do
  let (initialLabel, initialOpt, _) = case NE.filter (\(_, _, b) -> b) opts of
        opt : _ -> opt
        []      -> NE.head opts
  menu <- FL.menuButtonNew rect $ Just initialLabel
  ref <- newIORef initialOpt
  forM_ opts $ \(label, opt, _) -> do
    -- need to escape in menu items (but not button label) to not make submenus
    let label' = T.replace "/" "\\/" label
    FL.add menu label' Nothing
      ((Just $ \_ -> do
        writeIORef ref opt
        FL.setLabel menu label
      ) :: Maybe (FL.Ref FL.MenuItem -> IO ()))
      (FL.MenuItemFlags [])
  return $ readIORef ref

trimXbox
  :: Preferences
  -> FilePath
  -> FilePath
trimXbox prefs f = if prefTrimXbox prefs
  then validFileName NameRuleXbox f
  else f

loadingPhraseCHtoGH2
  :: Project
  -> Maybe T.Text
loadingPhraseCHtoGH2 proj = listToMaybe $ catMaybes $ do
  PS ps <- toList (projectSongYaml proj).targets
  return $ stripTags <$> ps.loadingPhrase

gh2DeluxeSelector
  :: (Event -> IO ())
  -> Rectangle
  -> IO (IO (Maybe Bool)) -- Bool is True if 2x drums
gh2DeluxeSelector sink rect = do
  let (checkArea, kicksArea) = chopRight 150 rect
  check <- FL.checkButtonNew checkArea $ Just "GH2 Deluxe features"
  kicks <- FL.choiceNew kicksArea Nothing
  mapM_ (FL.addName kicks) ["1x drums", "2x drums"]
  void $ FL.setValue kicks $ FL.MenuItemByIndex $ FL.AtIndex 0
  let updateKicks = do
        enabled <- FL.getValue check
        (if enabled then FL.activate else FL.deactivate) kicks
  FL.setCallback check $ \_ -> sink $ EventIO updateKicks
  updateKicks
  void $ FL.setValue check False
  return $ do
    enableDeluxe <- FL.getValue check
    if enableDeluxe
      then do
        FL.AtIndex i <- FL.getValue kicks
        return $ Just $ i == 1
      else return Nothing

warnCombineXboxGH2 :: (Event -> IO ()) -> IO () -> IO ()
warnCombineXboxGH2 sink go = sink $ EventOnyx $ do
  prefs <- readPreferences
  stackIO $ unless (prefWarnedXboxGH2 prefs) $ do
    void $ FL.flChoice (T.unlines
      [ "Note! When loading songs into Guitar Hero II for Xbox 360, you *must* combine them into packs (go to \"Quick convert\")."
      , "Loading more than 16 packages will fail to load some songs, and will corrupt your save!"
      ]) "OK" Nothing Nothing
    savePreferences prefs { prefWarnedXboxGH2 = True }
  stackIO go

warnXboxGHWoR :: (Event -> IO ()) -> Onyx () -> IO ()
warnXboxGHWoR sink go = sink $ EventOnyx $ do
  prefs <- readPreferences
  stackIO $ unless (prefWarnedXboxWoR prefs) $ do
    void $ FL.flChoice (T.unlines
      [ "Please use the \"WoR Song Cache\" tab in Other Tools after conversion,"
      , "to produce the extra file needed to load your songs."
      , ""
      , "IMPORTANT: There may be an issue with loading too many custom songs"
      , "into Guitar Hero: Warriors of Rock, that could corrupt your save."
      , "Please back up any save data you care about before loading custom songs!"
      ]) "OK" Nothing Nothing
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
getSTFSSpec f = Exc.try (STFS.withSTFSPackage f $ return . STFS.stfsMetadata) >>= \case
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
    , pkgContentID = PKG.pkgContentID $ PKG.pkgHeader pkg
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

isWoRCachable :: FilePath -> IO (Maybe (FilePath, T.Text))
isWoRCachable f = if "_TEXT.PAK.PS3.EDAT" `T.isSuffixOf` T.toUpper (T.pack f)
  then return $ Just (f, "") -- could read content ID from EDAT if we wanted
  else getPackageSpec f >>= \case
    Just (Left stfs) -> return $ Just (f, T.concat $ take 1 $ STFS.md_DisplayName $ stfsMeta stfs)
    Just (Right pkg) -> return $ Just (f, TE.decodeLatin1 $ pkgContentID pkg)
    Nothing          -> return Nothing

isGH3Cachable :: FilePath -> IO (Maybe (FilePath, T.Text))
isGH3Cachable f = getPackageSpec f >>= \case
  Just (Left stfs) -> return $ Just (f, T.concat $ take 1 $ STFS.md_DisplayName $ stfsMeta stfs)
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
  -> IO (FL.Ref FL.Group)
fileLoadWindow rect sink single plural modifyFiles startFiles step display = mdo
  group <- FL.groupNew rect Nothing
  let (labelRect, belowLabel) = chopTop 40 rect
      (termRect, buttonsRect) = chopBottom 50 belowLabel
      labelRect' = trimClock 10 10 5 10 labelRect
      termRect' = trimClock 5 10 5 10 termRect
      buttonsRect' = trimClock 5 10 10 10 buttonsRect
      [btnRectA, btnRectB] = splitHorizN 2 buttonsRect'
      (btnRectA', _) = chopRight 5 btnRectA
      (_, btnRectB') = chopLeft 5 btnRectB
  label <- FL.boxNew labelRect' Nothing
  tree <- FL.treeCustom termRect' Nothing Nothing $ Just FL.defaultCustomWidgetFuncs
    { FL.handleCustom = Just $ \_ evt -> case evt of
      FLE.Keydown -> do
        cmd <- FLTK.eventCommand
        let upToSong :: FL.Ref FL.TreeItem -> MaybeT IO (FL.Ref FL.TreeItem)
            upToSong i = lift (FL.getDepth i) >>= \case
              0 -> MaybeT $ return Nothing
              1 -> return i
              _ -> MaybeT (FL.getParent i) >>= upToSong
            getSongFocus :: MaybeT IO (FL.Ref FL.TreeItem, Int, FL.Ref FL.TreeItem)
            getSongFocus = do
              item <- MaybeT $ FL.getItemFocus tree
              songItem <- upToSong item
              root <- MaybeT $ FL.root tree
              FL.AtIndex ix <- MaybeT $ FL.findChild root
                $ FL.TreeItemPointerLocator $ FL.TreeItemPointer songItem
              return (root, ix, songItem)
        FLTK.eventKey >>= \case
          FL.SpecialKeyType FLE.Kb_Up | cmd -> do
            void $ runMaybeT $ do
              (root, ix, _songItem) <- getSongFocus
              unless (ix == 0) $ lift $ swapFiles root (ix - 1) ix
            return $ Right ()
          FL.SpecialKeyType FLE.Kb_Down | cmd -> do
            void $ runMaybeT $ do
              (root, ix, _songItem) <- getSongFocus
              len <- liftIO $ FL.children root
              unless (ix == len - 1) $ lift $ swapFiles root ix (ix + 1)
            return $ Right ()
          FL.SpecialKeyType FLE.Kb_Delete -> do
            void $ runMaybeT $ do
              (_root, ix, songItem) <- getSongFocus
              lift $ removeFile songItem ix
            return $ Right ()
          _ -> FL.handleTreeBase (FL.safeCast tree) evt
      _ -> FL.handleTreeBase (FL.safeCast tree) evt
    }
  FL.end tree
  FL.setResizable group $ Just tree
  let updateLabel fs = FL.setLabel label $ T.pack $ case length fs of
        1 -> "1 file loaded."
        n -> show n ++ " files loaded."
      clearFiles = modifyFiles $ \_ -> do
        FL.clear tree
        _ <- FL.add tree ""
        FL.rootLabel tree plural
        updateLabel []
        FLTK.redraw
        return []
      swapFiles :: FL.Ref FL.TreeItem -> Int -> Int -> IO ()
      swapFiles root index1 index2 = modifyFiles $ \fs -> do
        let fs' = zipWith (\i _ -> fs !! if i == index1 then index2 else if i == index2 then index1 else i) [0..] fs
        void $ FL.swapChildren root (FL.AtIndex index1) (FL.AtIndex index2)
        updateLabel fs'
        FLTK.redraw
        return fs'
      removeFile songItem index = modifyFiles $ \fs -> do
        let fs' = take index fs ++ drop (index + 1) fs
        void $ FL.remove tree songItem
        updateLabel fs'
        FLTK.redraw
        return fs'
      addFiles [] = return ()
      addFiles gs = modifyFiles $ \fs -> do
        Just root <- FL.root tree
        forM_ gs $ \imp -> do
          let (entry, sublines) = display imp
          Just item <- FL.addAt tree entry root
          forM_ sublines $ \subline -> void $ FL.addAt tree subline item
          FL.close item
        let fs' = fs ++ gs
        updateLabel fs'
        FLTK.redraw
        return fs'
      searchSongs [] = return ()
      searchSongs (loc : locs) = do
        (children, imps) <- step loc
        stackIO $ sink $ EventIO $ addFiles imps
        searchSongs $ locs ++ children
      forkSearch = void . forkOnyx . searchSongs
  clearFiles
  sink $ EventOnyx $ forkSearch startFiles
  void $ FL.boxCustom termRect' Nothing Nothing $ Just FL.defaultCustomWidgetFuncs
    { FL.handleCustom = Just
      $ dragAndDrop (sink . EventOnyx . forkSearch)
      . (\_ _ -> return $ Left FL.UnknownEvent)
    }
  btnA <- FL.buttonNew btnRectA' $ Just $ "Add " <> single
  FL.setCallback btnA $ \_ -> sink $ EventIO $ do
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseMultiFile
    FL.setTitle picker $ "Load " <> single
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> do
        n <- FL.getCount picker
        fs <- forM [0 .. n - 1] $ FL.getFilenameAt picker . FL.AtIndex
        sink $ EventOnyx $ forkSearch $ map T.unpack $ catMaybes fs
      _ -> return ()
  btnB <- FL.buttonNew btnRectB' $ Just $ "Clear " <> plural
  FL.setCallback btnB $ \_ -> sink $ EventIO clearFiles
  FL.end group
  return group

dragAndDrop
  :: ([String] -> IO ())
  -> (FLE.Event -> IO (Either a ()))
  -> FLE.Event
  -> IO (Either a ())
dragAndDrop f fallback = \case
  FLE.DndEnter -> return $ Right () -- we want dnd events
  FLE.DndDrag -> return $ Right ()
  FLE.DndLeave -> return $ Right ()
  FLE.DndRelease -> return $ Right () -- give us the text!
  FLE.Paste -> do
    str <- FLTK.eventText
    -- linux uses URL encoding starting with file://
    -- and then using percent encoding for non-ascii chars
    let removeProtocol s = case T.stripPrefix "file://" s of
          Nothing -> s
          Just s' -> TE.decodeUtf8 $ percentDecode s'
        percentDecode :: T.Text -> B.ByteString
        percentDecode s = case T.uncons s of
          Just ('%', t) -> case readHex $ T.unpack $ T.take 2 t of
            [(n, "")] -> B.cons n $ percentDecode $ T.drop 2 t
            _         -> B.cons 37 {- % -} $ percentDecode t -- shouldn't happen but whatever
          Just (h  , t) -> TE.encodeUtf8 (T.singleton h) <> percentDecode t
          Nothing       -> ""
    () <- f $ map (T.unpack . removeProtocol) $ T.lines str
    -- lines is because multiple files are separated by \n
    return $ Right ()
  e -> fallback e

type BuildYamlControl a = WriterT (IO (Endo a)) IO

customTitleSuffix
  :: (Event -> IO ())
  -> Rectangle
  -> IO T.Text
  -> (Maybe T.Text -> tgt -> tgt)
  -> BuildYamlControl tgt (IO ())
customTitleSuffix sink rect getSuffix setSuffix = do
  let (checkArea, inputArea) = chopLeft 200 rect
  check <- liftIO $ FL.checkButtonNew checkArea (Just "Custom Title Suffix")
  input <- liftIO $ FL.inputNew
    inputArea
    Nothing
    (Just FL.FlNormalInput) -- required for labels to work
  let controlInput = do
        b <- FL.getValue check
        if b
          then FL.activate input
          else do
            sfx <- getSuffix
            void $ FL.setValue input $ T.strip sfx
            FL.deactivate input
  liftIO $ sink $ EventIO controlInput -- have to delay; can't call now due to dependency loop!
  liftIO $ FL.setCallback check $ \_ -> controlInput
  tell $ do
    b <- FL.getValue check
    s <- FL.getValue input
    return $ Endo $ setSuffix $ guard b >> Just (T.strip s)
  return controlInput

songIDBox
  :: (?preferences :: Preferences)
  => Rectangle
  -> (RBSongID -> tgt -> tgt)
  -> BuildYamlControl tgt ()
songIDBox rect f = do
  let (checkArea, inputArea) = chopLeft 200 rect
  check <- liftIO $ FL.checkButtonNew checkArea (Just "Specific Song ID")
  input <- liftIO $ FL.inputNew
    inputArea
    Nothing
    (Just FL.FlNormalInput) -- required for labels to work
  let controlInput = do
        b <- FL.getValue check
        (if b then FL.activate else FL.deactivate) input
  liftIO controlInput
  liftIO $ FL.setCallback check $ \_ -> controlInput
  tell $ do
    b <- FL.getValue check
    s <- FL.getValue input
    return $ Endo $ f $ if b && s /= ""
      then case readMaybe $ T.unpack s of
        Nothing -> SongIDSymbol s
        Just i  -> SongIDInt i
      else if prefRBNumberID ?preferences
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
  check <- liftIO $ FL.checkButtonNew checkArea (Just lbl)
  input <- liftIO $ FL.inputNew
    inputArea
    Nothing
    (Just FL.FlNormalInput) -- required for labels to work
  let controlInput = do
        b <- FL.getValue check
        (if b then FL.activate else FL.deactivate) input
  liftIO controlInput
  liftIO $ FL.setCallback check $ \_ -> controlInput
  tell $ do
    b <- FL.getValue check
    s <- FL.getValue input
    return $ Endo $ f $ if b && s /= ""
      then readMaybe $ T.unpack s
      else Nothing

partSelectors
  :: (Default tgt)
  => Rectangle
  -> Project
  -> [(T.Text, tgt -> FlexPartName, FlexPartName -> tgt -> tgt, Part FilePath -> Bool)]
  -> BuildYamlControl tgt (Bool -> IO ())
partSelectors rect proj slots = let
  rects = splitHorizN (length slots) rect
  fparts = do
    (fpart, part) <- HM.toList (projectSongYaml proj).parts.getParts
    guard $ part /= emptyPart
    return (fpart, T.toTitle $ F.getPartName fpart, part)
  instSelector ((lbl, getter, setter, partFilter), r) = do
    let r' = trimClock 20 5 5 5 r
        fparts' = [ t | t@(_, _, part) <- fparts, partFilter part ]
    choice <- liftIO $ do
      choice <- FL.choiceNew r' $ Just lbl
      FL.setAlign choice $ FLE.Alignments [FLE.AlignTypeTop]
      FL.addName choice "(none)"
      forM_ fparts' $ \(_, opt, _) -> FL.addName choice opt
      void $ FL.setValue choice $ FL.MenuItemByIndex $ FL.AtIndex $
        case findIndex (\(fpart, _, _) -> fpart == getter def) fparts' of
          Nothing -> 0 -- (none)
          Just i  -> i + 1
      return choice
    tell $ do
      FL.AtIndex i <- FL.getValue choice
      return $ Endo $ setter $ case drop (i - 1) fparts' of
        (fpart, _, _) : _ | i /= 0 -> fpart
        _                          -> FlexExtra "undefined"
    return $ \b -> if b then FL.activate choice else FL.deactivate choice
  in do
    controls <- mapM instSelector $ zip slots rects
    return $ \b -> mapM_ ($ b) controls

mixDownStems :: SongYaml f -> SongYaml f
mixDownStems song = song
  { plans = flip fmap song.plans $ \case
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
    MoggPlan info -> MoggPlan info
      { parts = Parts HM.empty
      }
  }
