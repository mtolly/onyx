{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI.Song where

import           Onyx.GUI.Core
import           Onyx.GUI.Util                             (askFolder)

import           Control.Monad                             (forM_, void)
import           Control.Monad.IO.Class                    (MonadIO (..),
                                                            liftIO)
import           Control.Monad.Trans.Writer                (execWriterT, tell)
import           Data.Char                                 (toLower)
import           Data.Default.Class                        (def)
import           Data.Function                             ((&))
import           Data.IORef                                (modifyIORef,
                                                            newIORef, readIORef)
import           Data.Maybe                                (fromMaybe, isJust)
import           Data.Monoid                               (Endo (..))
import qualified Data.Text                                 as T
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
import           Graphics.UI.FLTK.LowLevel.FLTKHS          (Height (..),
                                                            Rectangle (..),
                                                            Size (..),
                                                            Width (..))
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
import           Onyx.Build                                (targetTitle)
import           Onyx.Harmonix.Ark.GH2                     (GH2InstallLocation (..))
import           Onyx.Import
import           Onyx.Mode                                 (anyDrums,
                                                            anyFiveFret)
import           Onyx.Preferences                          (Preferences (..),
                                                            getDefaultPS3Dir,
                                                            readPreferences)
import           Onyx.Project
import           Onyx.StackTrace
import           Optics                                    ((%), (.~))
import           System.FilePath                           (takeDirectory,
                                                            takeExtension,
                                                            (<.>))

songPageRB3
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (Bool -> TargetRB3 FilePath -> RB3Create -> IO ())
  -> IO ()
songPageRB3 sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \rb3 ->
        (rb3 :: TargetRB3 FilePath) & #common % #speed .~ Just speed
      return counter
    box2x <- fullWidth 35 \rect' -> do
      box <- liftIO $ FL.checkButtonNew rect' (Just "2x Bass Pedal drums")
      tell $ FL.getValue box >>= \b -> return $ Endo \rb3 ->
        (rb3 :: TargetRB3 FilePath) & #is2xBassPedal .~ b
      return box
    fullWidth 35 \rect' -> songIDBox rect' $ \sid rb3 ->
      rb3 & #songID .~ sid
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Guitar", (.guitar), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #guitar .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.proGuitar) -- technically pro guitar can make five fret. but good to be safe
        )
      , ( "Bass"  , (.bass  ), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #bass   .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.proGuitar)
        )
      , ( "Keys"  , (.keys  ), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #keys   .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.proKeys) -- similarly pro keys can make five fret
        )
      , ( "Drums" , (.drums ), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #drums  .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( "Vocal" , (.vocal ), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #vocal  .~ v)
        , (\p -> isJust p.vocal)
        )
      ]
    fullWidth 35 \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget ?preferences >>= \rb3 -> return $ targetTitle
          proj.projectSongYaml
          (RB3 $ rb3 & #common % #override .~ overrideEmptyTitle)
        )
        (\msfx rb3 -> (rb3 :: TargetRB3 FilePath)
          & #common % #label_ .~ msfx
        )
      liftIO $ FL.setCallback counterSpeed \_ -> controlInput
      liftIO $ FL.setCallback box2x \_ -> controlInput
  let makeTarget :: Preferences -> IO (TargetRB3 FilePath)
      makeTarget newPreferences = do
        modifier <- targetModifier
        return $ modifier def
          { magma = newPreferences.prefMagma
          , legalTempos = newPreferences.prefLegalTempos
          , common = def
            { label2x = newPreferences.prefLabel2x
            }
          , encoding = newPreferences.prefRB3Encoding
          }
      makeFinalTarget = readPreferences >>= stackIO . makeTarget
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create Xbox 360 CON file"
    FL.setCallback btn1 \_ -> sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      let qcPossible = fromMaybe 1 tgt.common.speed == 1
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save RB3 CON file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_rb3con" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> sink $ EventOnyx do
            newPreferences <- readPreferences
            stackIO $ build qcPossible tgt $ RB3CON $ trimXbox newPreferences f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create PS3 PKG file"
    FL.setCallback btn2 \_ -> sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      let qcPossible = fromMaybe 1 tgt.common.speed == 1
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save RB3 PKG file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> ".pkg" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build qcPossible tgt $ RB3PKG if map toLower (takeExtension f) == ".pkg"
            then f
            else f <.> "pkg"
        _ -> return ()
    color <- taskColor
    FL.setColor btn1 color
    FL.setColor btn2 color
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create Magma project"
    FL.setCallback btn1 \_ -> sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save Magma v2 project"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_project" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build False tgt $ RB3Magma f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create PS3/RPCS3 folder"
    FL.setCallback btn2 \_ -> sink $ EventOnyx do
      tgt <- makeFinalTarget
      defDir <- getDefaultPS3Dir
      let qcPossible = fromMaybe 1 tgt.common.speed == 1
      stackIO $ askFolder defDir \dout -> do
        build qcPossible tgt $ RB3LoosePS3 dout
    color <- taskColor
    FL.setColor btn1 color
    FL.setColor btn2 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPageRB2
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetRB2 FilePath -> RB2Create -> IO ())
  -> IO ()
songPageRB2 sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \rb2 ->
        (rb2 :: TargetRB2 FilePath) & #common % #speed .~ Just speed
      return counter
    box2x <- fullWidth 35 \rect' -> do
      box <- liftIO $ FL.checkButtonNew rect' (Just "2x Bass Pedal drums")
      tell $ FL.getValue box >>= \b -> return $ Endo \rb2 ->
        (rb2 :: TargetRB2 FilePath) & #is2xBassPedal .~ b
      return box
    fullWidth 35 \rect' -> songIDBox rect' $ \sid rb2 ->
      rb2 & #songID .~ sid
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Guitar", (.guitar), (\v rb2 -> (rb2 :: TargetRB2 FilePath) & #guitar .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( "Bass"  , (.bass  ), (\v rb2 -> (rb2 :: TargetRB2 FilePath) & #bass   .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( "Drums" , (.drums ), (\v rb2 -> (rb2 :: TargetRB2 FilePath) & #drums  .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( "Vocal" , (.vocal ), (\v rb2 -> (rb2 :: TargetRB2 FilePath) & #vocal  .~ v)
        , (\p -> isJust p.vocal)
        )
      ]
    fullWidth 35 \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget ?preferences >>= \rb2 -> return $ targetTitle
          proj.projectSongYaml
          (RB2 $ rb2 & #common % #override .~ overrideEmptyTitle)
        )
        (\msfx rb2 -> (rb2 :: TargetRB2 FilePath)
          & #common % #label_ .~ msfx
        )
      liftIO $ FL.setCallback counterSpeed \_ -> controlInput
      liftIO $ FL.setCallback box2x \_ -> controlInput
  let makeTarget :: Preferences -> IO (TargetRB2 FilePath)
      makeTarget newPreferences = do
        modifier <- targetModifier
        return $ modifier $ def
          & #magma .~ newPreferences.prefMagma
          & #legalTempos .~ newPreferences.prefLegalTempos
          & #common .~ do
            def & #label2x .~ newPreferences.prefLabel2x
          & #ps3Encrypt .~ newPreferences.prefPS3Encrypt
      makeFinalTarget = readPreferences >>= stackIO . makeTarget

  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create Xbox 360 CON file"
    FL.setCallback btn1 \_ -> sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save RB2 CON file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_rb2con" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> sink $ EventOnyx do
            newPreferences <- readPreferences
            stackIO $ build tgt $ RB2CON $ trimXbox newPreferences f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create PS3 PKG file"
    FL.setCallback btn2 \_ -> sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save RB2 PKG file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> ".pkg" -- TODO add modifiers
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ RB2PKG if map toLower (takeExtension f) == ".pkg"
            then f
            else f <.> "pkg"
        _ -> return ()
    color <- taskColor
    FL.setColor btn1 color
    FL.setColor btn2 color
  fullWidth 35 \rect' -> do
    btn1 <- FL.buttonNew rect' $ Just "Create PS3/RPCS3 folder"
    FL.setCallback btn1 \_ -> sink $ EventOnyx do
      tgt <- makeFinalTarget
      defDir <- getDefaultPS3Dir
      stackIO $ askFolder defDir \dout -> do
        build tgt $ RB2LoosePS3 dout
    color <- taskColor
    FL.setColor btn1 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPageGHWOR
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetGH5 FilePath -> GHWORCreate -> IO ())
  -> IO ()
songPageGHWOR sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \gh5 ->
        (gh5 :: TargetGH5 FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 35 \rect' -> do
      getProTo4 <- liftIO $ horizRadio rect'
        [ ("Pro Drums to 5 lane", False, not ?preferences.prefGH4Lane)
        , ("Pro Drums to 4 lane", True, ?preferences.prefGH4Lane)
        ]
      tell do
        b <- getProTo4
        return $ Endo \gh5 -> (gh5 :: TargetGH5 FilePath) & #proTo4 .~ fromMaybe False b
    fullWidth 35 \rect' -> numberBox rect' "Custom Song ID (dlc)" $ \sid gh5 ->
      gh5 & #songID .~ sid
    fullWidth 35 \rect' -> numberBox rect' "Custom Package ID (cdl)" $ \sid gh5 ->
      gh5 & #cdl .~ sid
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Guitar", (.guitar), (\v gh5 -> (gh5 :: TargetGH5 FilePath) & #guitar .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( "Bass"  , (.bass  ), (\v gh5 -> (gh5 :: TargetGH5 FilePath) & #bass   .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( "Drums" , (.drums ), (\v gh5 -> (gh5 :: TargetGH5 FilePath) & #drums  .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( "Vocal" , (.vocal ), (\v gh5 -> (gh5 :: TargetGH5 FilePath) & #vocal  .~ v)
        , (\p -> isJust p.vocal)
        )
      ]
    fullWidth 35 \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \gh5 -> return $ targetTitle
          proj.projectSongYaml
          (GH5 $ gh5 & #common % #override .~ overrideEmptyTitle)
        )
        (\msfx gh5 -> (gh5 :: TargetGH5 FilePath)
          & #common % #label_ .~ msfx
        )
      liftIO $ FL.setCallback counterSpeed \_ -> controlInput
  let initTarget = def
      makeTarget = fmap ($ initTarget) targetModifier
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create Xbox 360 LIVE file"
    FL.setCallback btn1 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save GH:WoR LIVE file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_ghwor" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> sink $ EventOnyx do
            newPreferences <- readPreferences
            stackIO $ warnXboxGHWoR sink $ stackIO $ build tgt $ GHWORLIVE $ trimXbox newPreferences f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create PS3 PKG file"
    FL.setCallback btn2 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save GH:WoR PKG file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> ".pkg" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> warnXboxGHWoR sink $ stackIO $ build tgt $ GHWORPKG f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn1 color
    FL.setColor btn2 color
  fullWidth 35 \rect' -> do
    btn1 <- FL.buttonNew rect' $ Just "Create PS3/RPCS3 folder"
    FL.setCallback btn1 \_ -> sink $ EventOnyx do
      tgt <- stackIO makeTarget
      defDir <- getDefaultPS3Dir
      stackIO $ askFolder defDir \dout -> do
        build tgt $ GHWORLoosePS3 dout
    color <- taskColor
    FL.setColor btn1 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPageRR
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetRR FilePath -> RRCreate -> IO ())
  -> IO ()
songPageRR sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \rr ->
        (rr :: TargetRR FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 35 \rect' -> do
      getProTo4 <- liftIO $ horizRadio rect'
        [ ("Pro Drums to 6 pad", False, False)
        , ("Pro Drums to 4 pad", True, True)
        ]
      tell do
        b <- getProTo4
        return $ Endo \rr -> (rr :: TargetRR FilePath) & #proTo4 .~ fromMaybe False b
    fullWidth 35 \rect' -> numberBox rect' "Custom Song ID" $ \sid rr ->
      rr & #songID .~ sid
    fullWidth 35 \rect' -> numberBox rect' "Custom Leaderboard ID" $ \sid rr ->
      rr & #leaderboardID .~ sid
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Guitar"           , (.guitar), (\v rr -> (rr :: TargetRR FilePath) & #guitar .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( "Bass"             , (.bass  ), (\v rr -> (rr :: TargetRR FilePath) & #bass   .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( "Drums"            , (.drums ), (\v rr -> (rr :: TargetRR FilePath) & #drums  .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( "Vocal (animation)", (.vocal ), (\v rr -> (rr :: TargetRR FilePath) & #vocal  .~ v)
        , (\p -> isJust p.vocal)
        )
      ]
    fullWidth 35 \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \rr -> return $ targetTitle
          proj.projectSongYaml
          (RR $ rr & #common % #override .~ overrideEmptyTitle)
        )
        (\msfx rr -> (rr :: TargetRR FilePath)
          & #common % #label_ .~ msfx
        )
      liftIO $ FL.setCallback counterSpeed \_ -> controlInput
  let initTarget = def
      makeTarget = fmap ($ initTarget) targetModifier
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create Xbox 360 LIVE file"
    FL.setCallback btn1 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save RR LIVE file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_rr" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> sink $ EventOnyx do
            newPreferences <- readPreferences
            stackIO $ build tgt $ RRLIVE $ trimXbox newPreferences f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create RPCS3 PKG file"
    FL.setCallback btn2 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save RR PKG file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> ".pkg" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ RRPKG f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn1 color
    FL.setColor btn2 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPagePS
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetPS FilePath -> PSCreate -> IO ())
  -> IO ()
songPagePS sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \ps ->
        (ps :: TargetPS FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Guitar"     , (.guitar    ), (\v ps -> (ps :: TargetPS FilePath) & #guitar     .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.ghl || isJust p.proGuitar) -- pro guitar redundant since it can make five fret
        )
      , ( "Bass"       , (.bass      ), (\v ps -> (ps :: TargetPS FilePath) & #bass       .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.ghl || isJust p.proGuitar)
        )
      , ( "Keys"       , (.keys      ), (\v ps -> (ps :: TargetPS FilePath) & #keys       .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.proKeys) -- pro keys redundant since it can make five fret
        )
      , ( "Drums"      , (.drums     ), (\v ps -> (ps :: TargetPS FilePath) & #drums      .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( "Vocal"      , (.vocal     ), (\v ps -> (ps :: TargetPS FilePath) & #vocal      .~ v)
        , (\p -> isJust p.vocal)
        )
      , ( "Rhythm"     , (.rhythm    ), (\v ps -> (ps :: TargetPS FilePath) & #rhythm     .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( "Guitar Coop", (.guitarCoop), (\v ps -> (ps :: TargetPS FilePath) & #guitarCoop .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      ]
    fullWidth 35 \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \ps -> return $ targetTitle
          proj.projectSongYaml
          (PS $ ps & #common % #override .~ overrideEmptyTitle)
        )
        (\msfx ps -> (ps :: TargetPS FilePath)
          & #common % #label_ .~ msfx
        )
      liftIO $ FL.setCallback counterSpeed \_ -> controlInput
  let makeTarget = fmap ($ def) targetModifier
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 5 0 5 -> r2, trimClock 0 0 0 5 -> r3] = splitHorizN 3 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create CH/PS song folder"
    FL.setCallback btn1 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save CH/PS song folder"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_chps" -- TODO add modifiers
      forM_ ?preferences.prefDirCH $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ PSDir f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create CH .sng file"
    FL.setCallback btn2 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save CH/PS .sng file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> ".sng" -- TODO add modifiers
      forM_ ?preferences.prefDirCH $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ PSSng f
        _ -> return ()
    btn3 <- FL.buttonNew r3 $ Just "Create CH/PS zip file"
    FL.setCallback btn3 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save CH/PS zip file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_chps.zip" -- TODO add modifiers
      forM_ ?preferences.prefDirCH $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ PSZip f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn1 color
    FL.setColor btn2 color
    FL.setColor btn3 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPageGH3
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetGH3 FilePath -> GH3Create -> IO ())
  -> IO ()
songPageGH3 sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \gh3 ->
        (gh3 :: TargetGH3 FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Guitar", (.guitar), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #guitar .~ v)
        , isJust . anyFiveFret
        )
      ]
    fullWidth 50 \rect' -> do
      let [bassArea, coopArea, rhythmArea] = splitHorizN 3 rect'
      void $ partSelectors bassArea proj
        [ ( "Bass"  , (.bass  ), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #bass .~ v)
          , isJust . anyFiveFret
          )
        ]
      controlRhythm <- partSelectors rhythmArea proj
        [ ( "Rhythm", (.rhythm), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #rhythm .~ v)
          , isJust . anyFiveFret
          )
        ]
      coopPart <- liftIO $ newIORef GH2Bass
      liftIO do
        coopButton <- FL.buttonNew coopArea Nothing
        let updateCoopButton = do
              coop <- readIORef coopPart
              FL.setLabel coopButton case coop of
                GH2Bass   -> "Coop: Bass"
                GH2Rhythm -> "Coop: Rhythm"
              controlRhythm $ coop == GH2Rhythm
        updateCoopButton
        FL.setCallback coopButton \_ -> sink $ EventIO do
          modifyIORef coopPart \case
            GH2Bass   -> GH2Rhythm
            GH2Rhythm -> GH2Bass
          updateCoopButton
      tell $ readIORef coopPart >>= \coop -> return $ Endo \gh3 -> (gh3 :: TargetGH3 FilePath) & #coop .~ coop
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Keys" , (.keys ), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #keys  .~ v)
        , isJust . anyFiveFret
        )
      , ( "Drums", (.drums), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #drums .~ v)
        , isJust . (.drums)
        )
      , ( "Vocal", (.vocal), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #vocal .~ v)
        , isJust . (.vocal)
        )
      ]
    fullWidth 35 \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \gh3 -> return $ targetTitle
          proj.projectSongYaml
          (GH3 $ gh3 & #common % #override .~ overrideEmptyTitle)
        )
        (\msfx gh3 -> (gh3 :: TargetGH3 FilePath)
          & #common % #label_ .~ msfx
        )
      liftIO $ FL.setCallback counterSpeed \_ -> controlInput
  let initTarget = def :: TargetGH3 FilePath
      makeTarget = fmap ($ initTarget) targetModifier
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create Xbox 360 LIVE file"
    FL.setCallback btn1 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save GH3 LIVE file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_gh3live" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> sink $ EventOnyx do
            newPreferences <- readPreferences
            stackIO $ build tgt $ GH3LIVE $ trimXbox newPreferences f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create PS3 PKG file"
    FL.setCallback btn2 \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save GH3 PKG file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> ".pkg" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ GH3PKG f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn1 color
    FL.setColor btn2 color
  fullWidth 35 \rect' -> do
    btn1 <- FL.buttonNew rect' $ Just "Create PS3/RPCS3 folder"
    FL.setCallback btn1 \_ -> sink $ EventOnyx do
      tgt <- stackIO makeTarget
      defDir <- getDefaultPS3Dir
      stackIO $ askFolder defDir \dout -> do
        build tgt $ GH3LoosePS3 dout
    color <- taskColor
    FL.setColor btn1 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPageGH1
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetGH1 FilePath -> GH1Create -> IO ())
  -> IO ()
songPageGH1 sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \gh1 ->
        (gh1 :: TargetGH1 FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Guitar", (.guitar), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #guitar .~ v)
        , isJust . anyFiveFret
        )
      ]
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Bass" , (.bass ), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #bass  .~ v)
        , isJust . anyFiveFret
        )
      , ( "Keys" , (.keys ), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #keys  .~ v)
        , isJust . anyFiveFret
        )
      , ( "Drums", (.drums), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #drums .~ v)
        , isJust . (.drums)
        )
      , ( "Vocal", (.vocal), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #vocal .~ v)
        , isJust . (.vocal)
        )
      ]
    fullWidth 35 \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \gh1 -> return $ targetTitle
          proj.projectSongYaml
          (GH1 $ gh1 & #common % #override .~ overrideEmptyTitle)
        )
        (\msfx gh1 -> (gh1 :: TargetGH1 FilePath)
          & #common % #label_ .~ msfx
        )
      liftIO $ FL.setCallback counterSpeed \_ -> controlInput
  let initTarget :: Preferences -> TargetGH1 FilePath
      initTarget prefs = (def :: TargetGH1 FilePath) & #offset .~ prefs.prefGH2Offset
      makeTarget = fmap ($ initTarget ?preferences) targetModifier
      -- make sure we reload offset before compiling
      makeTargetUpdatePrefs go = targetModifier >>= \modifier -> sink $ EventOnyx do
        newPrefs <- readPreferences
        stackIO $ go $ modifier $ initTarget newPrefs
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Add to PS2 ARK as Bonus Song"
    FL.setCallback btn1 \_ -> makeTargetUpdatePrefs \tgt -> do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Select .HDR file"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> let
            gen = takeDirectory f
            in build tgt $ GH1ARK gen
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create PS2 DIY folder"
    FL.setCallback btn2 \_ -> makeTargetUpdatePrefs \tgt -> do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Create DIY folder"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_gh1"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ GH1DIYPS2 f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn1 color
    FL.setColor btn2 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPageGH2
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetGH2 FilePath -> GH2Create -> IO ())
  -> IO ()
songPageGH2 sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \gh2 ->
        (gh2 :: TargetGH2 FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Guitar", (.guitar), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #guitar .~ v)
        , isJust . anyFiveFret
        )
      ]
    fullWidth 50 \rect' -> do
      let [bassArea, coopArea, rhythmArea] = splitHorizN 3 rect'
      void $ partSelectors bassArea proj
        [ ( "Bass"  , (.bass  ), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #bass .~ v)
          , isJust . anyFiveFret
          )
        ]
      controlRhythm <- partSelectors rhythmArea proj
        [ ( "Rhythm", (.rhythm), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #rhythm .~ v)
          , isJust . anyFiveFret
          )
        ]
      coopPart <- liftIO $ newIORef GH2Bass
      liftIO do
        coopButton <- FL.buttonNew coopArea Nothing
        let updateCoopButton = do
              coop <- readIORef coopPart
              FL.setLabel coopButton case coop of
                GH2Bass   -> "Coop: Bass"
                GH2Rhythm -> "Coop: Rhythm"
              controlRhythm $ coop == GH2Rhythm
        updateCoopButton
        FL.setCallback coopButton \_ -> sink $ EventIO do
          modifyIORef coopPart \case
            GH2Bass   -> GH2Rhythm
            GH2Rhythm -> GH2Bass
          updateCoopButton
      tell $ readIORef coopPart >>= \coop -> return $ Endo \gh2 -> (gh2 :: TargetGH2 FilePath) & #coop .~ coop
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( "Keys" , (.keys ), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #keys  .~ v)
        , isJust . anyFiveFret
        )
      , ( "Drums", (.drums), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #drums .~ v)
        , isJust . anyDrums
        )
      , ( "Vocal", (.vocal), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #vocal .~ v)
        , isJust . (.vocal)
        )
      ]
    fullWidth 35 \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \gh2 -> return $ targetTitle
          proj.projectSongYaml
          (GH2 $ gh2 & #common % #override .~ overrideEmptyTitle)
        )
        (\msfx gh2 -> (gh2 :: TargetGH2 FilePath)
          & #common % #label_ .~ msfx
        )
      liftIO $ FL.setCallback counterSpeed \_ -> controlInput
    fullWidth 35 \rect' -> do
      let [rectA, rectB] = splitHorizN 2 rect'
      boxA <- liftIO $ FL.checkButtonNew rectA (Just "Make practice mode audio for PS2")
      tell $ FL.getValue boxA >>= \b -> return $ Endo \gh2 ->
        gh2 { practiceAudio = b }
      getDeluxe <- liftIO $ gh2DeluxeSelector sink rectB
      tell $ getDeluxe >>= \opt -> return $ Endo \gh2 -> case opt of
        Nothing   -> gh2 { gh2Deluxe = False, is2xBassPedal = False }
        Just is2x -> gh2 { gh2Deluxe = True , is2xBassPedal = is2x  }
  let initTarget :: Preferences -> TargetGH2 FilePath
      initTarget prefs = (def :: TargetGH2 FilePath) & #offset .~ prefs.prefGH2Offset
      makeTarget = fmap ($ initTarget ?preferences) targetModifier
      -- make sure we reload offset before compiling
      makeTargetUpdatePrefs go = targetModifier >>= \modifier -> sink $ EventOnyx do
        newPrefs <- readPreferences
        stackIO $ go $ modifier $ initTarget newPrefs
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Add to PS2 ARK as Bonus Song"
    FL.setCallback btn1 \_ -> makeTargetUpdatePrefs \tgt -> do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Select .HDR file"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> let
            gen = takeDirectory f
            in build tgt $ GH2ARK gen GH2AddBonus
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create PS2 DIY folder"
    FL.setCallback btn2 \_ -> makeTargetUpdatePrefs \tgt -> do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Create DIY folder"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_gh2"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ GH2DIYPS2 f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn1 color
    FL.setColor btn2 color
  fullWidth 35 \rect' -> do
    btn2 <- FL.buttonNew rect' $ Just "Create Xbox 360 LIVE file"
    FL.setCallback btn2 \_ -> makeTargetUpdatePrefs \tgt -> do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save GH2 LIVE file"
      FL.setPresetFile picker $ T.pack $ proj.projectTemplate <> "_gh2live" -- TODO add modifiers
      forM_ ?preferences.prefDirRB $ FL.setDirectory picker . T.pack
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> sink $ EventOnyx do
            newPreferences <- readPreferences
            stackIO $ warnCombineXboxGH2 sink $ build tgt $ GH2LIVE $ trimXbox newPreferences f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn2 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

overrideEmptyTitle :: Metadata f
overrideEmptyTitle = def & #title .~ Just ""
