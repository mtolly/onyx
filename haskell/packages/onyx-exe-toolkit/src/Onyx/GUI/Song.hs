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
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI.Song where

import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO (..), liftIO)
import           Control.Monad.Trans.Writer (execWriterT, tell)
import           Data.Char                  (toLower)
import           Data.Default.Class         (def)
import           Data.Function              ((&))
import           Data.IORef                 (modifyIORef, newIORef, readIORef)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Monoid                (Endo (..))
import qualified Data.Text                  as T
import           Foreign                    (Ptr)
import qualified Language.C.Inline.Cpp      as C
import           Onyx.Build                 (targetTitle)
import           Onyx.GUI.Binding
import           Onyx.GUI.Core
import           Onyx.GUI.Util              (askFolder, checkValue, setCallback,
                                             setLabelTooltip, withUTF8)
import           Onyx.Harmonix.Ark.GH2      (GH2InstallLocation (..))
import           Onyx.Import
import           Onyx.Localization          (L, l10n, l10nFull)
import           Onyx.Mode                  (anyDrums, anyFiveFret)
import           Onyx.Preferences           (Preferences (..), getDefaultPS3Dir,
                                             readPreferences)
import           Onyx.Project
import           Onyx.StackTrace
import           Optics                     ((%), (.~))
import           System.FilePath            (takeDirectory, takeExtension,
                                             (<.>))

C.context flContext

C.include "FL/Fl.H"
C.include "FL/Fl_Group.H"
C.include "FL/Fl_Button.H"
C.include "FL/Fl_Check_Button.H"
C.include "FL/Fl_Pack.H"

songPageRB3
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> Project
  -> (Bool -> TargetRB3 FilePath -> RB3Create -> IO ())
  -> IO ()
songPageRB3 sink rect tab proj build = mdo
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' -> do
      (getSpeed, counter) <- liftIO $ centerFixed rect' $
        speedPercent' True (trimClock 0 250 0 250 rect')
      tell $ getSpeed >>= \speed -> return $ Endo \rb3 ->
        (rb3 :: TargetRB3 FilePath) & #common % #speed .~ Just speed
      return counter
    box2x <- fullWidth 35 \rect' -> do
      box <- liftIO $ withRect rect' \cx cy cw ch ->
        [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
      liftIO $ liftIO $ setLabelTooltip (l10nFull "song-toggle-2x-bass-pedal" []) box
      tell do
        b <- checkValue box
        return $ Endo \rb3 -> (rb3 :: TargetRB3 FilePath) & #is2xBassPedal .~ b
      return box
    fullWidth 35 \rect' -> songIDBox rect' \sid rb3 ->
      rb3 & #songID .~ sid
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-guitar", (.guitar), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #guitar .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.proGuitar) -- technically pro guitar can make five fret. but good to be safe
        )
      , ( l10n "part-bass"  , (.bass  ), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #bass   .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.proGuitar)
        )
      , ( l10n "part-keys"  , (.keys  ), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #keys   .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.proKeys) -- similarly pro keys can make five fret
        )
      , ( l10n "part-drums" , (.drums ), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #drums  .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( l10n "part-vocal" , (.vocal ), (\v rb3 -> (rb3 :: TargetRB3 FilePath) & #vocal  .~ v)
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
      liftIO $ setCallback counterSpeed controlInput
      liftIO $ setCallback box2x controlInput
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
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-360-con") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 $ sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      let qcPossible = fromMaybe 1 tgt.common.speed == 1
      saveFileChooser (l10n "file-picker-save-rb3-con")
        (T.pack $ proj.projectTemplate <> "_rb3con")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> sink $ EventOnyx do
          newPreferences <- readPreferences
          stackIO $ build qcPossible tgt $ RB3CON $ trimXbox newPreferences f
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps3-pkg") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 $ sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      let qcPossible = fromMaybe 1 tgt.common.speed == 1
      saveFileChooser (l10n "file-picker-save-rb3-pkg")
        (T.pack $ proj.projectTemplate <> ".pkg")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> build qcPossible tgt $ RB3PKG if map toLower (takeExtension f) == ".pkg"
          then f
          else f <.> "pkg"
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-magma") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 $ sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      saveFileChooser (l10n "file-picker-save-magma-v2")
        (T.pack $ proj.projectTemplate <> "_project")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> build False tgt $ RB3Magma f
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps3-folder") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 $ sink $ EventOnyx do
      tgt <- makeFinalTarget
      defDir <- getDefaultPS3Dir
      let qcPossible = fromMaybe 1 tgt.common.speed == 1
      stackIO $ askFolder defDir \dout -> do
        build qcPossible tgt $ RB3LoosePS3 dout
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

songPageRB2
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> Project
  -> (TargetRB2 FilePath -> RB2Create -> IO ())
  -> IO ()
songPageRB2 sink rect tab proj build = mdo
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' -> do
      (getSpeed, counter) <- liftIO $ centerFixed rect' $
        speedPercent' True (trimClock 0 250 0 250 rect')
      tell $ getSpeed >>= \speed -> return $ Endo \rb2 ->
        (rb2 :: TargetRB2 FilePath) & #common % #speed .~ Just speed
      return counter
    box2x <- fullWidth 35 \rect' -> do
      box <- liftIO $ withRect rect' \cx cy cw ch ->
        [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
      liftIO $ setLabelTooltip (l10nFull "song-toggle-2x-bass-pedal" []) box
      tell do
        b <- checkValue box
        return $ Endo \rb2 -> (rb2 :: TargetRB2 FilePath) & #is2xBassPedal .~ b
      return box
    fullWidth 35 \rect' -> songIDBox rect' \sid rb2 ->
      rb2 & #songID .~ sid
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-guitar", (.guitar), (\v rb2 -> (rb2 :: TargetRB2 FilePath) & #guitar .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( l10n "part-bass"  , (.bass  ), (\v rb2 -> (rb2 :: TargetRB2 FilePath) & #bass   .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( l10n "part-drums" , (.drums ), (\v rb2 -> (rb2 :: TargetRB2 FilePath) & #drums  .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( l10n "part-vocal" , (.vocal ), (\v rb2 -> (rb2 :: TargetRB2 FilePath) & #vocal  .~ v)
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
      liftIO $ setCallback counterSpeed controlInput
      liftIO $ setCallback box2x controlInput
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
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-360-con") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 $ sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      saveFileChooser (l10n "file-picker-save-rb2-con")
        (T.pack $ proj.projectTemplate <> "_rb2con")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> sink $ EventOnyx do
          newPreferences <- readPreferences
          stackIO $ build tgt $ RB2CON $ trimXbox newPreferences f
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps3-pkg") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 $ sink $ EventOnyx $ makeFinalTarget >>= \tgt -> stackIO do
      saveFileChooser (l10n "file-picker-save-rb2-pkg")
        (T.pack $ proj.projectTemplate <> ".pkg")
        Nothing
        \f -> build tgt $ RB2PKG if map toLower (takeExtension f) == ".pkg"
          then f
          else f <.> "pkg"
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  fullWidth 35 \rect' -> do
    btn1 <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps3-folder") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 $ sink $ EventOnyx do
      tgt <- makeFinalTarget
      defDir <- getDefaultPS3Dir
      stackIO $ askFolder defDir \dout -> do
        build tgt $ RB2LoosePS3 dout
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

songPageGHWOR
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> Project
  -> (TargetGH5 FilePath -> GHWORCreate -> IO ())
  -> IO ()
songPageGHWOR sink rect tab proj build = mdo
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' -> do
      (getSpeed, counter) <- liftIO $ centerFixed rect' $
        speedPercent' True (trimClock 0 250 0 250 rect')
      tell $ getSpeed >>= \speed -> return $ Endo \gh5 ->
        (gh5 :: TargetGH5 FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 35 \rect' -> do
      getProTo4 <- liftIO $ horizRadio rect'
        [ (l10n "batch-tab-gh-drums-5lane", False, not ?preferences.prefGH4Lane)
        , (l10n "batch-tab-gh-drums-4lane", True, ?preferences.prefGH4Lane)
        ]
      tell do
        b <- getProTo4
        return $ Endo \gh5 -> (gh5 :: TargetGH5 FilePath) & #proTo4 .~ fromMaybe False b
    fullWidth 35 \rect' -> numberBox rect' (l10n "numberbox-gh5-song-id") \sid gh5 ->
      gh5 & #songID .~ sid
    fullWidth 35 \rect' -> numberBox rect' (l10n "numberbox-gh5-cdl") \sid gh5 ->
      gh5 & #cdl .~ sid
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-guitar", (.guitar), (\v gh5 -> (gh5 :: TargetGH5 FilePath) & #guitar .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( l10n "part-bass"  , (.bass  ), (\v gh5 -> (gh5 :: TargetGH5 FilePath) & #bass   .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( l10n "part-drums" , (.drums ), (\v gh5 -> (gh5 :: TargetGH5 FilePath) & #drums  .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( l10n "part-vocal" , (.vocal ), (\v gh5 -> (gh5 :: TargetGH5 FilePath) & #vocal  .~ v)
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
      liftIO $ setCallback counterSpeed controlInput
  let initTarget = def
      makeTarget = fmap ($ initTarget) targetModifier
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-360-live") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-ghwor-live")
        (T.pack $ proj.projectTemplate <> "_ghwor") -- TODO add modifiers
        (T.pack <$> ?preferences.prefDirRB)
        \f -> sink $ EventOnyx do
          newPreferences <- readPreferences
          stackIO $ warnXboxGHWoR sink $ stackIO $ build tgt $ GHWORLIVE $ trimXbox newPreferences f
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps3-pkg") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-ghwor-pkg")
        (T.pack $ proj.projectTemplate <> ".pkg") -- TODO add modifiers
        (T.pack <$> ?preferences.prefDirRB)
        \f -> warnXboxGHWoR sink $ stackIO $ build tgt $ GHWORPKG f
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  fullWidth 35 \rect' -> do
    btn1 <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps3-folder") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 $ sink $ EventOnyx do
      tgt <- stackIO makeTarget
      defDir <- getDefaultPS3Dir
      stackIO $ askFolder defDir \dout -> do
        build tgt $ GHWORLoosePS3 dout
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

songPageRR
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> Project
  -> (TargetRR FilePath -> RRCreate -> IO ())
  -> IO ()
songPageRR sink rect tab proj build = mdo
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' -> do
      (getSpeed, counter) <- liftIO $ centerFixed rect' $
        speedPercent' True (trimClock 0 250 0 250 rect')
      tell $ getSpeed >>= \speed -> return $ Endo \rr ->
        (rr :: TargetRR FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 35 \rect' -> do
      getProTo4 <- liftIO $ horizRadio rect'
        [ (l10n "batch-tab-rr-drums-6pad", False, False)
        , (l10n "batch-tab-rr-drums-4pad", True, True)
        ]
      tell do
        b <- getProTo4
        return $ Endo \rr -> (rr :: TargetRR FilePath) & #proTo4 .~ fromMaybe False b
    fullWidth 35 \rect' -> numberBox rect' (l10n "numberbox-rr-song-id") \sid rr ->
      rr & #songID .~ sid
    fullWidth 35 \rect' -> numberBox rect' (l10n "numberbox-rr-leaderboard-id") \sid rr ->
      rr & #leaderboardID .~ sid
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-guitar"         , (.guitar), (\v rr -> (rr :: TargetRR FilePath) & #guitar .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( l10n "part-bass"           , (.bass  ), (\v rr -> (rr :: TargetRR FilePath) & #bass   .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( l10n "part-drums"          , (.drums ), (\v rr -> (rr :: TargetRR FilePath) & #drums  .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( l10n "part-vocal-animation", (.vocal ), (\v rr -> (rr :: TargetRR FilePath) & #vocal  .~ v)
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
      liftIO $ setCallback counterSpeed controlInput
  let initTarget = def
      makeTarget = fmap ($ initTarget) targetModifier
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-360-live") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-rr-live")
        (T.pack $ proj.projectTemplate <> "_rr")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> sink $ EventOnyx do
          newPreferences <- readPreferences
          stackIO $ build tgt $ RRLIVE $ trimXbox newPreferences f
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-rpcs3-pkg") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-rr-pkg")
        (T.pack $ proj.projectTemplate <> ".pkg")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> build tgt $ RRPKG f
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

songPagePS
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> Project
  -> (TargetPS FilePath -> PSCreate -> IO ())
  -> IO ()
songPagePS sink rect tab proj build = mdo
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' -> do
      (getSpeed, counter) <- liftIO $ centerFixed rect' $
        speedPercent' True (trimClock 0 250 0 250 rect')
      tell $ getSpeed >>= \speed -> return $ Endo \ps ->
        (ps :: TargetPS FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-guitar"     , (.guitar    ), (\v ps -> (ps :: TargetPS FilePath) & #guitar     .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.ghl || isJust p.proGuitar)
        )
      , ( l10n "part-bass"       , (.bass      ), (\v ps -> (ps :: TargetPS FilePath) & #bass       .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.ghl || isJust p.proGuitar)
        )
      , ( l10n "part-keys"       , (.keys      ), (\v ps -> (ps :: TargetPS FilePath) & #keys       .~ v)
        , (\p -> isJust (anyFiveFret p) || isJust p.proKeys)
        )
      , ( l10n "part-drums"      , (.drums     ), (\v ps -> (ps :: TargetPS FilePath) & #drums      .~ v)
        , (\p -> isJust $ anyDrums p)
        )
      , ( l10n "part-vocal"      , (.vocal     ), (\v ps -> (ps :: TargetPS FilePath) & #vocal      .~ v)
        , (\p -> isJust p.vocal)
        )
      , ( l10n "part-rhythm"     , (.rhythm    ), (\v ps -> (ps :: TargetPS FilePath) & #rhythm     .~ v)
        , (\p -> isJust $ anyFiveFret p)
        )
      , ( l10n "part-guitar-coop", (.guitarCoop), (\v ps -> (ps :: TargetPS FilePath) & #guitarCoop .~ v)
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
      liftIO $ setCallback counterSpeed controlInput
  let makeTarget = fmap ($ def) targetModifier
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 5 0 5 -> r2, trimClock 0 0 0 5 -> r3] = splitHorizN 3 rect'
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ch-folder") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-ch-folder")
        (T.pack $ proj.projectTemplate <> "_chps") -- TODO add modifiers
        (T.pack <$> ?preferences.prefDirCH)
        \f -> build tgt $ PSDir f
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ch-sng") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-ch-sng")
        (T.pack $ proj.projectTemplate <> ".sng") -- TODO add modifiers
        (T.pack <$> ?preferences.prefDirCH)
        \f -> build tgt $ PSSng f
    btn3 <- withRect r3 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ch-zip") \clbl ->
      [C.block| void { $(Fl_Button* btn3)->copy_label($(char* clbl)); } |]
    setCallback btn3 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-ch-zip")
        (T.pack $ proj.projectTemplate <> "_chps.zip") -- TODO add modifiers
        (T.pack <$> ?preferences.prefDirCH)
        \f -> build tgt $ PSZip f
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn3)->color($(Fl_Color taskColor)); } |]
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

songPageGH3
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> Project
  -> (TargetGH3 FilePath -> GH3Create -> IO ())
  -> IO ()
songPageGH3 sink rect tab proj build = mdo
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \gh3 ->
        (gh3 :: TargetGH3 FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-guitar", (.guitar), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #guitar .~ v)
        , isJust . anyFiveFret
        )
      ]
    fullWidth 50 \rect' -> do
      let [bassArea, coopArea, rhythmArea] = splitHorizN 3 rect'
      void $ partSelectors bassArea proj
        [ ( l10n "part-bass"  , (.bass  ), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #bass .~ v)
          , isJust . anyFiveFret
          )
        ]
      controlRhythm <- partSelectors rhythmArea proj
        [ ( l10n "part-rhythm", (.rhythm), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #rhythm .~ v)
          , isJust . anyFiveFret
          )
        ]
      coopPart <- liftIO $ newIORef GH2Bass
      liftIO do
        coopButton <- withRect coopArea \bx by bw bh ->
          [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
        let updateCoopButton = do
              coop <- readIORef coopPart
              let lbl :: T.Text
                  lbl = case coop of
                    GH2Bass   -> l10n "coop-bass"
                    GH2Rhythm -> l10n "coop-rhythm"
              withUTF8 lbl \clbl ->
                [C.block| void { $(Fl_Button* coopButton)->copy_label($(char* clbl)); } |]
              controlRhythm $ coop == GH2Rhythm
        updateCoopButton
        setCallback coopButton $ sink $ EventIO do
          modifyIORef coopPart \case
            GH2Bass   -> GH2Rhythm
            GH2Rhythm -> GH2Bass
          updateCoopButton
      tell $ readIORef coopPart >>= \coop -> return $ Endo \gh3 -> (gh3 :: TargetGH3 FilePath) & #coop .~ coop
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-keys" , (.keys ), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #keys  .~ v)
        , isJust . anyFiveFret
        )
      , ( l10n "part-drums", (.drums), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #drums .~ v)
        , isJust . (.drums)
        )
      , ( l10n "part-vocal", (.vocal), (\v gh3 -> (gh3 :: TargetGH3 FilePath) & #vocal .~ v)
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
      liftIO $ setCallback counterSpeed controlInput
  let initTarget = def :: TargetGH3 FilePath
      makeTarget = fmap ($ initTarget) targetModifier
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-360-live") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-gh3-live")
        (T.pack $ proj.projectTemplate <> "_gh3live")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> sink $ EventOnyx do
          newPreferences <- readPreferences
          stackIO $ build tgt $ GH3LIVE $ trimXbox newPreferences f
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps3-pkg") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 do
      tgt <- makeTarget
      saveFileChooser (l10n "file-picker-save-gh3-pkg")
        (T.pack $ proj.projectTemplate <> ".pkg")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> build tgt $ GH3PKG f
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  fullWidth 35 \rect' -> do
    btn1 <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps3-folder") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 $ sink $ EventOnyx do
      tgt <- stackIO makeTarget
      defDir <- getDefaultPS3Dir
      stackIO $ askFolder defDir \dout -> do
        build tgt $ GH3LoosePS3 dout
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

songPageGH1
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> Project
  -> (TargetGH1 FilePath -> GH1Create -> IO ())
  -> IO ()
songPageGH1 sink rect tab proj build = mdo
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \gh1 ->
        (gh1 :: TargetGH1 FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-guitar", (.guitar), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #guitar .~ v)
        , isJust . anyFiveFret
        )
      ]
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-bass" , (.bass ), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #bass  .~ v)
        , isJust . anyFiveFret
        )
      , ( l10n "part-keys" , (.keys ), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #keys  .~ v)
        , isJust . anyFiveFret
        )
      , ( l10n "part-drums", (.drums), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #drums .~ v)
        , isJust . (.drums)
        )
      , ( l10n "part-vocal", (.vocal), (\v gh1 -> (gh1 :: TargetGH1 FilePath) & #vocal .~ v)
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
      liftIO $ setCallback counterSpeed controlInput
  let initTarget :: Preferences -> TargetGH1 FilePath
      initTarget prefs = (def :: TargetGH1 FilePath) & #offset .~ prefs.prefGH2Offset
      makeTarget = fmap ($ initTarget ?preferences) targetModifier
      -- make sure we reload offset before compiling
      makeTargetUpdatePrefs go = targetModifier >>= \modifier -> sink $ EventOnyx do
        newPrefs <- readPreferences
        stackIO $ go $ modifier $ initTarget newPrefs
  fullWidth 35 \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps2-add-bonus") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 $ makeTargetUpdatePrefs \tgt -> do
      browseFileChooser (l10n "file-picker-load-hdr") \f -> let
        gen = takeDirectory f
        in build tgt $ GH1ARK gen
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps2-diy") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 $ makeTargetUpdatePrefs \tgt -> do
      saveFileChooser (l10n "file-picker-create-diy-folder")
        (T.pack $ proj.projectTemplate <> "_gh1")
        Nothing
        \f -> build tgt $ GH1DIYPS2 f
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

songPageGH2
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> Project
  -> (TargetGH2 FilePath -> GH2Create -> IO ())
  -> IO ()
songPageGH2 sink rect tab proj build = mdo
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo \gh2 ->
        (gh2 :: TargetGH2 FilePath) & #common % #speed .~ Just speed
      return counter
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-guitar", (.guitar), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #guitar .~ v)
        , isJust . anyFiveFret
        )
      ]
    fullWidth 50 \rect' -> do
      let [bassArea, coopArea, rhythmArea] = splitHorizN 3 rect'
      void $ partSelectors bassArea proj
        [ ( l10n "part-bass"  , (.bass  ), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #bass .~ v)
          , isJust . anyFiveFret
          )
        ]
      controlRhythm <- partSelectors rhythmArea proj
        [ ( l10n "part-rhythm", (.rhythm), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #rhythm .~ v)
          , isJust . anyFiveFret
          )
        ]
      coopPart <- liftIO $ newIORef GH2Bass
      liftIO do
        coopButton <- withRect coopArea \bx by bw bh ->
          [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
        let updateCoopButton = do
              coop <- readIORef coopPart
              let lbl :: T.Text
                  lbl = case coop of
                    GH2Bass   -> l10n "coop-bass"
                    GH2Rhythm -> l10n "coop-rhythm"
              withUTF8 lbl \clbl ->
                [C.block| void { $(Fl_Button* coopButton)->copy_label($(char* clbl)); } |]
              controlRhythm $ coop == GH2Rhythm
        updateCoopButton
        setCallback coopButton $ sink $ EventIO do
          modifyIORef coopPart \case
            GH2Bass   -> GH2Rhythm
            GH2Rhythm -> GH2Bass
          updateCoopButton
      tell $ readIORef coopPart >>= \coop -> return $ Endo \gh2 -> (gh2 :: TargetGH2 FilePath) & #coop .~ coop
    fullWidth 50 \rect' -> void $ partSelectors rect' proj
      [ ( l10n "part-keys" , (.keys ), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #keys  .~ v)
        , isJust . anyFiveFret
        )
      , ( l10n "part-drums", (.drums), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #drums .~ v)
        , isJust . anyDrums
        )
      , ( l10n "part-vocal", (.vocal), (\v gh2 -> (gh2 :: TargetGH2 FilePath) & #vocal .~ v)
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
      liftIO $ setCallback counterSpeed controlInput
    fullWidth 35 \rect' -> do
      let [rectA, rectB] = splitHorizN 2 rect'
      boxA <- liftIO $ withRect rectA \cx cy cw ch ->
        [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
      liftIO $ setLabelTooltip (l10nFull "toggle-ps2-practice-audio" []) boxA
      tell do
        b <- checkValue boxA
        return $ Endo \gh2 -> gh2 { practiceAudio = b }
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
    btn1 <- withRect r1 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps2-add-bonus") \clbl ->
      [C.block| void { $(Fl_Button* btn1)->copy_label($(char* clbl)); } |]
    setCallback btn1 $ makeTargetUpdatePrefs \tgt -> do
      browseFileChooser (l10n "file-picker-load-hdr") \f -> let
        gen = takeDirectory f
        in build tgt $ GH2ARK gen GH2AddBonus
    btn2 <- withRect r2 \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-ps2-diy") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 $ makeTargetUpdatePrefs \tgt -> do
      saveFileChooser (l10n "file-picker-create-diy-folder")
        (T.pack $ proj.projectTemplate <> "_gh2")
        Nothing
        \f -> build tgt $ GH2DIYPS2 f
    [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  fullWidth 35 \rect' -> do
    btn2 <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "create-360-live") \clbl ->
      [C.block| void { $(Fl_Button* btn2)->copy_label($(char* clbl)); } |]
    setCallback btn2 $ makeTargetUpdatePrefs \tgt -> do
      saveFileChooser (l10n "file-picker-save-gh2-live")
        (T.pack $ proj.projectTemplate <> "_gh2live")
        (T.pack <$> ?preferences.prefDirRB)
        \f -> sink $ EventOnyx do
          newPreferences <- readPreferences
          stackIO $ warnCombineXboxGH2 sink $ build tgt $ GH2LIVE $ trimXbox newPreferences f
    [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

overrideEmptyTitle :: Metadata f
overrideEmptyTitle = def & #title .~ Just ""
