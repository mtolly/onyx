{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI.Batch where

import           Control.Applicative   ((<|>))
import           Control.Monad         ((>=>))
import           Data.Default.Class    (def)
import           Data.Function         ((&))
import qualified Data.HashMap.Strict   as HM
import           Data.List             (find)
import           Data.Maybe            (fromMaybe, isJust)
import qualified Data.Text             as T
import           Foreign               (Ptr)
import qualified Language.C.Inline.Cpp as C
import           Onyx.GUI.Binding
import           Onyx.GUI.Core
import           Onyx.GUI.Util         (askFolder, checkValue, setCallback,
                                        setLabelTooltip)
import           Onyx.Harmonix.Ark.GH2 (GH2InstallLocation (..))
import           Onyx.Import
import           Onyx.Localization     (L, l10n, l10nFull)
import qualified Onyx.MIDI.Track.File  as F
import           Onyx.Mode             (anyDrums, anyFiveFret)
import           Onyx.Preferences      (Preferences (..), getDefaultPS3Dir,
                                        readPreferences)
import           Onyx.Project
import           Onyx.StackTrace
import           Optics                (mapped, (%), (%~), (.~))
import           System.FilePath       (takeDirectory)

C.context flContext

C.include "FL/Fl.H"
C.include "FL/Fl_Group.H"
C.include "FL/Fl_Button.H"
C.include "FL/Fl_Check_Button.H"
C.include "FL/Fl_Input.H"
C.include "FL/Fl_Pack.H"

batchPagePreview
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ((Project -> FilePath) -> IO ())
  -> IO ()
batchPagePreview sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  makeTemplateRunner
    sink
    (l10n "batch-task-preview")
    (maybe "%input_dir%" T.pack ?preferences.prefDirPreview <> "/%input_base%_player")
    (\template -> build \proj -> T.unpack $ templateApplyInput proj Nothing template)
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

batchButtonLoosePS3
  :: (?loc :: L)
  => (Event -> IO ())
  -> (FilePath -> IO ())
  -> IO ()
batchButtonLoosePS3 sink usePath = padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect -> do
  let (rect', _) = chopLeft 250 rect
  button <- withRect rect' \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
  setLabelTooltip (l10nFull "batch-task-ps3-folder" []) button
  [C.block| void { $(Fl_Button* button)->color($(Fl_Color taskColor)); } |]
  setCallback button $ sink $ EventOnyx do
    defDir <- getDefaultPS3Dir
    stackIO $ askFolder defDir usePath

batchPageRB3
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> (Bool -> (Project -> ([(TargetRB3 FilePath, RB3Create)], SongYaml FilePath)) -> IO ())
  -> IO ()
batchPageRB3 sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' ->
    centerFixed rect' $ speedPercent True (trimClock 0 250 0 250 rect')
  getToms <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    box <- withRect rect' \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "batch-tab-rb3-all-toms" []) box
    return $ checkValue box
  getPreset <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    makePresetDropdown rect' batchPartPresetsRB3
  getKicks <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    fn <- horizRadio rect'
      [ (l10n "bass-pedal-1x", Kicks1x, False)
      , (l10n "bass-pedal-2x", Kicks2x, False)
      , (l10n "bass-pedal-both-output", KicksBoth, True)
      ]
    return $ fromMaybe KicksBoth <$> fn
  let getTargetSong isXbox usePath template = do
        speed <- stackIO getSpeed
        toms <- stackIO getToms
        (isDefaultPreset, preset) <- stackIO getPreset
        kicks <- stackIO getKicks
        newPreferences <- readPreferences
        let qcPossible = speed == 1 && isDefaultPreset
        return (qcPossible, \proj -> let
          defRB3 = def :: TargetRB3 FilePath
          tgt = preset yaml defRB3
            { common = defRB3.common
              { speed = Just speed
              , label2x = newPreferences.prefLabel2x
              }
            , legalTempos = newPreferences.prefLegalTempos
            , magma = newPreferences.prefMagma
            , songID = if newPreferences.prefRBNumberID
              then SongIDAutoInt
              else SongIDAutoSymbol
            , encoding = newPreferences.prefRB3Encoding
            }
          -- TODO need to use anyDrums
          kicksConfigs = case (kicks, maybe Kicks1x (.kicks) $ getPart F.PartDrums yaml >>= (.drums)) of
            (_        , Kicks1x) -> [(False, ""   )]
            (Kicks1x  , _      ) -> [(False, "_1x")]
            (Kicks2x  , _      ) -> [(True , "_2x")]
            (KicksBoth, _      ) -> [(False, "_1x"), (True, "_2x")]
          fout kicksLabel = (if isXbox then trimXbox newPreferences else id) $ T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ RB3 tgt
            , let
              modifiers = T.concat
                [ T.pack case tgt.common.speed of
                  Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                  _               -> ""
                , kicksLabel
                ]
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          yaml
            = (if toms then id else forceProDrums)
            $ proj.projectSongYaml
          in
            ( [ ((tgt :: TargetRB3 FilePath) & #is2xBassPedal .~ is2x, usePath $ fout kicksLabel)
              | (is2x, kicksLabel) <- kicksConfigs
              ]
            , yaml
            )
          )
  makeTemplateRunner
    sink
    (l10n "batch-task-360-con")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%_rb3con")
    (\template -> sink $ EventOnyx $ getTargetSong True RB3CON template >>= \(qcPossible, x) -> stackIO $ build qcPossible x)
  makeTemplateRunner
    sink
    (l10n "batch-task-ps3-pkg")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%.pkg")
    (\template -> sink $ EventOnyx $ getTargetSong False RB3PKG template >>= \(qcPossible, x) -> stackIO $ build qcPossible x)
  batchButtonLoosePS3 sink \dout -> do
    sink $ EventOnyx $ getTargetSong False (const $ RB3LoosePS3 dout) "" >>= \(qcPossible, x) -> stackIO $ build qcPossible x
  makeTemplateRunner
    sink
    (l10n "batch-task-magma")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%_project")
    (\template -> sink $ EventOnyx $ getTargetSong False RB3Magma template >>= \(_, x) -> stackIO $ build False x)
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

batchPageGH1
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ((Project -> (TargetGH1 FilePath, GH1Create)) -> IO ())
  -> IO ()
batchPageGH1 sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' ->
    centerFixed rect' $ speedPercent True (trimClock 0 250 0 250 rect')
  let getTargetSong usePath template go = sink $ EventOnyx $ readPreferences >>= \newPrefs -> stackIO do
        speed <- getSpeed
        go \proj -> let
          leadPart = find (hasPartWith anyFiveFret proj.projectSongYaml)
            [ F.PartGuitar
            , F.PartName "rhythm"
            , F.PartKeys
            , F.PartBass
            , F.PartDrums
            , F.PartName "dance"
            ]
          tgt = (def :: TargetGH1 FilePath)
            & #common % #speed .~ Just speed
            & #guitar          .~ fromMaybe (F.PartName "undefined") leadPart
            & #offset          .~ newPrefs.prefGH2Offset
          fout = T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ GH1 tgt
            , let
              modifiers = T.pack case tgt.common.speed of
                Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                _               -> ""
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          in (tgt, usePath fout)
  padded 5 10 5 10 (Size (Width 500) (Height 35)) \r -> do
    btn <- withRect r \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    setLabelTooltip (l10nFull "batch-task-ps2-add" []) btn
    [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
    setCallback btn do
      browseFileChooser (l10n "file-picker-load-hdr") \f -> let
        gen = takeDirectory f
        in getTargetSong id "" \song -> do
          build \proj -> let
            (tgt, _) = song proj
            in (tgt, GH1ARK gen)
  makeTemplateRunner
    sink
    (l10n "batch-task-ps2-diy")
    "%input_dir%/%input_base%%modifiers%_gh1"
    (\template -> getTargetSong GH1DIYPS2 template build)
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

batchPageGH3
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ((Project -> (TargetGH3 FilePath, GH3Create)) -> IO ())
  -> IO ()
batchPageGH3 sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' ->
    centerFixed rect' $ speedPercent True (trimClock 0 250 0 250 rect')
  let getTargetSong xbox usePath template go = sink $ EventOnyx $ readPreferences >>= \newPrefs -> stackIO do
        speed <- getSpeed
        go \proj -> let
          hasPart = hasPartWith anyFiveFret proj.projectSongYaml
          leadPart = find hasPart
            [ F.PartGuitar
            , F.PartName "rhythm"
            , F.PartKeys
            , F.PartBass
            , F.PartDrums
            , F.PartName "dance"
            ]
          coopPart = find (\(p, _) -> hasPart p && leadPart /= Just p)
            [ (F.PartGuitar       , GH2Rhythm)
            , (F.PartName "rhythm", GH2Rhythm)
            , (F.PartBass         , GH2Bass  )
            , (F.PartKeys         , GH2Rhythm)
            , (F.PartDrums        , GH2Rhythm)
            , (F.PartName "dance" , GH2Rhythm)
            ]
          tgt = (def :: TargetGH3 FilePath)
            & #common % #speed .~ Just speed
            & #guitar .~ fromMaybe (F.PartName "undefined") leadPart
            & #bass %~ case coopPart of
              Just (x, GH2Bass) -> const x
              _                 -> id
            & #rhythm %~ case coopPart of
              Just (x, GH2Rhythm) -> const x
              _                   -> id
            & #coop %~ case coopPart of
              Just (_, coop) -> const coop
              _              -> id
          fout = (if xbox then trimXbox newPrefs else id) $ T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ GH3 tgt
            , let
              modifiers = T.pack case tgt.common.speed of
                Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                _               -> ""
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          in (tgt, usePath fout)
  makeTemplateRunner
    sink
    (l10n "batch-task-360-live")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%_gh3live")
    (\template -> getTargetSong True GH3LIVE template build)
  makeTemplateRunner
    sink
    (l10n "batch-task-ps3-pkg")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%.pkg")
    (\template -> getTargetSong False GH3PKG template build)
  batchButtonLoosePS3 sink \dout -> do
    sink $ EventIO $ getTargetSong False (const $ GH3LoosePS3 dout) "" build
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

batchPageGH2
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ((Project -> (TargetGH2 FilePath, GH2Create)) -> IO ())
  -> IO ()
batchPageGH2 sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' ->
    centerFixed rect' $ speedPercent True (trimClock 0 250 0 250 rect')
  (getPracticeAudio, getDeluxe) <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    let [rectA, rectB] = splitHorizN 2 rect'
    boxA <- withRect rectA \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "toggle-ps2-practice-audio" []) boxA
    getDeluxe <- gh2DeluxeSelector sink rectB
    return (checkValue boxA, getDeluxe)
  let getTargetSong xbox usePath template go = sink $ EventOnyx $ readPreferences >>= \newPrefs -> stackIO do
        speed <- getSpeed
        practiceAudio <- getPracticeAudio
        deluxe2x <- getDeluxe
        go \proj -> let
          defGH2 = def :: TargetGH2 FilePath
          hasPart = hasPartWith anyFiveFret proj.projectSongYaml
          leadPart = find hasPart
            [ F.PartGuitar
            , F.PartName "rhythm"
            , F.PartKeys
            , F.PartBass
            , F.PartDrums
            , F.PartName "dance"
            ]
          coopPart = find (\(p, _) -> hasPart p && leadPart /= Just p)
            [ (F.PartGuitar       , GH2Rhythm)
            , (F.PartName "rhythm", GH2Rhythm)
            , (F.PartBass         , GH2Bass  )
            , (F.PartKeys         , GH2Rhythm)
            , (F.PartDrums        , GH2Rhythm)
            , (F.PartName "dance" , GH2Rhythm)
            ]
          tgt = defGH2
            { common = defGH2.common
              { speed = Just speed
              }
            , practiceAudio = practiceAudio
            , guitar = fromMaybe (F.PartName "undefined") leadPart
            , bass = case coopPart of
              Just (x, GH2Bass) -> x
              _                 -> defGH2.bass
            , rhythm = case coopPart of
              Just (x, GH2Rhythm) -> x
              _                   -> defGH2.rhythm
            , coop = case coopPart of
              Just (_, coop) -> coop
              _              -> defGH2.coop
            , offset = newPrefs.prefGH2Offset
            , gh2Deluxe = isJust deluxe2x
            , is2xBassPedal = fromMaybe False deluxe2x
            }
          fout = (if xbox then trimXbox newPrefs else id) $ T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ GH2 tgt
            , let
              modifiers = T.pack case tgt.common.speed of
                Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                _               -> ""
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          in (tgt, usePath fout)
  padded 5 10 5 10 (Size (Width 500) (Height 35)) \r -> do
    btn <- withRect r \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    setLabelTooltip (l10nFull "batch-task-ps2-add" []) btn
    [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
    setCallback btn do
      browseFileChooser (l10n "file-picker-load-hdr") \f -> let
        gen = takeDirectory f
        in getTargetSong False id "" \song -> do
          build \proj -> let
            (tgt, _) = song proj
            in (tgt, GH2ARK gen GH2AddBonus)
  makeTemplateRunner
    sink
    (l10n "batch-task-ps2-diy")
    "%input_dir%/%input_base%%modifiers%_gh2"
    (\template -> getTargetSong False GH2DIYPS2 template build)
  makeTemplateRunner
    sink
    (l10n "batch-task-360-live")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%_gh2live")
    (\template -> warnCombineXboxGH2 sink $ getTargetSong True GH2LIVE template build)
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

batchPageGHWOR
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ((Project -> ((TargetGH5 FilePath, GHWORCreate), SongYaml FilePath)) -> IO ())
  -> IO ()
batchPageGHWOR sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' ->
    centerFixed rect' $ speedPercent True (trimClock 0 250 0 250 rect')
  getProTo4 <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    fn <- horizRadio rect'
      [ (l10n "batch-tab-gh-drums-5lane", False, not ?preferences.prefGH4Lane)
      , (l10n "batch-tab-gh-drums-4lane", True, ?preferences.prefGH4Lane)
      ]
    return $ fromMaybe False <$> fn
  let getTargetSong isXbox usePath template = do
        speed <- stackIO getSpeed
        proTo4 <- stackIO getProTo4
        newPreferences <- readPreferences
        return \proj -> let
          hasPart p = isJust $ HM.lookup p proj.projectSongYaml.parts.getParts >>= anyFiveFret
          pickedGuitar = find hasPart
            [ F.PartGuitar
            , F.PartName "rhythm"
            , F.PartKeys
            , F.PartBass
            ]
          pickedBass = find (\p -> hasPart p && pickedGuitar /= Just p)
            [ F.PartGuitar
            , F.PartName "rhythm"
            , F.PartBass
            , F.PartKeys
            ]
          defGH5 = def :: TargetGH5 FilePath
          tgt = (defGH5 :: TargetGH5 FilePath)
            & #common % #speed .~ Just speed
            & #guitar          .~ fromMaybe defGH5.guitar pickedGuitar
            & #bass            .~ fromMaybe defGH5.bass (pickedBass <|> pickedGuitar)
            & #drums           .~ getDrumsOrDance proj.projectSongYaml
            & #proTo4          .~ proTo4
          fout = (if isXbox then trimXbox newPreferences else id) $ T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ GH5 tgt
            , let
              modifiers = T.concat
                [ T.pack case tgt.common.speed of
                  Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                  _               -> ""
                ]
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          in ((tgt, usePath fout), proj.projectSongYaml)
  makeTemplateRunner
    sink
    (l10n "batch-task-360-live")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%_ghwor")
    (\template -> warnXboxGHWoR sink $ getTargetSong True GHWORLIVE template >>= stackIO . build)
  makeTemplateRunner
    sink
    (l10n "batch-task-ps3-pkg")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%.pkg")
    (\template -> warnXboxGHWoR sink $ getTargetSong False GHWORPKG template >>= stackIO . build)
  batchButtonLoosePS3 sink \dout -> do
    warnXboxGHWoR sink $ getTargetSong False (const $ GHWORLoosePS3 dout) "" >>= stackIO . build
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

batchPageRR
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ((Project -> ((TargetRR FilePath, RRCreate), SongYaml FilePath)) -> IO ())
  -> IO ()
batchPageRR sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' ->
    centerFixed rect' $ speedPercent True (trimClock 0 250 0 250 rect')
  getProTo4 <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    fn <- horizRadio rect'
      [ (l10n "batch-tab-rr-drums-6pad", False, False)
      , (l10n "batch-tab-rr-drums-4pad", True, True)
      ]
    return $ fromMaybe False <$> fn
  let getTargetSong isXbox usePath template = do
        speed <- stackIO getSpeed
        proTo4 <- stackIO getProTo4
        newPreferences <- readPreferences
        return \proj -> let
          hasPart p = isJust $ HM.lookup p proj.projectSongYaml.parts.getParts >>= anyFiveFret
          pickedGuitar = find hasPart
            [ F.PartGuitar
            , F.PartName "rhythm"
            , F.PartKeys
            , F.PartBass
            ]
          pickedBass = find (\p -> hasPart p && pickedGuitar /= Just p)
            [ F.PartGuitar
            , F.PartName "rhythm"
            , F.PartBass
            , F.PartKeys
            ]
          defRR = def :: TargetRR FilePath
          tgt = (defRR :: TargetRR FilePath)
            & #common % #speed .~ Just speed
            & #guitar          .~ fromMaybe defRR.guitar pickedGuitar
            & #bass            .~ fromMaybe defRR.bass (pickedBass <|> pickedGuitar)
            & #drums           .~ getDrumsOrDance proj.projectSongYaml
            & #proTo4          .~ proTo4
          fout = (if isXbox then trimXbox newPreferences else id) $ T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ RR tgt
            , let
              modifiers = T.concat
                [ T.pack case tgt.common.speed of
                  Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                  _               -> ""
                ]
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          in ((tgt, usePath fout), proj.projectSongYaml)
  makeTemplateRunner
    sink
    (l10n "batch-task-360-live")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%_rr")
    (\template -> sink $ EventOnyx $ getTargetSong True RRLIVE template >>= stackIO . build)
  makeTemplateRunner
    sink
    (l10n "batch-task-rpcs3-pkg")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%.pkg")
    (\template -> sink $ EventOnyx $ getTargetSong False RRPKG template >>= stackIO . build)
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

batchPageRB2
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ((Project -> ([(TargetRB2 FilePath, RB2Create)], SongYaml FilePath)) -> IO ())
  -> IO ()
batchPageRB2 sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' ->
    centerFixed rect' $ speedPercent True (trimClock 0 250 0 250 rect')
  getPreset <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    makePresetDropdown rect' batchPartPresetsRB2
  getKicks <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    fn <- horizRadio rect'
      [ (l10n "bass-pedal-1x", Kicks1x, False)
      , (l10n "bass-pedal-2x", Kicks2x, False)
      , (l10n "bass-pedal-both-output", KicksBoth, True)
      ]
    return $ fromMaybe KicksBoth <$> fn
  let getTargetSong isXbox usePath template = do
        speed <- stackIO getSpeed
        preset <- stackIO getPreset
        kicks <- stackIO getKicks
        newPreferences <- readPreferences
        return \proj -> let
          tgt = preset yaml $ def
            & #common % #speed   .~ Just speed
            & #common % #label2x .~ newPreferences.prefLabel2x
            & #legalTempos       .~ newPreferences.prefLegalTempos
            & #magma             .~ newPreferences.prefMagma
            & #songID            .~ do
              if newPreferences.prefRBNumberID
                then SongIDAutoInt
                else SongIDAutoSymbol
            & #ps3Encrypt        .~ newPreferences.prefPS3Encrypt
          -- TODO need to use anyDrums
          kicksConfigs = case (kicks, maybe Kicks1x (.kicks) $ getPart F.PartDrums yaml >>= (.drums)) of
            (_        , Kicks1x) -> [(False, ""   )]
            (Kicks1x  , _      ) -> [(False, "_1x")]
            (Kicks2x  , _      ) -> [(True , "_2x")]
            (KicksBoth, _      ) -> [(False, "_1x"), (True, "_2x")]
          fout kicksLabel = (if isXbox then trimXbox newPreferences else id) $ T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ RB2 tgt
            , let
              modifiers = T.concat
                [ T.pack case tgt.common.speed of
                  Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                  _               -> ""
                , kicksLabel
                ]
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          yaml = forceProDrums proj.projectSongYaml
          in
            ( [ ((tgt :: TargetRB2 FilePath) & #is2xBassPedal .~ is2x, usePath $ fout kicksLabel)
              | (is2x, kicksLabel) <- kicksConfigs
              ]
            , yaml
            )
  makeTemplateRunner
    sink
    (l10n "batch-task-360-con")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%_rb2con")
    (\template -> sink $ EventOnyx $ getTargetSong True RB2CON template >>= stackIO . build)
  makeTemplateRunner
    sink
    (l10n "batch-task-ps3-pkg")
    (maybe "%input_dir%" T.pack ?preferences.prefDirRB <> "/%input_base%%modifiers%.pkg")
    (\template -> sink $ EventOnyx $ getTargetSong False RB2PKG template >>= stackIO . build)
  batchButtonLoosePS3 sink \dout -> do
    sink $ EventOnyx $ getTargetSong False (const $ RB2LoosePS3 dout) "" >>= stackIO . build
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

batchPagePS
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ((Project -> (TargetPS FilePath, PSCreate)) -> IO ())
  -> IO ()
batchPagePS sink rect tab build = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) \rect' ->
    centerFixed rect' $ speedPercent True (trimClock 0 250 0 250 rect')
  getPreset <- padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    makePresetDropdown rect' batchPartPresetsCH
  let getTargetSong usePath template = do
        speed <- getSpeed
        preset <- getPreset
        return \proj -> let
          defPS = def :: TargetPS FilePath
          tgt = preset proj.projectSongYaml
            $ defPS & #common % #speed .~ Just speed
          fout = T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ PS tgt
            , let
              modifiers = T.pack case tgt.common.speed of
                Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                _               -> ""
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          in (tgt, usePath fout)
  makeTemplateRunner
    sink
    (l10n "batch-task-ch-folder")
    (maybe "%input_dir%" T.pack ?preferences.prefDirCH <> "/%artist% - %title%")
    (getTargetSong PSDir >=> build)
  makeTemplateRunner
    sink
    (l10n "batch-task-ch-sng")
    (maybe "%input_dir%" T.pack ?preferences.prefDirCH <> "/%artist% - %title%.sng")
    (getTargetSong PSSng >=> build)
  makeTemplateRunner
    sink
    (l10n "batch-task-ch-zip")
    (maybe "%input_dir%" T.pack ?preferences.prefDirCH <> "/%input_base%%modifiers%_ps.zip")
    (getTargetSong PSZip >=> build)
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
  return ()

forceProDrums :: SongYaml f -> SongYaml f
forceProDrums = #parts % mapped % #drums % mapped % #mode %~ \case
  Drums4 -> DrumsPro
  mode   -> mode

-- Batch mode presets for changing parts around

hasPartWith :: (Part f -> Maybe a) -> SongYaml f -> F.PartName -> Bool
hasPartWith p song flex = isJust $ getPart flex song >>= p

getDrumsOrDance :: SongYaml f -> F.PartName
getDrumsOrDance song
  | hasPartWith Just song (F.PartName "dance") && not (hasPartWith anyDrums song F.PartDrums)
    = F.PartName "dance"
  | otherwise = F.PartDrums

batchPartPresetsRB3 :: (?loc :: L) => [(T.Text, (Bool, SongYaml f -> TargetRB3 f -> TargetRB3 f))]
batchPartPresetsRB3 =
  [ (l10n "batch-configuration-default", (True, \song tgt -> tgt
    & #drums .~ getDrumsOrDance song
    ))
  , (l10n "batch-configuration-rb3-fill-guitar-drums", (False, \song tgt -> let
    fillPart p defPart parts = case filter (hasPartWith p song) (defPart : parts) of
      part : _ -> part
      []       -> defPart
    in tgt
      & #guitar .~ fillPart anyFiveFret F.PartGuitar [F.PartDrums, F.PartName "dance"]
      & #bass   .~ fillPart anyFiveFret F.PartBass [F.PartGuitar, F.PartDrums, F.PartName "dance"]
      & #keys   .~ fillPart anyFiveFret F.PartKeys [F.PartGuitar, F.PartDrums, F.PartName "dance"]
      & #drums  .~ fillPart anyDrums F.PartDrums [F.PartGuitar, F.PartName "dance"]
      ))
  , (l10n "batch-configuration-rb3-fill-guitar", (False, \_song tgt -> tgt
    & #bass  .~ F.PartGuitar
    & #keys  .~ F.PartGuitar
    & #drums .~ F.PartGuitar
    ))
  , (l10n "batch-configuration-rb3-fill-drums", (False, \song tgt -> let
    dod = getDrumsOrDance song
    in tgt
      & #guitar .~ dod
      & #bass   .~ dod
      & #keys   .~ dod
      & #drums  .~ dod
    ))
  ]

batchPartPresetsRB2 :: (?loc :: L) => [(T.Text, SongYaml f -> TargetRB2 f -> TargetRB2 f)]
batchPartPresetsRB2 =
  [ (l10n "batch-configuration-default", \song tgt -> tgt
    & #drums .~ getDrumsOrDance song
    )
  , (l10n "batch-configuration-rb2-guitar-to-bass-empty", \song tgt -> tgt
    & #bass .~ do
      if hasPartWith anyFiveFret song F.PartBass then F.PartBass else F.PartGuitar
    & #drums .~ getDrumsOrDance song
    )
  , (l10n "batch-configuration-rb2-guitar-to-bass", \song tgt -> tgt
    & #bass .~ F.PartGuitar
    & #drums .~ getDrumsOrDance song
    )
  , (l10n "batch-configuration-rb2-keys-to-guitar-bass-empty", \song tgt -> tgt
    & #guitar .~ do
      if hasPartWith anyFiveFret song F.PartGuitar then F.PartGuitar else F.PartKeys
    & #bass   .~ do
      if hasPartWith anyFiveFret song F.PartBass   then F.PartBass   else F.PartKeys
    & #drums  .~ getDrumsOrDance song
    )
  , (l10n "batch-configuration-rb2-keys-to-guitar", \song tgt -> tgt
    & #guitar .~ F.PartKeys
    & #drums .~ getDrumsOrDance song
    )
  , (l10n "batch-configuration-rb2-keys-to-bass", \song tgt -> tgt
    & #bass .~ F.PartKeys
    & #drums .~ getDrumsOrDance song
    )
  , (l10n "batch-configuration-rb2-drums-to-guitar-bass-empty", \song tgt -> let
    dod = getDrumsOrDance song
    in tgt
      & #guitar .~ do
        if hasPartWith anyFiveFret song F.PartGuitar then F.PartGuitar else dod
      & #bass   .~ do
        if hasPartWith anyFiveFret song F.PartBass   then F.PartBass   else dod
      & #drums  .~ dod
    )
  , (l10n "batch-configuration-rb2-drums-to-guitar-bass", \song tgt -> let
    dod = getDrumsOrDance song
    in tgt
      & #guitar .~ dod
      & #bass   .~ dod
      & #drums  .~ dod
    )
  ]

batchPartPresetsCH :: (?loc :: L) => [(T.Text, SongYaml f -> TargetPS f -> TargetPS f)]
batchPartPresetsCH =
  [ (l10n "batch-configuration-default", \song tgt -> tgt
    & #drums .~ getDrumsOrDance song
    )
  , (l10n "batch-configuration-ch-drums-to-guitar-empty", \song tgt -> let
    dod = getDrumsOrDance song
    in tgt
      & #guitar .~ do
        if hasPartWith anyFiveFret song F.PartGuitar then F.PartGuitar else dod
      & #drums .~ dod
    )
  , (l10n "batch-configuration-ch-drums-to-guitar", \song tgt -> let
    dod = getDrumsOrDance song
    in tgt
      & #guitar .~ dod
      & #drums .~ dod
    )
  , (l10n "batch-configuration-ch-drums-to-rhythm-empty", \song tgt -> let
    dod = getDrumsOrDance song
    in tgt
      & #rhythm .~ do
        if hasPartWith anyFiveFret song $ F.PartName "rhythm" then F.PartName "rhythm" else dod
      & #drums .~ dod
    )
  , (l10n "batch-configuration-ch-drums-to-rhythm", \song tgt -> let
    dod = getDrumsOrDance song
    in tgt
      & #rhythm .~ dod
      & #drums .~ dod
    )
  , (l10n "batch-configuration-ch-guitar-drums-fill", \song tgt -> let
    fillPart p defPart parts = case filter (hasPartWith p song) (defPart : parts) of
      part : _ -> part
      []       -> defPart
    in tgt
      & #guitar .~ fillPart anyFiveFret F.PartGuitar [F.PartDrums]
      & #drums .~ fillPart anyDrums F.PartDrums [F.PartGuitar]
    )
  ]
