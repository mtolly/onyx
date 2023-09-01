{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI.Misc where

import           Onyx.GUI.Core

import           Control.Applicative                       ((<|>))
import           Control.Concurrent                        (modifyMVar_,
                                                            newMVar, readMVar)
import           Control.Monad                             (forM, guard, join,
                                                            void)
import           Data.Binary.Put                           (runPut)
import qualified Data.ByteString.Lazy                      as BL
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe, isJust,
                                                            listToMaybe)
import qualified Data.Text                                 as T
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
import           Graphics.UI.FLTK.LowLevel.FLTKHS          (Height (..),
                                                            Rectangle (..),
                                                            Size (..),
                                                            Width (..))
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
import           Onyx.Audio                                (Audio (..),
                                                            buildSource',
                                                            runAudio)
import           Onyx.Build.Neversoft                      (makeMetadataLIVE,
                                                            makeMetadataPKG)
import           Onyx.GUI.Util                             (askFolder)
import           Onyx.Harmonix.RockBand.Milo               as Milo
import           Onyx.MIDI.Read                            (mapTrack)
import qualified Onyx.MIDI.Track.File                      as F
import           Onyx.MIDI.Track.Lipsync
import           Onyx.MIDI.Track.Vocal                     (nullVox)
import           Onyx.Neversoft.Metadata                   (combineGH3SongCache360,
                                                            combineGH3SongCachePS3)
import           Onyx.Project
import           Onyx.QuickConvert
import           Onyx.StackTrace
import           Onyx.Vocal.DryVox
import qualified Onyx.Xbox.STFS                            as STFS
import qualified Sound.MIDI.Util                           as U
import qualified System.Directory                          as Dir
import           System.FilePath                           (dropExtension,
                                                            takeDirectory,
                                                            (<.>), (</>))

miscPageMilo
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMilo sink rect tab startTasks = do
  pack <- FL.packNew rect Nothing
  padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Extract .milo to folder"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> do
      picker1 <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker1 "Load .milo"
      FL.setFilter picker1 "*.{milo_xbox,milo_ps3,milo_wii}"
      FL.showWidget picker1 >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker1 >>= \case
          Nothing  -> return ()
          Just finText@(T.unpack -> fin) -> do
            picker2 <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
            FL.setTitle picker2 "Create folder"
            FL.setPresetFile picker2 $ finText <> "-extract"
            FL.showWidget picker2 >>= \case
              FL.NativeFileChooserPicked -> FL.getFilename picker2 >>= \case
                Nothing -> return ()
                Just (T.unpack -> dout) -> sink $ EventOnyx $ let
                  task = do
                    unpackMilo fin dout
                    return [dout]
                  in startTasks [("Extract " <> fin, task)]
              _ -> return ()
        _ -> return ()
    return ()
  padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Repack .milo from folder"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> askFolder Nothing $ \din -> do
      let dinText = T.pack din
      miloPicker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle miloPicker "Save .milo"
      FL.setPresetFile miloPicker $ case T.stripSuffix "-extract" dinText of
        Just stripped -> stripped
        Nothing       -> dinText <> ".milo"
      FL.setFilter miloPicker "*.{milo_xbox,milo_ps3,milo_wii}"
      FL.showWidget miloPicker >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename miloPicker >>= \case
          Nothing -> return ()
          Just (T.unpack -> fout) -> sink $ EventOnyx $ let
            task = do
              packMilo din fout
              return [fout]
            in startTasks [("Repack " <> din, task)]
        _ -> return ()
    return ()
  {-
  padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    group <- FL.groupNew rect' Nothing
    let (trimClock 0 10 0 100 -> dropdownArea, btnArea) = chopRight 160 rect'
    choice <- FL.choiceNew dropdownArea $ Just "Contents"
    mapM_ (FL.addName choice)
      [ "RB2, empty"
      , "RB2, 1 lipsync"
      , "RB3, empty"
      , "RB3, 1 lipsync"
      , "RB3, 2 lipsync"
      , "RB3, 3 lipsync"
      , "RB3, venue"
      , "RB3, 1 lipsync + venue"
      , "RB3, 2 lipsync + venue"
      , "RB3, 3 lipsync + venue"
      ]
    btn <- FL.buttonNew btnArea $ Just "Create .milo"
    -- TODO
    FL.end group
    FL.setResizable group $ Just choice
    return ()
  -}
  FL.end pack
  FL.setResizable tab $ Just pack

miscPageLipsync
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageLipsync sink rect tab startTasks = do
  pack <- FL.packNew rect Nothing
  pickedFile <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "MIDI file")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Load MIDI file"
      FL.setFilter picker "*.{mid,midi}" -- TODO also handle .chart?
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
          Nothing -> return ()
          Just f  -> void $ FL.setValue input f
        _                          -> return ()
    return $ fmap T.unpack $ FL.getValue input
  (getVowels, getTransition) <- padded 2 10 2 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (trimClock 0 100 0 0 -> langArea, transArea) = chopRight 150 rect'
    getVowels <- horizRadio langArea
      [ ("English", englishSyllables, True)
      , ("German", germanVowels, False)
      , ("Spanish", spanishVowels, False)
      ]
    counter <- FL.counterNew transArea $ Just "Transition (ms)"
    FL.setLabelsize counter $ FL.FontSize 13
    FL.setLabeltype counter FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign counter $ FLE.Alignments [FLE.AlignTypeLeft]
    FL.setStep counter 1
    FL.setLstep counter 5
    FL.setMinimum counter 0
    void $ FL.setValue counter $ fromInteger $ round $ defaultTransition * 1000
    return (fromMaybe englishSyllables <$> getVowels, (/ 1000) . realToFrac <$> FL.getValue counter)

  let saveVoc input go = do
        picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
        FL.setTitle picker "Save .voc file"
        FL.setFilter picker "*.voc"
        FL.setPresetFile picker $ T.pack $ dropExtension input <.> "voc"
        FL.showWidget picker >>= \case
          FL.NativeFileChooserPicked -> FL.getFilename picker >>= mapM_ (go . T.unpack)
          _                          -> return ()

  padded 10 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Make GH2 .voc from PART VOCALS"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      vowels <- getVowels
      saveVoc input $ \voc -> sink $ EventOnyx $ let
        task = do
          mid <- F.loadMIDI input
          stackIO
            $ BL.writeFile voc
            $ runPut $ putVocFile
            $ gh2Lipsync vowels
            $ mapTrack (U.applyTempoTrack $ F.s_tempos mid)
            $ F.fixedPartVocals $ F.s_tracks mid
          return [voc]
        in startTasks [("Make GH2 .voc: " <> voc, task)]

  let pickMilo go = do
        picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
        FL.setTitle picker "Load Milo file"
        FL.setFilter picker "*.{milo_xbox,milo_ps3,milo_wii}"
        FL.showWidget picker >>= \case
          FL.NativeFileChooserPicked -> FL.getFilename picker >>= mapM_ (go . T.unpack)
          _                          -> return ()
      saveMilo input go = do
        picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
        FL.setTitle picker "Save Milo file"
        FL.setFilter picker "*.{milo_xbox,milo_ps3,milo_wii}"
        FL.setPresetFile picker $ T.pack $ dropExtension input <.> "milo_xbox"
        FL.showWidget picker >>= \case
          FL.NativeFileChooserPicked -> FL.getFilename picker >>= mapM_ (go . T.unpack)
          _                          -> return ()
      hasLipsyncTracks mid = let
        lipsyncTracks =
          [ F.fixedLipsyncJohn, F.fixedLipsyncPaul, F.fixedLipsyncGeorge, F.fixedLipsyncRingo
          , F.fixedLipsync1, F.fixedLipsync2, F.fixedLipsync3, F.fixedLipsync4
          ]
        in if any (\t -> t (F.s_tracks mid) /= mempty) lipsyncTracks
          then do
            lg "Updating milo from LIPSYNC* tracks."
            return True
          else do
            lg "Updating milo from vocal chart tracks."
            return False
  padded 10 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let [trimClock 0 5 0 0 -> makeArea, trimClock 0 5 0 5 -> updateArea, trimClock 0 0 0 5 -> rawArea] = splitHorizN 3 rect'
    makeButton <- FL.buttonNew makeArea $ Just "Make RB3 .milo_*"
    taskColor >>= FL.setColor makeButton
    FL.setTooltip makeButton "Create a RB3 .milo_* file from the first present out of: LIPSYNC* tracks, HARM* tracks, or PART VOCALS."
    FL.setCallback makeButton $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      vowels <- getVowels
      trans <- getTransition
      saveMilo input $ \milo -> sink $ EventOnyx $ let
        task = do
          mid <- F.loadMIDI input
          vmapRB3 <- loadVisemesRB3
          useLipsyncTracks <- hasLipsyncTracks mid
          stackIO $ BL.writeFile milo $ magmaMilo $ let
            lip1 = if useLipsyncTracks
              then getLipsync F.fixedLipsync1
              else getVocal [F.fixedHarm1, F.fixedPartVocals]
            lip2 = if useLipsyncTracks
              then getLipsync F.fixedLipsync2
              else getVocal [F.fixedHarm2]
            lip3 = if useLipsyncTracks
              then getLipsync F.fixedLipsync3
              else getVocal [F.fixedHarm3]
            lip4 = getLipsync F.fixedLipsync4
            empty = lipsyncFromStates []
            getVocal getTracks = do
              trk <- listToMaybe $ filter (/= mempty) $ map ($ F.s_tracks mid) getTracks
              Just
                $ autoLipsync trans vmapRB3 vowels
                $ mapTrack (U.applyTempoTrack $ F.s_tempos mid) trk
            getLipsync getTrack = let
              trk = getTrack $ F.s_tracks mid
              in do
                guard $ trk /= mempty
                Just
                  $ lipsyncFromMIDITrack' Milo.LipsyncRB3 vmapRB3
                  $ mapTrack (U.applyTempoTrack $ F.s_tempos mid) trk
            in case (lip1, lip2, lip3, lip4) of
              (_, _     , _      , Just _ ) -> MagmaLipsync4 (fromMaybe empty lip1) (fromMaybe empty lip2) (fromMaybe empty lip3) (fromMaybe empty lip4)
              (_, _     , Just _ , Nothing) -> MagmaLipsync3 (fromMaybe empty lip1) (fromMaybe empty lip2) (fromMaybe empty lip3)
              (_, Just _, Nothing, Nothing) -> MagmaLipsync2 (fromMaybe empty lip1) (fromMaybe empty lip2)
              _                             -> MagmaLipsync1 (fromMaybe empty lip1)
          return [milo]
        in startTasks [("Create .milo from MIDI lipsync: " <> milo, task)]
    updateButton <- FL.buttonNew updateArea $ Just "Update RB3/TBRB .milo_*"
    taskColor >>= FL.setColor updateButton
    FL.setTooltip updateButton "Update a RB3 .milo_* file from LIPSYNC* tracks, HARM* tracks, or PART VOCALS. Or, update a TBRB .milo_* file from LIPSYNC_[beatle] tracks."
    FL.setCallback updateButton $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      vowels <- getVowels
      trans <- getTransition
      pickMilo $ \milo -> sink $ EventOnyx $ let
        task = do
          stackIO $ Dir.copyFile milo $ milo <> ".bak"
          mid <- F.loadMIDI input
          topDir <- loadMilo milo
          vmapBeatles <- loadVisemesTBRB
          vmapRB3 <- loadVisemesRB3
          useLipsyncTracks <- hasLipsyncTracks mid
          let editDir dir = dir
                { miloFiles = do
                  ((_, name), contents) <- zip (miloEntryNames dir) (miloFiles dir)
                  return $ case name of
                    "john.lipsync"   -> getLipsync Milo.LipsyncTBRB vmapBeatles F.fixedLipsyncJohn
                    "paul.lipsync"   -> getLipsync Milo.LipsyncTBRB vmapBeatles F.fixedLipsyncPaul
                    "george.lipsync" -> getLipsync Milo.LipsyncTBRB vmapBeatles F.fixedLipsyncGeorge
                    "ringo.lipsync"  -> getLipsync Milo.LipsyncTBRB vmapBeatles F.fixedLipsyncRingo
                    "song.lipsync"   -> if useLipsyncTracks
                      then getLipsync Milo.LipsyncRB3 vmapRB3 F.fixedLipsync1
                      else getVocal [F.fixedHarm1, F.fixedPartVocals]
                    "part2.lipsync"  -> if useLipsyncTracks
                      then getLipsync Milo.LipsyncRB3 vmapRB3 F.fixedLipsync2
                      else getVocal [F.fixedHarm2]
                    "part3.lipsync"  -> if useLipsyncTracks
                      then getLipsync Milo.LipsyncRB3 vmapRB3 F.fixedLipsync3
                      else getVocal [F.fixedHarm3]
                    "part4.lipsync"  -> getLipsync Milo.LipsyncRB3 vmapRB3 F.fixedLipsync4
                    _                -> contents
                , miloSubdirs = map editDir $ miloSubdirs dir
                }
              getVocal getTracks = let
                trk = fromMaybe mempty $ listToMaybe $ filter (/= mempty) $ map ($ F.s_tracks mid) getTracks
                in runPut $ putLipsync
                  $ autoLipsync trans vmapRB3 vowels
                  $ mapTrack (U.applyTempoTrack $ F.s_tempos mid) trk
              getLipsync tgt vmap getTrack = let
                trk = getTrack $ F.s_tracks mid
                in runPut $ putLipsync
                  $ lipsyncFromMIDITrack' tgt vmap
                  $ mapTrack (U.applyTempoTrack $ F.s_tempos mid) trk
          stackIO $ BL.writeFile milo $ addMiloHeader $ makeMiloFile $ editDir topDir
          return [milo]
        in startTasks [("Update .milo with MIDI lipsync: " <> milo, task)]
    rawButton <- FL.buttonNew rawArea $ Just "Create *.lipsync"
    taskColor >>= FL.setColor rawButton
    FL.setTooltip rawButton "Export all LIPSYNC* and LIPSYNC_[beatle] tracks to .lipsync files, ready to be inserted into .milo_* files."
    FL.setCallback rawButton $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      askFolder (Just $ takeDirectory input) $ \dout -> sink $ EventOnyx $ let
        task = do
          mid <- F.loadMIDI input
          vmapBeatles <- loadVisemesTBRB
          vmapRB3 <- loadVisemesRB3
          let tracks = catMaybes
                [ makeFixed "john.lipsync"   Milo.LipsyncTBRB vmapBeatles F.fixedLipsyncJohn
                , makeFixed "paul.lipsync"   Milo.LipsyncTBRB vmapBeatles F.fixedLipsyncPaul
                , makeFixed "george.lipsync" Milo.LipsyncTBRB vmapBeatles F.fixedLipsyncGeorge
                , makeFixed "ringo.lipsync"  Milo.LipsyncTBRB vmapBeatles F.fixedLipsyncRingo
                , makeFixed "song.lipsync"   Milo.LipsyncRB3  vmapRB3     F.fixedLipsync1
                , makeFixed "part2.lipsync"  Milo.LipsyncRB3  vmapRB3     F.fixedLipsync2
                , makeFixed "part3.lipsync"  Milo.LipsyncRB3  vmapRB3     F.fixedLipsync3
                , makeFixed "part4.lipsync"  Milo.LipsyncRB3  vmapRB3     F.fixedLipsync4
                ]
              makeFixed name tgt vmap getTrack = let
                trk = getTrack $ F.s_tracks mid
                in do
                  guard $ trk /= mempty
                  Just (dout </> name, runPut $ putLipsync
                    $ lipsyncFromMIDITrack' tgt vmap
                    $ mapTrack (U.applyTempoTrack $ F.s_tempos mid) trk)
          forM tracks $ \(path, bs) -> do
            stackIO $ BL.writeFile path bs
            return path
        in startTasks [("Export .lipsync to folder", task)]

  let makeLipsyncTracks loadVisemes toEvents = do
        input <- pickedFile
        vowels <- getVowels
        trans <- getTransition
        sink $ EventOnyx $ let
          task = do
            stackIO $ Dir.copyFile input $ input <> ".bak"
            mid <- F.loadMIDI input
            midRaw <- F.loadMIDI input
            vmap <- loadVisemes
            let makeLipsync vox = if nullVox vox
                  then Nothing
                  else Just mempty
                    { lipEvents
                      = U.unapplyTempoTrack (F.s_tempos mid)
                      $ toEvents trans vmap vowels
                      $ mapTrack (U.applyTempoTrack $ F.s_tempos mid) vox
                    }
                lipsync1 = makeLipsync (F.fixedHarm1 $ F.s_tracks mid)
                  <|> makeLipsync (F.fixedPartVocals $ F.s_tracks mid)
                lipsync2 = makeLipsync (F.fixedHarm2 $ F.s_tracks mid)
                lipsync3 = makeLipsync (F.fixedHarm3 $ F.s_tracks mid)
                notLipsync trk = notElem (U.trackName trk) [Just "LIPSYNC1", Just "LIPSYNC2", Just "LIPSYNC3", Just "LIPSYNC4"]
                lipsyncRaw = F.showMIDITracks mid
                  { F.s_tracks = mempty
                    { F.fixedLipsync1 = fromMaybe mempty lipsync1
                    , F.fixedLipsync2 = fromMaybe mempty lipsync2
                    , F.fixedLipsync3 = fromMaybe mempty lipsync3
                    }
                  }
                combinedRaw = midRaw
                  { F.s_tracks = F.RawFile
                    $ filter notLipsync (F.rawTracks $ F.s_tracks midRaw)
                    <> F.s_tracks lipsyncRaw
                  }
            F.saveMIDIUtf8 input combinedRaw
            return [input]
          in startTasks [("Convert vocal tracks to LIPSYNC tracks: " <> input, task)]

  padded 10 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Turn vocal tracks into LIPSYNC* tracks for RB3"
    taskColor >>= FL.setColor btn
    FL.setTooltip btn "Uses HARM* or PART VOCALS to produce LIPSYNC{1-3} tracks containing viseme information, using the standard RB viseme set."
    FL.setCallback btn $ \_ -> sink $ EventIO $ makeLipsyncTracks loadVisemesRB3 autoLipsync'

  padded 10 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Turn vocal tracks into LIPSYNC* tracks for TBRB"
    taskColor >>= FL.setColor btn
    FL.setTooltip btn "Uses HARM* or PART VOCALS to produce LIPSYNC{1-3} tracks containing viseme information, using the Beatles RB viseme set."
    FL.setCallback btn $ \_ -> sink $ EventIO $ makeLipsyncTracks loadVisemesTBRB beatlesLipsync'

  FL.end pack
  FL.setResizable tab $ Just pack

miscPageDryVox
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageDryVox sink rect tab startTasks = do
  pack <- FL.packNew rect Nothing
  pickedFile <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "MIDI file")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Load MIDI file"
      FL.setFilter picker "*.{mid,midi}" -- TODO also handle .chart?
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
          Nothing -> return ()
          Just f  -> void $ FL.setValue input f
        _                          -> return ()
    return $ fmap T.unpack $ FL.getValue input
  getVocalTrack <- padded 2 10 2 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    fn <- horizRadio rect'
      [ ("PART VOCALS", Just Nothing, True)
      , ("[PART] HARM1", Just $ Just Vocal1, False)
      , ("[PART] HARM2", Just $ Just Vocal2, False)
      , ("[PART] HARM3", Just $ Just Vocal3, False)
      , ("(blank)", Nothing, False)
      ]
    return $ join <$> fn
  let getSelectedVox = \case
        Nothing            -> const mempty
        Just Nothing       -> F.fixedPartVocals . F.s_tracks
        Just (Just Vocal1) -> F.fixedHarm1      . F.s_tracks
        Just (Just Vocal2) -> F.fixedHarm2      . F.s_tracks
        Just (Just Vocal3) -> F.fixedHarm3      . F.s_tracks
      defaultSuffix = \case
        Nothing            -> "-blank"
        Just Nothing       -> "-solovox"
        Just (Just Vocal1) -> "-harm1"
        Just (Just Vocal2) -> "-harm2"
        Just (Just Vocal3) -> "-harm3"
  let dryvoxButton label fn = padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
        btn <- FL.buttonNew rect' $ Just label
        taskColor >>= FL.setColor btn
        FL.setCallback btn $ \_ -> sink $ EventIO $ do
          input <- pickedFile
          voc <- getVocalTrack
          picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
          FL.setTitle picker "Save lipsync audio"
          FL.setFilter picker "*.wav"
          FL.setPresetFile picker $ T.pack $ (dropExtension input ++ defaultSuffix voc) <.> "wav"
          FL.showWidget picker >>= \case
            FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
              Nothing -> return ()
              Just f  -> sink $ EventOnyx $ let
                task = do
                  mid <- F.loadMIDI input
                  let trk = mapTrack (U.applyTempoTrack $ F.s_tempos mid) $ getSelectedVox voc mid
                  src <- toDryVoxFormat <$> fn trk
                  runAudio src f
                  return [f]
                in startTasks [(T.unpack label <> ": " <> input, task)]
            _ -> return ()
  dryvoxButton "Make sine wave dry vox from MIDI" $ return . sineDryVox
  pickedAudio <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "Audio file")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Load song or vocals audio"
      FL.setFilter picker "*.{wav,ogg,mp3,flac}"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
          Nothing -> return ()
          Just f  -> void $ FL.setValue input f
        _                          -> return ()
    return $ fmap T.unpack $ FL.getValue input
  dryvoxButton "Make clipped dry vox from MIDI and audio" $ \trk -> do
    audio <- stackIO pickedAudio
    src <- buildSource' $ Input audio
    return $ clipDryVox (isJust <$> vocalTubes trk) src
  FL.end pack
  FL.setResizable tab $ Just pack

miscPageGH3SongCache
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageGH3SongCache sink rect tab startTasks = do
  loadedFiles <- newMVar []
  let (filesRect, startRect) = chopBottom 50 rect
      [chopRight 5 -> (xboxRect, _), chopLeft 5 -> (_, ps3Rect)] = splitHorizN 2 $ trimClock 5 10 10 10 startRect
  group <- fileLoadWindow filesRect sink "Package" "Packages" (modifyMVar_ loadedFiles) [] searchGH3Cachable $ \info -> let
    entry = T.pack $ fst info
    sublines = filter (not . T.null) [snd info]
    in (entry, sublines)
  FL.setResizable tab $ Just group
  btn1 <- FL.buttonNew xboxRect $ Just "Create GH3 song cache (360)"
  taskColor >>= FL.setColor btn1
  FL.setCallback btn1 $ \_ -> sink $ EventIO $ do
    inputs <- map fst <$> readMVar loadedFiles
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
    FL.setTitle picker "Save GH3 cache file (360)"
    case inputs of
      f : _ -> FL.setDirectory picker $ T.pack $ takeDirectory f
      _     -> return ()
    FL.setPresetFile picker "gh3_custom_cache"
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
        Nothing -> return ()
        Just f  -> sink $ EventOnyx $ startTasks $ let
          task = do
            combineGH3SongCache360 inputs f
            return [f]
          in [("Make GH3 cache file", task)]
      _ -> return ()
  btn2 <- FL.buttonNew ps3Rect $ Just "Create GH3 song cache (PS3)"
  taskColor >>= FL.setColor btn2
  FL.setCallback btn2 $ \_ -> sink $ EventIO $ do
    inputs <- map fst <$> readMVar loadedFiles
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
    FL.setTitle picker "Save GH3 cache file (PS3)"
    case inputs of
      f : _ -> FL.setDirectory picker $ T.pack $ takeDirectory f
      _     -> return ()
    FL.setPresetFile picker "gh3_custom_cache.pkg"
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
        Nothing -> return ()
        Just f  -> sink $ EventOnyx $ startTasks $ let
          task = do
            combineGH3SongCachePS3 inputs f
            return [f]
          in [("Make GH3 cache file", task)]
      _ -> return ()

miscPageWoRSongCache
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageWoRSongCache sink rect tab startTasks = do
  loadedFiles <- newMVar []
  let (filesRect, startRect) = chopBottom 50 rect
      [chopRight 5 -> (xboxRect, _), chopLeft 5 -> (_, ps3Rect)] = splitHorizN 2 $ trimClock 5 10 10 10 startRect
  group <- fileLoadWindow filesRect sink "Package" "Packages" (modifyMVar_ loadedFiles) [] searchWoRCachable $ \info -> let
    entry = T.pack $ fst info
    sublines = filter (not . T.null) [snd info]
    in (entry, sublines)
  FL.setResizable tab $ Just group
  btn1 <- FL.buttonNew xboxRect $ Just "Create GH:WoR song cache (360)"
  taskColor >>= FL.setColor btn1
  FL.setCallback btn1 $ \_ -> sink $ EventIO $ do
    inputs <- map fst <$> readMVar loadedFiles
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
    FL.setTitle picker "Save WoR cache file (360)"
    case inputs of
      f : _ -> FL.setDirectory picker $ T.pack $ takeDirectory f
      _     -> return ()
    FL.setPresetFile picker "ghwor_custom_cache"
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
        Nothing -> return ()
        Just f  -> sink $ EventOnyx $ startTasks $ let
          task = do
            makeMetadataLIVE inputs f
            return [f]
          in [("Make WoR cache file", task)]
      _ -> return ()
  btn2 <- FL.buttonNew ps3Rect $ Just "Create GH:WoR song cache (PS3)"
  taskColor >>= FL.setColor btn2
  FL.setCallback btn2 $ \_ -> sink $ EventIO $ do
    inputs <- map fst <$> readMVar loadedFiles
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
    FL.setTitle picker "Save WoR cache file (PS3)"
    case inputs of
      f : _ -> FL.setDirectory picker $ T.pack $ takeDirectory f
      _     -> return ()
    FL.setPresetFile picker "ghwor_custom_cache.pkg"
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
        Nothing -> return ()
        Just f  -> sink $ EventOnyx $ startTasks $ let
          task = do
            makeMetadataPKG inputs f
            return [f]
          in [("Make WoR cache file", task)]
      _ -> return ()

miscPageCONtoPKG
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageCONtoPKG sink rect tab startTasks = do
  loadedFiles <- newMVar []
  let (filesRect, startRect) = chopBottom 50 rect
      [chopRight 5 -> (rb2Rect, _), chopLeft 5 -> (_, rb3Rect)] = splitHorizN 2 $ trimClock 5 10 10 10 startRect
  group <- fileLoadWindow filesRect sink "Rock Band CON/LIVE" "Rock Band CON/LIVE" (modifyMVar_ loadedFiles) [] searchSTFS $ \info -> let
    entry = T.pack $ stfsPath info
    sublines = take 1 $ STFS.md_DisplayName $ stfsMeta info
    in (entry, sublines)
  FL.setResizable tab $ Just group
  let callback isRB3 = sink $ EventIO $ do
        inputs <- map stfsPath <$> readMVar loadedFiles
        sink $ EventOnyx $ startTasks $ flip map inputs $ \input -> let
          pkg = input <> ".pkg"
          task = do
            conToPkg isRB3 input pkg
            return [pkg]
          in (input, task)
  btn1 <- FL.buttonNew rb2Rect $ Just "Convert to PKG (RB2)"
  taskColor >>= FL.setColor btn1
  FL.setCallback btn1 $ \_ -> callback False
  btn2 <- FL.buttonNew rb3Rect $ Just "Convert to PKG (RB3)"
  taskColor >>= FL.setColor btn2
  FL.setCallback btn2 $ \_ -> callback True
