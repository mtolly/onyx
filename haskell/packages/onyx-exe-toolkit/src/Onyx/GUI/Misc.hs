{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI.Misc where

import           Control.Applicative          ((<|>))
import           Control.Concurrent           (modifyMVar_, newMVar, readMVar)
import           Control.Monad                (forM, guard, join, void, when)
import           Data.Bifunctor               (first)
import           Data.Binary.Put              (runPut)
import qualified Data.ByteString.Lazy         as BL
import           Data.Functor                 ((<&>))
import           Data.Maybe                   (catMaybes, fromMaybe, isJust,
                                               listToMaybe)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Foreign                      (Ptr)
import qualified Language.C.Inline.Cpp        as C
import           Onyx.Audio                   (Audio (..), buildSource',
                                               runAudio)
import           Onyx.Build.GuitarHero5.Logic (getAllMetadata, makeMetadataLIVE,
                                               makeMetadataPKG,
                                               metadataPS3Folder)
import           Onyx.GUI.Binding
import           Onyx.GUI.Core
import           Onyx.GUI.Util                (askFolder, peekUTF8, setCallback,
                                               withNativeFileChooser, withUTF8)
import           Onyx.Harmonix.RockBand.Milo  as Milo
import           Onyx.Import                  (installPS3Folder)
import           Onyx.Localization            (L, l10n, l10nArgs, l10nFull,
                                               vString)
import           Onyx.MIDI.Read               (mapTrack)
import qualified Onyx.MIDI.Track.File         as F
import           Onyx.MIDI.Track.Lipsync
import           Onyx.MIDI.Track.Vocal        (nullVox)
import           Onyx.Neversoft.GH3.Metadata  (GH3Game (..),
                                               combineGH3SongCache360,
                                               combineGH3SongCachePS3,
                                               combineGH3SongCachePS3Folder)
import           Onyx.Preferences
import           Onyx.Project
import           Onyx.QuickConvert.RockBand
import           Onyx.StackTrace
import           Onyx.Vocal.DryVox
import qualified Onyx.Xbox.STFS               as STFS
import qualified Sound.MIDI.Util              as U
import qualified System.Directory             as Dir
import           System.FilePath              (dropExtension, takeDirectory,
                                               (<.>), (</>))

C.context flContext

C.include "FL/Fl.H"
C.include "FL/Fl_Group.H"
C.include "FL/Fl_Box.H"
C.include "FL/Fl_Button.H"
C.include "FL/Fl_Input.H"
C.include "FL/Fl_Counter.H"
C.include "FL/Fl_Pack.H"
C.include "FL/Fl_Native_File_Chooser.H"

-- | Widgets for the .milo tab, laid out in fluid/misc.fl.
data MiloTabWidgets = MiloTabWidgets
  { mtExtract :: Ptr Fl_Button
  , mtRepack  :: Ptr Fl_Button
  }

miscPageMilo
  :: (?loc :: L)
  => (Event -> IO ())
  -> MiloTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMilo sink tab startTasks = do
  setCallback tab.mtExtract $ withNativeFileChooser
    [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |]
    \picker1 -> do
      withUTF8 (l10n "file-picker-load-milo") \s -> [C.block| void {
        $(Fl_Native_File_Chooser* picker1)->title($(char* s));
        $(Fl_Native_File_Chooser* picker1)->filter("*.{milo_xbox,milo_ps3,milo_wii}");
      } |]
      result1 <- [C.exp| int { $(Fl_Native_File_Chooser* picker1)->show() } |]
      when (result1 == 0) do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker1)->filename() } |]
        fin <- peekUTF8 cstr
        withNativeFileChooser
          [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |]
          \picker2 -> do
          withUTF8 (l10n "file-picker-create-folder") \s ->
            [C.block| void { $(Fl_Native_File_Chooser* picker2)->title($(char* s)); } |]
          withUTF8 (fin <> "-extract") \cpreset ->
            [C.block| void { $(Fl_Native_File_Chooser* picker2)->preset_file($(char* cpreset)); } |]
          result2 <- [C.exp| int { $(Fl_Native_File_Chooser* picker2)->show() } |]
          when (result2 == 0) do
            cstr2 <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker2)->filename() } |]
            dout <- peekUTF8 cstr2
            sink $ EventOnyx let
              task = do
                unpackMilo fin dout
                return [dout]
              in startTasks [(T.unpack $ l10nArgs "misc-task-extract-milo" [("path", vString $ T.pack fin)], task)]
  setCallback tab.mtRepack $ askFolder Nothing \din -> do
    let dinText = T.pack din
        presetFile = case T.stripSuffix "-extract" dinText of
          Just stripped -> stripped
          Nothing       -> dinText <> ".milo"
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \miloPicker -> do
      withUTF8 (l10n "file-picker-save-milo") \s -> [C.block| void {
        $(Fl_Native_File_Chooser* miloPicker)->title($(char* s));
        $(Fl_Native_File_Chooser* miloPicker)->filter("*.{milo_xbox,milo_ps3,milo_wii}");
      } |]
      withUTF8 presetFile \cpreset ->
        [C.block| void { $(Fl_Native_File_Chooser* miloPicker)->preset_file($(char* cpreset)); } |]
      result <- [C.exp| int { $(Fl_Native_File_Chooser* miloPicker)->show() } |]
      when (result == 0) do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* miloPicker)->filename() } |]
        fout <- peekUTF8 cstr
        sink $ EventOnyx let
          task = do
            packMilo din fout
            return [fout]
          in startTasks [(T.unpack $ l10nArgs "misc-task-repack-milo" [("path", vString $ T.pack din)], task)]

-- | Widgets for the lipsync tab, laid out in fluid/misc.fl.
data LipsyncTabWidgets = LipsyncTabWidgets
  { lsMidiFile   :: Ptr FilePickerRow
  , lsLanguages  :: [Ptr Fl_Round_Button] -- ^ one per vowel-set option
  , lsTransition :: Ptr Fl_Counter
  , lsMakeVoc    :: Ptr Fl_Button
  , lsMakeMilo   :: Ptr Fl_Button
  , lsUpdateMilo :: Ptr Fl_Button
  , lsRawLipsync :: Ptr Fl_Button
  , lsTracksRB3  :: Ptr Fl_Button
  , lsTracksTBRB :: Ptr Fl_Button
  }

miscPageLipsync
  :: (?loc :: L)
  => (Event -> IO ())
  -> LipsyncTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageLipsync sink tab startTasks = do
  pickedFile <- filePickerRow tab.lsMidiFile (l10n "label-midi-file")
    \useResult -> sink $ EventIO do
      withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |] \picker -> do
        withUTF8 (l10n "file-picker-load-midi") \s -> [C.block| void {
          $(Fl_Native_File_Chooser* picker)->title($(char* s));
          $(Fl_Native_File_Chooser* picker)->filter("*.{mid,midi}"); // TODO also handle .chart?
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          peekUTF8 cstr >>= useResult
  (getVowels, getTransition) <- do
    getVowels <- horizRadioSelect tab.lsLanguages Nothing
      [ (englishSyllables, True)
      , (germanVowels, False)
      , (spanishVowels, False)
      ]
    let counter = tab.lsTransition
    [C.block| void {
      $(Fl_Counter* counter)->step(1);
      $(Fl_Counter* counter)->lstep(5);
      $(Fl_Counter* counter)->minimum(0);
    } |]
    let defVal = realToFrac (round (defaultTransition * 1000) :: Integer) :: C.CDouble
    [C.block| void { $(Fl_Counter* counter)->value($(double defVal)); } |]
    return
      ( fromMaybe englishSyllables <$> getVowels
      , do
          v <- [C.exp| double { $(Fl_Counter* counter)->value() } |]
          return $ realToFrac v / 1000
      )

  let saveVoc input go = do
        withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
          withUTF8 (l10n "file-picker-save-voc") \s -> [C.block| void {
            $(Fl_Native_File_Chooser* picker)->title($(char* s));
            $(Fl_Native_File_Chooser* picker)->filter("*.voc");
          } |]
          withUTF8 (dropExtension input <.> "voc") \cpreset ->
            [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file($(char* cpreset)); } |]
          result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
          when (result == 0) do
            cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
            fout <- peekUTF8 cstr
            go fout

  setCallback tab.lsMakeVoc $ sink $ EventIO do
    input <- pickedFile
    vowels <- getVowels
    saveVoc input \voc -> sink $ EventOnyx let
      task = do
        mid <- F.loadMIDI input
        let _ = mid :: F.Song (F.FixedFile U.Beats)
        stackIO
          $ BL.writeFile voc
          $ runPut $ putVocFile
          $ gh2Lipsync vowels
          $ mapTrack (U.applyTempoTrack mid.tempos)
          $ mid.tracks.fixedPartVocals
        return [voc]
      in startTasks [(T.unpack $ l10nArgs "misc-task-make-voc" [("path", vString $ T.pack voc)], task)]

  let pickMilo go = do
        withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |] \picker -> do
          withUTF8 (l10n "file-picker-load-milo") \s -> [C.block| void {
            $(Fl_Native_File_Chooser* picker)->title($(char* s));
            $(Fl_Native_File_Chooser* picker)->filter("*.{milo_xbox,milo_ps3,milo_wii}");
          } |]
          result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
          when (result == 0) do
            cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
            f <- peekUTF8 cstr
            go f
      saveMilo input go = do
        withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
          withUTF8 (l10n "file-picker-save-milo") \s -> [C.block| void {
            $(Fl_Native_File_Chooser* picker)->title($(char* s));
            $(Fl_Native_File_Chooser* picker)->filter("*.{milo_xbox,milo_ps3,milo_wii}");
          } |]
          withUTF8 (dropExtension input <.> "milo_xbox") \cpreset ->
            [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file($(char* cpreset)); } |]
          result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
          when (result == 0) do
            cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
            fout <- peekUTF8 cstr
            go fout
      hasLipsyncTracks :: (SendMessage m) => F.Song (F.FixedFile U.Beats) -> StackTraceT m Bool
      hasLipsyncTracks mid = let
        trks = mid.tracks
        lipsyncTracks =
          [ trks.fixedLipsyncJohn, trks.fixedLipsyncPaul, trks.fixedLipsyncGeorge, trks.fixedLipsyncRingo
          , trks.fixedLipsync1, trks.fixedLipsync2, trks.fixedLipsync3, trks.fixedLipsync4
          ]
        in if any (/= mempty) lipsyncTracks
          then do
            lg "Updating milo from LIPSYNC* tracks."
            return True
          else do
            lg "Updating milo from vocal chart tracks."
            return False

  setCallback tab.lsMakeMilo $ sink $ EventIO do
    input <- pickedFile
    vowels <- getVowels
    trans <- getTransition
    saveMilo input \milo -> sink $ EventOnyx let
      task = do
        mid <- F.loadMIDI input
        vmapRB3 <- loadVisemesRB3
        useLipsyncTracks <- hasLipsyncTracks mid
        stackIO $ BL.writeFile milo $ magmaMilo let
          lip1 = if useLipsyncTracks
            then getLipsync (.fixedLipsync1)
            else getVocal [(.fixedHarm1), (.fixedPartVocals)]
          lip2 = if useLipsyncTracks
            then getLipsync (.fixedLipsync2)
            else getVocal [(.fixedHarm2)]
          lip3 = if useLipsyncTracks
            then getLipsync (.fixedLipsync3)
            else getVocal [(.fixedHarm3)]
          lip4 = getLipsync (.fixedLipsync4)
          empty = lipsyncFromStates []
          getVocal getTracks = do
            trk <- listToMaybe $ filter (/= mempty) $ map ($ mid.tracks) getTracks
            Just
              $ autoLipsync trans vmapRB3 vowels
              $ mapTrack (U.applyTempoTrack mid.tempos) trk
          getLipsync getTrack = let
            trk = getTrack mid.tracks
            in do
              guard $ trk /= mempty
              Just
                $ lipsyncFromMIDITrack' Milo.LipsyncRB3 vmapRB3
                $ mapTrack (U.applyTempoTrack mid.tempos) trk
          in case (lip1, lip2, lip3, lip4) of
            (_, _     , _      , Just _ ) -> MagmaLipsync4 (fromMaybe empty lip1) (fromMaybe empty lip2) (fromMaybe empty lip3) (fromMaybe empty lip4)
            (_, _     , Just _ , Nothing) -> MagmaLipsync3 (fromMaybe empty lip1) (fromMaybe empty lip2) (fromMaybe empty lip3)
            (_, Just _, Nothing, Nothing) -> MagmaLipsync2 (fromMaybe empty lip1) (fromMaybe empty lip2)
            _                             -> MagmaLipsync1 (fromMaybe empty lip1)
        return [milo]
      in startTasks [(T.unpack $ l10nArgs "misc-task-make-milo" [("path", vString $ T.pack milo)], task)]
  setCallback tab.lsUpdateMilo $ sink $ EventIO do
    input <- pickedFile
    vowels <- getVowels
    trans <- getTransition
    pickMilo \milo -> sink $ EventOnyx let
      task = do
        stackIO $ Dir.copyFile milo $ milo <> ".bak"
        mid <- F.loadMIDI input
        topDir <- loadMilo milo
        vmapBeatles <- loadVisemesTBRB
        vmapRB3 <- loadVisemesRB3
        useLipsyncTracks <- hasLipsyncTracks mid
        let editDir dir = dir
              { files = do
                ((_, name), entry) <- zip dir.entryNames dir.files
                let contents = entry.data_
                return $ (\d -> entry { data_ = d }) case name of
                  "john.lipsync"   -> getLipsync Milo.LipsyncTBRB vmapBeatles (.fixedLipsyncJohn  )
                  "paul.lipsync"   -> getLipsync Milo.LipsyncTBRB vmapBeatles (.fixedLipsyncPaul  )
                  "george.lipsync" -> getLipsync Milo.LipsyncTBRB vmapBeatles (.fixedLipsyncGeorge)
                  "ringo.lipsync"  -> getLipsync Milo.LipsyncTBRB vmapBeatles (.fixedLipsyncRingo )
                  "song.lipsync"   -> if useLipsyncTracks
                    then getLipsync Milo.LipsyncRB3 vmapRB3 (.fixedLipsync1)
                    else getVocal [(.fixedHarm1), (.fixedPartVocals)]
                  "part2.lipsync"  -> if useLipsyncTracks
                    then getLipsync Milo.LipsyncRB3 vmapRB3 (.fixedLipsync2)
                    else getVocal [(.fixedHarm2)]
                  "part3.lipsync"  -> if useLipsyncTracks
                    then getLipsync Milo.LipsyncRB3 vmapRB3 (.fixedLipsync3)
                    else getVocal [(.fixedHarm3)]
                  "part4.lipsync"  -> getLipsync Milo.LipsyncRB3 vmapRB3 (.fixedLipsync4)
                  _                -> contents
              , objectDir = dir.objectDir
                { subdirs = do
                  sd <- dir.objectDir.subdirs
                  return sd { dir = fmap editDir sd.dir }
                }
              }
            getVocal getTracks = let
              trk = fromMaybe mempty $ listToMaybe $ filter (/= mempty) $ map ($ mid.tracks) getTracks
              in runPut $ putLipsync
                $ autoLipsync trans vmapRB3 vowels
                $ mapTrack (U.applyTempoTrack mid.tempos) trk
            getLipsync tgt vmap getTrack = let
              trk = getTrack mid.tracks
              in runPut $ putLipsync
                $ lipsyncFromMIDITrack' tgt vmap
                $ mapTrack (U.applyTempoTrack mid.tempos) trk
        stackIO $ BL.writeFile milo $ addMiloHeader $ makeMiloFile $ editDir topDir
        return [milo]
      in startTasks [(T.unpack $ l10nArgs "misc-task-update-milo" [("path", vString $ T.pack milo)], task)]
  setCallback tab.lsRawLipsync $ sink $ EventIO do
    input <- pickedFile
    askFolder (Just $ takeDirectory input) \dout -> sink $ EventOnyx let
      task = do
        mid <- F.loadMIDI input
        let _ = mid :: F.Song (F.FixedFile U.Beats)
        vmapBeatles <- loadVisemesTBRB
        vmapRB3 <- loadVisemesRB3
        let tracks = catMaybes
              [ makeFixed "john.lipsync"   Milo.LipsyncTBRB vmapBeatles (.fixedLipsyncJohn  )
              , makeFixed "paul.lipsync"   Milo.LipsyncTBRB vmapBeatles (.fixedLipsyncPaul  )
              , makeFixed "george.lipsync" Milo.LipsyncTBRB vmapBeatles (.fixedLipsyncGeorge)
              , makeFixed "ringo.lipsync"  Milo.LipsyncTBRB vmapBeatles (.fixedLipsyncRingo )
              , makeFixed "song.lipsync"   Milo.LipsyncRB3  vmapRB3     (.fixedLipsync1     )
              , makeFixed "part2.lipsync"  Milo.LipsyncRB3  vmapRB3     (.fixedLipsync2     )
              , makeFixed "part3.lipsync"  Milo.LipsyncRB3  vmapRB3     (.fixedLipsync3     )
              , makeFixed "part4.lipsync"  Milo.LipsyncRB3  vmapRB3     (.fixedLipsync4     )
              ]
            makeFixed name tgt vmap getTrack = let
              trk = getTrack mid.tracks
              in do
                guard $ trk /= mempty
                Just (dout </> name, runPut $ putLipsync
                  $ lipsyncFromMIDITrack' tgt vmap
                  $ mapTrack (U.applyTempoTrack mid.tempos) trk)
        forM tracks \(path, bs) -> do
          stackIO $ BL.writeFile path bs
          return path
      in startTasks [(T.unpack $ l10n "misc-task-export-lipsync", task)]

  let makeLipsyncTracks loadVisemes toEvents = do
        input <- pickedFile
        vowels <- getVowels
        trans <- getTransition
        sink $ EventOnyx let
          task = do
            stackIO $ Dir.copyFile input $ input <> ".bak"
            mid <- F.loadMIDI input
            midRaw <- F.loadMIDI input
            let _ = mid    :: F.Song (F.FixedFile U.Beats)
                _ = midRaw :: F.Song (F.RawFile   U.Beats)
            vmap <- loadVisemes
            let makeLipsync vox = if nullVox vox
                  then Nothing
                  else Just mempty
                    { lipEvents
                      = U.unapplyTempoTrack mid.tempos
                      $ toEvents trans vmap vowels
                      $ mapTrack (U.applyTempoTrack mid.tempos) vox
                    }
                lipsync1 = makeLipsync mid.tracks.fixedHarm1
                  <|>      makeLipsync mid.tracks.fixedPartVocals
                lipsync2 = makeLipsync mid.tracks.fixedHarm2
                lipsync3 = makeLipsync mid.tracks.fixedHarm3
                notLipsync trk = notElem (U.trackName trk) [Just "LIPSYNC1", Just "LIPSYNC2", Just "LIPSYNC3", Just "LIPSYNC4"]
                lipsyncRaw = F.showMIDITracks mid
                  { F.tracks = mempty
                    { F.fixedLipsync1 = fromMaybe mempty lipsync1
                    , F.fixedLipsync2 = fromMaybe mempty lipsync2
                    , F.fixedLipsync3 = fromMaybe mempty lipsync3
                    }
                  }
                combinedRaw = midRaw
                  { F.tracks = F.RawFile
                    $ filter notLipsync midRaw.tracks.rawTracks
                    <> lipsyncRaw.tracks
                  }
            F.saveMIDIUtf8 input combinedRaw
            return [input]
          in startTasks [(T.unpack $ l10nArgs "misc-task-vocal-to-lipsync" [("path", vString $ T.pack input)], task)]

  setCallback tab.lsTracksRB3  $ sink $ EventIO $ makeLipsyncTracks loadVisemesRB3  autoLipsync'
  setCallback tab.lsTracksTBRB $ sink $ EventIO $ makeLipsyncTracks loadVisemesTBRB beatlesLipsync'

-- | Widgets for the dry vox tab, laid out in fluid/misc.fl.
data DryVoxTabWidgets = DryVoxTabWidgets
  { dvMidiFile    :: Ptr FilePickerRow
  , dvVocalTracks :: [Ptr Fl_Round_Button] -- ^ one per vocal track option
  , dvSine        :: Ptr Fl_Button
  , dvAudioFile   :: Ptr FilePickerRow
  , dvClipped     :: Ptr Fl_Button
  }

miscPageDryVox
  :: (?loc :: L)
  => (Event -> IO ())
  -> DryVoxTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageDryVox sink tab startTasks = do
  pickedFile <- filePickerRow tab.dvMidiFile (l10n "label-midi-file")
    \useResult -> sink $ EventIO do
      withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |] \picker -> do
        withUTF8 (l10n "file-picker-load-midi") \s -> [C.block| void {
          $(Fl_Native_File_Chooser* picker)->title($(char* s));
          $(Fl_Native_File_Chooser* picker)->filter("*.{mid,midi}"); // TODO also handle .chart?
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          peekUTF8 cstr >>= useResult
  getVocalTrack <- do
    fn <- horizRadioSelect tab.dvVocalTracks Nothing
      [ (Just Nothing, True)
      , (Just $ Just Vocal1, False)
      , (Just $ Just Vocal2, False)
      , (Just $ Just Vocal3, False)
      , (Nothing, False)
      ]
    return $ join <$> fn
  let getSelectedVox = \case
        Nothing            -> const mempty
        Just Nothing       -> (.tracks.fixedPartVocals)
        Just (Just Vocal1) -> (.tracks.fixedHarm1     )
        Just (Just Vocal2) -> (.tracks.fixedHarm2     )
        Just (Just Vocal3) -> (.tracks.fixedHarm3     )
      defaultSuffix = \case
        Nothing            -> "-blank"
        Just Nothing       -> "-solovox"
        Just (Just Vocal1) -> "-harm1"
        Just (Just Vocal2) -> "-harm2"
        Just (Just Vocal3) -> "-harm3"
  let dryvoxButton btn label fn = do
        setCallback (btn :: Ptr Fl_Button) $ sink $ EventIO do
          input <- pickedFile
          voc <- getVocalTrack
          withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
            withUTF8 (l10n "file-picker-save-lipsync-audio") \s -> [C.block| void {
              $(Fl_Native_File_Chooser* picker)->title($(char* s));
              $(Fl_Native_File_Chooser* picker)->filter("*.wav");
            } |]
            withUTF8 ((dropExtension input ++ defaultSuffix voc) <.> "wav") \cpreset ->
              [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file($(char* cpreset)); } |]
            result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
            when (result == 0) do
              cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
              f <- peekUTF8 cstr
              sink $ EventOnyx let
                task = do
                  mid <- F.loadMIDI input
                  let _ = mid :: F.Song (F.FixedFile U.Beats)
                      trk = mapTrack (U.applyTempoTrack mid.tempos) $ getSelectedVox voc mid
                  src <- toDryVoxFormat <$> fn trk
                  runAudio src f
                  return [f]
                in startTasks [(T.unpack $ l10nArgs "misc-task-dryvox" [("name", vString (label :: T.Text)), ("path", vString $ T.pack input)], task)]
  dryvoxButton tab.dvSine (l10n "misc-button-dryvox-sine") $ return . sineDryVox
  pickedAudio <- filePickerRow tab.dvAudioFile (l10n "label-audio-file")
    \useResult -> sink $ EventIO do
      withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |] \picker -> do
        withUTF8 (l10n "file-picker-load-dryvox-source-audio") \s -> [C.block| void {
          $(Fl_Native_File_Chooser* picker)->title($(char* s));
          $(Fl_Native_File_Chooser* picker)->filter("*.{wav,ogg,mp3,flac}");
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          peekUTF8 cstr >>= useResult
  dryvoxButton tab.dvClipped (l10n "misc-button-dryvox-clipped") \trk -> do
    audio <- stackIO pickedAudio
    src <- buildSource' $ Input audio
    return $ clipDryVox (isJust <$> vocalTubes trk) src

-- | The widgets of the GH3 song cache tab: a file-load panel over a row of
-- three task buttons.
data GH3CacheTabWidgets = GH3CacheTabWidgets
  { gcFiles       :: Ptr FileLoadPanel
  , gcButton360   :: Ptr Fl_Button
  , gcButtonPS3   :: Ptr Fl_Button
  , gcButtonRPCS3 :: Ptr Fl_Button
  }

miscPageGH3SongCache
  :: (?loc :: L)
  => (Event -> IO ())
  -> GH3CacheTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageGH3SongCache sink tab startTasks = do
  loadedFiles <- newMVar []
  _ <- fileLoadPanel tab.gcFiles sink (l10nFull "file-type-package" []) (modifyMVar_ loadedFiles) [] searchGH3Cachable \info -> let
    entry = T.pack $ fst info
    sublines = filter (not . T.null) [snd info]
    in (entry, sublines)
  setCallback tab.gcButton360 $ sink $ EventIO do
    inputs <- map fst <$> readMVar loadedFiles
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
      withUTF8 (l10n "file-picker-save-gh3-cache-360") \s ->
        [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* s)); } |]
      case inputs of
        f : _ -> withUTF8 (takeDirectory f) \cdir ->
          [C.block| void { $(Fl_Native_File_Chooser* picker)->directory($(char* cdir)); } |]
        _ -> return ()
      [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file("gh3_custom_cache"); } |]
      result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
      when (result == 0) do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
        f <- peekUTF8 cstr
        sink $ EventOnyx $ startTasks let
          task = do
            combineGH3SongCache360 inputs f
            return [f]
          in [(T.unpack $ l10n "misc-task-make-gh3-cache", task)]
  setCallback tab.gcButtonPS3 $ sink $ EventIO do
    inputs <- map fst <$> readMVar loadedFiles
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
      withUTF8 (l10n "file-picker-save-gh3-cache-ps3") \s ->
        [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* s)); } |]
      case inputs of
        f : _ -> withUTF8 (takeDirectory f) \cdir ->
          [C.block| void { $(Fl_Native_File_Chooser* picker)->directory($(char* cdir)); } |]
        _ -> return ()
      [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file("gh3_custom_cache.pkg"); } |]
      result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
      when (result == 0) do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
        f <- peekUTF8 cstr
        sink $ EventOnyx $ startTasks let
          task = do
            combineGH3SongCachePS3 inputs f
            return [f]
          in [(T.unpack $ l10n "misc-task-make-gh3-cache", task)]
  setCallback tab.gcButtonRPCS3 $ sink $ EventOnyx do
    inputs <- stackIO $ map fst <$> readMVar loadedFiles
    defDir <- getDefaultPS3Dir
    stackIO $ askFolder defDir \dout -> sink $ EventOnyx $ startTasks let
      task = do
        (folder, game) <- combineGH3SongCachePS3Folder inputs
        let gameID = case game of
              GuitarHero3         -> "BLUS30074"
              GuitarHeroAerosmith -> "BLUS30133"
        stackIO $ installPS3Folder gameID (Just "pkg-contents/gh3") (first TE.decodeLatin1 folder) dout
      in [(T.unpack $ l10n "misc-task-install-gh3-cache", task)]

-- | The widgets of the GH:WoR song cache tab. Same shape as
-- 'GH3CacheTabWidgets', but a separate tab in the layout with its own wording.
data WoRCacheTabWidgets = WoRCacheTabWidgets
  { wcFiles       :: Ptr FileLoadPanel
  , wcButton360   :: Ptr Fl_Button
  , wcButtonPS3   :: Ptr Fl_Button
  , wcButtonRPCS3 :: Ptr Fl_Button
  }

miscPageWoRSongCache
  :: (?loc :: L)
  => (Event -> IO ())
  -> WoRCacheTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageWoRSongCache sink tab startTasks = do
  loadedFiles <- newMVar []
  _ <- fileLoadPanel tab.wcFiles sink (l10nFull "file-type-package" []) (modifyMVar_ loadedFiles) [] searchWoRCachable \info -> let
    entry = T.pack $ fst info
    sublines = filter (not . T.null) [snd info]
    in (entry, sublines)
  setCallback tab.wcButton360 $ sink $ EventIO do
    inputs <- map fst <$> readMVar loadedFiles
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
      withUTF8 (l10n "file-picker-save-wor-cache-360") \s ->
        [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* s)); } |]
      case inputs of
        f : _ -> withUTF8 (takeDirectory f) \cdir ->
          [C.block| void { $(Fl_Native_File_Chooser* picker)->directory($(char* cdir)); } |]
        _ -> return ()
      [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file("ghwor_custom_cache"); } |]
      result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
      when (result == 0) do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
        f <- peekUTF8 cstr
        sink $ EventOnyx $ startTasks let
          task = do
            makeMetadataLIVE inputs f
            return [f]
          in [(T.unpack $ l10n "misc-task-make-ghwor-cache", task)]
  setCallback tab.wcButtonPS3 $ sink $ EventIO do
    inputs <- map fst <$> readMVar loadedFiles
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
      withUTF8 (l10n "file-picker-save-wor-cache-ps3") \s ->
        [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* s)); } |]
      case inputs of
        f : _ -> withUTF8 (takeDirectory f) \cdir ->
          [C.block| void { $(Fl_Native_File_Chooser* picker)->directory($(char* cdir)); } |]
        _ -> return ()
      [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file("ghwor_custom_cache.pkg"); } |]
      result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
      when (result == 0) do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
        f <- peekUTF8 cstr
        sink $ EventOnyx $ startTasks let
          task = do
            makeMetadataPKG inputs f
            return [f]
          in [(T.unpack $ l10n "misc-task-make-ghwor-cache", task)]
  setCallback tab.wcButtonRPCS3 $ sink $ EventOnyx do
    inputs <- stackIO $ map fst <$> readMVar loadedFiles
    defDir <- getDefaultPS3Dir
    stackIO $ askFolder defDir \dout -> sink $ EventOnyx $ startTasks let
      task = do
        folder <- getAllMetadata inputs >>= stackIO . metadataPS3Folder
        stackIO $ installPS3Folder "BLUS30487" (Just "pkg-contents/ghwor") (first TE.decodeLatin1 folder) dout
      in [(T.unpack $ l10n "misc-task-install-ghwor-cache", task)]

-- | Widgets for the legacy CON->PKG tab, laid out in fluid/quick_convert.fl.
data CONtoPKGTabWidgets = CONtoPKGTabWidgets
  { cpFiles :: Ptr FileLoadPanel
  , cpRB2   :: Ptr Fl_Button
  , cpRB3   :: Ptr Fl_Button
  }

miscPageCONtoPKG
  :: (?loc :: L)
  => (Event -> IO ())
  -> CONtoPKGTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageCONtoPKG sink widgets startTasks = do
  loadedFiles <- newMVar []
  void $ fileLoadPanel widgets.cpFiles sink (l10nFull "file-type-rb-con-live" []) (modifyMVar_ loadedFiles) [] searchSTFS \info -> let
    entry = T.pack $ stfsPath info
    sublines = take 1 info.stfsMeta.md_DisplayName
    in (entry, sublines)
  let callback isRB3 = sink $ EventIO do
        inputs <- map stfsPath <$> readMVar loadedFiles
        sink $ EventOnyx $ startTasks $ inputs <&> \input -> let
          pkg = input <> ".pkg"
          task = do
            conToPkg isRB3 input pkg
            return [pkg]
          in (input, task)
  setCallback widgets.cpRB2 $ callback False
  setCallback widgets.cpRB3 $ callback True
