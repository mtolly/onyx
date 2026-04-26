{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI.Misc where

import           Control.Applicative          ((<|>))
import           Control.Concurrent           (modifyMVar_, newMVar, readMVar)
import           Control.Monad                (forM, guard, join, when)
import           Data.Bifunctor               (first)
import           Data.Binary.Put              (runPut)
import qualified Data.ByteString              as B
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

miscPageMilo
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMilo sink rect tab startTasks = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    btn <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Extract .milo to folder") } |]
    [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
    setCallback btn $ withNativeFileChooser
      [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |]
      \picker1 -> do
        [C.block| void {
          $(Fl_Native_File_Chooser* picker1)->title("Load .milo");
          $(Fl_Native_File_Chooser* picker1)->filter("*.{milo_xbox,milo_ps3,milo_wii}");
        } |]
        result1 <- [C.exp| int { $(Fl_Native_File_Chooser* picker1)->show() } |]
        when (result1 == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker1)->filename() } |]
          fin <- peekUTF8 cstr
          withNativeFileChooser
            [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |]
            \picker2 -> do
            [C.block| void { $(Fl_Native_File_Chooser* picker2)->title("Create folder"); } |]
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
                in startTasks [("Extract " <> fin, task)]
  padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
    btn <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Repack .milo from folder") } |]
    [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
    setCallback btn $ askFolder Nothing \din -> do
      let dinText = T.pack din
          presetFile = case T.stripSuffix "-extract" dinText of
            Just stripped -> stripped
            Nothing       -> dinText <> ".milo"
      withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \miloPicker -> do
        [C.block| void {
          $(Fl_Native_File_Chooser* miloPicker)->title("Save .milo");
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
            in startTasks [("Repack " <> din, task)]
  [C.block| void {
    $(Fl_Pack* pack)->end();
    $(Fl_Group* tab)->resizable($(Fl_Pack* pack));
  } |]

miscPageLipsync
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageLipsync sink rect tab startTasks = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  pickedFile <- padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- withRect inputRect \ix iy iw ih ->
      [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
    [C.block| void {
      $(Fl_Input* input)->labelsize(13);
      $(Fl_Input* input)->align(FL_ALIGN_LEFT);
    } |]
    [C.block| void { $(Fl_Input* input)->label("MIDI file"); } |]
    pick <- withRect pickRect \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@fileopen") } |]
    setCallback pick $ sink $ EventIO do
      withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |] \picker -> do
        [C.block| void {
          $(Fl_Native_File_Chooser* picker)->title("Load MIDI file");
          $(Fl_Native_File_Chooser* picker)->filter("*.{mid,midi}"); // TODO also handle .chart?
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          f <- peekUTF8 cstr
          withUTF8 (f :: B.ByteString) \cf ->
            [C.block| void { $(Fl_Input* input)->value($(char* cf)); } |]
    return do
      cstr <- [C.exp| const char* { $(Fl_Input* input)->value() } |]
      peekUTF8 cstr
  (getVowels, getTransition) <- padded 2 10 2 10 (Size (Width 800) (Height 35)) \rect' -> do
    let (trimClock 0 100 0 0 -> langArea, transArea) = chopRight 150 rect'
    getVowels <- horizRadio langArea
      [ ("English", englishSyllables, True)
      , ("German", germanVowels, False)
      , ("Spanish", spanishVowels, False)
      ]
    counter <- withRect transArea \cx cy cw ch ->
      [C.exp| Fl_Counter* { new Fl_Counter($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    [C.block| void {
      $(Fl_Counter* counter)->labelsize(13);
      $(Fl_Counter* counter)->align(FL_ALIGN_LEFT);
      $(Fl_Counter* counter)->step(1);
      $(Fl_Counter* counter)->lstep(5);
      $(Fl_Counter* counter)->minimum(0);
    } |]
    [C.block| void { $(Fl_Counter* counter)->label("Transition (ms)"); } |]
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
          [C.block| void {
            $(Fl_Native_File_Chooser* picker)->title("Save .voc file");
            $(Fl_Native_File_Chooser* picker)->filter("*.voc");
          } |]
          withUTF8 (dropExtension input <.> "voc") \cpreset ->
            [C.block| void { $(Fl_Native_File_Chooser* picker)->preset_file($(char* cpreset)); } |]
          result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
          when (result == 0) do
            cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
            fout <- peekUTF8 cstr
            go fout

  padded 10 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    btn <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Make GH2 .voc from PART VOCALS") } |]
    [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
    setCallback btn $ sink $ EventIO do
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
        in startTasks [("Make GH2 .voc: " <> voc, task)]

  let pickMilo go = do
        withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |] \picker -> do
          [C.block| void {
            $(Fl_Native_File_Chooser* picker)->title("Load Milo file");
            $(Fl_Native_File_Chooser* picker)->filter("*.{milo_xbox,milo_ps3,milo_wii}");
          } |]
          result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
          when (result == 0) do
            cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
            f <- peekUTF8 cstr
            go f
      saveMilo input go = do
        withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
          [C.block| void {
            $(Fl_Native_File_Chooser* picker)->title("Save Milo file");
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
  padded 10 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    let [trimClock 0 5 0 0 -> makeArea, trimClock 0 5 0 5 -> updateArea, trimClock 0 0 0 5 -> rawArea] = splitHorizN 3 rect'
    makeButton <- withRect makeArea \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Make RB3 .milo_*") } |]
    [C.block| void { $(Fl_Button* makeButton)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* makeButton)->tooltip(
      "Create a RB3 .milo_* file from the first present out of: LIPSYNC* tracks, HARM* tracks, or PART VOCALS."
    ); } |]
    setCallback makeButton $ sink $ EventIO do
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
        in startTasks [("Create .milo from MIDI lipsync: " <> milo, task)]
    updateButton <- withRect updateArea \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Update RB3/TBRB .milo_*") } |]
    [C.block| void { $(Fl_Button* updateButton)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* updateButton)->tooltip(
      "Update a RB3 .milo_* file from LIPSYNC* tracks, HARM* tracks, or PART VOCALS. Or, update a TBRB .milo_* file from LIPSYNC_[beatle] tracks."
    ); } |]
    setCallback updateButton $ sink $ EventIO do
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
                { miloFiles = do
                  ((_, name), contents) <- zip dir.miloEntryNames dir.miloFiles
                  return case name of
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
                , miloSubdirs = map editDir dir.miloSubdirs
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
        in startTasks [("Update .milo with MIDI lipsync: " <> milo, task)]
    rawButton <- withRect rawArea \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Create *.lipsync") } |]
    [C.block| void { $(Fl_Button* rawButton)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* rawButton)->tooltip(
      "Export all LIPSYNC* and LIPSYNC_[beatle] tracks to .lipsync files, ready to be inserted into .milo_* files."
    ); } |]
    setCallback rawButton $ sink $ EventIO do
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
        in startTasks [("Export .lipsync to folder", task)]

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
          in startTasks [("Convert vocal tracks to LIPSYNC tracks: " <> input, task)]

  padded 10 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    btn <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Turn vocal tracks into LIPSYNC* tracks for RB3") } |]
    [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn)->tooltip(
      "Uses HARM* or PART VOCALS to produce LIPSYNC{1-3} tracks containing viseme information, using the standard RB viseme set."
    ); } |]
    setCallback btn $ sink $ EventIO $ makeLipsyncTracks loadVisemesRB3 autoLipsync'

  padded 10 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    btn <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Turn vocal tracks into LIPSYNC* tracks for TBRB") } |]
    [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
    [C.block| void { $(Fl_Button* btn)->tooltip(
      "Uses HARM* or PART VOCALS to produce LIPSYNC{1-3} tracks containing viseme information, using the Beatles RB viseme set."
    ); } |]
    setCallback btn $ sink $ EventIO $ makeLipsyncTracks loadVisemesTBRB beatlesLipsync'

  [C.block| void {
    $(Fl_Pack* pack)->end();
    $(Fl_Group* tab)->resizable($(Fl_Pack* pack));
  } |]

miscPageDryVox
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageDryVox sink rect tab startTasks = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  pickedFile <- padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- withRect inputRect \ix iy iw ih ->
      [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
    [C.block| void {
      $(Fl_Input* input)->labelsize(13);
      $(Fl_Input* input)->align(FL_ALIGN_LEFT);
    } |]
    [C.block| void { $(Fl_Input* input)->label("MIDI file"); } |]
    pick <- withRect pickRect \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@fileopen") } |]
    setCallback pick $ sink $ EventIO do
      withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |] \picker -> do
        [C.block| void {
          $(Fl_Native_File_Chooser* picker)->title("Load MIDI file");
          $(Fl_Native_File_Chooser* picker)->filter("*.{mid,midi}"); // TODO also handle .chart?
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          f <- peekUTF8 cstr
          withUTF8 (f :: B.ByteString) \cf ->
            [C.block| void { $(Fl_Input* input)->value($(char* cf)); } |]
    return do
      cstr <- [C.exp| const char* { $(Fl_Input* input)->value() } |]
      peekUTF8 cstr
  getVocalTrack <- padded 2 10 2 10 (Size (Width 800) (Height 35)) \rect' -> do
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
  let dryvoxButton label fn = padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
        btn <- withRect rect' \bx by bw bh ->
          [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
        withUTF8 (label :: T.Text) \clbl ->
          [C.block| void {
            $(Fl_Button* btn)->copy_label($(char* clbl));
            $(Fl_Button* btn)->color($(Fl_Color taskColor));
          } |]
        setCallback btn $ sink $ EventIO do
          input <- pickedFile
          voc <- getVocalTrack
          withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
            [C.block| void {
              $(Fl_Native_File_Chooser* picker)->title("Save lipsync audio");
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
                in startTasks [(T.unpack label <> ": " <> input, task)]
  dryvoxButton "Make sine wave dry vox from MIDI" $ return . sineDryVox
  pickedAudio <- padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- withRect inputRect \ix iy iw ih ->
      [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
    [C.block| void {
      $(Fl_Input* input)->labelsize(13);
      $(Fl_Input* input)->align(FL_ALIGN_LEFT);
    } |]
    [C.block| void { $(Fl_Input* input)->label("Audio file"); } |]
    pick <- withRect pickRect \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@fileopen") } |]
    setCallback pick $ sink $ EventIO do
      withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |] \picker -> do
        [C.block| void {
          $(Fl_Native_File_Chooser* picker)->title("Load song or vocals audio");
          $(Fl_Native_File_Chooser* picker)->filter("*.{wav,ogg,mp3,flac}");
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          f <- peekUTF8 cstr
          withUTF8 (f :: B.ByteString) \cf ->
            [C.block| void { $(Fl_Input* input)->value($(char* cf)); } |]
    return do
      cstr <- [C.exp| const char* { $(Fl_Input* input)->value() } |]
      peekUTF8 cstr
  dryvoxButton "Make clipped dry vox from MIDI and audio" \trk -> do
    audio <- stackIO pickedAudio
    src <- buildSource' $ Input audio
    return $ clipDryVox (isJust <$> vocalTubes trk) src
  [C.block| void {
    $(Fl_Pack* pack)->end();
    $(Fl_Group* tab)->resizable($(Fl_Pack* pack));
  } |]

miscPageGH3SongCache
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageGH3SongCache sink rect tab startTasks = do
  loadedFiles <- newMVar []
  let (filesRect, startRect) = chopBottom 50 rect
      [chopRight 5 -> (xboxRect, _), trimClock 0 5 0 5 -> ps3Rect, chopLeft 5 -> (_, looseRect)] = splitHorizN 3 $ trimClock 5 10 10 10 startRect
  group <- fileLoadWindow filesRect sink "Package" "Packages" (modifyMVar_ loadedFiles) [] searchGH3Cachable \info -> let
    entry = T.pack $ fst info
    sublines = filter (not . T.null) [snd info]
    in (entry, sublines)
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* group)); } |]
  btn1 <- withRect xboxRect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Create GH3 cache (360)") } |]
  [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
  setCallback btn1 $ sink $ EventIO do
    inputs <- map fst <$> readMVar loadedFiles
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
      [C.block| void { $(Fl_Native_File_Chooser* picker)->title("Save GH3 cache file (360)"); } |]
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
          in [("Make GH3 cache file", task)]
  btn2 <- withRect ps3Rect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Create GH3 cache (PS3)") } |]
  [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  setCallback btn2 $ sink $ EventIO do
    inputs <- map fst <$> readMVar loadedFiles
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
      [C.block| void { $(Fl_Native_File_Chooser* picker)->title("Save GH3 cache file (PS3)"); } |]
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
          in [("Make GH3 cache file", task)]
  btn3 <- withRect looseRect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Install GH3 cache (RPCS3)") } |]
  [C.block| void { $(Fl_Button* btn3)->color($(Fl_Color taskColor)); } |]
  setCallback btn3 $ sink $ EventOnyx do
    inputs <- stackIO $ map fst <$> readMVar loadedFiles
    defDir <- getDefaultPS3Dir
    stackIO $ askFolder defDir \dout -> sink $ EventOnyx $ startTasks let
      task = do
        (folder, game) <- combineGH3SongCachePS3Folder inputs
        let gameID = case game of
              GuitarHero3         -> "BLUS30074"
              GuitarHeroAerosmith -> "BLUS30133"
        stackIO $ installPS3Folder gameID (Just "pkg-contents/gh3") (first TE.decodeLatin1 folder) dout
      in [("Install GH3 cache file", task)]

miscPageWoRSongCache
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageWoRSongCache sink rect tab startTasks = do
  loadedFiles <- newMVar []
  let (filesRect, startRect) = chopBottom 50 rect
      [chopRight 5 -> (xboxRect, _), trimClock 0 5 0 5 -> ps3Rect, chopLeft 5 -> (_, looseRect)] = splitHorizN 3 $ trimClock 5 10 10 10 startRect
  group <- fileLoadWindow filesRect sink "Package" "Packages" (modifyMVar_ loadedFiles) [] searchWoRCachable \info -> let
    entry = T.pack $ fst info
    sublines = filter (not . T.null) [snd info]
    in (entry, sublines)
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* group)); } |]
  btn1 <- withRect xboxRect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Create GH:WoR cache (360)") } |]
  [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
  setCallback btn1 $ sink $ EventIO do
    inputs <- map fst <$> readMVar loadedFiles
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
      [C.block| void { $(Fl_Native_File_Chooser* picker)->title("Save WoR cache file (360)"); } |]
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
          in [("Make WoR cache file", task)]
  btn2 <- withRect ps3Rect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Create GH:WoR cache (PS3)") } |]
  [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  setCallback btn2 $ sink $ EventIO do
    inputs <- map fst <$> readMVar loadedFiles
    withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
      [C.block| void { $(Fl_Native_File_Chooser* picker)->title("Save WoR cache file (PS3)"); } |]
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
          in [("Make WoR cache file", task)]
  btn3 <- withRect looseRect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Install GH:WoR cache (RPCS3)") } |]
  [C.block| void { $(Fl_Button* btn3)->color($(Fl_Color taskColor)); } |]
  setCallback btn3 $ sink $ EventOnyx do
    inputs <- stackIO $ map fst <$> readMVar loadedFiles
    defDir <- getDefaultPS3Dir
    stackIO $ askFolder defDir \dout -> sink $ EventOnyx $ startTasks let
      task = do
        folder <- getAllMetadata inputs >>= stackIO . metadataPS3Folder
        stackIO $ installPS3Folder "BLUS30487" (Just "pkg-contents/ghwor") (first TE.decodeLatin1 folder) dout
      in [("Install GH3 cache file", task)]

miscPageCONtoPKG
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageCONtoPKG sink rect tab startTasks = do
  loadedFiles <- newMVar []
  let (filesRect, startRect) = chopBottom 50 rect
      [chopRight 5 -> (rb2Rect, _), chopLeft 5 -> (_, rb3Rect)] = splitHorizN 2 $ trimClock 5 10 10 10 startRect
  group <- fileLoadWindow filesRect sink "Rock Band CON/LIVE" "Rock Band CON/LIVE" (modifyMVar_ loadedFiles) [] searchSTFS \info -> let
    entry = T.pack $ stfsPath info
    sublines = take 1 info.stfsMeta.md_DisplayName
    in (entry, sublines)
  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* group)); } |]
  let callback isRB3 = sink $ EventIO do
        inputs <- map stfsPath <$> readMVar loadedFiles
        sink $ EventOnyx $ startTasks $ inputs <&> \input -> let
          pkg = input <> ".pkg"
          task = do
            conToPkg isRB3 input pkg
            return [pkg]
          in (input, task)
  btn1 <- withRect rb2Rect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Convert to PKG (RB2)") } |]
  [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
  setCallback btn1 $ callback False
  btn2 <- withRect rb3Rect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Convert to PKG (RB3)") } |]
  [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
  setCallback btn2 $ callback True
