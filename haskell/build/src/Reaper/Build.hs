module Reaper.Build where

import           Reaper.Base

import           Control.Monad                    (forM_, when, unless)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Writer
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base64           as B64
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe                       (fromMaybe, listToMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Numeric                          (showHex)
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import           RockBand.Common                  (Key(..))
import qualified RockBand.Vocals                  as Vox
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Message               as Message
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension, takeFileName)

line :: (Monad m) => String -> [String] -> WriterT [Element] m ()
line k atoms = tell [Element k atoms Nothing]

block :: (Monad m) => String -> [String] -> WriterT [Element] m () -> WriterT [Element] m ()
block k atoms sub = do
  sublines <- lift $ execWriterT sub
  tell [Element k atoms $ Just sublines]

rpp :: (Monad m) => String -> [String] -> WriterT [Element] m () -> m Element
rpp k atoms sub = do
  sublines <- execWriterT sub
  return $ Element k atoms $ Just sublines

processTempoTrack :: (NNC.C t) => RTB.T t E.T -> RTB.T t (Meta.Tempo, Maybe (Int, Int))
processTempoTrack = go 500000 . RTB.collectCoincident where
  go tempo rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evts), rtb') -> let
      newTempo = listToMaybe [ t          | E.MetaEvent (Meta.SetTempo t     ) <- evts ]
      newSig   = listToMaybe [ (n, 2 ^ d) | E.MetaEvent (Meta.TimeSig n d _ _) <- evts ]
      in case (newTempo, newSig) of
        (Nothing, Nothing) -> RTB.delay dt $ go tempo rtb'
        (Just tempo', _) -> RTB.cons dt (tempo', newSig) $ go tempo' rtb'
        (Nothing, Just sig) -> RTB.cons dt (tempo, Just sig) $ go tempo rtb'

tempoTrack :: (Monad m) =>
  ATB.T U.Seconds (Meta.Tempo, Maybe (Int, Int)) -> WriterT [Element] m ()
tempoTrack trk = block "TEMPOENVEX" [] $ do
  forM_ (ATB.toPairList trk) $ \(posn, (uspqn, tsig)) -> do
    let secs, bpm :: Double
        secs = realToFrac posn
        bpm = 60000000 / fromIntegral uspqn
    line "PT" $ [show secs, show bpm, "1"] ++ case tsig of
      Nothing           -> []
      Just (num, denom) -> [show $ num + denom * 0x10000, "0", "1"]

event :: (Monad m) => Int -> E.T -> WriterT [Element] m ()
event tks = \case
  E.MIDIEvent e -> let
    bs = Message.toByteString $ Message.Channel e
    showByte n = case showHex n "" of
      [c] -> ['0', c]
      s   -> s
    in line "E" $ show tks : map showByte (BL.unpack bs)
  E.MetaEvent e -> let
    stringBytes = TE.encodeUtf8 . T.pack
    bytes = B.cons 0xFF $ case e of
      Meta.TextEvent s -> B.cons 1 $ stringBytes s
      Meta.TrackName s -> B.cons 3 $ stringBytes s
      Meta.Lyric s -> B.cons 5 $ stringBytes s
      _ -> error $ "unhandled case in reaper event parser: " ++ show e
    splitChunks bs = if B.length bs <= 40
      then [bs]
      else case B.splitAt 40 bs of
        (x, y) -> x : splitChunks y
    in block "X" [show tks, "0"] $ forM_ (splitChunks bytes) $ \chunk -> do
      line (B8.unpack $ B64.encode chunk) []
  E.SystemExclusive _ -> undefined

track :: (Monad m, NNC.C t, Integral t) => NN.Int -> U.Seconds -> NN.Int -> RTB.T t E.T -> WriterT [Element] m ()
track lenTicks lenSecs resn trk = let
  name = fromMaybe "untitled track" $ U.trackName trk
  in block "TRACK" [] $ do
    line "NAME" [name]
    let yellow = (255, 255, 0)
        green = (0, 255, 0)
        red = (255, 0, 0)
        blue = (0, 0, 255)
        orange = (255, 128, 0)
    case lookup name
      [ ("PART DRUMS", yellow)
      , ("PART GUITAR", blue)
      , ("PART BASS", red)
      , ("PART VOCALS", orange)
      , ("HARM1", orange)
      , ("HARM2", orange)
      , ("HARM3", orange)
      , ("PART KEYS", green)
      , ("PART REAL_KEYS_X", green)
      , ("PART REAL_KEYS_H", green)
      , ("PART REAL_KEYS_M", green)
      , ("PART REAL_KEYS_E", green)
      ] of
      Nothing -> return ()
      Just (r, g, b) -> let
        encoded :: Int
        encoded = 0x1000000 + 0x10000 * b + 0x100 * g + r
        in line "PEAKCOL" [show encoded]
    line "TRACKHEIGHT" ["0", "0"]
    let isProKeys = elem name ["PART REAL_KEYS_E", "PART REAL_KEYS_M", "PART REAL_KEYS_H", "PART REAL_KEYS_X"]
        isVox = elem name ["PART VOCALS", "HARM1", "HARM2", "HARM3"]
        isPitched = isProKeys || isVox
    when isPitched $ line "FX" ["0"]
    case lookup name
      [ ("PART DRUMS", drumNoteNames)
      , ("PART GUITAR", gryboNoteNames False)
      , ("PART BASS", gryboNoteNames False)
      , ("PART KEYS", gryboNoteNames True)
      , ("PART REAL_KEYS_X", proKeysNoteNames)
      , ("PART REAL_KEYS_H", proKeysNoteNames)
      , ("PART REAL_KEYS_M", proKeysNoteNames)
      , ("PART REAL_KEYS_E", proKeysNoteNames)
      , ("BEAT", [(13, "Up Beats"), (12, "Downbeat")])
      , ("PART VOCALS", vocalNoteNames)
      , ("HARM1", vocalNoteNames)
      , ("HARM2", vocalNoteNames)
      , ("HARM3", vocalNoteNames)
      ] of
      Nothing -> return ()
      Just names -> do
        block "MIDINOTENAMES" [] $ do
          forM_ names $ \(pitch, noteName) -> line "-1" [show pitch, noteName]
    block "FXCHAIN" [] $ if isPitched
      then do
        line "SHOW" ["0"]
        line "LASTSEL" ["0"]
        line "DOCKED" ["0"]
        let mutePitches pmin pmax = do
              line "BYPASS" ["0", "0", "0"]
              block "JS" ["IX/MIDI_Tool II", ""] $ do
                line "0.000000" $ show (pmin :: Int) : show (pmax :: Int) : words "0.000000 0.000000 0.000000 100.000000 0.000000 0.000000 127.000000 0.000000 0.000000 1.000000 0.000000 0.000000 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
              line "FLOATPOS" ["0", "0", "0", "0"]
              line "WAK" ["0"]
        if isProKeys
          then do
            mutePitches 0 47
            mutePitches 73 127
            line "BYPASS" ["0", "0", "0"]
            block "VST" ["VSTi: ReaSynth (Cockos)", "reasynth.vst.dylib", "0", "", "1919251321"] $ do
              line "eXNlcu9e7f4AAAAAAgAAAAEAAAAAAAAAAgAAAAAAAAA8AAAAAAAAAAAAEADvvq3eDfCt3qabxDsXt9E6MzMTPwAAAAAAAAAAAACAP+lniD0AAAAAAAAAPwAAgD8AAIA/" []
              line "AACAPwAAgD8AABAAAAA=" [] -- pro keys: tuned up one octave
            line "FLOATPOS" ["0", "0", "0", "0"]
            line "WAK" ["0"]
          else do
            mutePitches 0 35
            mutePitches 85 127
            line "BYPASS" ["0", "0", "0"]
            block "VST" ["VSTi: ReaSynth (Cockos)", "reasynth.vst.dylib", "0", "", "1919251321"] $ do
              line "eXNlcu9e7f4AAAAAAgAAAAEAAAAAAAAAAgAAAAAAAAA8AAAAAAAAAAAAEADvvq3eDfCt3qabxDsXt9E6MzMTPwAAAAAAAAAAAACAP+lniD0AAAAAAAAAPwAAgD8AAIA/" []
              line "AAAAPwAAgD8AABAAAAA=" [] -- vox: normal tuning
            line "FLOATPOS" ["0", "0", "0", "0"]
            line "WAK" ["0"]
      else return () -- not sure why, but you still need empty FXCHAIN so note names work
    block "ITEM" [] $ do
      line "POSITION" ["0"]
      line "LOOP" ["0"]
      line "LENGTH" [show (realToFrac lenSecs :: Double)]
      line "NAME" [name]
      block "SOURCE" ["MIDI"] $ do
        line "HASDATA" ["1", show resn, "QN"]
        forM_ (RTB.toPairList trk) $ \(tks, e) -> event (fromIntegral tks) e
        let lastEvent = case reverse $ ATB.getTimes $ RTB.toAbsoluteEventList 0 trk of
              t : _ -> fromIntegral t
              []    -> 0
        line "E" [show $ lenTicks NNC.-| lastEvent, "b0", "7b", "00"]

audio :: (Monad m) => U.Seconds -> FilePath -> WriterT [Element] m ()
audio len path = let
  name = takeFileName path
  in block "TRACK" [] $ do
    line "NAME" [name]
    block "ITEM" [] $ do
      line "POSITION" ["0"]
      line "LOOP" ["0"]
      line "LENGTH" [show (realToFrac len :: Double)]
      line "NAME" [name]
      let fmt = case map toLower $ takeExtension path of
            ".wav" -> "WAVE"
            ".mp3" -> "MP3"
            ".ogg" -> "VORBIS"
            ".flac" -> "FLAC"
            _ -> error $ "While generating a Reaper project: I don't know the audio format of this file: " ++ show path
      block "SOURCE" [fmt] $ do
        line "FILE" [path]

drumNoteNames :: [(Int, String)]
drumNoteNames = execWriter $ do
  o 127 "Roll Marker 2-Lane"
  o 126 "Roll Marker 1-Lane"
  x 125
  o 124 "DRUM FILL"
  o 123 "DRUM FILL"
  o 122 "DRUM FILL"
  o 121 "DRUM FILL"
  o 120 "DRUM FILL (use all 5)"
  x 118
  o 116 "OVERDRIVE"
  x 114
  o 112 "PRO Green Tom"
  o 111 "PRO Blue Tom"
  o 110 "PRO Yellow Tom"
  x 108
  o 103 "Solo Marker"
  x 102
  o 100 "EXPERT Green"
  o 99 "EXPERT Blue"
  o 98 "EXPERT Yellow"
  o 97 "EXPERT Red"
  o 96 "EXPERT Kick"
  o 95 "PS Left Kick"
  x 92
  o 88 "HARD Green"
  o 87 "HARD Blue"
  o 86 "HARD Yellow"
  o 85 "HARD Red"
  o 84 "HARD Kick"
  x 80
  o 76 "MEDIUM Green"
  o 75 "MEDIUM Blue"
  o 74 "MEDIUM Yellow"
  o 73 "MEDIUM Red"
  o 72 "MEDIUM Kick"
  x 68
  o 64 "EASY Green"
  o 63 "EASY Blue"
  o 62 "EASY Yellow"
  o 61 "EASY Red"
  o 60 "EASY Kick"
  x 56
  o 52 "--DRUM ANIMATION--"
  o 51 "FLOOR TOM RH"
  o 50 "FLOOR TOM LH"
  o 49 "TOM2 RH"
  o 48 "TOM2 LH"
  o 47 "TOM1 RH"
  o 46 "TOM1 LH"
  o 45 "CRASH2 SOFT LH"
  o 44 "CRASH2 HARD LH"
  o 43 "RIDE CYM LH"
  o 42 "RIDE CYM RH"
  o 41 "CRASH1 CHOKE" -- docs incorrectly say CRASH2 CHOKE
  o 40 "CRASH2 CHOKE" -- docs incorrectly say CRASH1 CHOKE
  o 39 "CRASH2 SOFT RH"
  o 38 "CRASH2 HARD RH"
  o 37 "CRASH1 SOFT RH"
  o 36 "CRASH1 HARD RH"
  o 35 "CRASH1 SOFT LH"
  o 34 "CRASH1 HARD LH"
  -- 33 unused
  o 32 "PERCUSSION RH"
  o 31 "HI-HAT RH"
  o 30 "HI-HAT LH"
  o 29 "SNARE SOFT RH"
  o 28 "SNARE SOFT LH"
  o 27 "SNARE HARD RH"
  o 26 "SNARE HARD LH"
  o 25 "HI-HAT OPEN"
  o 24 "KICK RF"
  where o k v = tell [(k, v)]
        x k = tell [(k, "----")]

gryboNoteNames :: Bool -> [(Int, String)]
gryboNoteNames isKeys = execWriter $ do
  o 127 "Trill Marker"
  unless isKeys $ o 126 "Tremolo Marker"
  x 125
  o 124 "BRE"
  o 123 "BRE"
  o 122 "BRE"
  o 121 "BRE"
  o 120 "BRE (use all 5)"
  x 118
  o 116 "OVERDRIVE"
  x 115
  o 103 "Solo Marker"
  o 102 $ if isKeys then "(Keytar) HOPO Off" else "Force HOPO Off"
  o 101 $ if isKeys then "(Keytar) HOPO On"  else "Force HOPO On"
  o 100 "EXPERT Orange"
  o 99 "EXPERT Blue"
  o 98 "EXPERT Yellow"
  o 97 "EXPERT Red"
  o 96 "EXPERT Green"
  x 95
  o 90 $ if isKeys then "(Keytar) HOPO Off" else "Force HOPO Off"
  o 89 $ if isKeys then "(Keytar) HOPO On"  else "Force HOPO On"
  o 88 "HARD Orange"
  o 87 "HARD Blue"
  o 86 "HARD Yellow"
  o 85 "HARD Red"
  o 84 "HARD Green"
  x 77
  o 76 "MEDIUM Orange"
  o 75 "MEDIUM Blue"
  o 74 "MEDIUM Yellow"
  o 73 "MEDIUM Red"
  o 72 "MEDIUM Green"
  x 65
  o 64 "EASY Orange"
  o 63 "EASY Blue"
  o 62 "EASY Yellow"
  o 61 "EASY Red"
  o 60 "EASY Green"
  unless isKeys $ do
    o 59 "Left Hand Highest"
    forM_ [58, 57 .. 41] $ \i -> o i "-"
    o 40 "Left Hand Lowest"
  where o k v = tell [(k, v)]
        x k = tell [(k, "----")]

proKeysNoteNames :: [(Int, String)]
proKeysNoteNames = execWriter $ do
  o 127 "Trill Marker"
  o 126 "Glissando Marker"
  x 125
  o 120 "BRE"
  x 118
  o 116 "OVERDRIVE"
  o 115 "Solo Marker"
  x 114
  o 72 "C3 (highest)"
  o 71 "B2"
  o 70 "A#2"
  o 69 "A2"
  o 68 "G#2"
  o 67 "G2"
  o 66 "F#2"
  o 65 "F2"
  o 64 "E2"
  o 63 "D#2"
  o 62 "D2"
  o 61 "C#2"
  o 60 "C2"
  o 59 "B1"
  o 58 "A#1"
  o 57 "A1"
  o 56 "G#1"
  o 55 "G1"
  o 54 "F#1"
  o 53 "F1"
  o 52 "E1"
  o 51 "D#1"
  o 50 "D1"
  o 49 "C#1"
  o 48 "C1 (lowest)"
  x 12
  o 9 "Range A1 to C3"
  o 7 "Range G1 to B2"
  o 5 "Range F1 to A2"
  o 4 "Range E1 to G2"
  o 2 "Range D1 to F2"
  o 0 "Range C1 to E2"
  where o k v = tell [(k, v)]
        x k = tell [(k, "----")]

vocalNoteNames :: [(Int, String)]
vocalNoteNames = execWriter $ do
  o 116 "OVERDRIVE"
  o 106 "Phrase (Face-Off P2)"
  o 105 "Phrase"
  x 104
  o 97 "Percussion Sound"
  o 96 "Percussion"
  x 85
  forM_ [maxBound, pred maxBound .. minBound] $ \voxpitch -> do
    let midpitch = fromEnum voxpitch + 36
        str = case voxpitch of
          Vox.Octave36 C -> "C (lowest)"
          Vox.Octave36 p -> showPitch p
          Vox.Octave48 p -> showPitch p
          Vox.Octave60 p -> showPitch p
          Vox.Octave72 p -> showPitch p
          Vox.Octave84C -> "C (highest) (bugged)"
        showPitch = map (\case 's' -> '#'; c -> c) . show
    o midpitch str
  x 35
  o 1 "Lyric Shift"
  o 0 "Range Shift"
  where o k v = tell [(k, v)]
        x k = tell [(k, "----")]
