{-# LANGUAGE LambdaCase #-}
module Reaper.Build where

import Reaper.Base

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)
import qualified Sound.MIDI.Message as Message
import Numeric (showHex)
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Control.Monad (forM_)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Sound.MIDI.Util as U
import qualified Data.ByteString.Base64 as B64
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NNC
import Data.Maybe (fromMaybe, listToMaybe)
import System.FilePath (takeExtension, takeFileName, dropExtension)
import Data.Char (toLower)

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

track :: (Monad m, NNC.C t, Integral t) => U.Seconds -> NN.Int -> RTB.T t E.T -> WriterT [Element] m ()
track len resn trk = let
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
      , ("PART GUITAR", green)
      , ("PART BASS", red)
      , ("PART VOCALS", blue)
      , ("HARM1", blue)
      , ("HARM2", blue)
      , ("HARM3", blue)
      , ("PART KEYS", orange)
      , ("PART REAL_KEYS_X", orange)
      , ("PART REAL_KEYS_H", orange)
      , ("PART REAL_KEYS_M", orange)
      , ("PART REAL_KEYS_E", orange)
      ] of
      Nothing -> return ()
      Just (r, g, b) -> let
        encoded :: Int
        encoded = 0x1000000 + 0x10000 * b + 0x100 * g + r
        in line "PEAKCOL" [show encoded]
    line "TRACKHEIGHT" ["0", "0"]
    case lookup name
      [ ("PART DRUMS", drumNoteNames)
      ] of
      Nothing -> return ()
      Just names -> do
        block "MIDINOTENAMES" [] $ do
          forM_ names $ \(pitch, noteName) -> line "-1" [show pitch, noteName]
    block "FXCHAIN" [] $ return () -- not sure why, but this is needed for note names
    block "ITEM" [] $ do
      line "POSITION" ["0"]
      line "LOOP" ["0"]
      line "LENGTH" [show (realToFrac len :: Double)]
      line "NAME" [name]
      block "SOURCE" ["MIDI"] $ do
        line "HASDATA" ["1", show resn, "QN"]
        forM_ (RTB.toPairList trk) $ \(tks, e) -> event (fromIntegral tks) e
        line "E" $ words "99999999 b0 7b 00"

audio :: (Monad m) => U.Seconds -> FilePath -> WriterT [Element] m ()
audio len path = let
  name = takeFileName $ dropExtension path
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
  o 120 "DRUM FILL (use all 5"
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
