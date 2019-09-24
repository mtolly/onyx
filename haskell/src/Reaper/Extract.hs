{-# LANGUAGE LambdaCase #-}
module Reaper.Extract where

import           Control.Monad                         (guard)
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Base64                as B64
import qualified Data.ByteString.Char8                 as B8
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.EventList.Absolute.TimeBody      as ATB
import qualified Data.EventList.Relative.TimeBody      as RTB
import           Data.Maybe                            (fromMaybe, mapMaybe)
import           Numeric                               (readHex)
import qualified Numeric.NonNegative.Wrapper           as NN
import           Reaper.Base
import qualified RockBand.Codec.File                   as RBFile
import qualified Sound.MIDI.File.Event                 as E
import qualified Sound.MIDI.File.Event.Meta            as Meta
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.Message                    as Message
import qualified Sound.MIDI.Parser.Report              as Report
import qualified Sound.MIDI.Util                       as U
import           Text.Read                             (readMaybe)

getChildren :: Element -> [Element]
getChildren (Element _ _ children) = fromMaybe [] children

args :: Element -> [String]
args (Element _ xs _) = xs

isNamed :: String -> Element -> Bool
isNamed x (Element k _ _) = k == x

getProject :: Element -> [Element]
getProject e = if isNamed "REAPER_PROJECT" e
  then getChildren e
  else error "getProject: couldn't detect Reaper project"

getTiming :: Element -> ATB.T U.Seconds (U.BPS, Maybe U.TimeSig)
getTiming e = case getProject e of
  children -> case filter (isNamed "TEMPOENVEX") children of
    [env] -> let
      evts = filter (isNamed "PT") $ getChildren env
      readDouble :: String -> Double
      readDouble = read
      in ATB.fromPairList $ flip map evts $ \pt -> let
        (secs, bpm) = case args pt of
          secsArg : bpmArg : _ -> (realToFrac $ readDouble secsArg, realToFrac (readDouble bpmArg) / 60)
          _ -> error "getTiming: couldn't parse a PT event"
        tsig = case args pt of
          _ : _ : _ : tsigArg : _ -> Just $ let
            (d, n) = quotRem (read tsigArg :: Integer) 0x10000
            unit = 4 / realToFrac d
            in U.TimeSig (realToFrac n * unit) unit
          _ -> Nothing
        in (secs, (bpm, tsig))
    [] -> ATB.empty
    _ -> error "getTiming: more than 1 TEMPOENVEX chunk found"

timingToMaps :: ATB.T U.Seconds (U.BPS, Maybe U.TimeSig) -> (U.TempoMap, U.MeasureMap)
timingToMaps timing = let
  rel = RTB.fromAbsoluteEventList timing
  tmap = U.tempoMapFromSecondsBPS $ fmap fst rel
  unaligned = U.unapplyTempoTrack tmap $ RTB.mapMaybe snd rel
  resolution = 480
  -- The time signatures can get slightly off due to the real time representation,
  -- so we round them based on a default resolution
  aligned
    = RTB.mapTime (\i -> fromIntegral (i :: NN.Integer) / resolution)
    $ RTB.discretize
    $ RTB.mapTime (* resolution) unaligned
  mmap = U.measureMapFromTimeSigs U.Truncate aligned
  in (tmap, mmap)

getTracks :: Element -> [RTB.T U.Beats E.T]
getTracks proj = let
  children = getProject proj
  trks = filter (isNamed "TRACK") children
  in filter (not . RTB.null) $ flip map trks $ \trk -> let
    -- TODO track mute
    items = filter (isNamed "ITEM") $ getChildren trk
    in foldr RTB.merge RTB.empty $ flip concatMap items $ \item -> do
      -- TODO item mute, item position
      src <- getChildren item
      guard $ isNamed "SOURCE" src
      guard $ take 1 (args src) == ["MIDI"]
      let srcLines = getChildren src
          isResolution = \case
            Element "HASDATA" ["1", res, "QN"] Nothing -> readMaybe res
            _ -> Nothing
      resolution <- take 1 $ mapMaybe isResolution srcLines :: [Integer]
      let unhex s = case readHex s of
            [(n, "")] -> Just n
            _         -> Nothing
          pairs :: [(Integer, E.T)]
          pairs = flip mapMaybe srcLines $ \case
            Element etype (tks : bytes) Nothing | etype == "E" || etype == "e" -> do
              tks' <- readMaybe tks
              bytes' <- mapM unhex bytes
              case Message.maybeFromByteString $ BL.pack bytes' of
                Report.Cons _ (Right (Message.Channel c))
                  -> Just (tks', E.MIDIEvent c)
                -- don't think we need to support sysex messages here
                _                       -> Nothing
            Element "X" (tks : _) (Just xdata) -> do
              tks' <- readMaybe tks
              let bytes = B.concat $ map (B64.decodeLenient . B8.pack)
                    [ b64 | Element b64 [] Nothing <- xdata ]
                  text = B8.unpack $ B.drop 2 bytes
              e <- case B.unpack bytes of
                0xFF : 0x01 : _ -> Just $ E.MetaEvent $ Meta.TextEvent      text
                0xFF : 0x02 : _ -> Just $ E.MetaEvent $ Meta.Copyright      text
                0xFF : 0x03 : _ -> Just $ E.MetaEvent $ Meta.TrackName      text
                0xFF : 0x04 : _ -> Just $ E.MetaEvent $ Meta.InstrumentName text
                0xFF : 0x05 : _ -> Just $ E.MetaEvent $ Meta.Lyric          text
                0xFF : 0x06 : _ -> Just $ E.MetaEvent $ Meta.Marker         text
                0xFF : 0x07 : _ -> Just $ E.MetaEvent $ Meta.CuePoint       text
                0xF0 : bin      -> Just $ E.SystemExclusive $ SysEx.Regular bin
                _               -> Nothing
              return (tks', e)
            _ -> Nothing
      return $ RTB.mapTime (\tks -> fromIntegral tks / fromIntegral resolution) $ RTB.fromPairList pairs

getMIDI :: Element -> RBFile.Song [RTB.T U.Beats E.T]
getMIDI elt = let
  (tmap, mmap) = timingToMaps $ getTiming elt
  trks = getTracks elt
  in RBFile.Song tmap mmap trks
