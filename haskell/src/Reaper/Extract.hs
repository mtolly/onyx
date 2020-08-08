{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaper.Extract where

import           Control.Monad.Extra                   (forM, mapMaybeM)
import           Control.Monad.Trans.StackTrace        (SendMessage,
                                                        StackTraceT, fatal,
                                                        inside, warn)
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Base64                as B64
import qualified Data.ByteString.Char8                 as B8
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.EventList.Absolute.TimeBody      as ATB
import qualified Data.EventList.Relative.TimeBody      as RTB
import           Data.Maybe                            (fromMaybe, mapMaybe)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import           Numeric                               (readHex)
import qualified Numeric.NonNegative.Wrapper           as NN
import           Reaper.Base
import qualified RockBand.Codec.File                   as RBFile
import qualified Sound.MIDI.File.Event                 as E
import qualified Sound.MIDI.File.Event.Meta            as Meta
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.Message                    as Message
import qualified Sound.MIDI.Message.Channel            as C
import qualified Sound.MIDI.Message.Channel.Mode       as Mode
import qualified Sound.MIDI.Parser.Report              as Report
import qualified Sound.MIDI.Util                       as U
import           Text.Read                             (readMaybe)

getChildren :: Element -> [Element]
getChildren (Element _ _ children) = fromMaybe [] children

args :: Element -> [T.Text]
args (Element _ xs _) = xs

isNamed :: T.Text -> Element -> Bool
isNamed x (Element k _ _) = k == x

getProject :: Element -> [Element]
getProject e = if isNamed "REAPER_PROJECT" e
  then getChildren e
  else error "getProject: couldn't detect Reaper project"

getTiming :: (Monad m) => Element -> StackTraceT m (ATB.T U.Seconds (U.BPS, Maybe U.TimeSig))
getTiming e = case getProject e of
  children -> case filter (isNamed "TEMPOENVEX") children of
    [env] -> let
      evts = filter (isNamed "PT") $ getChildren env
      readDouble :: T.Text -> Double
      readDouble = read . T.unpack -- TODO handle read failure
      in fmap ATB.fromPairList $ forM evts $ \pt -> do
        (secs, bpm) <- case args pt of
          secsArg : bpmArg : _ -> return (realToFrac $ readDouble secsArg, realToFrac (readDouble bpmArg) / 60)
          _ -> fatal "getTiming: couldn't parse a PT event"
        let tsig = case args pt of
              _ : _ : _ : tsigArg : _ -> Just $ let
                (d, n) = quotRem (read $ T.unpack tsigArg :: Integer) 0x10000 -- TODO handle read failure
                unit = 4 / realToFrac d
                in U.TimeSig (realToFrac n * unit) unit
              _ -> Nothing
        return (secs, (bpm, tsig))
    [] -> return ATB.empty
    _ -> fatal "getTiming: more than 1 TEMPOENVEX chunk found"

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

parseEvent :: (SendMessage m) => Element -> StackTraceT m (Maybe (Integer, Maybe E.T))
parseEvent elt@(Element etype eparams blk) = inside ("RPP line: " <> show elt) $ do
  let unhex s = case readHex $ T.unpack s of
        [(n, "")] -> Just n
        _         -> Nothing
  case T.toUpper etype of
    -- lowercase event type just means the event is selected
    "E" -> do
      case blk of
        Just _  -> warn "E event has unexpected indented block"
        Nothing -> return ()
      case eparams of
        [] -> warn "E event has no parameters" >> return Nothing
        tks : bytes -> case readMaybe $ T.unpack tks of
          Nothing -> warn "Couldn't read delta ticks from E event" >> return Nothing
          Just tks' -> case mapM unhex bytes of
            Nothing -> do
              warn "Couldn't read hex bytes from E event"
              return $ Just (tks', Nothing)
            Just bytes' -> case Message.maybeFromByteString $ BL.pack bytes' of
              Report.Cons _ (Right (Message.Channel c)) -> return $ case c of
                C.Cons _ (C.Mode Mode.AllNotesOff) -> Nothing
                _ -> Just (tks', Just $ E.MIDIEvent c)
              -- don't think we need to support sysex messages here
              _ -> do
                warn "Couldn't understand MIDI message in E event"
                return $ Just (tks', Nothing)
    "X" -> case eparams of
      [] -> warn "X event has no parameters" >> return Nothing
      tks : _ -> do
        case readMaybe $ T.unpack tks of
          Nothing -> warn "Couldn't read delta ticks from X event" >> return Nothing
          Just tks' -> case blk of
            Nothing -> warn "X event has no indented block" >> return Nothing
            Just xdata -> do
              let bytes = B.concat $ map (B64.decodeLenient . TE.encodeUtf8)
                    [ b64 | Element b64 [] Nothing <- xdata ]
                  text = B8.unpack $ B.drop 2 bytes
              mevt <- case B.unpack bytes of
                0xFF : 0x01 : _ -> return $ Just $ E.MetaEvent $ Meta.TextEvent      text
                0xFF : 0x02 : _ -> return $ Just $ E.MetaEvent $ Meta.Copyright      text
                0xFF : 0x03 : _ -> return $ Just $ E.MetaEvent $ Meta.TrackName      text
                0xFF : 0x04 : _ -> return $ Just $ E.MetaEvent $ Meta.InstrumentName text
                0xFF : 0x05 : _ -> return $ Just $ E.MetaEvent $ Meta.Lyric          text
                0xFF : 0x06 : _ -> return $ Just $ E.MetaEvent $ Meta.Marker         text
                0xFF : 0x07 : _ -> return $ Just $ E.MetaEvent $ Meta.CuePoint       text
                0xF0 : bin      -> return $ Just $ E.SystemExclusive $ SysEx.Regular bin
                _               -> warn "Couldn't understand MIDI meta/sysex in X event" >> return Nothing
              return $ Just (tks', mevt)
    _ -> return Nothing

getTracks :: (SendMessage m) => Element -> StackTraceT m [RTB.T U.Beats E.T]
getTracks proj = let
  children = getProject proj
  trks = filter (isNamed "TRACK") children
  in fmap (filter $ not . RTB.null) $ forM trks $ \trk -> let
    -- TODO track mute
    items = filter (isNamed "ITEM") $ getChildren trk
    in fmap (foldr RTB.merge RTB.empty . concat) $ forM items $ \item -> do
      -- TODO item mute, item position
      forM (getChildren item) $ \src -> do
        if not $ isNamed "SOURCE" src && take 1 (args src) == ["MIDI"]
          then return RTB.empty
          else let
            srcLines = getChildren src
            isResolution = \case
              Element "HASDATA" ["1", res, "QN"] Nothing -> readMaybe $ T.unpack res :: Maybe Integer
              _                                          -> Nothing
            in case mapMaybe isResolution srcLines of
              []             -> return RTB.empty
              resolution : _ -> do
                pairs <- mapMaybeM parseEvent srcLines
                return
                  $ RTB.catMaybes
                  $ RTB.mapTime (\tks -> fromInteger tks / fromInteger resolution)
                  $ RTB.fromPairList pairs

getMIDI :: (SendMessage m) => Element -> StackTraceT m (RBFile.Song [RTB.T U.Beats E.T])
getMIDI elt = do
  (tmap, mmap) <- timingToMaps <$> getTiming elt
  trks <- getTracks elt
  return $ RBFile.Song tmap mmap trks
