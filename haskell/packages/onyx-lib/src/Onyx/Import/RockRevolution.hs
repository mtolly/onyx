{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.RockRevolution where

import           Control.Concurrent.Async         (forConcurrently)
import           Control.Monad                    (forM, forM_, guard, when)
import           Control.Monad.Codec
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit, toLower)
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd, nubSort, (!?))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, listToMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Onyx.Audio                       (Audio (..))
import           Onyx.Audio.FSB
import           Onyx.Audio.FSB.FEV
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..))
import qualified Onyx.MIDI.Track.Drums.Elite      as ED
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.RockRevolution.MIDI
import           Onyx.Sections                    (simpleSection)
import           Onyx.StackTrace
import           Onyx.Util.Binary                 (runGetM)
import           Onyx.Util.Handle
import           Text.Read                        (readMaybe)

importRR :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> [Import m]
importRR dir = map (importRRSong dir) $ findRRSongKeys dir

findRRSongKeys :: Folder T.Text Readable -> [T.Text]
findRRSongKeys dir = do
  (name, _) <- folderFiles dir
  key <- toList $ T.stripPrefix "s" (T.toLower name) >>= T.stripSuffix ".lua"
  guard $ T.all isDigit key
  return key

importRRSong :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> T.Text -> Import m
importRRSong dir key level = inside ("Rock Revolution song " <> show key) $ do

  when (level == ImportFull) $ lg $ "Importing Rock Revolution song [" <> T.unpack key <> "]"

  let need f = needRead f >>= \r -> stackIO $ useHandle r handleToByteString
      needRead f = case findFileCI (pure f) dir of
        Nothing -> fatal $ "Couldn't locate file: " <> T.unpack f
        Just r  -> return r

  -- these strings appear to be utf-8; see e.g. French_s1002_Strings.bin
  strings <- map TE.decodeUtf8 . B.split 0 . BL.toStrict <$> need ("English_s" <> key <> "_Strings.bin")
  lua <- TE.decodeLatin1 . BL.toStrict <$> need ("s" <> key <> ".lua")

  let luaLines = T.lines lua

      syncHackMS :: Maybe Int
      syncHackMS = listToMaybe $ do
        ln <- luaLines
        sfx <- toList $ T.stripPrefix rrMP3HackString ln
        toList $ readMaybe $ T.unpack sfx

      year :: Maybe Int
      year = listToMaybe
        $ mapMaybe (\case ["Year", "=", n] -> readMaybe $ T.unpack n; _ -> Nothing)
        $ map T.words luaLines

      -- difficulty rating referred to as "stars" in lua but skulls in game
      getSkulls :: T.Text -> Maybe Int
      getSkulls diffKey = listToMaybe $ do
        ln <- luaLines
        braceList <- case T.words ln of
          k : "=" : rest | k == diffKey -> [T.unwords rest]
          _                             -> []
        -- convert the brace list to brackets list and read as haskell [Int]
        nums <- toList $ readMaybe $ T.unpack $ T.map (\case '{' -> '['; '}' -> ']'; c -> c) braceList
        -- just take the last number
        take 1 $ reverse nums
      skullsToTier = \case
        Nothing -> Just $ Tier 0
        Just 0  -> Nothing -- used by onyx export to mark non-present parts
        Just 1  -> Just $ Tier 2
        Just 2  -> Just $ Tier 4
        Just 3  -> Just $ Tier 5
        Just 4  -> Just $ Tier 6
        _       -> Just $ Tier 7

      loadAudio r = do
        fsb <- parseFSB r
        let names = map fsbSongName $ either fsb3Songs fsb4Songs $ fsbHeader fsb
        streams <- splitFSBStreams' fsb
        forM (zip names streams) $ \(streamName, stream) -> do
          (streamData, ext) <- getFSBStreamBytes stream
          return (TE.decodeLatin1 streamName <> "." <> ext, streamData, ext)

  findDef <- case level of
    ImportQuick -> return $ const Nothing
    ImportFull -> do
      fev <- need ("s" <> key <> ".fev") >>= runGetM (codecIn binFEV)
      return $ \eventName -> listToMaybe $ do
        let bsLower = B8.map toLower
        eventGroup <- fev.topLevelEventGroups
        guard $ bsLower eventGroup.name == "stems"
        event <- eventGroup.events
        guard $ bsLower event.name == eventName
        layer <- event.layers
        inst <- layer.soundDefInstances
        soundDef <- case inst.nameOrIndex of
          -- everything on 360
          Left sdefName -> do
            let sdefName' = bsLower sdefName
            filter (\soundDef -> bsLower soundDef.name == sdefName') fev.soundDefs
          -- (later fev version) ps3, dlc and probably disc
          Right sdefIndex -> toList $ fev.soundDefs !? fromIntegral sdefIndex
        [waveform] <- return soundDef.waveforms
        return (T.toCaseFold $ TE.decodeLatin1 waveform.bankName, waveform.indexInBank)
  let guitarDef  = findDef "guitar"
      bassDef    = findDef "bass"
      drumsDef   = findDef "drums"
      backingDef = findDef "backing"
      allFSBNames = nubOrd $ map fst $ catMaybes [guitarDef, bassDef, drumsDef, backingDef]
  allFSB <- forM allFSBNames $ \bankName -> do
    r <- need $ bankName <> ".fsb"
    return (bankName, r)
  loadedFSB <- stackIO $ forConcurrently allFSB $ \(bankName, r) -> do
    streams <- loadAudio r
    return (bankName, streams)
  let getStream (bankName, index) = do
        streamsInFile <- lookup bankName loadedFSB
        streamsInFile !? fromIntegral index
      guitarStream  = guitarDef  >>= getStream
      bassStream    = bassDef    >>= getStream
      drumsStream   = drumsDef   >>= getStream
      backingStream = backingDef >>= getStream

  (controlMidPreHack, control) <- case level of
    ImportQuick -> return (emptyChart, Nothing)
    ImportFull  -> do
      let midName = "s" <> key <> "_control.mid"
      inside ("Loading " <> T.unpack midName) $ do
        mid <- needRead midName >>= F.loadRawMIDIReadable >>= F.readMixedMIDI
        let (parsedControl, unrec) = readRRControl mid.s_tracks
        forM_ (nubSort $ RTB.getBodies unrec) $ \e -> warn $ "Unrecognized MIDI event: " <> show e
        return (mid, Just parsedControl)
  -- undo the sync hack if we did that on exporting a custom song,
  -- by increasing tempo at start of midi to account for mp3 delay
  let controlMid = case syncHackMS of
        Just ms | ms /= 0 -> controlMidPreHack
          { F.s_tempos = applyMIDIOffsetMS (negate ms) controlMidPreHack.s_tempos
          }
        _ -> controlMidPreHack

  let loadGuitarBass inst = case level of
        ImportQuick -> return mempty
        ImportFull  -> do
          fiveDiffs <- forM diffs $ \(num, diff) -> do
            let midName = T.concat ["s", key, "_", inst, "_", num, ".mid"]
            inside ("Loading " <> T.unpack midName) $ do
              mid <- needRead midName >>= F.loadRawMIDIReadable >>= F.readMixedMIDI
              rr <- F.parseTrackReport mid.s_tracks
              return (diff, (rr, importRRGuitarBass rr))
          return mempty
            { Five.fiveDifficulties = fmap snd $ Map.fromList fiveDiffs
            , Five.fiveSolo = maybe RTB.empty (rrfSolo . fst) $ lookup Expert fiveDiffs
            }
      loadDrums = case level of
        ImportQuick -> return mempty
        ImportFull  -> do
          rrDiffs <- fmap Map.fromList $ forM diffs $ \(num, diff) -> do
            let midName = T.concat ["s", key, "_drums_", num, ".mid"]
            inside ("Loading " <> T.unpack midName) $ do
              mid <- needRead midName >>= F.loadRawMIDIReadable >>= F.readMixedMIDI
              rrd <- F.parseTrackReport mid.s_tracks
              return (diff, rrd)
          return
            ( importRRDrums rrDiffs
            , mempty
              { ED.tdDifficulties = fmap importRREliteDrums rrDiffs
              , ED.tdLanes = maybe RTB.empty importRREliteLanes $ Map.lookup Expert rrDiffs
              , ED.tdSolo = maybe RTB.empty rrdSolo $ Map.lookup Expert rrDiffs
              }
            , mempty { ED.tdDifficulties = fmap importRRHiddenDrums rrDiffs }
            )
      diffs = [("02", Easy), ("03", Medium), ("04", Hard), ("05", Expert)]
  guitar <- loadGuitarBass "guitar"
  bass <- loadGuitarBass "bass"
  (drums, eliteDrums, hiddenDrums) <- loadDrums

  return SongYaml
    { metadata = def'
      { title        = Just $ T.strip $ strings !! 1
      , artist       = Just $ T.strip $ strings !! 0
      , album        = Nothing
      , year         = year
      , comments     = []
      , fileAlbumArt = Nothing
      }
    , jammit = mempty
    , targets = HM.empty
    , global = def'
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart $ case level of
        ImportFull  -> controlMid
          { F.s_tracks = mempty
            { F.onyxParts = Map.fromList
              [ (F.FlexGuitar, mempty
                { F.onyxPartGuitar = guitar
                })
              , (F.FlexBass, mempty
                { F.onyxPartGuitar = bass
                })
              , (F.FlexDrums, mempty
                { F.onyxPartDrums = drums
                , F.onyxPartEliteDrums = eliteDrums
                })
              , (F.FlexExtra "hidden-drums", mempty
                { F.onyxPartEliteDrums = hiddenDrums
                })
              ]
            , F.onyxEvents = mempty
              { eventsSections = maybe RTB.empty (fmap simpleSection . importRRSections strings . rrcSections) control
              , eventsEnd = maybe RTB.empty rrcEnd $ control
              }
            }
          }
        ImportQuick -> emptyChart
      , fileSongAnim = Nothing
      }
    , audio = HM.fromList $ do
      (name, bs, _) <- catMaybes [guitarStream, bassStream, drumsStream, backingStream]
      let str = T.unpack name
      return (name, AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , filePath = Just $ SoftFile str $ SoftReadable
          $ makeHandle str $ byteStringSimpleHandle bs
        , commands = []
        , rate = Nothing
        , channels = 2 -- TODO maybe verify
        })
    , plans = HM.singleton "rr" $ StandardPlan StandardPlanInfo
      { song = (\(s, _, _) -> Input $ Named s) <$> backingStream
      , parts = Parts $ HM.fromList $ catMaybes
        [ flip fmap guitarStream $ \(s, _, _) -> (F.FlexGuitar, PartSingle $ Input $ Named s)
        , flip fmap bassStream   $ \(s, _, _) -> (F.FlexBass  , PartSingle $ Input $ Named s)
        , flip fmap drumsStream  $ \(s, _, _) -> (F.FlexDrums , PartSingle $ Input $ Named s)
        ]
      , crowd = Nothing
      , comments = []
      , tuningCents = 0
      , fileTempo = Nothing
      }
    , parts = Parts $ HM.fromList $ concat
      [ case skullsToTier $ getSkulls "GuitarDifficulty" of
        Just tier -> [(F.FlexGuitar, emptyPart
          { grybo = Just (def :: PartGRYBO)
            { difficulty = tier
            }
          })]
        Nothing -> []
      , case skullsToTier $ getSkulls "BassDifficulty" of
        Just tier -> [(F.FlexBass, emptyPart
          { grybo = Just (def :: PartGRYBO)
            { difficulty = tier
            }
          })]
        Nothing -> []
      , case skullsToTier $ getSkulls "DrumDifficulty" of
        Just tier -> [(F.FlexDrums, (emptyPart :: Part SoftFile)
          { drums = Just (emptyPartDrums DrumsTrue Kicks1x :: PartDrums SoftFile)
            { difficulty = tier
            }
          })]
        Nothing -> []
      {-
      , (F.FlexExtra "hidden-drums", def
        { partDrums = Just emptyPartDrums DrumsTrue Kicks1x
        })
      -}
      ]
    }
