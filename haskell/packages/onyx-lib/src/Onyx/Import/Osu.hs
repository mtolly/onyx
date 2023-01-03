{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Onyx.Import.Osu where

import           Control.Monad                    (forM, guard, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Bits                        (testBit)
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit, toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Onyx.Audio
import           Onyx.Guitar                      (emit5')
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..), Key (..),
                                                   StrumHOPOTap (..),
                                                   blipEdgesRB_)
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.MIDI.Track.ProKeys
import           Onyx.Osu.Base
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle
import           Onyx.Util.Text.Decode            (decodeGeneral)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension, (<.>))
import           Text.Read                        (readMaybe)

importOsu :: (SendMessage m, MonadIO m) => Bool -> FilePath -> StackTraceT m [Import m]
importOsu separateSongs f = do

  tree <- loadZipTree f

  loadedOsus <- fmap catMaybes $ forM (folderFiles tree) $ \(name, getBS) -> do
    inside ("Loading: " <> T.unpack name) $ do
      case map toLower $ takeExtension $ T.unpack name of
        ".osu" -> fmap Just $ do
          txt <- fmap decodeGeneral $ stackIO $ getBS
          splitOsuFile txt >>= readOsu . snd
        _ -> return Nothing

  -- currently only loading Mania charts
  let filteredOsus = filter (\osu -> osu.general.mode == 3) loadedOsus
  when (null filteredOsus) $ warn "No importable .osu files found in .osz"
  osuSets <- if separateSongs
    then return $ map (\x -> (x, [x])) filteredOsus
    else case filteredOsus of
      []    -> return []
      -- arbitrarily pick one .osu to use for metadata and timing
      o : _ -> return [(o, filteredOsus)]

  return $ flip map osuSets $ \(primary, osus) level -> do

    let timingMid = getOsuTiming primary

    mania <- fmap catMaybes $ forM osus $ \osu -> case osu.metadata.version of
      Nothing -> return Nothing
      Just version -> case osu.general.mode of
        3 -> let
          track = if maniaColumnCount osu <= 5
            then Left  $ maniaToFiveKeys (F.s_tempos timingMid) osu
            else Right $ maniaToProKeys  (F.s_tempos timingMid) osu
          partName = if separateSongs
            then F.FlexKeys
            else F.FlexExtra version
          in return $ Just (partName, track)
        _ -> return Nothing

    -- Note, we do not yet handle "Sample" commands in the events list.
    -- See "92190 Yuuna Sasara feat. Tai no Kobone - Imperishable Night 2006"
    -- for usage of this, and also "AudioFilename: virtual" which I assume means
    -- basically no single backing track (all Sample + hit sounds)

    audio <- case level of
      ImportQuick -> return Nothing
      ImportFull -> do
        audioBytes <- case splitPath primary.general.audioFilename >>= (`findFileCI` tree) of
          Nothing    -> fatal $ "Couldn't find audio file: " <> T.unpack primary.general.audioFilename
          Just getBS -> BL.fromStrict <$> stackIO getBS
        let audioName = "audio" <.> takeExtension (T.unpack primary.general.audioFilename)
        return $ Just (audioBytes, audioName)

    background <- case level of
      ImportQuick -> return Nothing
      ImportFull -> let
        backgrounds = do
          event <- V.toList primary.events
          guard $ event.eventType == Right 0
          name <- toList $ event.eventParams V.!? 1
          getBG <- toList $ splitPath name >>= (`findFileCI` tree)
          return (name, getBG)
        in case backgrounds of
          []                -> return Nothing
          (name, getBG) : _ -> do
            let newName = "background" <.> takeExtension (T.unpack name)
            bytes <- BL.fromStrict <$> stackIO getBG
            return $ Just $ SoftFile newName $ SoftReadable
              $ makeHandle newName $ byteStringSimpleHandle bytes

    let addVersion title = case (separateSongs, primary.metadata.version) of
          (True, Just version) -> title <> " [" <> version <> "]"
          _                    -> title

    return SongYaml
      { metadata = def'
        { title = addVersion <$> primary.metadata.title
        , titleJP = addVersion <$> primary.metadata.titleUnicode
        , artist = primary.metadata.artist
        , artistJP = primary.metadata.artistUnicode
        , previewStart = Just $ PreviewSeconds $ fromIntegral primary.general.previewTime / 1000
        , fileAlbumArt = background -- just so there's not nothing
        , author = case nubOrd $ mapMaybe (.metadata.creator) osus of
          []      -> Nothing
          authors -> Just $ T.intercalate ", " authors
        }
      , global = def'
        { _backgroundVideo = Nothing -- TODO
        , _fileBackgroundImage = background
        , _fileMidi = SoftFile "notes.mid" $ SoftChart $ case level of
          ImportFull  -> timingMid
            { F.s_tracks = mempty
              { F.onyxParts = Map.fromList $ do
                (partName, track) <- mania
                return (partName, case track of
                  Left five -> mempty
                    { F.onyxPartKeys = five
                    }
                  Right pk -> mempty
                    { F.onyxPartRealKeysX = pk
                    }
                  )
              }
            }
          ImportQuick -> emptyChart
        , _fileSongAnim = Nothing
        }
      , jammit = HM.empty
      , audio = HM.singleton "osu-audio-file" $ AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , filePath = flip fmap audio $ \(audioBytes, audioName) ->
          SoftFile audioName $ SoftReadable
            $ makeHandle audioName $ byteStringSimpleHandle audioBytes
        , commands = []
        , rate = Nothing
        , channels = 2 -- TODO maybe verify
        }
      , plans = HM.singleton "osu-audio" $ StandardPlan StandardPlanInfo
        { song = Just $ let
          -- Need to do this or audio is out of sync. I assume the game is
          -- skipping MP3 encoder delay. But .ogg also appears to need adjustment?
          -- Also see Onyx.Audio.buildSource' for note about something wrong in our ffmpeg seek code
          mp3Delay = Drop Start (CA.Seconds 0.02)
          in PlanAudio (mp3Delay $ Input $ Named "osu-audio-file") [] []
        , countin = Countin []
        , parts = Parts HM.empty
        , crowd = Nothing
        , comments = []
        , tuningCents = 0
        , fileTempo = Nothing
        }
      , targets = HM.empty
      , parts = Parts $ HM.fromList $ do
        (partName, track) <- mania
        return $ (partName, case track of
          Left _five -> def
            { partGRYBO = Just def
            }
          Right _pk -> def
            { partProKeys = Just PartProKeys
              { pkDifficulty  = Tier 1
              , pkFixFreeform = True
              }
            }
          )
      }

maniaColumnCount :: OsuFile -> Integer
maniaColumnCount osu = min 10 $ max 1 $ maybe 10 round osu.difficulty.circleSize

getManiaChart :: OsuFile -> [(U.Seconds, (Integer, Maybe U.Seconds))]
getManiaChart osu = let
  columnCount = maniaColumnCount osu
  xToColumn :: Integer -> Integer
  xToColumn x = max 0 $ min (columnCount - 1) $ quot (x * columnCount) 512
  msToSecs :: Integer -> U.Seconds
  msToSecs ms = fromIntegral ms / 1000
  -- fix cases where hit objects that should be simultaneous are off by 1 ms
  unslopHitObjects _           []           = []
  unslopHitObjects Nothing     (hit : rest) = hit : unslopHitObjects (Just hit) rest
  unslopHitObjects (Just prev) (hit : rest) = let
    hit' = if hit.time == prev.time + 1
      then OsuHitObject
        -- this should just be a record update for "time",
        -- but overloaded record update is not really in ghc yet, wtf!
        -- can do it with DuplicateRecordFields but has a warning about not
        -- working in future ghc??
        { x            = hit.x
        , y            = hit.y
        , time         = prev.time
        , type_        = hit.type_
        , hitSound     = hit.hitSound
        , objectParams = hit.objectParams
        }
      else hit
    in hit' : unslopHitObjects (Just hit') rest
  in do
    hit <- unslopHitObjects Nothing $ V.toList osu.hitObjects
    note <- if testBit hit.type_ 0 -- hit circle
      then return (xToColumn hit.x, Nothing)
      else if testBit hit.type_ 7 -- hold
        then let
          endTime = fmap msToSecs $ hit.objectParams V.!? 0 >>= readMaybe . takeWhile isDigit . T.unpack
          in return (xToColumn hit.x, endTime)
        else []
    return (msToSecs hit.time, note)

maniaToFiveKeys :: U.TempoMap -> OsuFile -> Five.FiveTrack U.Beats
maniaToFiveKeys tmap osu = let
  mania = getManiaChart osu
  fretOffset = case maniaColumnCount osu of
    1 -> 2 -- Y
    2 -> 2 -- Y B
    3 -> 1 -- R Y B
    4 -> 0 -- G R Y B
    5 -> 0 -- G R Y B O
    _ -> 0 -- shouldn't happen
  toFret column = case column + fretOffset of
    1 -> Five.Red
    2 -> Five.Yellow
    3 -> Five.Blue
    n -> if n <= 0 then Five.Green else Five.Orange
  in mempty
    { Five.fiveDifficulties = Map.singleton Expert $ emit5' $ RTB.fromAbsoluteEventList $ ATB.fromPairList $
      mania >>= \(secs, (column, endHold)) -> let
        startBeats = U.unapplyTempoMap tmap secs
        holdBeats = (\endSecs -> U.unapplyTempoMap tmap endSecs - startBeats) <$> endHold
        in return (startBeats, ((Just $ toFret column, Tap), holdBeats))
    }

maniaToProKeys :: U.TempoMap -> OsuFile -> ProKeysTrack U.Beats
maniaToProKeys tmap osu = let
  mania = getManiaChart osu
  -- pick a range and key set so we are centered on either the yellow or blue key areas
  (range, keys) = case maniaColumnCount osu of
    1  -> (RangeF, [                                                    BlueGreen D                                                    ])
    3  -> (RangeF, [                                       BlueGreen C, BlueGreen D, BlueGreen E                                       ])
    5  -> (RangeF, [                          RedYellow B, BlueGreen C, BlueGreen D, BlueGreen E, BlueGreen F                          ])
    7  -> (RangeF, [             RedYellow A, RedYellow B, BlueGreen C, BlueGreen D, BlueGreen E, BlueGreen F, BlueGreen G             ])
    9  -> (RangeF, [RedYellow G, RedYellow A, RedYellow B, BlueGreen C, BlueGreen D, BlueGreen E, BlueGreen F, BlueGreen G, BlueGreen A])
    2  -> (RangeC, [                                                    RedYellow G, RedYellow A                                                    ])
    4  -> (RangeC, [                                       RedYellow F, RedYellow G, RedYellow A, RedYellow B                                       ])
    6  -> (RangeC, [                          RedYellow E, RedYellow F, RedYellow G, RedYellow A, RedYellow B, BlueGreen C                          ])
    8  -> (RangeC, [             RedYellow D, RedYellow E, RedYellow F, RedYellow G, RedYellow A, RedYellow B, BlueGreen C, BlueGreen D             ])
    10 -> (RangeC, [RedYellow C, RedYellow D, RedYellow E, RedYellow F, RedYellow G, RedYellow A, RedYellow B, BlueGreen C, BlueGreen D, BlueGreen E])
    _  -> (RangeD, []) -- shouldn't happen
  in mempty
    { pkLanes = RTB.singleton 0 range
    , pkNotes = blipEdgesRB_ $ RTB.fromAbsoluteEventList $ ATB.fromPairList $
      mania >>= \(secs, (column, endHold)) -> case drop (fromIntegral column) keys of
        key : _ -> let
          startBeats = U.unapplyTempoMap tmap secs
          holdBeats = (\endSecs -> U.unapplyTempoMap tmap endSecs - startBeats) <$> endHold
          in return (startBeats, (key, holdBeats))
        []      -> [] -- key out of range somehow
    }
