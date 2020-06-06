{-# LANGUAGE OverloadedStrings #-}
module Rocksmith.Import where

import           Audio                            (Audio (..))
import           Config
import           Control.Monad                    (forM)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.Aeson                       as A
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, isNothing)
import qualified Data.Text                        as T
import           JSONData                         (toJSON, yamlEncodeFile)
import           RockBand.Codec                   (mapTrack)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.ProGuitar
import           RockBand.Common                  (Difficulty (..))
import           Rocksmith.Base
import           Rocksmith.BNK                    (extractRSOgg)
import           Rocksmith.PSARC
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath                  ((<.>), (</>))

importRS :: (SendMessage m, MonadResource m) => FilePath -> FilePath -> StackTraceT m ()
importRS psarc dout = tempDir "onyx_rocksmith" $ \temp -> do
  stackIO $ extractPSARC psarc temp
  manifestDirs <- stackIO $ Dir.listDirectory $ temp </> "manifests"
  song <- case manifestDirs of
    -- TODO support index into multiple songs
    [song] -> return song
    _      -> fatal "Not exactly 1 song in .psarc"
  lg $ "Manifest folder: " <> song
  manifest <- stackIO (A.eitherDecodeFileStrict $ temp </> "manifests" </> song </> song <.> "hsan") >>= either fatal return
  let prop k rec = case rec of
        A.Object o -> case HM.lookup k o of
          Nothing -> fatal $ "No key " <> show k <> " in object"
          Just v  -> return v
        _ -> fatal $ "Tried to read key " <> show k <> " of non-object"
      urn urnType o = case o of
        A.String s -> case T.stripPrefix urnType s of
          Just s' -> return s'
          Nothing -> fatal $ "Couldn't read urn of type " <> show urnType
        _ -> fatal $ "Tried to read urn " <> show urnType <> " from non-string"
  A.Object entries <- prop "Entries" manifest
  lg $ show $ HM.keys entries
  parts <- fmap catMaybes $ forM (HM.toList entries) $ \(k, entry) -> inside ("Entry " <> T.unpack k) $ do
    attrs <- prop "Attributes" entry
    partJsonPath <- prop "ManifestUrn" attrs >>= urn "urn:database:json-db:"
    part <- stackIO (A.eitherDecodeFileStrict $ temp </> "manifests" </> song </> T.unpack partJsonPath <.> "json") >>= either fatal return
    partAttrs <- prop "Entries" part >>= prop k >>= prop "Attributes"
    partXmlPath <- prop "SongXml" partAttrs >>= urn "urn:application:xml:"
    bnkPath <- prop "SongBank" partAttrs >>= urn ""
    xml <- parseFile $ temp </> "songs/arr" </> T.unpack partXmlPath <.> "xml"
    return $ case xml of
      PartVocals          -> Nothing
      PartArrangement arr -> let
        props = arr_arrangementProperties arr
        partName = case (ap_pathLead props, ap_pathRhythm props, ap_pathBass props, ap_bonusArr props) of
          (True, _, _, False) -> RBFile.FlexGuitar
          (_, True, _, False) -> RBFile.FlexExtra "rhythm"
          (_, _, True, False) -> RBFile.FlexBass
          (True, _, _, True) -> RBFile.FlexExtra "bonus-lead"
          (_, True, _, True) -> RBFile.FlexExtra "bonus-rhythm"
          (_, _, True, True) -> RBFile.FlexExtra "bonus-bass"
          (False, False, False, _) -> RBFile.FlexExtra $ arr_arrangement arr
        in Just (partName, arr, bnkPath)
  (_, firstArr, bnk) <- case parts of
    []    -> fatal "No entries found in song"
    p : _ -> return p
  -- TODO handle if the bnks are different in different parts?
  -- how does multiplayer handle this?
  -- TODO handle folders other than windows
  stackIO $ extractRSOgg (temp </> "audio/windows" </> T.unpack bnk) $ dout </> "song.ogg"
  let modifiedBeats = case arr_ebeats firstArr of
        ebeats@(Ebeat 0 _ : _) -> ebeats
        ebeats                 -> Ebeat 0 Nothing : ebeats
        -- TODO maybe be smarter about the initial added tempo
      temps = U.tempoMapFromBPS $ let
        makeTempo b1 b2 = U.makeTempo 1 (eb_time b2 - eb_time b1)
        in RTB.fromPairList
          $ zip (0 : repeat 1)
          $ zipWith makeTempo modifiedBeats (drop 1 modifiedBeats)
      sigs = U.measureMapFromLengths U.Truncate $ let
        makeBarLengths [] = []
        makeBarLengths (_ : ebeats) = case span (isNothing . eb_measure) ebeats of
          (inThisBar, rest) -> (1 + fromIntegral (length inThisBar)) : makeBarLengths rest
        assembleMMap lens = RTB.fromPairList $ zip (0 : lens) lens
        in assembleMMap $ makeBarLengths modifiedBeats
  stackIO $ Save.toFile (dout </> "notes.mid") $ RBFile.showMIDIFile'
    $ RBFile.Song temps sigs mempty
      { RBFile.onyxParts = Map.fromList $ do
        (partName, arr, _) <- parts
        -- if transcriptionTrack is present we could use that, but not all songs have that
        let getNote note = let
              str = case n_string note of
                0 -> S6
                1 -> S5
                2 -> S4
                3 -> S3
                4 -> S2
                5 -> S1
                _ -> S7 -- TODO raise error
              ntype = NormalNote -- TODO
              fret = n_fret note
              len = n_sustain note
              in (n_time note, (str, (ntype, fret, len)))
            iterBoundaries = zip (arr_phraseIterations arr)
              (map Just (drop 1 $ arr_phraseIterations arr) ++ [Nothing])
            getPhrase iter1 miter2 = let
              phrase = arr_phrases arr !! pi_phraseId iter1
              lvl = arr_levels arr !! ph_maxDifficulty phrase
              inBounds note = pi_time iter1 <= n_time note
                && all (\iter2 -> n_time note < pi_time iter2) miter2
              lvlAllNotes = lvl_notes lvl ++ concatMap chd_chordNotes (lvl_chords lvl)
              in map getNote $ filter inBounds lvlAllNotes
            notes = let
              in RTB.fromAbsoluteEventList
                $ ATB.fromPairList
                $ sort
                $ concatMap (uncurry getPhrase) iterBoundaries
            diff = mapTrack (U.unapplyTempoTrack temps) mempty
              { pgNotes = notes -- :: RTB.T t (GtrString, (NoteType, GtrFret, Maybe t))
              }
            trk = mempty
              { pgDifficulties = Map.singleton Expert diff
              }
        return (partName, mempty { RBFile.onyxPartRealGuitar = trk })
      }
  stackIO $ yamlEncodeFile (dout </> "song.yml") $ toJSON (SongYaml
    { _metadata = (def :: Metadata FilePath)
      { _title        = Just $ arr_title firstArr
      , _artist       = Just $ arr_artistName firstArr
      , _album        = Just $ arr_albumName firstArr
      , _year         = arr_albumYear firstArr
      , _fileAlbumArt = Nothing -- TODO
      }
    , _jammit = HM.empty
    , _targets = HM.empty
    , _global = def
    , _audio = HM.singleton "song.ogg" $ AudioFile AudioInfo
      { _md5      = Nothing
      , _frames   = Nothing
      , _filePath = Just "song.ogg"
      , _commands = []
      , _rate     = Nothing
      , _channels = 2 -- TODO get real count
      }
    , _plans = HM.singleton "rs" Plan
      { _song         = Just PlanAudio
        { _planExpr = Input $ Named "song.ogg"
        , _planPans = []
        , _planVols = []
        }
      , _countin      = Countin []
      , _planParts    = Parts HM.empty
      , _crowd        = Nothing
      , _planComments = []
      , _tuningCents  = arr_centOffset firstArr
      , _fileTempo    = Nothing
      }
    , _parts = Parts $ HM.fromList $ do
      (partName, arr, _) <- parts
      let part = def
            { partProGuitar = Just PartProGuitar
              { pgDifficulty    = Tier 1
              , pgHopoThreshold = 170
              , pgTuning        = if ap_pathBass $ arr_arrangementProperties arr
                then GtrTuning
                  { gtrBase    = Bass4
                  , gtrOffsets = case arr_tuning arr of
                    Tuning a b c d _ _ -> [a, b, c, d]
                  , gtrGlobal  = 0 -- TODO use arr_capo ?
                  }
                else GtrTuning
                  { gtrBase    = Guitar6
                  , gtrOffsets = case arr_tuning arr of
                    Tuning a b c d e f -> [a, b, c, d, e, f]
                  , gtrGlobal  = 0 -- TODO use arr_capo ?
                  }
              , pgFixFreeform   = False
              }
            }
      return (partName, part)
    } :: SongYaml FilePath)
  {-
  1. pick manifests/foo
  2. load manifests/foo/foo.hsan (json)
  3. parts located in values of the object.Entries object
  4. for each part:
    1. part.Attributes.ManifestUrn is "urn:database:json-db:thepartname"
    2. load manifests/foo/thepartname.json
    3. object.Entries should only have one key-value pair?
    4. values to read to find the song files:
      thatValue.Attributes.SongXml is "urn:application:xml:something"
        points to songs/arr/something.xml
      thatValue.Attributes.ShowlightsXML is "urn:application:xml:something"
        points to songs/arr/something.xml
      thatValue.Attributes.SongAsset is "urn:application:musicgame-song:something"
        points to songs/bin/generic/something.sng
      thatValue.Attributes.SongBank is "something.bnk"
        points to audio/windows/something.bnk
        (assuming windows could be xbox/ps/mac or something)
      thatValue.Attributes.AlbumArt is "urn:image:dds:album_cuspikotaroppap"
        points to gfxassets/album_art/something_size.dds
        where size is 64, 128, 256
        (this is also in the .hsan file)
      (note that album art and audio could technically be different between parts,
        but we don't need to support that for now)
    5. use extractRSOgg to follow .bnk and extract .ogg
    6. existing DXT code should work with .dds with some small additions
  -}
  return ()
