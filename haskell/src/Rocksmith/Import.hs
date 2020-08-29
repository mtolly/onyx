{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.Import where

import           Audio                            (Audio (..))
import           Codec.Picture                    (writePng)
import           Config
import           Control.Monad                    (forM, guard)
import           Control.Monad.Codec.Onyx.JSON    (toJSON, yamlEncodeFile)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.Aeson                       as A
import           Data.Bits                        ((.&.))
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace)
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (find, sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Text.Encoding.Error         (lenientDecode)
import           Image                            (readDDS)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.ProGuitar
import           RockBand.Common                  (blipEdgesRB,
                                                   splitEdgesSimple)
import           Rocksmith.BNK                    (extractRSOgg)
import           Rocksmith.Crypt
import           Rocksmith.MIDI
import           Rocksmith.PSARC
import           Rocksmith.Sng2014
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath                  (takeExtension, (<.>), (</>))
import           Text.Decode                      (decodeGeneral)
import           Text.XML.Light

data RSSong = RSSong
  { rsHeader        :: String
  , rsManifest      :: String
  , rsSngAsset      :: String
  , rsSoundBank     :: String
  , rsAlbumArtLarge :: String
  } deriving (Eq, Show)

importRS :: (SendMessage m, MonadResource m) => FilePath -> FilePath -> StackTraceT m ()
importRS psarc dout = tempDir "onyx_rocksmith" $ \temp -> do
  stackIO $ extractPSARC psarc temp

  audioDirs <- stackIO $ Dir.listDirectory $ temp </> "audio"
  (audioDir, platform) <- case audioDirs of
    ["windows"] -> return ("windows", PC)
    ["xbox360"] -> return ("xbox360", Xbox360)
    ["mac"]     -> return ("mac", Mac)
    -- TODO find other folder names
    _           -> fatal "Couldn't determine platform of .psarc"

  xblocks <- fmap (filter $ (== ".xblock") . takeExtension)
    $ stackIO $ Dir.listDirectory $ temp </> "gamexblocks/nsongs"
  xblockSongs <- forM xblocks $ \xblock -> do
    let xblockFull = "gamexblocks/nsongs" </> xblock
    xml <- stackIO (B.readFile $ temp </> xblockFull) >>= \bs -> case parseXMLDoc bs of
      Nothing  -> fatal $ "Couldn't parse XML from: " <> xblockFull
      Just xml -> return xml
    return $ do
      entitySet <- findChildren (QName "entitySet" Nothing Nothing) xml
      entity <- findChildren (QName "entity" Nothing Nothing) entitySet
      let mapping = do
            properties <- findChildren (QName "properties" Nothing Nothing) entity
            prop <- findChildren (QName "property" Nothing Nothing) properties
            propName <- toList $ findAttr (QName "name" Nothing Nothing) prop
            propSet <- findChildren (QName "set" Nothing Nothing) prop
            propValue <- toList $ findAttr (QName "value" Nothing Nothing) propSet
            return (propName, propValue)
      rsHeader <- toList $ lookup "Header" mapping
      rsManifest <- toList $ lookup "Manifest" mapping
      rsSngAsset <- toList $ lookup "SngAsset" mapping
      rsSoundBank <- toList $ lookup "SoundBank" mapping
      rsAlbumArtLarge <- toList $ lookup "AlbumArtLarge" mapping
      return RSSong{..}

  song <- case xblockSongs of
    [song] -> return song
    _      -> fatal "Not exactly 1 song in .psarc"
  let urn s = case T.splitOn ":" $ T.pack s of
        ["urn", _, _, value] -> return $ T.unpack value
        _ -> fatal $ "Couldn't parse urn value: " <> show s
      prop k rec = case rec of
        A.Object o -> case HM.lookup k o of
          Nothing -> fatal $ "No key " <> show k <> " in object"
          Just v  -> return v
        _ -> fatal $ "Tried to read key " <> show k <> " of non-object"
      singleKey rec = case rec of
        A.Object o -> case HM.toList o of
          [(_, v)] -> return v
          _        -> fatal "JSON object has more than 1 key, 1 expected"
        _ -> fatal "Unexpected non-object in JSON file"
      getString o = case o of
        A.String s -> return s
        _          -> fatal "Expected string in JSON file"
      getInt o = case o of
        A.Number n -> return $ round n
        _          -> fatal "Expected integer in JSON file"
      getBool o = case o of
        A.Number n -> return $ n /= 0
        _          -> fatal "Expected boolean (0 or 1) in JSON file"
  parts <- fmap catMaybes $ forM song $ \entity -> inside ("Entity " <> show (rsManifest entity)) $ do
    header <- urn $ rsHeader entity -- urn:database:hsan-db or hson-db
    manifest <- urn $ rsManifest entity -- urn:database:json-db
    sngPath <- urn $ rsSngAsset entity -- urn:application:musicgame-song
    bnkPath <- urn $ rsSoundBank entity -- urn:audio:wwise-sound-bank
    let binFolder = case platform of
          Xbox360 -> "xbox360"
          Mac     -> "macos"
          -- TODO probably need ps3 name
          _       -> "generic" -- PC
    sng <- stackIO $ loadSNG platform $ temp </> "songs/bin" </> binFolder </> sngPath <.> "sng"
    if not $ null $ sng_Vocals sng
      then return Nothing
      else do
        -- TODO I don't know if this is correct, just what I saw in dlc files
        let manifestDir = case platform of
              Xbox360 -> "songs_dlc"
              _       -> header -- on PC
        json <- stackIO (A.eitherDecodeFileStrict $ temp </> "manifests" </> manifestDir </> manifest <.> "json") >>= either fatal return
        jsonAttrs <- prop "Entries" json >>= singleKey >>= prop "Attributes"
        title <- prop "SongName" jsonAttrs >>= getString
        artist <- prop "ArtistName" jsonAttrs >>= getString
        album <- prop "AlbumName" jsonAttrs >>= getString
        year <- prop "SongYear" jsonAttrs >>= getInt
        arrProps <- prop "ArrangementProperties" jsonAttrs
        isLead <- prop "pathLead" arrProps >>= getBool
        isRhythm <- prop "pathRhythm" arrProps >>= getBool
        isBass <- prop "pathBass" arrProps >>= getBool
        isBonus <- prop "bonusArr" arrProps >>= getBool
        arrName <- prop "ArrangementName" jsonAttrs >>= getString
        let name = case (isLead, isRhythm, isBass, isBonus) of
              (True, _, _, False)      -> RBFile.FlexGuitar
              (_, True, _, False)      -> RBFile.FlexExtra "rhythm"
              (_, _, True, False)      -> RBFile.FlexBass
              (True, _, _, True)       -> RBFile.FlexExtra "bonus-lead"
              (_, True, _, True)       -> RBFile.FlexExtra "bonus-rhythm"
              (_, _, True, True)       -> RBFile.FlexExtra "bonus-bass"
              (False, False, False, _) -> RBFile.FlexExtra $ T.filter (not . isSpace) arrName
        return $ Just (name, sng, bnkPath, (title, artist, album, year), isBass)
  (_, firstArr, bnk, (title, artist, album, year), _) <- case parts of
    []    -> fatal "No entries found in song"
    p : _ -> return p
  art <- case map rsAlbumArtLarge song of
    [] -> return Nothing
    art : _ -> do
      f <- urn art -- urn:image:dds
      bs <- stackIO $ BL.readFile $ temp </> "gfxassets/album_art" </> f <.> "dds"
      forM (readDDS bs) $ \img -> do
        stackIO $ writePng (dout </> "cover.png") img
        return "cover.png"
  -- TODO handle if the bnks are different in different parts?
  -- how does multiplayer handle this?
  stackIO $ extractRSOgg (temp </> "audio" </> audioDir </> bnk <.> "bnk") $ dout </> "song.ogg"
  let modifiedBeats = case sng_BPMs firstArr of
        ebeats@(BPM { bpm_Time = 0 } : _) -> ebeats
        ebeats                            -> BPM
          { bpm_Time            = 0
          , bpm_Measure         = -1
          , bpm_Beat            = 0
          , bpm_PhraseIteration = 0
          , bpm_Mask            = 0
          } : ebeats
        -- TODO maybe be smarter about the initial added tempo
      temps = U.tempoMapFromBPS $ let
        makeTempo b1 b2 = U.makeTempo 1 (realToFrac $ bpm_Time b2 - bpm_Time b1)
        in RTB.fromPairList
          $ zip (0 : repeat 1)
          $ zipWith makeTempo modifiedBeats (drop 1 modifiedBeats)
      sigs = U.measureMapFromLengths U.Truncate $ let
        startsBar = (== 0) . bpm_Beat
        makeBarLengths [] = []
        makeBarLengths (_ : ebeats) = case break startsBar ebeats of
          (inThisBar, rest) -> (1 + fromIntegral (length inThisBar)) : makeBarLengths rest
        assembleMMap lens = RTB.fromPairList $ zip (0 : lens) lens
        in assembleMMap $ makeBarLengths modifiedBeats
  stackIO $ Save.toFile (dout </> "notes.mid") $ RBFile.showMIDIFile'
    $ RBFile.Song temps sigs mempty
      { RBFile.onyxParts = Map.fromList $ do
        (partName, sng, _, _, isBass) <- parts
        let toSeconds = realToFrac :: Float -> U.Seconds
            getNotes note = let
              secs = toSeconds $ notes_Time note
              beats = U.unapplyTempoMap temps secs
              len = do
                guard $ notes_Sustain note > 0
                let endSecs = secs <> toSeconds (notes_Sustain note)
                Just $ U.unapplyTempoMap temps endSecs - beats
              in do
                (str, fret) <- case notes_ChordId note of
                  -1 -> let
                    fret = fromIntegral $ notes_FretId note
                    str = case notes_StringIndex note of
                      0 -> S6
                      1 -> S5
                      2 -> S4
                      3 -> S3
                      4 -> S2
                      5 -> S1
                      _ -> S7 -- TODO raise error
                    in [(str, fret)]
                  chordID -> do
                    pair@(_str, fret) <- zip [S6, S5 ..]
                      $ map fromIntegral
                      $ chord_Frets
                      $ sng_Chords sng !! fromIntegral chordID
                    guard $ fret >= 0
                    return pair
                return (beats, (fret, str, len))
            makeShape fprint = let
              secs = toSeconds $ fp_StartTime fprint
              beats = U.unapplyTempoMap temps secs
              len = let
                endSecs = toSeconds $ fp_EndTime fprint
                in U.unapplyTempoMap temps endSecs - beats
              in do
                (str, fret) <- zip [S6, S5 ..]
                  $ map fromIntegral
                  $ chord_Frets
                  $ sng_Chords sng !! fromIntegral (fp_ChordId fprint)
                guard $ fret >= 0
                return (beats, (fret, str, len))
            iterBoundaries = zip (sng_PhraseIterations sng)
              (fmap Just (drop 1 $ sng_PhraseIterations sng) <> [Nothing])
            getPhraseNotes iter1 miter2 = let
              phrase = sng_Phrases sng !! fromIntegral (pi_PhraseId iter1)
              lvl = sng_Arrangements sng !! fromIntegral (phrase_MaxDifficulty phrase)
              inBounds note = pi_StartTime iter1 <= notes_Time note
                && all (\iter2 -> notes_Time note < pi_StartTime iter2) miter2
              in filter inBounds $ arr_Notes lvl
            getPhraseAnchors iter1 miter2 = let
              phrase = sng_Phrases sng !! fromIntegral (pi_PhraseId iter1)
              lvl = sng_Arrangements sng !! fromIntegral (phrase_MaxDifficulty phrase)
              inBounds anchor = pi_StartTime iter1 <= anchor_StartBeatTime anchor
                && all (\iter2 -> anchor_StartBeatTime anchor < pi_StartTime iter2) miter2
              in filter inBounds $ arr_Anchors lvl
            getHandShapes iter1 miter2 = let
              phrase = sng_Phrases sng !! fromIntegral (pi_PhraseId iter1)
              lvl = sng_Arrangements sng !! fromIntegral (phrase_MaxDifficulty phrase)
              inBounds fprint = pi_StartTime iter1 <= fp_StartTime fprint
                && all (\iter2 -> fp_StartTime fprint < pi_StartTime iter2) miter2
              in filter inBounds $ arr_Fingerprints1 lvl <> arr_Fingerprints2 lvl
              -- arr_Fingerprints2 is arpeggios, but we'll get that from the mask later
            maxLevelNotes = iterBoundaries >>= uncurry getPhraseNotes
            maxLevelShapes = iterBoundaries >>= uncurry getHandShapes
            notes = RTB.fromAbsoluteEventList
              $ ATB.fromPairList
              $ sort
              $ maxLevelNotes >>= getNotes
            anchors = U.unapplyTempoTrack temps
              $ RTB.fromAbsoluteEventList
              $ ATB.fromPairList
              $ sort
              $ fmap (\anc -> let
                t = toSeconds $ anchor_StartBeatTime anc
                lowFret = fromIntegral $ anchor_FretId anc
                highFret = lowFret + fromIntegral (anchor_Width anc) - 1
                in (t, (lowFret, highFret))
                )
              $ iterBoundaries >>= uncurry getPhraseAnchors
            shapes = RTB.fromAbsoluteEventList
              $ ATB.fromPairList
              $ sort
              $ maxLevelShapes >>= makeShape
            noteChordInfo = RTB.fromAbsoluteEventList
              $ ATB.fromPairList
              $ map (makeChordInfo ChordLocNotes)
              $ sort
              $ mapMaybe (\n -> guard (notes_ChordId n /= (-1)) >> Just (notes_Time n, notes_ChordId n))
              $ maxLevelNotes
            shapeChordInfo = RTB.fromAbsoluteEventList
              $ ATB.fromPairList
              $ map (makeChordInfo ChordLocShape)
              $ sort
              $ map (\fp -> (fp_StartTime fp, fp_ChordId fp))
              $ maxLevelShapes
            makeChordInfo loc (t, chordID) = (t', ChordInfo
              { ciLocation    = loc
              , ciName        = case TE.decodeUtf8With lenientDecode $ chord_Name chord of
                ""   -> Nothing
                name -> Just name
              , ciFingers     = map (toEnum . fromIntegral) $ filter (/= (-1)) $ chord_Fingers chord
              , ciArpeggio = chord_Mask chord .&. 0x00000001 /= 0
              , ciNop      = chord_Mask chord .&. 0x00000002 /= 0
              }) where
                chord = sng_Chords sng !! fromIntegral chordID
                t' = U.unapplyTempoMap temps $ toSeconds t
            trk = RocksmithTrack
              { rsNotes = blipEdgesRB notes
              , rsPhrases = U.unapplyTempoTrack temps
                $ RTB.fromAbsoluteEventList
                $ ATB.fromPairList
                $ sort
                $ flip map (sng_PhraseIterations sng)
                $ \iter -> let
                  t = toSeconds $ pi_StartTime iter
                  phrase = sng_Phrases sng !! fromIntegral (pi_PhraseId iter)
                  name = TE.decodeUtf8With lenientDecode $ phrase_Name phrase
                  in (t, name)
              , rsSections = U.unapplyTempoTrack temps
                $ RTB.fromAbsoluteEventList
                $ ATB.fromPairList
                $ sort
                $ flip map (sng_Sections sng)
                $ \sect -> let
                  t = toSeconds $ sect_StartTime sect
                  name = TE.decodeUtf8With lenientDecode $ sect_Name sect
                  in (t, name)
              , rsAnchorLow  = fmap fst anchors
              , rsAnchorHigh = fmap snd anchors
              , rsModifiers  = RTB.empty -- TODO
              , rsTones      = U.unapplyTempoTrack temps
                $ RTB.fromAbsoluteEventList
                $ ATB.fromPairList
                $ sort
                $ flip map (sng_Tones sng)
                $ \tid -> let
                  t = toSeconds $ tid_Time tid
                  tone = case tid_ID tid of
                    0 -> ToneA
                    1 -> ToneB
                    2 -> ToneC
                    3 -> ToneD
                    _ -> ToneA -- TODO error?
                  in (t, tone)
              , rsBends      = RTB.empty -- TODO
              , rsHandShapes = splitEdgesSimple shapes
              , rsChords = RTB.merge noteChordInfo shapeChordInfo
              }
        return (partName, if isBass
          then mempty { RBFile.onyxPartRSBass   = trk }
          else mempty { RBFile.onyxPartRSGuitar = trk })
      }

  -- Lots of authors don't put their name into CST for some reason,
  -- so it just shows up as Custom Song Creator...
  -- Is it a newer added feature?
  let toolkitPath = temp </> "toolkit.version"
  author <- stackIO $ Dir.doesFileExist toolkitPath >>= \case
    False -> return Nothing
    True -> do
      txt <- decodeGeneral <$> B.readFile toolkitPath
      return $ find (`notElem` ["", "Custom Song Creator"]) $ map T.strip
        $ mapMaybe (T.stripPrefix "Package Author:") $ T.lines txt

  stackIO $ yamlEncodeFile (dout </> "song.yml") $ toJSON (SongYaml
    { _metadata = (def :: Config.Metadata FilePath)
      { _title        = Just title
      , _artist       = Just artist
      , _album        = Just album
      , _year         = Just year
      , _fileAlbumArt = art
      , _author       = author
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
      , _tuningCents  = 0 -- TODO get from manifest .json (CentOffset)
      , _fileTempo    = Nothing
      }
    , _parts = Parts $ HM.fromList $ do
      (partName, sng, _, _, isBass) <- parts
      let part = def
            { partProGuitar = Just PartProGuitar
              { pgDifficulty    = Tier 1
              , pgHopoThreshold = 170
              , pgTuning        = if isBass
                then GtrTuning
                  { gtrBase    = Bass4
                  , gtrOffsets = map fromIntegral $ take 4 $ meta_Tuning $ sng_Metadata sng
                  , gtrGlobal  = 0 -- TODO use arr_capo ?
                  }
                else GtrTuning
                  { gtrBase    = Guitar6
                  , gtrOffsets = map fromIntegral $ meta_Tuning $ sng_Metadata sng
                  , gtrGlobal  = 0 -- TODO use arr_capo ?
                  }
              , pgFixFreeform   = False
              , pgTones = Nothing -- TODO
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
