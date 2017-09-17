{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import (importFoF, importRBA, importSTFSDir, importSTFS, simpleRBAtoCON, HasKicks(..)) where

import           Audio
import qualified C3
import           Codec.Picture                    (convertRGB8, readImage)
import           Config                           hiding (Difficulty)
import qualified Config
import           Control.Applicative              ((<|>))
import           Control.Arrow                    (first)
import           Control.Exception                (evaluate)
import           Control.Monad                    (forM, guard, when)
import           Control.Monad.Extra              (mapMaybeM)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.Digest.Pure.MD5             as MD5
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.Magma         as RBProj
import qualified Data.DTA.Serialize.RB3           as D
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (elemIndex, nub)
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import qualified Data.Yaml                        as Y
import           Difficulty
import qualified FretsOnFire                      as FoF
import           Image                            (toPNG_XBOX)
import           JSONData                         (toJSON)
import           Magma                            (getRBAFile)
import           MoggDecrypt                      (moggToOgg)
import           PrettyDTA                        (C3DTAComments (..),
                                                   DTASingle (..),
                                                   readDTASingle, readRB3DTA,
                                                   writeDTASingle)
import           RockBand.Common                  (Difficulty (..),
                                                   LongNote (..), joinEdges)
import qualified RockBand.Drums                   as RBDrums
import           RockBand.File                    (FlexPartName (..))
import qualified RockBand.File                    as RBFile
import           RockBand.PhaseShiftMessage       (discardPS, withRB)
import qualified RockBand.Vocals                  as RBVox
import           Scripts                          (loadMIDI)
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           STFS.Extract                     (extractSTFS)
import qualified System.Directory                 as Dir
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   takeFileName, (-<.>), (<.>),
                                                   (</>))
import           X360DotNet                       (rb3pkg)

fixDoubleSwells :: RBFile.PSFile U.Beats -> RBFile.PSFile U.Beats
fixDoubleSwells ps = let
  fixTrack trk = let
    (lanes, notLanes) = flip RTB.partitionMaybe trk $ \case
      RBDrums.SingleRoll b -> Just b
      _                    -> Nothing
    notes = flip RTB.mapMaybe trk $ \case
      RBDrums.DiffEvent Expert (RBDrums.Note gem) -> Just gem
      _                                           -> Nothing
    lanesAbs = RTB.toAbsoluteEventList 0 $ joinEdges $ fmap (\b -> if b then NoteOn () () else NoteOff ()) lanes
    lanesAbs' = ATB.fromPairList $ flip map (ATB.toPairList lanesAbs) $ \(startTime, lane) -> case lane of
      ((), (), Nothing ) -> error "fixDoubleSwells: panic! blip in swells list"
      ((), (), Just len) -> let
        shouldBeDouble = case toList $ U.trackTake len $ U.trackDrop startTime notes of
          g1 : g2 : g3 : g4 : _ | g1 == g3 && g2 == g4 && g1 /= g2 -> True
          _                     -> False
        in (startTime,) $ RTB.fromPairList $ if shouldBeDouble
          then [(0, RBDrums.DoubleRoll True), (len, RBDrums.DoubleRoll False)]
          else [(0, RBDrums.SingleRoll True), (len, RBDrums.SingleRoll False)]
    in RTB.merge (U.trackJoin $ RTB.fromAbsoluteEventList lanesAbs') notLanes
  in ps
    { RBFile.psPartDrums       = withRB fixTrack $ RBFile.psPartDrums       ps
    , RBFile.psPartDrums2x     = withRB fixTrack $ RBFile.psPartDrums2x     ps
    , RBFile.psPartRealDrumsPS = withRB fixTrack $ RBFile.psPartRealDrumsPS ps
    }

importFoF :: (MonadIO m) => Bool -> FilePath -> FilePath -> StackTraceT m HasKicks
importFoF detectBasicDrums src dest = do
  song <- FoF.loadSong $ src </> "song.ini"

  hasAlbumArt <- stackIO $ Dir.doesFileExist $ src </> "album.png"
  when hasAlbumArt $ stackIO $ Dir.copyFile (src </> "album.png") (dest </> "album.png")

  audioFiles <- stackIO $ let
    loadAudio x = do
      b <- Dir.doesFileExist $ src </> x
      if b
        then do
          Dir.copyFile (src </> x) (dest </> x)
          return $ Just x
        else return Nothing
    in mapMaybeM loadAudio
      [ "drums.ogg", "drums_1.ogg", "drums_2.ogg", "drums_3.ogg", "drums_4.ogg"
      , "guitar.ogg", "keys.ogg", "rhythm.ogg", "vocals.ogg", "vocals_1.ogg", "vocals_2.ogg"
      , "crowd.ogg", "song.ogg"
      ]
  audioFilesWithChannels <- forM audioFiles $ \af -> audioChannels (dest </> af) >>= \case
    Nothing    -> fatal $ "Couldn't get channel count of audio file: " <> af
    Just chans -> return (af, chans)

  let gtrAudio = case audioFiles of
        ["guitar.ogg"] -> [] -- assume sole guitar is no-stems audio
        _              -> filter (== "guitar.ogg") audioFiles
      bassAudio = filter (== "rhythm.ogg") audioFiles
      keysAudio = filter (== "keys.ogg") audioFiles
      crowdAudio = filter (== "crowd.ogg") audioFiles
      voxAudio = filter (`elem` ["vocals.ogg", "vocals_1.ogg", "vocals_2.ogg"]) audioFiles
      d0 = "drums.ogg"
      d1 = "drums_1.ogg"
      d2 = "drums_2.ogg"
      d3 = "drums_3.ogg"
      d4 = "drums_4.ogg"
      (drumsAudio, kickAudio, snareAudio)
        | all (`elem` audioFiles) [d1, d2, d3, d4] = ([d3, d4], [d1], [d2])
        | all (`elem` audioFiles) [d1, d2, d3] = ([d3], [d1], [d2])
        | d0 `elem` audioFiles = ([d0], [], [])
        | otherwise = ([], [], [])
      songAudio = case audioFiles of
        ["guitar.ogg"] -> ["guitar.ogg"]
        _              -> filter (== "song.ogg") audioFiles

  parsed <- loadMIDI $ src </> "notes.mid"
  let (delayAudio, delayMIDI) = case FoF.delay song of
        Nothing -> (id, id)
        Just n -> case compare n 0 of
          EQ -> (id, id)
          GT -> let
            secs = fromIntegral n / 1000
            midiDelay = ceiling secs
            audioDelay = fromIntegral midiDelay - secs
            in (Pad Start $ CA.Seconds audioDelay, RBFile.padPSMIDI midiDelay)
          LT ->
            ( Pad Start $ CA.Seconds $ fromIntegral (abs n) / 1000
            , id
            )
      audioExpr [] = Nothing
      audioExpr [aud] = Just PlanAudio
        { _planExpr = delayAudio $ Input $ Named $ T.pack aud
        , _planPans = []
        , _planVols = []
        }
      audioExpr auds = Just PlanAudio
        { _planExpr = delayAudio $ Mix $ map (Input . Named . T.pack) auds
        , _planPans = []
        , _planVols = []
        }
      hasVocalNotes = not $ null $ do
        trk <- [RBFile.psPartVocals, RBFile.psHarm1, RBFile.psHarm2, RBFile.psHarm3]
        RBVox.Note _ _ <- toList $ discardPS $ trk $ RBFile.s_tracks parsed
        return ()

  let toTier = maybe (Tier 1) $ \n -> Tier $ max 1 $ min 7 $ fromIntegral n + 1

  mid2x <- stackIO $ Dir.doesFileExist $ src </> "expert+.mid"
  add2x <- if mid2x
    then do
      parsed2x <- loadMIDI $ src </> "expert+.mid"
      let trk2x = RBFile.psPartDrums $ RBFile.s_tracks parsed2x
      return $ if RTB.null trk2x
        then id
        else \mid -> mid { RBFile.psPartDrums2x = trk2x }
    else return id
  let (title, is2x) = case FoF.name song of
        Nothing   -> (Nothing, False)
        Just name -> first Just $ determine2xBass name
      hasKicks = if mid2x || elem RBDrums.Kick2x (discardPS $ RBFile.psPartDrums $ RBFile.s_tracks parsed)
        then HasBoth
        else if is2x then Has2x else Has1x

  stackIO $ Save.toFile (dest </> "notes.mid") $ RBFile.showMIDIFile' $ delayMIDI parsed
    { RBFile.s_tracks = fixDoubleSwells $ add2x $ RBFile.s_tracks parsed
    }

  vid <- case FoF.video song of
    Nothing -> return Nothing
    Just s | all isSpace s -> return Nothing
    Just v -> inside "copying PS video file to onyx project" $ do
      stackIO $ Dir.copyFile (src </> v) (dest </> "video.avi")
      return $ Just "video.avi"

  let hasTrack :: (RBFile.PSFile U.Beats -> RTB.T U.Beats a) -> Bool
      hasTrack f = not $ null $ f $ RBFile.s_tracks parsed
      vocalMode = if hasTrack RBFile.psPartVocals && FoF.diffVocals song /= Just (-1) && hasVocalNotes && fmap T.toLower (FoF.charter song) /= Just "sodamlazy"
        then if hasTrack RBFile.psHarm2 && FoF.diffVocalsHarm song /= Just (-1)
          then if hasTrack RBFile.psHarm3
            then Just Vocal3
            else Just Vocal2
          else Just Vocal1
        else Nothing

  stackIO $ Y.encodeFile (dest </> "song.yml") $ toJSON SongYaml
    { _metadata = Metadata
      { _title        = title
      , _artist       = FoF.artist song
      , _album        = FoF.album song
      , _genre        = FoF.genre song
      , _subgenre     = Nothing
      , _year         = FoF.year song
      , _fileAlbumArt = guard hasAlbumArt >> Just "album.png"
      , _trackNumber  = FoF.track song
      , _comments     = []
      , _key          = Nothing
      , _autogenTheme = RBProj.DefaultTheme
      , _author       = FoF.charter song
      , _rating       = Unrated
      , _previewStart = case FoF.previewStartTime song of
        Just ms | ms >= 0 -> Just $ PreviewSeconds $ fromIntegral ms / 1000
        _       -> Nothing
      , _previewEnd   = Nothing
      , _languages    = _languages def
      , _convert      = _convert def
      , _rhythmKeys   = _rhythmKeys def
      , _rhythmBass   = _rhythmBass def
      , _catEMH       = _catEMH def
      , _expertOnly   = _expertOnly def
      , _cover        = _cover def
      , _difficulty   = toTier $ FoF.diffBand song
      }
    , _audio = HM.fromList $ flip map audioFilesWithChannels $ \(aud, chans) ->
      (T.pack aud, AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just aud
        , _rate = Nothing
        , _channels = chans
        }
      )
    , _jammit = HM.empty
    , _plans = HM.singleton "fof" Plan
      { _song         = audioExpr songAudio
      , _countin      = Countin []
      , _planParts    = Parts $ HM.fromList $ concat
        [ case audioExpr gtrAudio of Nothing -> []; Just x -> [(FlexGuitar, PartSingle x)]
        , case audioExpr bassAudio of Nothing -> []; Just x -> [(FlexBass, PartSingle x)]
        , case audioExpr keysAudio of Nothing -> []; Just x -> [(FlexKeys, PartSingle x)]
        , case audioExpr voxAudio of Nothing -> []; Just x -> [(FlexVocal, PartSingle x)]
        , case (audioExpr drumsAudio, audioExpr kickAudio, audioExpr snareAudio) of
          (Nothing, Nothing, Nothing) -> []
          (Just kit, Nothing, Nothing) -> [(FlexDrums, PartSingle kit)]
          (Just kit, kick, snare) -> [(FlexDrums, PartDrumKit
            { drumsSplitKit = kit
            , drumsSplitKick = kick
            , drumsSplitSnare = snare
            })]
          _ -> error "FoF import: unsupported drums audio configuration (kick/snare but no kit)"
        ]
      , _crowd = audioExpr crowdAudio
      , _planComments = []
      }
    , _targets = HM.singleton "ps" $ PS def { ps_FileVideo = vid }
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, def
        { partDrums = guard (hasTrack RBFile.psPartDrums && FoF.diffDrums song /= Just (-1)) >> Just PartDrums
          { drumsDifficulty = toTier $ FoF.diffDrums song
          , drumsPro = not detectBasicDrums || case FoF.proDrums song of
            Just b  -> b
            Nothing -> any
              (\case RBDrums.ProType _ _ -> True; _ -> False)
              (discardPS $ RBFile.psPartDrums $ RBFile.s_tracks parsed)
          , drumsAuto2xBass = False
          , drumsFixFreeform = False
          , drumsKit = HardRockKit
          , drumsLayout = StandardLayout
          }
        })
      , ( FlexGuitar, def
        { partGRYBO = guard (hasTrack RBFile.psPartGuitar && FoF.diffGuitar song /= Just (-1)) >> Just PartGRYBO
          { gryboDifficulty = toTier $ FoF.diffGuitar song
          , gryboHopoThreshold = 170
          , gryboFixFreeform = False
          }
        , partProGuitar = let
          b =  (hasTrack RBFile.psPartRealGuitar   && FoF.diffGuitarReal   song /= Just (-1))
            || (hasTrack RBFile.psPartRealGuitar22 && FoF.diffGuitarReal22 song /= Just (-1))
          in guard b >> Just PartProGuitar
            { pgDifficulty = toTier $ FoF.diffGuitarReal song
            , pgHopoThreshold = 170
            , pgTuning = []
            , pgFixFreeform = False
            }
        })
      , ( FlexBass, def
        { partGRYBO = guard (hasTrack RBFile.psPartBass && FoF.diffBass song /= Just (-1)) >> Just PartGRYBO
          { gryboDifficulty = toTier $ FoF.diffBass song
          , gryboHopoThreshold = 170
          , gryboFixFreeform = False
          }
        , partProGuitar = let
          b =  (hasTrack RBFile.psPartRealBass   && FoF.diffBassReal   song /= Just (-1))
            || (hasTrack RBFile.psPartRealBass22 && FoF.diffBassReal22 song /= Just (-1))
          in guard b >> Just PartProGuitar
            { pgDifficulty = toTier $ FoF.diffBassReal song
            , pgHopoThreshold = 170
            , pgTuning = []
            , pgFixFreeform = False
            }
        })
      , ( FlexKeys, def
        { partGRYBO = guard (hasTrack RBFile.psPartKeys && FoF.diffKeys song /= Just (-1)) >> Just PartGRYBO
          { gryboDifficulty = toTier $ FoF.diffKeys song
          , gryboHopoThreshold = 170
          , gryboFixFreeform = False
          }
        , partProKeys = guard (hasTrack RBFile.psPartRealKeysX && FoF.diffKeysReal song /= Just (-1)) >> Just PartProKeys
          { pkDifficulty = toTier $ FoF.diffKeysReal song
          , pkFixFreeform = False
          }
        })
      , ( FlexVocal, def
        { partVocal = flip fmap vocalMode $ \vc -> PartVocal
          { vocalDifficulty = toTier $ FoF.diffVocals song
          , vocalCount = vc
          , vocalGender = Nothing
          }
        })
      ]
    }

  return hasKicks

determine2xBass :: T.Text -> (T.Text, Bool)
determine2xBass s = case T.stripSuffix " (2x Bass Pedal)" s <|> T.stripSuffix " (2X Bass Pedal)" s of
  Nothing -> (s , False)
  Just s' -> (s', True )

data HasKicks = Has1x | Has2x | HasBoth
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

importSTFSDir :: (MonadIO m) => FilePath -> Maybe FilePath -> FilePath -> StackTraceT m HasKicks
importSTFSDir temp mtemp2x dir = do
  DTASingle _ pkg comments <- readDTASingle $ temp </> "songs/songs.dta"
  let c3Title = fromMaybe (D.name pkg) $ c3dtaSong comments
      (title, is2x) = case c3dta2xBass comments of
        Just b  -> (fst $ determine2xBass c3Title, b)
        Nothing -> determine2xBass c3Title
      meta = def
        { _author = c3dtaAuthoredBy comments
        , _title = Just title
        , _convert = fromMaybe (_convert def) $ c3dtaConvert comments
        , _rhythmKeys = fromMaybe (_rhythmKeys def) $ c3dtaRhythmKeys comments
        , _rhythmBass = fromMaybe (_rhythmBass def) $ c3dtaRhythmBass comments
        , _catEMH = fromMaybe (_catEMH def) $ c3dtaCATemh comments
        , _expertOnly = fromMaybe (_expertOnly def) $ c3dtaExpertOnly comments
        , _languages = fromMaybe (_languages def) $ c3dtaLanguages comments
        }
      karaoke = fromMaybe False $ c3dtaKaraoke comments
      multitrack = fromMaybe False $ c3dtaMultitrack comments
      base = T.unpack $ D.songName $ D.song pkg
      -- Note: the base path does NOT necessarily have to be songs/foo/foo
      -- where foo is the top key of songs.dta. foo can be different!
      -- e.g. C3's "Escape from the City" has a top key 'SonicAdvCityEscape2x'
      -- and a 'name' of "songs/sonicadv2cityescape2x/sonicadv2cityescape2x"
      with2xPath maybe2x = do
        let hasKicks = if isJust mtemp2x then HasBoth else if is2x then Has2x else Has1x
        importRB3 pkg meta karaoke multitrack hasKicks
          (temp </> base <.> "mid") maybe2x (temp </> base <.> "mogg")
          (temp </> takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_xbox"))
          "cover.png_xbox" dir
        return hasKicks
  case mtemp2x of
    Nothing -> with2xPath Nothing
    Just temp2x -> do
      DTASingle _ pkg2x _ <- readDTASingle $ temp2x </> "songs/songs.dta"
      let base2x = T.unpack $ D.songName $ D.song pkg2x
      with2xPath $ Just (pkg2x, temp2x </> base2x <.> "mid")

importSTFS :: (MonadIO m) => FilePath -> Maybe FilePath -> FilePath -> StackTraceT m HasKicks
importSTFS file file2x dir = tempDir "onyx_con" $ \temp -> do
  stackIO $ extractSTFS file temp
  let with2xPath mtemp2x = importSTFSDir temp mtemp2x dir
  case file2x of
    Nothing -> with2xPath Nothing
    Just f2x -> tempDir "onyx_con2x" $ \temp2x -> do
      stackIO $ extractSTFS f2x temp2x
      with2xPath $ Just temp2x

-- | Converts a Magma v2 RBA to CON without going through an import + recompile.
simpleRBAtoCON :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
simpleRBAtoCON rba con = inside ("converting RBA " ++ show rba ++ " to CON " ++ show con) $ do
  tempDir "onyx_rba2con" $ \temp -> do
    md5 <- stackIO $ BL.readFile rba >>= evaluate . MD5.md5
    let shortName = "onyx" ++ take 10 (show md5)
    stackIO $ Dir.createDirectoryIfMissing True $ temp </> "songs" </> shortName </> "gen"
    getRBAFile 0 rba $ temp </> "temp_songs.dta"
    getRBAFile 1 rba $ temp </> "songs" </> shortName </> shortName <.> "mid"
    getRBAFile 2 rba $ temp </> "songs" </> shortName </> shortName <.> "mogg"
    getRBAFile 3 rba $ temp </> "songs" </> shortName </> "gen" </> shortName <.> "milo_xbox"
    getRBAFile 4 rba $ temp </> "temp_cover.bmp"
    -- 5 is weights.bin (empty in magma v2)
    getRBAFile 6 rba $ temp </> "temp_extra.dta"
    (_, pkg, isUTF8) <- readRB3DTA $ temp </> "temp_songs.dta"
    extra <- (if isUTF8 then D.readFileDTA_utf8' else D.readFileDTA_latin1') $ temp </> "temp_extra.dta"
    stackIO $ TIO.writeFile (temp </> "songs/songs.dta") $ writeDTASingle DTASingle
      { dtaTopKey = T.pack shortName
      , dtaSongPackage = pkg
        { D.song = (D.song pkg)
          { D.songName = T.pack $ "songs" </> shortName </> shortName
          }
        , D.songId = Right $ T.pack shortName
        }
      , dtaC3Comments = C3DTAComments
        { c3dtaCreatedUsing = Nothing
        , c3dtaAuthoredBy   = case extra of
          D.DTA _ (D.Tree _ [D.Parens (D.Tree _
            ( D.String "backend"
            : D.Parens (D.Tree _ [D.Key "author", D.String s])
            : _
            ))])
            -> Just s
          _ -> Nothing
        , c3dtaSong         = Nothing
        , c3dtaLanguages    = Nothing -- TODO
        , c3dtaKaraoke      = Nothing
        , c3dtaMultitrack   = Nothing
        , c3dtaConvert      = Nothing
        , c3dta2xBass       = Nothing
        , c3dtaRhythmKeys   = Nothing
        , c3dtaRhythmBass   = Nothing
        , c3dtaCATemh       = Nothing
        , c3dtaExpertOnly   = Nothing
        }
      }
    stackIO $ readImage (temp </> "temp_cover.bmp") >>= \case
      Left err -> error err -- TODO
      Right dyn -> let
        out = temp </> "songs" </> shortName </> "gen" </> (shortName ++ "_keep.png_xbox")
        in BL.writeFile out $ toPNG_XBOX $ convertRGB8 dyn
    stackIO $ do
      Dir.removeFile $ temp </> "temp_songs.dta"
      Dir.removeFile $ temp </> "temp_cover.bmp"
      Dir.removeFile $ temp </> "temp_extra.dta"
    let label = D.name pkg <> " (" <> D.artist pkg <> ")"
    rb3pkg label label temp con

importRBA :: (MonadIO m) => FilePath -> Maybe FilePath -> FilePath -> StackTraceT m HasKicks
importRBA file file2x dir = tempDir "onyx_rba" $ \temp -> do
  getRBAFile 0 file $ temp </> "songs.dta"
  getRBAFile 1 file $ temp </> "notes.mid"
  getRBAFile 2 file $ temp </> "audio.mogg"
  getRBAFile 4 file $ temp </> "cover.bmp"
  getRBAFile 6 file $ temp </> "extra.dta"
  (_, pkg, isUTF8) <- readRB3DTA $ temp </> "songs.dta"
  extra <- (if isUTF8 then D.readFileDTA_utf8' else D.readFileDTA_latin1') $ temp </> "extra.dta"
  let author = case extra of
        D.DTA _ (D.Tree _ [D.Parens (D.Tree _
          ( D.String "backend"
          : D.Parens (D.Tree _ [D.Key "author", D.String s])
          : _
          ))])
          -> Just s
        _ -> Nothing
      (title, is2x) = determine2xBass $ D.name pkg
      -- TODO: import more stuff from the extra dta
      meta = def
        { _author = author
        , _title = Just title
        }
  files2x <- forM file2x $ \f2x -> do
    let mid2x = temp </> "notes-2x.mid"
        dta2x = temp </> "songs-2x.dta"
    getRBAFile 0 f2x dta2x
    getRBAFile 1 f2x mid2x
    (_, pkg2x, _) <- readRB3DTA dta2x
    return (pkg2x, mid2x)
  let hasKicks = if isJust file2x then HasBoth else if is2x then Has2x else Has1x
  importRB3 pkg meta False True hasKicks
    (temp </> "notes.mid") files2x (temp </> "audio.mogg")
    (temp </> "cover.bmp") "cover.bmp" dir
  return hasKicks

-- | Collects the contents of an RBA or CON file into an Onyx project.
importRB3 :: (MonadIO m) => D.SongPackage -> Metadata -> Bool -> Bool -> HasKicks -> FilePath -> Maybe (D.SongPackage, FilePath) -> FilePath -> FilePath -> FilePath -> FilePath -> StackTraceT m ()
importRB3 pkg meta karaoke multitrack hasKicks mid files2x mogg cover coverName dir = do
  stackIO $ Dir.copyFile mogg $ dir </> "audio.mogg"

  isRB2 <- case D.songFormat pkg of
    10 -> return False
    4  -> return True
    -- TODO catalog other values from DLC
    n  -> do
      warn $ "Unrecognized 'format' value (" ++ show n ++ "), assuming RB3 VENUE format"
      return False
  RBFile.Song temps sigs (RBFile.RawFile trks1x) <- loadMIDI mid
  trksAdd2x <- case files2x of
    Nothing -> return trks1x
    Just (_pkg2x, mid2x) -> do
      RBFile.Song _ _ (RBFile.RawFile trks2x) <- loadMIDI mid2x
      let make2xTrack trk = case U.trackName trk of
            Just "PART DRUMS" -> Just $ U.setTrackName "PART DRUMS_2X" trk
            _                 -> Nothing
      return $ trks1x ++ mapMaybe make2xTrack trks2x
  let trksRenameVenue = if isRB2
        then flip map trksAdd2x $ \trk -> case U.trackName trk of
          Just "VENUE" -> U.setTrackName "VENUE RB2" trk
          _            -> trk
        else trksAdd2x
  stackIO $ Save.toFile (dir </> "notes.mid") $ RBFile.showMIDIFile'
    $ RBFile.Song temps sigs $ RBFile.RawFile trksRenameVenue

  stackIO $ Dir.copyFile cover $ dir </> coverName
  md5 <- stackIO $ show . MD5.md5 <$> BL.readFile (dir </> "audio.mogg")
  rb3mid <- loadMIDI mid
  drumkit <- case D.drumBank pkg of
    Nothing -> return HardRockKit
    Just x -> case x of
      "sfx/kit01_bank.milo" -> return HardRockKit
      "sfx/kit02_bank.milo" -> return ArenaKit
      "sfx/kit03_bank.milo" -> return VintageKit
      "sfx/kit04_bank.milo" -> return TrashyKit
      "sfx/kit05_bank.milo" -> return ElectronicKit
      s -> do
        warn $ "Unrecognized drum bank " ++ show s
        return HardRockKit
  let diffMap :: HM.HashMap T.Text Config.Difficulty
      diffMap = let
        -- We assume that if every rank value is a tier boundary,
        -- it's a Magma-produced song where the author selected tiers.
        -- So we should import to tiers, not ranks.
        isTierBoundary (k, v) = case k of
          "drum"        -> (k,) <$> elemIndex v (0 : 1 : drumsDiffMap)
          "guitar"      -> (k,) <$> elemIndex v (0 : 1 : guitarDiffMap)
          "bass"        -> (k,) <$> elemIndex v (0 : 1 : bassDiffMap)
          "vocals"      -> (k,) <$> elemIndex v (0 : 1 : vocalDiffMap)
          "keys"        -> (k,) <$> elemIndex v (0 : 1 : keysDiffMap)
          "real_keys"   -> (k,) <$> elemIndex v (0 : 1 : keysDiffMap)
          "real_guitar" -> (k,) <$> elemIndex v (0 : 1 : proGuitarDiffMap)
          "real_bass"   -> (k,) <$> elemIndex v (0 : 1 : proBassDiffMap)
          "band"        -> (k,) <$> elemIndex v (0 : 1 : bandDiffMap)
          _             -> Nothing
        in case mapM isTierBoundary $ HM.toList $ D.rank pkg of
          Nothing    -> Rank                <$> D.rank pkg
          Just tiers -> Tier . fromIntegral <$> HM.fromList tiers
      hasRankStr s = maybe False (/= 0) $ HM.lookup s $ D.rank pkg
  vocalMode <- if hasRankStr "vocals"
    then case D.vocalParts $ D.song pkg of
      Nothing -> return $ Just Vocal1
      Just 0  -> return Nothing
      Just 1  -> return $ Just Vocal1
      Just 2  -> return $ Just Vocal2
      Just 3  -> return $ Just Vocal3
      n       -> fatal $ "Invalid vocal count of " ++ show n
    else return Nothing
  let hopoThresh = fromIntegral $ fromMaybe 170 $ D.hopoThreshold $ D.song pkg

  -- try to scan MOGG file for silent channels
  silentChannels <- tempDir "moggscan" $ \temp -> do
    let ogg = temp </> "audio.ogg"
    res <- errorToEither $ moggToOgg (dir </> "audio.mogg") ogg
    case res of
      Left  _err -> do
        warn "Couldn't decrypt MOGG to scan for empty channels."
        return []
      Right ()   -> emptyChannels ogg
  stackIO $ putStrLn $ "Detected the following channels as silent: " ++ show silentChannels

  drumMix <- let
    drumEvents = toList $ RBFile.rb3PartDrums $ RBFile.s_tracks rb3mid
    drumMixes = [ aud | RBDrums.DiffEvent _ (RBDrums.Mix aud _) <- drumEvents ]
    in case drumMixes of
      [] -> return RBDrums.D0
      aud : auds -> if all (== aud) auds
        then return aud
        else fatal $ "Inconsistent drum mixes: " ++ show (nub drumMixes)
  let instChans :: HM.HashMap T.Text [Int]
      instChans = fmap (map fromIntegral) $ D.tracks $ D.song pkg
      drumChans = fromMaybe [] $ HM.lookup "drum" instChans
  drumSplit <- if not $ hasRankStr "drum" then return Nothing else case drumMix of
    RBDrums.D0 -> case drumChans of
      [kitL, kitR] -> return $ Just $ PartSingle [kitL, kitR]
      _ -> fatal $ "mix 0 needs 2 drums channels, " ++ show (length drumChans) ++ " given"
    RBDrums.D1 -> case drumChans of
      [kick, snare, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snare]) [kitL, kitR]
      _ -> fatal $ "mix 1 needs 4 drums channels, " ++ show (length drumChans) ++ " given"
    RBDrums.D2 -> case drumChans of
      [kick, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snareL, snareR]) [kitL, kitR]
      _ -> fatal $ "mix 2 needs 5 drums channels, " ++ show (length drumChans) ++ " given"
    RBDrums.D3 -> case drumChans of
      [kickL, kickR, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kickL, kickR]) (Just [snareL, snareR]) [kitL, kitR]
      _ -> fatal $ "mix 3 needs 6 drums channels, " ++ show (length drumChans) ++ " given"
    RBDrums.D4 -> case drumChans of
      [kick, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) Nothing [kitL, kitR]
      _ -> fatal $ "mix 4 needs 3 drums channels, " ++ show (length drumChans) ++ " given"

  stackIO $ Y.encodeFile (dir </> "song.yml") $ toJSON SongYaml
    { _metadata = Metadata
      { _title        = _title meta <|> Just (D.name pkg)
      , _artist       = Just $ D.artist pkg
      , _album        = Just $ fromMaybe "Unknown Album" $ D.albumName pkg
      , _genre        = Just $ D.genre pkg
      , _subgenre     = D.subGenre pkg >>= T.stripPrefix "subgenre_"
      , _year         = Just $ fromIntegral $ D.yearReleased pkg
      , _fileAlbumArt = Just coverName
      , _trackNumber  = fromIntegral <$> D.albumTrackNumber pkg
      , _comments     = []
      , _difficulty   = fromMaybe (Tier 1) $ HM.lookup "band" diffMap
      , _key          = toEnum . fromEnum <$> D.vocalTonicNote pkg
      , _autogenTheme = RBProj.DefaultTheme
      , _author       = _author meta
      , _rating       = toEnum $ fromIntegral $ D.rating pkg - 1
      , _previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ D.preview pkg) / 1000
      , _previewEnd   = Just $ PreviewSeconds $ fromIntegral (snd $ D.preview pkg) / 1000
      , _languages    = _languages meta
      , _convert      = _convert meta
      , _rhythmKeys   = _rhythmKeys meta
      , _rhythmBass   = _rhythmBass meta
      , _catEMH       = _catEMH meta
      , _expertOnly   = _expertOnly meta
      , _cover        = not $ D.master pkg
      }
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton "mogg" MoggPlan
      { _moggMD5 = T.pack md5
      , _moggParts = Parts $ HM.fromList $ concat
        [ [ (FlexGuitar, PartSingle ns) | ns <- toList $ HM.lookup "guitar" instChans ]
        , [ (FlexBass  , PartSingle ns) | ns <- toList $ HM.lookup "bass"   instChans ]
        , [ (FlexKeys  , PartSingle ns) | ns <- toList $ HM.lookup "keys"   instChans ]
        , [ (FlexVocal , PartSingle ns) | ns <- toList $ HM.lookup "vocals" instChans ]
        , [ (FlexDrums , ds           ) | Just ds <- [drumSplit] ]
        ]
      , _moggCrowd  = maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
      , _pans = map realToFrac $ D.pans $ D.song pkg
      , _vols = map realToFrac $ D.vols $ D.song pkg
      , _planComments = []
      , _karaoke = karaoke
      , _multitrack = multitrack
      , _silent = silentChannels
      }
    , _targets = let
      getSongID = \case
        Left  i -> guard (i /= 0) >> Just (Left i)
        Right k -> Just $ Right k
      songID1x = getSongID $ D.songId pkg
      songID2x = if hasKicks == Has2x
        then songID1x
        else files2x >>= getSongID . D.songId . fst
      version1x = songID1x >> Just (D.version pkg)
      version2x = songID2x >> fmap (D.version . fst) files2x
      target1x = ("rb3", RB3 def
        { rb3_2xBassPedal = False
        , rb3_SongID = songID1x
        , rb3_Version = version1x
        })
      target2x = ("rb3-2x", RB3 def
        { rb3_2xBassPedal = True
        , rb3_SongID = songID2x
        , rb3_Version = version2x
        })
      in HM.fromList $ concat [[target1x | hasKicks /= Has2x], [target2x | hasKicks /= Has1x]]
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, def
        { partDrums = guard (hasRankStr "drum") >> Just PartDrums
          { drumsDifficulty = fromMaybe (Tier 1) $ HM.lookup "drum" diffMap
          , drumsPro = True
          , drumsAuto2xBass = False
          , drumsFixFreeform = False
          , drumsKit = drumkit
          , drumsLayout = StandardLayout -- TODO import this
          }
        })
      , ( FlexGuitar, def
        { partGRYBO = guard (hasRankStr "guitar") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "guitar" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProGuitar = guard (hasRankStr "real_guitar") >> Just PartProGuitar
          { pgDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_guitar" diffMap
          , pgHopoThreshold = hopoThresh
          , pgTuning = fromMaybe [] $ map fromIntegral <$> D.realGuitarTuning pkg
          , pgFixFreeform = False
          }
        })
      , ( FlexBass, def
        { partGRYBO = guard (hasRankStr "bass") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "bass" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProGuitar = guard (hasRankStr "real_bass") >> Just PartProGuitar
          { pgDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_bass" diffMap
          , pgHopoThreshold = hopoThresh
          , pgTuning = fromMaybe [] $ map fromIntegral <$> D.realBassTuning pkg
          , pgFixFreeform = False
          }
        })
      , ( FlexKeys, def
        { partGRYBO = guard (hasRankStr "keys") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "keys" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProKeys = guard (hasRankStr "real_keys") >> Just PartProKeys
          { pkDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_keys" diffMap
          , pkFixFreeform = False
          }
        })
      , ( FlexVocal, def
        { partVocal = flip fmap vocalMode $ \vc -> PartVocal
          { vocalDifficulty = fromMaybe (Tier 1) $ HM.lookup "vocals" diffMap
          , vocalCount = vc
          , vocalGender = Just $ D.vocalGender pkg
          }
        })
      ]
    }

-- | TODO
importMagma :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
importMagma fin dir = do

  let oldDir = takeDirectory fin
  RBProj.RBProj rbproj <- stackIO (D.readFileDTA fin) >>= D.unserialize D.stackChunks

  c3 <- do
    let pathC3 = fin -<.> "c3"
    hasC3 <- stackIO $ Dir.doesFileExist pathC3
    if hasC3
      then fmap Just $ stackIO (TIO.readFile pathC3) >>= C3.readC3
      else return Nothing

  let art = fromMaybe (T.unpack $ RBProj.albumArtFile $ RBProj.albumArt rbproj)
        $ C3.songAlbumArt <$> c3
      art' = "album" <.> takeExtension art
  stackIO $ Dir.copyFile (oldDir </> art) (dir </> art')

  let hopoThresh = case fmap C3.hopoThresholdIndex c3 of
        Nothing -> 170
        Just 0  -> 90
        Just 1  -> 130
        Just 2  -> 170
        Just 3  -> 250
        Just _  -> 170

  let getTrack s f = let
        aud = f $ RBProj.tracks rbproj
        in if RBProj.audioEnabled aud
          then return Nothing
          else do
            let src = oldDir </> T.unpack (RBProj.audioFile aud)
                dst = s -<.> takeExtension src
            stackIO $ Dir.copyFile src (dir </> dst)
            return $ Just
              ( PlanAudio
                { _planExpr = Input $ Named $ T.pack s
                , _planPans = map realToFrac $ RBProj.pan aud
                , _planVols = map realToFrac $ RBProj.vol aud
                }
              , ( T.pack s
                , AudioFile AudioInfo
                  { _md5 = Nothing
                  , _frames = Nothing
                  , _filePath = Just dst
                  , _commands = []
                  , _rate = Nothing
                  , _channels = fromIntegral $ RBProj.channels aud
                  }
                )
              )
  drums <- getTrack "drums" RBProj.drumKit
  kick <- getTrack "kick" RBProj.drumKick
  snare <- getTrack "snare" RBProj.drumSnare
  gtr <- getTrack "guitar" RBProj.guitar
  bass <- getTrack "bass" RBProj.bass
  keys <- getTrack "keys" RBProj.keys
  vox <- getTrack "vocal" RBProj.vocals
  song <- getTrack "song" RBProj.backing
  crowd <- case c3 >>= C3.crowdAudio of
    Nothing -> return Nothing
    Just f  -> do
      let src = oldDir </> T.unpack f
          s = "crowd"
          dst = s -<.> takeExtension src
      stackIO $ Dir.copyFile src (dir </> dst)
      chans <- audioChannels src >>= \case
        Just c -> return c
        Nothing -> do
          warn "Couldn't detect crowd audio channels; assuming 2."
          return 2
      return $ Just
        ( PlanAudio
          { _planExpr = Input $ Named $ T.pack s
          , _planPans = []
          , _planVols = toList $ c3 >>= C3.crowdVol
          }
        , ( T.pack s
          , AudioFile AudioInfo
            { _md5 = Nothing
            , _frames = Nothing
            , _filePath = Just dst
            , _commands = []
            , _rate = Nothing
            , _channels = chans
            }
          )
        )
  let allAudio = map snd $ catMaybes [drums, kick, snare, gtr, bass, keys, vox, song, crowd]

  stackIO $ Y.encodeFile (dir </> "song.yml") $ toJSON SongYaml
    { _metadata = Metadata
      { _title        = Just $ RBProj.songName $ RBProj.metadata rbproj
      , _artist       = Just $ RBProj.artistName $ RBProj.metadata rbproj
      , _album        = Just $ RBProj.albumName $ RBProj.metadata rbproj
      , _genre        = Just $ RBProj.genre $ RBProj.metadata rbproj
      , _subgenre     = Just $ RBProj.subGenre $ RBProj.metadata rbproj
      , _year         = Just $ fromIntegral $ RBProj.yearReleased $ RBProj.metadata rbproj
      , _fileAlbumArt = Just art'
      , _trackNumber  = Just $ fromIntegral $ RBProj.trackNumber $ RBProj.metadata rbproj
      , _comments     = []
      , _difficulty   = Tier $ RBProj.rankBand $ RBProj.gamedata rbproj
      , _key          = c3 >>= C3.tonicNote
      , _autogenTheme = case RBProj.autogenTheme $ RBProj.midi rbproj of
        Left theme -> theme
        Right _str -> RBProj.DefaultTheme -- TODO
      , _author       = Just $ RBProj.author $ RBProj.metadata rbproj
      , _rating       = case fmap C3.songRating c3 of
        Nothing -> Unrated
        Just 1  -> FamilyFriendly
        Just 2  -> SupervisionRecommended
        Just 3  -> Mature
        Just 4  -> Unrated
        Just _  -> Unrated
      , _previewStart = Just $ PreviewSeconds $ fromIntegral (RBProj.previewStartMs $ RBProj.gamedata rbproj) / 1000
      , _previewEnd   = Nothing
      , _languages    = let
        lang s f = [s | fromMaybe False $ f $ RBProj.languages rbproj]
        in concat
          [ lang "English"  RBProj.english
          , lang "French"   RBProj.french
          , lang "Italian"  RBProj.italian
          , lang "Spanish"  RBProj.spanish
          , lang "German"   RBProj.german
          , lang "Japanese" RBProj.japanese
          ]
      , _convert      = maybe False C3.convert c3
      , _rhythmKeys   = maybe False C3.rhythmKeys c3
      , _rhythmBass   = maybe False C3.rhythmBass c3
      , _catEMH       = False -- not stored in .c3 file
      , _expertOnly   = maybe False C3.expertOnly c3
      , _cover        = maybe False (not . C3.isMaster) c3
      }
    , _audio = HM.fromList allAudio
    , _jammit = HM.empty
    , _plans = HM.singleton "rbproj" Plan
      { _song = fmap fst song
      , _countin = Countin []
      , _planParts = Parts $ HM.fromList $ concat
        [ case drums of
          Nothing -> []
          Just (drumsAud, _) ->
            [(FlexDrums, case (kick, snare) of
              (Nothing, Nothing) -> PartSingle drumsAud
              _ -> PartDrumKit (fmap fst kick) (fmap fst snare) drumsAud
            )]
        , toList $ fmap (\(aud, _) -> (FlexGuitar, PartSingle aud)) gtr
        , toList $ fmap (\(aud, _) -> (FlexBass  , PartSingle aud)) bass
        , toList $ fmap (\(aud, _) -> (FlexKeys  , PartSingle aud)) keys
        , toList $ fmap (\(aud, _) -> (FlexVocal , PartSingle aud)) vox
        ]
      , _crowd = fmap fst crowd
      , _planComments = []
      }
    , _targets = undefined
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, def
        { partDrums = guard (isJust drums) >> Just PartDrums
          { drumsDifficulty = Tier $ RBProj.rankDrum $ RBProj.gamedata rbproj
          , drumsPro = True -- TODO set to false for magma v1?
          , drumsAuto2xBass = False
          , drumsFixFreeform = False
          , drumsKit = case fmap C3.drumKitSFX c3 of
            Nothing -> HardRockKit
            Just 0  -> HardRockKit
            Just 1  -> ArenaKit
            Just 2  -> VintageKit
            Just 3  -> TrashyKit
            Just 4  -> ElectronicKit
            Just _  -> HardRockKit
          , drumsLayout = StandardLayout
          }
        })
      , ( FlexGuitar, def
        { partGRYBO = guard (isJust gtr) >> Just PartGRYBO
          { gryboDifficulty = Tier $ RBProj.rankGuitar $ RBProj.gamedata rbproj
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProGuitar = undefined
        })
      , ( FlexBass, def
        { partGRYBO = guard (isJust bass) >> Just PartGRYBO
          { gryboDifficulty = Tier $ RBProj.rankBass $ RBProj.gamedata rbproj
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProGuitar = undefined
        })
      , ( FlexKeys, def
        { partGRYBO = guard (isJust keys) >> Just PartGRYBO
          { gryboDifficulty = Tier $ RBProj.rankKeys $ RBProj.gamedata rbproj
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProKeys = guard (isJust keys && maybe False (not . C3.disableProKeys) c3) >> Just PartProKeys
          { pkDifficulty = Tier $ RBProj.rankProKeys $ RBProj.gamedata rbproj
          , pkFixFreeform = False
          }
        })
      , ( FlexVocal, def
        { partVocal = undefined
        })
      ]
    }
