{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import where

import           Audio
import           Codec.Picture                    (convertRGB8, readImage)
import           Config                           hiding (Difficulty)
import           Control.Applicative              ((<|>))
import           Control.Exception                (evaluate)
import           Control.Monad                    (forM, guard, when)
import           Control.Monad.Extra              (mapMaybeM)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.Digest.Pure.MD5             as MD5
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize.RB3           as D
import qualified Data.DTA.Serialize2              as D2
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (nub)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import qualified Data.Yaml                        as Y
import qualified FretsOnFire                      as FoF
import           Image                            (toPNG_XBOX)
import           JSONData                         (JSONEither (..))
import           Magma                            (getRBAFile)
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
import           RockBand2                        (KeysRB2 (..))
import           Scripts                          (loadMIDI)
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           STFS.Extract                     (extractSTFS)
import qualified System.Directory                 as Dir
import           System.FilePath                  (takeDirectory, takeFileName,
                                                   (<.>), (</>))
import           X360                             (rb3pkg)

standardTargets :: Maybe (JSONEither Integer T.Text) -> Maybe Integer -> Bool -> KeysRB2 -> Maybe FilePath -> HM.HashMap T.Text Target
standardTargets songID version is2x krb2 vid = let
  targets1x =
    [ ("rb3", RB3 TargetRB3
        { rb3_Plan = Nothing
        , rb3_2xBassPedal = False
        , rb3_SongID = songID
        , rb3_Label = Nothing
        , rb3_Version = version
        , rb3_Guitar = FlexGuitar
        , rb3_Bass = FlexBass
        , rb3_Drums = FlexDrums
        , rb3_Vocal = FlexVocal
        , rb3_Keys = FlexKeys
        }
      )
    , ("rb2", RB2 TargetRB2
        { rb2_Plan = Nothing
        , rb2_2xBassPedal = False
        , rb2_SongID = songID
        , rb2_Label = Nothing
        , rb2_Version = version
        , rb2_Guitar = case krb2 of KeysGuitar -> FlexKeys; _ -> FlexGuitar
        , rb2_Bass = case krb2 of KeysBass -> FlexKeys; _ -> FlexBass
        , rb2_Drums = FlexDrums
        , rb2_Vocal = FlexVocal
        }
      )
    , ("ps", PS def
        { ps_FileVideo = vid
        }
      )
    ]
  targets2x =
    [ ("rb3-2x", RB3 TargetRB3
        { rb3_Plan = Nothing
        , rb3_2xBassPedal = True
        , rb3_SongID = songID
        , rb3_Label = Nothing
        , rb3_Version = version
        , rb3_Guitar = FlexGuitar
        , rb3_Bass = FlexBass
        , rb3_Drums = FlexDrums
        , rb3_Vocal = FlexVocal
        , rb3_Keys = FlexKeys
        }
      )
    , ("rb2-2x", RB2 TargetRB2
        { rb2_Plan = Nothing
        , rb2_2xBassPedal = True
        , rb2_SongID = songID
        , rb2_Label = Nothing
        , rb2_Version = version
        , rb2_Guitar = case krb2 of KeysGuitar -> FlexKeys; _ -> FlexGuitar
        , rb2_Bass = case krb2 of KeysBass -> FlexKeys; _ -> FlexBass
        , rb2_Drums = FlexDrums
        , rb2_Vocal = FlexVocal
        }
      )
    ]
  in HM.fromList $ targets1x ++ if is2x then targets2x else []

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
          _                                                        -> False
        in (startTime,) $ RTB.fromPairList $ if shouldBeDouble
          then [(0, RBDrums.DoubleRoll True), (len, RBDrums.DoubleRoll False)]
          else [(0, RBDrums.SingleRoll True), (len, RBDrums.SingleRoll False)]
    in RTB.merge (U.trackJoin $ RTB.fromAbsoluteEventList lanesAbs') notLanes
  in ps
    { RBFile.psPartDrums       = withRB fixTrack $ RBFile.psPartDrums       ps
    , RBFile.psPartDrums2x     = withRB fixTrack $ RBFile.psPartDrums2x     ps
    , RBFile.psPartRealDrumsPS = withRB fixTrack $ RBFile.psPartRealDrumsPS ps
    }

importFoF :: (MonadIO m) => KeysRB2 -> FilePath -> FilePath -> StackTraceT m ()
importFoF krb2 src dest = do
  song <- FoF.loadSong $ src </> "song.ini"

  hasAlbumArt <- liftIO $ Dir.doesFileExist $ src </> "album.png"
  when hasAlbumArt $ liftIO $ Dir.copyFile (src </> "album.png") (dest </> "album.png")

  audioFiles <- liftIO $ let
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
  let pad = RBFile.needsPad parsed
      padDelay :: (Num a) => a
      padDelay = if pad then 3 else 0
      maybePadAudio = if pad then Pad Start $ CA.Seconds padDelay else id
      (delayAudio, delayMIDI) = case FoF.delay song of
        Nothing -> (maybePadAudio, RBFile.padMIDI padDelay)
        Just n -> case compare n 0 of
          EQ -> (maybePadAudio, RBFile.padMIDI padDelay)
          GT -> let
            secs = fromIntegral n / 1000
            midiDelay = ceiling secs
            audioDelay = fromIntegral midiDelay - secs
            padDelay' :: (Num a) => a
            padDelay' = if midiDelay >= padDelay then 0 else padDelay - fromIntegral midiDelay
            in (Pad Start $ CA.Seconds $ audioDelay + padDelay', RBFile.padMIDI $ midiDelay + padDelay')
          LT ->
            ( Pad Start $ CA.Seconds $ fromIntegral (abs n) / 1000 + padDelay
            , RBFile.padMIDI padDelay
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

  when pad $ warn $ "Padding FoF/PS song by " ++ show (padDelay :: Int) ++ " seconds due to early start."

  let toTier = fromMaybe (Tier 1) . fmap (\n -> Tier $ max 1 $ min 7 $ fromIntegral n + 1)

  mid2x <- liftIO $ Dir.doesFileExist $ src </> "expert+.mid"
  add2x <- if mid2x
    then do
      parsed2x <- loadMIDI $ src </> "expert+.mid"
      let trk2x = RBFile.psPartDrums $ RBFile.s_tracks parsed2x
      return $ if RTB.null trk2x
        then id
        else \mid -> mid { RBFile.psPartDrums2x = trk2x }
    else return id
  let has2x = or
        [ mid2x
        , elem RBDrums.Kick2x $ discardPS $ RBFile.psPartDrums $ RBFile.s_tracks parsed
        ]

  liftIO $ Save.toFile (dest </> "notes.mid") $ RBFile.showMIDIFile' $ delayMIDI parsed
    { RBFile.s_tracks = fixDoubleSwells $ add2x $ RBFile.s_tracks parsed
    }

  vid <- case FoF.video song of
    Nothing -> return Nothing
    Just s | all isSpace s -> return Nothing
    Just v -> inside "copying PS video file to onyx project" $ do
      liftIO $ Dir.copyFile (src </> v) (dest </> "video.avi")
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

  liftIO $ Y.encodeFile (dest </> "song.yml") SongYaml
    { _metadata = Metadata
      { _title        = FoF.name song
      , _artist       = FoF.artist song
      , _album        = FoF.album song
      , _genre        = FoF.genre song
      , _subgenre     = Nothing
      , _year         = FoF.year song
      , _fileAlbumArt = guard hasAlbumArt >> Just "album.png"
      , _trackNumber  = FoF.track song
      , _comments     = []
      , _key          = Nothing
      , _autogenTheme = AutogenDefault
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
      (T.pack aud, AudioFile
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
    , _targets = standardTargets Nothing Nothing has2x krb2 vid
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, def
        { partDrums = guard (hasTrack RBFile.psPartDrums && FoF.diffDrums song /= Just (-1)) >> Just PartDrums
          { drumsDifficulty = toTier $ FoF.diffDrums song
          , drumsPro = case FoF.proDrums song of
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

determine2xBass :: T.Text -> (T.Text, Bool)
determine2xBass s = case T.stripSuffix " (2x Bass Pedal)" s <|> T.stripSuffix " (2X Bass Pedal)" s of
  Nothing -> (s , False)
  Just s' -> (s', True )

importSTFS :: (MonadIO m) => KeysRB2 -> FilePath -> Maybe FilePath -> FilePath -> StackTraceT m ()
importSTFS krb2 file file2x dir = tempDir "onyx_con" $ \temp -> do
  liftIO $ extractSTFS file temp
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
      with2xPath maybe2x = importRB3 krb2 pkg meta karaoke multitrack is2x
        (temp </> base <.> "mid") maybe2x (temp </> base <.> "mogg")
        (temp </> takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_xbox"))
        "cover.png_xbox" dir
  case file2x of
    Nothing -> with2xPath Nothing
    Just f2x -> tempDir "onyx_con2x" $ \temp2x -> do
      liftIO $ extractSTFS f2x temp2x
      DTASingle _ pkg2x _ <- readDTASingle $ temp2x </> "songs/songs.dta"
      let base2x = T.unpack $ D.songName $ D.song pkg2x
      with2xPath $ Just $ temp2x </> base2x <.> "mid"

-- | Converts a Magma v2 RBA to CON without going through an import + recompile.
simpleRBAtoCON :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m String
simpleRBAtoCON rba con = inside ("converting RBA " ++ show rba ++ " to CON " ++ show con) $ do
  tempDir "onyx_rba2con" $ \temp -> do
    md5 <- liftIO $ BL.readFile rba >>= evaluate . MD5.md5
    let shortName = "onyx" ++ take 10 (show md5)
    liftIO $ Dir.createDirectoryIfMissing True $ temp </> "songs" </> shortName </> "gen"
    getRBAFile 0 rba $ temp </> "temp_songs.dta"
    getRBAFile 1 rba $ temp </> "songs" </> shortName </> shortName <.> "mid"
    getRBAFile 2 rba $ temp </> "songs" </> shortName </> shortName <.> "mogg"
    getRBAFile 3 rba $ temp </> "songs" </> shortName </> "gen" </> shortName <.> "milo_xbox"
    getRBAFile 4 rba $ temp </> "temp_cover.bmp"
    -- 5 is weights.bin (empty in magma v2)
    getRBAFile 6 rba $ temp </> "temp_extra.dta"
    (_, pkg, isUTF8) <- readRB3DTA $ temp </> "temp_songs.dta"
    extra <- (if isUTF8 then D.readFileDTA_utf8' else D.readFileDTA_latin1') $ temp </> "temp_extra.dta"
    liftIO $ TIO.writeFile (temp </> "songs/songs.dta") $ writeDTASingle DTASingle
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
    liftIO $ readImage (temp </> "temp_cover.bmp") >>= \case
      Left err -> error err -- TODO
      Right dyn -> let
        out = temp </> "songs" </> shortName </> "gen" </> (shortName ++ "_keep.png_xbox")
        in BL.writeFile out $ toPNG_XBOX $ convertRGB8 dyn
    liftIO $ do
      Dir.removeFile $ temp </> "temp_songs.dta"
      Dir.removeFile $ temp </> "temp_cover.bmp"
      Dir.removeFile $ temp </> "temp_extra.dta"
    let label = D.name pkg <> " (" <> D.artist pkg <> ")"
    rb3pkg label label temp con

importRBA :: (MonadIO m) => KeysRB2 -> FilePath -> Maybe FilePath -> FilePath -> StackTraceT m ()
importRBA krb2 file file2x dir = tempDir "onyx_rba" $ \temp -> do
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
  mid2x <- forM file2x $ \f2x -> do
    let mid2x = temp </> "notes-2x.mid"
    getRBAFile 1 f2x mid2x
    return mid2x
  importRB3 krb2 pkg meta False True is2x
    (temp </> "notes.mid") mid2x (temp </> "audio.mogg")
    (temp </> "cover.bmp") "cover.bmp" dir

-- | Collects the contents of an RBA or CON file into an Onyx project.
importRB3 :: (MonadIO m) => KeysRB2 -> D.SongPackage -> Metadata -> Bool -> Bool -> Bool -> FilePath -> Maybe FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> StackTraceT m ()
importRB3 krb2 pkg meta karaoke multitrack is2x mid mid2x mogg cover coverName dir = do
  liftIO $ Dir.copyFile mogg $ dir </> "audio.mogg"
  case mid2x of
    Nothing  -> liftIO $ Dir.copyFile mid $ dir </> "notes.mid"
    Just f2x -> do
      RBFile.Song temps sigs (RBFile.RawFile trks1x) <- loadMIDI mid
      RBFile.Song _     _    (RBFile.RawFile trks2x) <- loadMIDI f2x
      let trks = trks1x ++ mapMaybe make2xTrack trks2x
          make2xTrack trk = case U.trackName trk of
            Just "PART DRUMS" -> Just $ U.setTrackName "PART DRUMS_2X" trk
            _                 -> Nothing
      liftIO $ Save.toFile (dir </> "notes.mid") $ RBFile.showMIDIFile'
        $ RBFile.Song temps sigs $ RBFile.RawFile trks
  liftIO $ Dir.copyFile cover $ dir </> coverName
  md5 <- liftIO $ show . MD5.md5 <$> BL.readFile (dir </> "audio.mogg")
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
  let diffMap :: Map.Map T.Text Integer
      diffMap = D2.fromDict $ D.rank pkg
  vocalMode <- if maybe False (/= 0) $ Map.lookup "vocals" diffMap
    then case D.vocalParts $ D.song pkg of
      Nothing -> return $ Just Vocal1
      Just 0  -> return Nothing
      Just 1  -> return $ Just Vocal1
      Just 2  -> return $ Just Vocal2
      Just 3  -> return $ Just Vocal3
      n       -> fatal $ "Invalid vocal count of " ++ show n
    else return Nothing
  let hopoThresh = fromIntegral $ fromMaybe 170 $ D.hopoThreshold $ D.song pkg
  drumMix <- let
    drumEvents = toList $ RBFile.rb3PartDrums $ RBFile.s_tracks rb3mid
    drumMixes = [ aud | RBDrums.DiffEvent _ (RBDrums.Mix aud _) <- drumEvents ]
    in case drumMixes of
      [] -> return RBDrums.D0
      aud : auds -> if all (== aud) auds
        then return aud
        else fatal $ "Inconsistent drum mixes: " ++ show (nub drumMixes)
  let instChans :: Map.Map T.Text [Int]
      instChans = fmap (map fromIntegral) $ D2.fromDict $ D.tracks $ D.song pkg
      drumChans = fromMaybe [] $ Map.lookup "drum" instChans
  drumSplit <- case drumMix of
    RBDrums.D0 -> case drumChans of
      [kitL, kitR] -> return $ PartSingle [kitL, kitR]
      _ -> fatal $ "mix 0 needs 2 drums channels, " ++ show (length drumChans) ++ " given"
    RBDrums.D1 -> case drumChans of
      [kick, snare, kitL, kitR] -> return $ PartDrumKit (Just [kick]) (Just [snare]) [kitL, kitR]
      _ -> fatal $ "mix 1 needs 4 drums channels, " ++ show (length drumChans) ++ " given"
    RBDrums.D2 -> case drumChans of
      [kick, snareL, snareR, kitL, kitR] -> return $ PartDrumKit (Just [kick]) (Just [snareL, snareR]) [kitL, kitR]
      _ -> fatal $ "mix 2 needs 5 drums channels, " ++ show (length drumChans) ++ " given"
    RBDrums.D3 -> case drumChans of
      [kickL, kickR, snareL, snareR, kitL, kitR] -> return $ PartDrumKit (Just [kickL, kickR]) (Just [snareL, snareR]) [kitL, kitR]
      _ -> fatal $ "mix 3 needs 6 drums channels, " ++ show (length drumChans) ++ " given"
    RBDrums.D4 -> case drumChans of
      [kick, kitL, kitR] -> return $ PartDrumKit (Just [kick]) Nothing [kitL, kitR]
      _ -> fatal $ "mix 4 needs 3 drums channels, " ++ show (length drumChans) ++ " given"
  liftIO $ Y.encodeFile (dir </> "song.yml") SongYaml
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
      , _difficulty   = Rank $ fromMaybe 1 $ Map.lookup "band" diffMap
      , _key          = toEnum . fromEnum <$> D.vocalTonicNote pkg
      , _autogenTheme = AutogenDefault
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
    , _plans = HM.singleton "mogg" $ MoggPlan
      { _moggMD5 = T.pack md5
      , _moggParts = Parts $ HM.fromList $ concat
        [ [ (FlexGuitar, PartSingle ns) | ns <- toList $ Map.lookup "guitar" instChans ]
        , [ (FlexBass  , PartSingle ns) | ns <- toList $ Map.lookup "bass"   instChans ]
        , [ (FlexKeys  , PartSingle ns) | ns <- toList $ Map.lookup "keys"   instChans ]
        , [ (FlexVocal , PartSingle ns) | ns <- toList $ Map.lookup "vocals" instChans ]
        , [ (FlexDrums , drumSplit) ]
        ]
      , _moggCrowd  = maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
      , _pans = map realToFrac $ D.pans $ D.song pkg
      , _vols = map realToFrac $ D.vols $ D.song pkg
      , _planComments = []
      , _karaoke = karaoke
      , _multitrack = multitrack
      }
    , _targets = let
      songID = fmap JSONEither $ case D.songId pkg of
        Left  i -> guard (i /= 0) >> Just (Left i)
        Right k -> Just $ Right k
      in standardTargets songID (songID >> Just (D.version pkg)) is2x krb2 Nothing
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, Part
        { partGRYBO = Nothing
        , partProKeys = Nothing
        , partProGuitar = Nothing
        , partDrums = guard (maybe False (/= 0) $ Map.lookup "drum" diffMap) >> Just PartDrums
          { drumsDifficulty = Rank $ fromMaybe 1 $ Map.lookup "drum" diffMap
          , drumsPro = True
          , drumsAuto2xBass = is2x
          , drumsFixFreeform = False
          , drumsKit = drumkit
          , drumsLayout = StandardLayout -- TODO import this
          }
        , partVocal = Nothing
        })
      , ( FlexGuitar, Part
        { partGRYBO = guard (maybe False (/= 0) $ Map.lookup "guitar" diffMap) >> Just PartGRYBO
          { gryboDifficulty = Rank $ fromMaybe 1 $ Map.lookup "guitar" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProKeys = Nothing
        , partProGuitar = guard (maybe False (/= 0) $ Map.lookup "real_guitar" diffMap) >> Just PartProGuitar
          { pgDifficulty = Rank $ fromMaybe 1 $ Map.lookup "real_guitar" diffMap
          , pgHopoThreshold = hopoThresh
          , pgTuning = fromMaybe [] $ map fromIntegral <$> D.realGuitarTuning pkg
          , pgFixFreeform = False
          }
        , partDrums = Nothing
        , partVocal = Nothing
        })
      , ( FlexBass, Part
        { partGRYBO = guard (maybe False (/= 0) $ Map.lookup "bass" diffMap) >> Just PartGRYBO
          { gryboDifficulty = Rank $ fromMaybe 1 $ Map.lookup "bass" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProKeys = Nothing
        , partProGuitar = guard (maybe False (/= 0) $ Map.lookup "real_bass" diffMap) >> Just PartProGuitar
          { pgDifficulty = Rank $ fromMaybe 1 $ Map.lookup "real_bass" diffMap
          , pgHopoThreshold = hopoThresh
          , pgTuning = fromMaybe [] $ map fromIntegral <$> D.realBassTuning pkg
          , pgFixFreeform = False
          }
        , partDrums = Nothing
        , partVocal = Nothing
        })
      , ( FlexKeys, Part
        { partGRYBO = guard (maybe False (/= 0) $ Map.lookup "keys" diffMap) >> Just PartGRYBO
          { gryboDifficulty = Rank $ fromMaybe 1 $ Map.lookup "keys" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          }
        , partProKeys = guard (maybe False (/= 0) $ Map.lookup "real_keys" diffMap) >> Just PartProKeys
          { pkDifficulty = Rank $ fromMaybe 1 $ Map.lookup "real_keys" diffMap
          , pkFixFreeform = False
          }
        , partProGuitar = Nothing
        , partDrums = Nothing
        , partVocal = Nothing
        })
      , ( FlexVocal, Part
        { partGRYBO = Nothing
        , partProKeys = Nothing
        , partProGuitar = Nothing
        , partDrums = Nothing
        , partVocal = flip fmap vocalMode $ \vc -> PartVocal
          { vocalDifficulty = Rank $ fromMaybe 1 $ Map.lookup "vocals" diffMap
          , vocalCount = vc
          , vocalGender = Just $ D.vocalGender pkg
          }
        })
      ]
    }
