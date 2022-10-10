{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Build.GuitarHero3 (gh3Rules) where

import           Audio
import           Build.Common
import           Config                           hiding (Difficulty)
import           Control.Monad                    (forM_, guard, when)
import           Control.Monad.Trans.Resource     (runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Put                  (putWord32be, runPut)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Conduit.Audio
import           Data.Conduit.Audio.LAME          (sinkMP3WithHandle)
import qualified Data.Conduit.Audio.LAME.Binding  as L
import           Data.DTA.Serialize.Magma         (Gender (..))
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Hashable                    (Hashable, hash)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
import           Data.SimpleHandle                (Folder (..), fileReadable)
import qualified Data.Text                        as T
import           Development.Shake                hiding (phony, (%>), (&%>))
import           Development.Shake.FilePath
import           Guitars                          (closeNotes', noOpenNotes',
                                                   strumHOPOTap')
import           Neversoft.Audio                  (gh3Encrypt)
import           Neversoft.Checksum               (qbKeyCRC)
import           Neversoft.GH3
import           Neversoft.Metadata               (gh3MysteryScript)
import           Neversoft.Pak                    (Node (..), buildPak)
import           Neversoft.QB
import           Resources                        (gh3Thumbnail)
import           RockBand.Codec.Beat              (BeatTrack (..))
import           RockBand.Codec.Events
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.File              (shakeMIDI)
import qualified RockBand.Codec.Five              as Five
import           RockBand.Common                  (Difficulty (..))
import           RockBand.Sections                (makePSSection)
import           RockBand3                        (BasicTiming (..),
                                                   basicTiming)
import           Sound.FSB                        (emitFSB, mp3sToFSB3)
import qualified Sound.MIDI.Util                  as U
import           STFS.Package                     (CreateOptions (..),
                                                   LicenseEntry (..),
                                                   makeCONReadable)
import           System.IO                        (IOMode (..), hFileSize,
                                                   withBinaryFile)

hashGH3 :: (Hashable f) => SongYaml f -> TargetGH3 -> Int
hashGH3 songYaml gh3 = let
  hashed =
    ( gh3
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

gh3Rules :: BuildInfo -> FilePath -> TargetGH3 -> QueueLog Rules ()
gh3Rules buildInfo dir gh3 = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo

  (planName, plan) <- case getPlan (tgt_Plan $ gh3_Common gh3) songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh3
    Just pair -> return pair
  let planDir = rel $ "gen/plan" </> T.unpack planName

  -- short name doesn't have to be "dlc*", so we add title/artist snippets by default.
  -- makeShortName applies max of 27 chars which works with GH3 max of 26.
  -- ("<shortname>_song.pak.xen" must fit in STFS's max of 40 chars)
  let hashed = hashGH3 songYaml gh3
      dlcID = B8.pack $ T.unpack $ case gh3_SongID gh3 of
        Nothing          -> makeShortName hashed songYaml
        Just (Left  n  ) -> makeShortName n      songYaml
        Just (Right str) -> str
      dl = "dl" <> show (fromMaybe hashed $ gh3_DL gh3)

  let coopPart = case gh3_Coop gh3 of
        GH2Bass   -> gh3_Bass   gh3
        GH2Rhythm -> gh3_Rhythm gh3
      gh3Parts = [gh3_Guitar gh3, coopPart]
      loadOnyxMidi :: Staction (RBFile.Song (RBFile.OnyxFile U.Beats))
      loadOnyxMidi = shakeMIDI $ planDir </> "raw.mid"

  let pathGuitar  = dir </> "guitar.mp3"
      pathRhythm  = dir </> "rhythm.mp3"
      pathSong    = dir </> "song.mp3"
      pathPreview = dir </> "preview.mp3"
      setup lame = liftIO $ do
        L.check $ L.setBrate lame 128
        L.check $ L.setQuality lame 5
        L.check $ L.setOutSamplerate lame 44100 -- SanicStudios uses this, seems to work
  pathGuitar %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo gh3Parts (gh3_Common gh3) mid 0 planName plan
      [(gh3_Guitar gh3, 1)]
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ clampIfSilent s
  pathRhythm %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo gh3Parts (gh3_Common gh3) mid 0 planName plan
      [(coopPart, 1)]
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ clampIfSilent s
  pathSong %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceSongCountin buildInfo (gh3_Common gh3) mid 0 True planName plan
      [ (gh3_Guitar gh3, 1)
      , (coopPart      , 1)
      ]
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ clampIfSilent s
  pathPreview %> \out -> do
    mid <- shakeMIDI $ planDir </> "processed.mid"
    let (pstart, pend) = previewBounds songYaml (mid :: RBFile.Song (RBFile.OnyxFile U.Beats)) 0 False
        fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
        previewExpr
          = Fade End (Seconds 5)
          $ Fade Start (Seconds 2)
          $ Take Start (fromMS $ pend - pstart)
          $ Drop Start (fromMS pstart)
          $ Input (planDir </> "everything.wav")
    src <- shk $ buildSource previewExpr
    stackIO $ runResourceT $ sinkMP3WithHandle out setup src

  let pathFsb = dir </> "audio.fsb"
      pathFsbXen = dir </> "gh3" </> (B8.unpack dlcID <> ".fsb.xen")
      pathDatXen = dir </> "gh3" </> (B8.unpack dlcID <> ".dat.xen")
      -- for some reason ffmpeg errors trying to read empty mp3s back!
      -- so we just look at filesize instead as a hack
      testSize f = stackIO $ (>= 5000) <$> withBinaryFile f ReadMode hFileSize
  pathFsb %> \out -> do
    shk $ need [pathGuitar, pathPreview, pathRhythm, pathSong]
    hasGuitarAudio <- testSize pathGuitar
    hasRhythmAudio <- testSize pathRhythm
    let tracks = catMaybes
          [ Just ("onyx_preview.mp3", pathPreview)
          , Just ("onyx_song.mp3", pathSong)
          , guard hasGuitarAudio >> Just ("onyx_guitar.mp3", pathGuitar)
          , guard hasRhythmAudio >> Just ("onyx_rhythm.mp3", pathRhythm)
          ]
    fsb <- mapM (\(name, path) -> do
      bs <- stackIO $ BL.readFile path
      return (name, bs)
      ) tracks >>= mp3sToFSB3
    stackIO $ BL.writeFile out $ emitFSB fsb
  pathFsbXen %> \out -> do
    shk $ need [pathFsb]
    fsb <- stackIO $ BL.readFile pathFsb
    stackIO $ BL.writeFile out $ gh3Encrypt fsb
  pathDatXen %> \out -> do
    shk $ need [pathGuitar, pathRhythm, pathFsbXen]
    hasGuitarAudio <- testSize pathGuitar
    hasRhythmAudio <- testSize pathRhythm
    fsbLength <- stackIO $ withBinaryFile pathFsbXen ReadMode hFileSize
    stackIO $ BL.writeFile out $ runPut $ do
      let count = 2 + (if hasGuitarAudio then 1 else 0) + (if hasRhythmAudio then 1 else 0)
      putWord32be count
      putWord32be $ fromIntegral fsbLength
      let datEntry slot index = do
            putWord32be $ qbKeyCRC $ dlcID <> "_" <> slot
            putWord32be index
            putWord32be 0
            putWord32be 0
            putWord32be 0
      datEntry "preview" 0
      datEntry "song" 1
      when hasGuitarAudio $ datEntry "guitar" 2
      when hasRhythmAudio $ datEntry "rhythm" $ if hasGuitarAudio then 3 else 2

  let pathDL = dir </> "gh3" </> (dl <> ".pak.xen")
      pathText = dir </> "gh3" </> (dl <> "_text.pak.xen")
      pathTextLangs = map
        (\lang -> dir </> "gh3" </> (dl <> "_text_" <> lang <> ".pak.xen"))
        ["f", "g", "i", "s"]
      pathSongPak = dir </> "gh3" </> (B8.unpack dlcID <> "_song.pak.xen")
  pathDL %> \out -> gh3MysteryScript (B8.pack dl) >>= stackIO . BL.writeFile out
  pathText %> \out -> do
    -- section names would also go in here, but we'll try to use the embedded section name format
    let nodes =
          [ ( Node {nodeFileType = qbKeyCRC ".qb", nodeOffset = 0, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = unk1, nodeFilenameCRC = unk2, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , putQB
              [ QBSectionStruct (qbKeyCRC "GH3_Download_Songs") unk1
                [ QBStructHeader
                , QBStructItemString830000 (qbKeyCRC "prefix") "download"
                , QBStructItemInteger810000 (qbKeyCRC "num_tiers") 1
                , QBStructItemStruct8A0000 (qbKeyCRC "tier1")
                  [ QBStructHeader
                  , QBStructItemStringW (qbKeyCRC "title") "Downloaded songs"
                  , QBStructItemArray8C0000 (qbKeyCRC "songs") $ QBArrayOfQbKey [qbKeyCRC dlcID]
                  , QBStructItemQbKey8D0000 0 (qbKeyCRC "unlockall")
                  , QBStructItemQbKey8D0000 (qbKeyCRC "level") (qbKeyCRC "load_z_artdeco")
                  ]
                ]
              , QBSectionArray (qbKeyCRC "download_songlist") unk1 $ QBArrayOfQbKey [qbKeyCRC dlcID]
              , QBSectionStruct (qbKeyCRC "download_songlist_props") unk1
                [ QBStructHeader
                , QBStructItemStruct8A0000 (qbKeyCRC dlcID)
                  [ QBStructHeader
                  , QBStructItemQbKey8D0000 (qbKeyCRC "checksum") (qbKeyCRC dlcID)
                  , QBStructItemString830000 (qbKeyCRC "name") dlcID
                  , QBStructItemStringW (qbKeyCRC "title") $ targetTitle songYaml (GH3 gh3)
                  , QBStructItemStringW (qbKeyCRC "artist") $ getArtist $ _metadata songYaml
                  , QBStructItemStringW (qbKeyCRC "year") $ T.pack $ ", " <> show (getYear $ _metadata songYaml)
                  , QBStructItemQbKeyString9A0000 (qbKeyCRC "artist_text") $ if _cover $ _metadata songYaml
                    then qbKeyCRC "artist_text_as_made_famous_by"
                    else qbKeyCRC "artist_text_by"
                  , QBStructItemInteger810000 (qbKeyCRC "original_artist") 0 -- TODO what is this? doesn't mean cover/master
                  , QBStructItemQbKey8D0000 (qbKeyCRC "version") (qbKeyCRC "gh3")
                  , QBStructItemInteger810000 (qbKeyCRC "leaderboard") 1
                  , QBStructItemInteger810000 (qbKeyCRC "gem_offset") 0
                  , QBStructItemInteger810000 (qbKeyCRC "input_offset") 0
                  , QBStructItemQbKey8D0000 (qbKeyCRC "singer") $ case getPart (gh3_Vocal gh3) songYaml >>= partVocal of
                    Nothing -> qbKeyCRC "none"
                    Just pv -> case vocalGender pv of
                      Just Female -> qbKeyCRC "female"
                      Just Male   -> qbKeyCRC "male"
                      Nothing     -> qbKeyCRC "male"
                  , QBStructItemQbKey8D0000 (qbKeyCRC "keyboard") $ case getPart (gh3_Keys gh3) songYaml >>= partGRYBO of
                    Nothing -> qbKeyCRC "false"
                    Just _  -> qbKeyCRC "true"
                  , QBStructItemInteger810000 (qbKeyCRC "band_playback_volume") 0
                  , QBStructItemInteger810000 (qbKeyCRC "guitar_playback_volume") 0
                  , QBStructItemString830000 (qbKeyCRC "countoff") "HiHat01"
                  , QBStructItemInteger810000 (qbKeyCRC "rhythm_track") $ case gh3_Coop gh3 of
                    GH2Bass   -> 0
                    GH2Rhythm -> 1
                  ]
                ]
              ]
            )
          , ( Node {nodeFileType = qbKeyCRC ".last", nodeOffset = 1, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qbKeyCRC "chunk.last", nodeFilenameCRC = qbKeyCRC "chunk", nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , BL.replicate 4 0xAB
            )
          ]
        -- don't know the actual prefix strings but these work
        unk1 = qbKeyCRC $ "1o99lm\\" <> B8.pack dl <> ".qb" -- in dl15_text.pak it's 1945578562. in disc qb.pak.xen it's 0x0b761ea7 "scripts\\guitar\\songlist.qb"
        unk2 = qbKeyCRC $ "7buqvk" <> B8.pack dl -- in dl15_text.pak it's 480172672. in disc qb.pak.xen it's 3114035354 "songlist"
    stackIO $ BL.writeFile out $ buildPak nodes
  forM_ pathTextLangs $ \lang -> lang %> \out -> do
    -- being lazy and copying english file. really we ought to swap out some translated strings
    shk $ copyFile' pathText out
  pathSongPak %> \out -> do
    mid <- shakeMIDI $ planDir </> "processed.mid"
    timing <- basicTiming mid $ getAudioLength buildInfo planName plan
    let nodes =
          [ ( Node {nodeFileType = qbKeyCRC ".qb", nodeOffset = 0, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = key1, nodeFilenameCRC = key2, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , putQB $ makeMidQB key1 dlcID $ makeGH3MidQB
              mid
              timing
              (getPartInfo $ gh3_Guitar gh3)
              (getPartInfo coopPart)
            )
          -- there's a small script qb here in official DLC. but SanicStudios customs don't have it
          -- .ska files would go here if we had any
          , ( Node {nodeFileType = qbKeyCRC ".last", nodeOffset = 1, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qbKeyCRC "chunk.last", nodeFilenameCRC = qbKeyCRC "chunk", nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , BL.replicate 4 0xAB
            )
          ]
        key1 = qbKeyCRC $ "songs\\" <> dlcID <> ".mid.qb"
        key2 = qbKeyCRC dlcID
        getPartInfo fpart = case getPart fpart songYaml >>= partGRYBO of
          Nothing -> (fpart, 170)
          Just pg -> (fpart, gryboHopoThreshold pg)
    stackIO $ BL.writeFile out $ buildPak nodes

  let gh3Files = pathFsbXen : pathDatXen : pathDL : pathText : pathSongPak : pathTextLangs
  phony (dir </> "gh3") $ do
    shk $ need gh3Files
  dir </> "gh3live" %> \out -> do
    let files = map (\f -> (takeFileName f, f)) gh3Files
        folder = Folder
          { folderSubfolders = []
          , folderFiles = map (\(dest, src) -> (T.pack dest, fileReadable src)) files
          }
        title = targetTitle songYaml (GH3 gh3)
        artist = getArtist $ _metadata songYaml
        packageTitles =
          [ title <> " by " <> artist
          , ""
          , "„" <> title <> "“ von " <> artist
          , title <> " par " <> artist
          , title <> " de " <> artist
          , title <> " degli " <> artist
          ]
    shk $ need $ map snd files
    thumb <- stackIO $ gh3Thumbnail >>= B.readFile
    stackIO $ makeCONReadable CreateOptions
      { createNames = packageTitles
      , createDescriptions = []
      , createTitleID = 0x415607F7
      , createTitleName = "Guitar Hero 3"
      , createThumb = thumb
      , createTitleThumb = thumb
      , createLicenses = [LicenseEntry (-1) 1 1, LicenseEntry (-1) 1 0]
      , createMediaID       = 0
      , createVersion       = 0
      , createBaseVersion   = 0
      , createTransferFlags = 0xC0
      , createLIVE = True
      } folder out

makeGH3MidQB
  :: RBFile.Song (RBFile.OnyxFile U.Beats)
  -> BasicTiming
  -> (RBFile.FlexPartName, Int)
  -> (RBFile.FlexPartName, Int)
  -> GH3MidQB
makeGH3MidQB song timing partLead partRhythm = let
  makeGH3Part (fpart, threshold) = let
    opart = RBFile.getFlexPart fpart $ RBFile.s_tracks song
    (trk, algo) = RBFile.selectGuitarTrack RBFile.FiveTypeGuitar opart
    in GH3Part
      { gh3Easy   = makeGH3Track Easy   trk algo threshold
      , gh3Medium = makeGH3Track Medium trk algo threshold
      , gh3Hard   = makeGH3Track Hard   trk algo threshold
      , gh3Expert = makeGH3Track Expert trk algo threshold
      }
  makeGH3Track diff trk algo threshold = let
    fiveDiff = fromMaybe mempty $ Map.lookup diff $ Five.fiveDifficulties trk
    sht = noOpenNotes' $ strumHOPOTap' algo (fromIntegral threshold / 480) $ closeNotes' fiveDiff
    in GH3Track
      { gh3Notes       = makeGH3TrackNotes (RBFile.s_tempos song) timeSigs beats sht
      , gh3StarPower   = [] -- TODO
      , gh3BattleStars = []
      }
  timeSigs = [(0, 4, 4)] -- TODO
  beats
    = map (\secs -> floor $ secs * 1000)
    $ ATB.getTimes
    $ RTB.toAbsoluteEventList 0
    $ U.applyTempoTrack (RBFile.s_tempos song)
    $ beatLines
    $ timingBeat timing
  sections
    = map (\(secs, (_, sect)) -> (floor $ secs * 1000, Right $ snd $ makePSSection sect))
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0
    $ U.applyTempoTrack (RBFile.s_tempos song)
    $ eventsSections (RBFile.getEventsTrack $ RBFile.s_tracks song)
  in emptyMidQB
    { gh3Guitar         = makeGH3Part partLead
    , gh3Rhythm         = makeGH3Part partRhythm
    , gh3TimeSignatures = timeSigs
    , gh3FretBars       = beats
    , gh3Markers        = sections
    , gh3P1FaceOff      = [] -- TODO
    , gh3P2FaceOff      = [] -- TODO
    }
