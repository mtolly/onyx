{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Build.GuitarHero3 (gh3Rules) where

import           Control.Monad                     (forM_, guard, when)
import           Control.Monad.Trans.Resource      (runResourceT)
import           Data.Binary.Put                   (putWord32be, runPut)
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as B8
import qualified Data.ByteString.Lazy              as BL
import           Data.Char                         (toUpper)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.LAME           (sinkMP3WithHandle)
import qualified Data.Conduit.Audio.LAME.Binding   as L
import qualified Data.EventList.Absolute.TimeBody  as ATB
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Foldable                     (toList)
import           Data.Hashable                     (Hashable, hash)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Text                         as T
import           Data.Tuple                        (swap)
import           Data.Word                         (Word32)
import           Development.Shake                 hiding (phony, (%>))
import           Development.Shake.FilePath
import           Onyx.Audio
import           Onyx.Audio.FSB                    (emitFSB, mp3sToFSB3)
import           Onyx.Build.Common
import           Onyx.Build.RB3CH                  (BasicTiming (..),
                                                    basicTiming)
import           Onyx.Guitar                       (gh3LegalHOPOs, noOpenNotes)
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.MIDI.Common                  (Difficulty (..), Edge (..),
                                                    joinEdgesSimple, trackGlue)
import           Onyx.MIDI.Track.Beat              (BeatEvent (..))
import           Onyx.MIDI.Track.Drums.True        (TrueDrumNote (..),
                                                    animationToTrueDrums)
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File              as F
import qualified Onyx.MIDI.Track.FiveFret          as Five
import           Onyx.Mode
import           Onyx.Neversoft.CRC                (qbKeyCRC)
import           Onyx.Neversoft.Crypt              (gh3Encrypt)
import           Onyx.Neversoft.GH3.Metadata       (gh3MysteryScript)
import           Onyx.Neversoft.GH3.MidQB
import           Onyx.Neversoft.Pak                (Node (..), buildPak)
import           Onyx.Neversoft.QB
import           Onyx.PlayStation.NPData           (gh3CustomMidEdatConfig,
                                                    npdContentID, packNPData)
import           Onyx.PlayStation.PKG              (makePKG)
import           Onyx.Project                      hiding (Difficulty)
import           Onyx.Reductions                   (completeFiveResult)
import           Onyx.Resources                    (getResourcesPath,
                                                    gh3Thumbnail)
import           Onyx.Sections                     (makeDisplaySection,
                                                    sectionBody)
import           Onyx.StackTrace
import           Onyx.Util.Files                   (shortWindowsPath)
import           Onyx.Util.Handle                  (Folder (..), fileReadable)
import           Onyx.Xbox.STFS                    (CreateOptions (..),
                                                    LicenseEntry (..),
                                                    makeCONReadable)
import qualified Sound.MIDI.File.Event             as E
import qualified Sound.MIDI.File.Event.Meta        as Meta
import qualified Sound.MIDI.Util                   as U
import           System.IO                         (IOMode (..), hFileSize,
                                                    withBinaryFile)

hashGH3 :: (Hashable f) => SongYaml f -> TargetGH3 f -> Int
hashGH3 songYaml gh3 = let
  hashed =
    ( gh3
    , songYaml.metadata.title
    , songYaml.metadata.artist
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

gh3Rules :: BuildInfo -> FilePath -> TargetGH3 FilePath -> QueueLog Rules ()
gh3Rules buildInfo dir gh3 = do

  let songYaml = biSongYaml buildInfo

  (planName, plan) <- case getPlan gh3.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh3
    Just pair -> return pair
  let planDir = biGen buildInfo $ "plan" </> T.unpack planName

  -- short name doesn't have to be "dlc*", so we add title/artist snippets by default.
  -- makeShortName applies max of 27 chars which works with GH3 max of 26.
  -- ("<shortname>_song.pak.xen" must fit in STFS's max of 40 chars)
  let hashed = hashGH3 songYaml gh3
      dlcID = B8.pack $ T.unpack $ case gh3.songID of
        Nothing          -> makeShortName hashed songYaml
        Just (Left  n  ) -> makeShortName n      songYaml
        Just (Right str) -> str
      dl = "dl" <> show (fromMaybe hashed gh3.dl)
      ps3Folder = makePS3Name hashed songYaml
      edatConfig = gh3CustomMidEdatConfig ps3Folder

  let coopPart = case gh3.coop of
        GH2Bass   -> gh3.bass
        GH2Rhythm -> gh3.rhythm
      gh3Parts = [gh3.guitar, coopPart]
      loadOnyxMidi :: Staction (F.Song (F.OnyxFile U.Beats))
      loadOnyxMidi = F.shakeMIDI $ planDir </> "raw.mid"

  let pathGuitar  = dir </> "guitar.mp3"
      pathRhythm  = dir </> "rhythm.mp3"
      pathSong    = dir </> "song.mp3"
      pathPreview = dir </> "preview.mp3"
      setup lame = liftIO $ do
        L.check $ L.setBrate lame 128
        L.check $ L.setQuality lame 5
        L.check $ L.setOutSamplerate lame 44100 -- SanicStudios uses this, seems to work
      pathPad = dir </> "pad.txt"
      readPad :: Staction Int
      readPad = shk $ read <$> readFile' pathPad
  pathGuitar %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo gh3Parts gh3.common mid 0 planName plan
      [(gh3.guitar, 1)]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ padAudio pad $ clampIfSilent s
  pathRhythm %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo gh3Parts gh3.common mid 0 planName plan
      [(coopPart, 1)]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ padAudio pad $ clampIfSilent s
  pathSong %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceBacking buildInfo gh3.common mid 0 planName plan
      [ (gh3.guitar, 1)
      , (coopPart      , 1)
      ]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ padAudio pad $ clampIfSilent s
  pathPreview %> \out -> do
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    -- Pad passed as 0 because we are just applying it to the unpadded audio
    let (pstart, pend) = previewBounds metadata (mid :: F.Song (F.OnyxFile U.Beats)) 0 False
        fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
        metadata = getTargetMetadata songYaml $ GH3 gh3
        previewExpr
          = Fade End (Seconds 5)
          $ Fade Start (Seconds 2)
          $ Take Start (fromMS $ pend - pstart)
          $ Drop Start (fromMS pstart)
          $ Input (planDir </> "everything.wav")
    src <- shk $ buildSource previewExpr
    stackIO $ runResourceT $ sinkMP3WithHandle out setup
      $ applySpeedAudio gh3.common src

  let pathFsb = dir </> "audio.fsb"
      pathFsbXen = dir </> "xbox" </> (B8.unpack dlcID <> ".fsb.xen")
      pathDatXen = dir </> "xbox" </> (B8.unpack dlcID <> ".dat.xen")
      -- for some reason ffmpeg errors trying to read empty mp3s back!
      -- so we just look at filesize instead as a hack
      testSize f = stackIO $ (>= 5000) <$> withBinaryFile f ReadMode hFileSize
  pathFsb %> \out -> do
    shk $ need [pathGuitar, pathPreview, pathRhythm, pathSong]
    tracks <- catMaybes <$> mapM
      (\(name, path) -> do
        hasAudio <- testSize path
        return $ guard hasAudio >> Just (name, path)
      )
      -- have to use testSize for preview as well, for weird songs where preview starts after song ends
      [ ("onyx_preview.mp3", pathPreview)
      , ("onyx_song.mp3", pathSong)
      , ("onyx_guitar.mp3", pathGuitar)
      , ("onyx_rhythm.mp3", pathRhythm)
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

  let pathDL = dir </> "xbox" </> (dl <> ".pak.xen")
      pathText = dir </> "xbox" </> (dl <> "_text.pak.xen")
      pathTextLangs = map
        (\lang -> dir </> "xbox" </> (dl <> "_text_" <> lang <> ".pak.xen"))
        ["f", "g", "i", "s"]
      pathSongPak = dir </> "xbox" </> (B8.unpack dlcID <> "_song.pak.xen")
  pathDL %> \out -> gh3MysteryScript (B8.pack dl) >>= stackIO . BL.writeFile out
  pathText %> \out -> do
    -- section names would also go in here, but we'll try to use the embedded section name format
    let metadata = getTargetMetadata songYaml $ GH3 gh3
        nodes =
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
                  , QBStructItemStringW (qbKeyCRC "artist") $ getArtist metadata
                  -- Need to have a year string, even if empty. Otherwise it glitches out and takes other strings' values
                  , QBStructItemStringW (qbKeyCRC "year") $ case metadata.year of
                    Nothing   -> ""
                    Just year -> T.pack $ ", " <> show year
                  , QBStructItemQbKeyString9A0000 (qbKeyCRC "artist_text") $ if metadata.cover
                    then qbKeyCRC "artist_text_as_made_famous_by"
                    else qbKeyCRC "artist_text_by"
                  , QBStructItemInteger810000 (qbKeyCRC "original_artist") 0 -- TODO what is this? doesn't mean cover/master
                  , QBStructItemQbKey8D0000 (qbKeyCRC "version") (qbKeyCRC "gh3")
                  , QBStructItemInteger810000 (qbKeyCRC "leaderboard") 1
                  , QBStructItemInteger810000 (qbKeyCRC "gem_offset") 0
                  , QBStructItemInteger810000 (qbKeyCRC "input_offset") 0
                  , QBStructItemQbKey8D0000 (qbKeyCRC "singer") $ case getPart gh3.vocal songYaml >>= (.vocal) of
                    Nothing -> qbKeyCRC "none"
                    Just pv -> case pv.gender of
                      Just Female -> qbKeyCRC "female"
                      Just Male   -> qbKeyCRC "male"
                      Nothing     -> qbKeyCRC "male"
                  , QBStructItemQbKey8D0000 (qbKeyCRC "keyboard") $ case getPart gh3.keys songYaml >>= (.grybo) of
                    Nothing -> qbKeyCRC "false"
                    Just _  -> qbKeyCRC "true"
                  , QBStructItemInteger810000 (qbKeyCRC "band_playback_volume") 0
                  , QBStructItemInteger810000 (qbKeyCRC "guitar_playback_volume") 0
                  , QBStructItemString830000 (qbKeyCRC "countoff") "HiHat01"
                  , QBStructItemInteger810000 (qbKeyCRC "rhythm_track") $ case gh3.coop of
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
        -- don't know the actual prefix strings but these give same results.
        -- also ps3 appears to have different names. but they probably don't matter anyway
        unk1 = qbKeyCRC $ "1o99lm\\" <> B8.pack dl <> ".qb" -- in dl15_text.pak it's 1945578562. in disc qb.pak.xen it's 0x0b761ea7 "scripts\\guitar\\songlist.qb"
        unk2 = qbKeyCRC $ "7buqvk" <> B8.pack dl -- in dl15_text.pak it's 480172672. in disc qb.pak.xen it's 3114035354 "songlist"
    stackIO $ BL.writeFile out $ buildPak nodes
  forM_ pathTextLangs $ \lang -> lang %> \out -> do
    -- being lazy and copying english file. really we ought to swap out some translated strings
    shk $ copyFile' pathText out
  [pathSongPak, pathPad] %> \_ -> do
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    let midApplied = applyTargetMIDI gh3.common mid
    timing <- basicTiming False midApplied $ getAudioLength buildInfo planName plan
    let (gh3Mid, padSeconds) = makeGH3MidQB
          songYaml
          midApplied
          timing
          gh3.guitar
          coopPart
          gh3.drums
        nodes =
          [ ( Node {nodeFileType = qbKeyCRC ".qb", nodeOffset = 0, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = key1, nodeFilenameCRC = key2, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , putQB $ makeMidQB key1 dlcID gh3Mid
            )
          -- there's a small script qb here in official DLC. but SanicStudios customs don't have it
          -- .ska files would go here if we had any
          , ( Node {nodeFileType = qbKeyCRC ".last", nodeOffset = 1, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qbKeyCRC "chunk.last", nodeFilenameCRC = qbKeyCRC "chunk", nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , BL.replicate 4 0xAB
            )
          ]
        key1 = qbKeyCRC $ "songs\\" <> dlcID <> ".mid.qb"
        key2 = qbKeyCRC dlcID
    stackIO $ BL.writeFile pathSongPak $ buildPak nodes
    stackIO $ writeFile pathPad $ show padSeconds

  let gh3Files = pathFsbXen : pathDatXen : pathDL : pathText : pathSongPak : pathTextLangs
  phony (dir </> "xbox") $ do
    shk $ need gh3Files
  dir </> "gh3live" %> \out -> do
    let files = map (\f -> (takeFileName f, f)) gh3Files
        folder = Folder
          { folderSubfolders = []
          , folderFiles = map (\(dest, src) -> (T.pack dest, fileReadable src)) files
          }
        title = targetTitle songYaml (GH3 gh3)
        artist = getArtist $ getTargetMetadata songYaml $ GH3 gh3
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

  let ps3Root = dir </> "ps3"
      ps3Fsb = ps3Root </> map toUpper (B8.unpack dlcID <> ".fsb.ps3.edat")
      ps3Dat = ps3Root </> map toUpper (B8.unpack dlcID <> ".dat.ps3.edat")
      ps3DL = ps3Root </> map toUpper (dl <> ".pak.ps3.edat")
      ps3Text = ps3Root </> map toUpper (dl <> "_text.pak.ps3.edat")
      ps3SongPak = ps3Root </> map toUpper (B8.unpack dlcID <> "_song.pak.ps3.edat")
      ps3TextLangs = map
        (\lang -> ps3Root </> map toUpper (dl <> "_text_" <> lang <> ".pak.ps3.edat"))
        ["f", "g", "i", "s"]
      makeEDAT fin fout = do
        shk $ need [fin]
        fin'  <- shortWindowsPath False fin
        fout' <- shortWindowsPath True  fout
        stackIO $ packNPData edatConfig fin' fout' $ B8.pack $ takeFileName fout
  ps3Fsb     %> makeEDAT pathFsbXen
  ps3Dat     %> makeEDAT pathDatXen
  ps3DL      %> makeEDAT pathDL
  ps3Text    %> makeEDAT pathText
  ps3SongPak %> makeEDAT pathSongPak
  forM_ (zip ps3TextLangs pathTextLangs) $ \(ps3, xen) -> ps3 %> makeEDAT xen
  -- apparently we don't need to add the empty VRAM paks, song works fine without them
  phony ps3Root $ do
    shk $ need $ ps3Fsb : ps3Dat : ps3DL : ps3Text : ps3SongPak : ps3TextLangs
  dir </> "ps3.pkg" %> \out -> do
    shk $ need [ps3Root]
    let container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
    main <- container "USRDIR" . container ps3Folder <$> crawlFolderBytes ps3Root
    extra <- stackIO (getResourcesPath "pkg-contents/gh3") >>= crawlFolderBytes
    stackIO $ makePKG (npdContentID edatConfig) (main <> extra) out

fromSeconds :: U.Seconds -> Word32
fromSeconds secs = floor $ secs * 1000

makeGH3Spans :: U.TempoMap -> RTB.T U.Beats Bool -> RTB.T U.Beats gem -> [(Word32, Word32, Word32)]
makeGH3Spans tmap spans gems = let
  spans' = ATB.toPairList $ RTB.toAbsoluteEventList 0 $ joinEdgesSimple $ flip fmap spans $ \case
    False -> EdgeOff ()
    True  -> EdgeOn  () ()
  collectedGems = RTB.collectCoincident gems
  fromBeats bts = fromSeconds $ U.applyTempoMap tmap bts
  in flip fmap spans' $ \(pos, ((), (), len)) -> let
    msStart = fromBeats pos
    msEnd   = fromBeats $ pos + len
    count   = fromIntegral $ length $ U.trackTake len $ U.trackDrop pos collectedGems
    in (msStart, msEnd - msStart, count)

-- returns (fretbars, timesigs)
makeGH3Timing :: U.TempoMap -> U.MeasureMap -> U.Beats -> ([Word32], [(Word32, Word32, Word32)])
makeGH3Timing tmap mmap end = let
  fretbars
    = map fromSeconds
    $ onePastEnd
    $ ATB.getTimes
    $ RTB.toAbsoluteEventList 0
    $ U.applyTempoTrack tmap
    $ go 0
  onePastEnd times = let
    endSeconds = U.applyTempoMap tmap end
    in case span (< endSeconds) times of
      (keep, rest) -> keep <> take 1 rest
  go i = let
    measureStart = U.unapplyMeasureMap mmap (i, 0)
    measureEnd = U.unapplyMeasureMap mmap (i + 1, 0)
    len = measureEnd - measureStart
    sig = U.timeSigAt measureStart mmap
    thisMeasure = U.trackTake len $ RTB.cons 0 Bar $ infiniteBeats
    infiniteBeats = RTB.cons (U.timeSigUnit sig) Beat infiniteBeats
    in trackGlue len thisMeasure $ go $ i + 1
  sigs
    = map (\(t, sig) -> case U.showSignatureFull sig of
      Just (E.MetaEvent (Meta.TimeSig n d _ _)) ->
        (fromSeconds t, fromIntegral n, 2 ^ d)
      _                                         ->
        (fromSeconds t, 4, 4) -- shouldn't happen!
      )
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0
    $ U.applyTempoTrack tmap
    $ U.measureMapToTimeSigs mmap
  in (fretbars, sigs)

makeGH3MidQB
  :: SongYaml f
  -> F.Song (F.OnyxFile U.Beats)
  -> BasicTiming
  -> F.FlexPartName
  -> F.FlexPartName
  -> F.FlexPartName
  -> (GH3MidQB, Int)
makeGH3MidQB songYaml origSong timing partLead partRhythm partDrummer = let

  -- first, pad input midi if needed
  firstEvent rtb = case RTB.viewL rtb of
    Just ((dt, _), _) -> dt
    Nothing           -> 999
  firstNoteBeats = foldr min 999 $ do
    part <- [partLead, partRhythm]
    let opart = F.getFlexPart part $ F.s_tracks origSong
    builder <- toList $ getPart part songYaml >>= anyFiveFret
    let result = builder FiveTypeGuitar ModeInput
          { tempo = F.s_tempos origSong
          , events = F.onyxEvents $ F.s_tracks origSong
          , part = opart
          }
    map firstEvent $ Map.elems result.notes
  firstNoteSeconds = U.applyTempoMap (F.s_tempos origSong) firstNoteBeats
  padSeconds = max 0 $ ceiling $ 3 - (realToFrac firstNoteSeconds :: Rational)
  song = F.padAnyFile padSeconds origSong

  makeGH3Part fpart = let
    emptyTrack = GH3Track [] [] []
    in case getPart fpart songYaml >>= anyFiveFret of
      Nothing -> (mempty, GH3Part emptyTrack emptyTrack emptyTrack emptyTrack)
      Just builder -> let
        result = completeFiveResult False (F.s_signatures song) $ builder FiveTypeGuitar ModeInput
          { tempo  = F.s_tempos song
          , events = F.onyxEvents $ F.s_tracks song
          , part   = F.getFlexPart fpart $ F.s_tracks song
          }
        in (result.other, GH3Part
          { gh3Easy   = makeGH3Track Easy   result
          , gh3Medium = makeGH3Track Medium result
          , gh3Hard   = makeGH3Track Hard   result
          , gh3Expert = makeGH3Track Expert result
          })
  makeGH3Track :: Difficulty -> FiveResult -> GH3Track
  makeGH3Track diff result = let
    sht
      = gh3LegalHOPOs
      $ noOpenNotes result.settings.detectMutedOpens
      -- TODO guitarify' is called by makeGH3TrackNotes but should we remove ext sustains before noOpenNotes?
      $ fromMaybe RTB.empty
      $ Map.lookup diff result.notes
    starPower = makeGH3Spans (F.s_tempos song) (Five.fiveOverdrive result.other) sht
    in GH3Track
      { gh3Notes       = makeGH3TrackNotes (F.s_tempos song) timeSigs beats sht
      , gh3StarPower   = starPower
      , gh3BattleStars = starPower
      }
  (beats, timeSigs) = makeGH3Timing
    (F.s_tempos song)
    (F.s_signatures song)
    (timingEnd timing)
  sections
    = map (\(secs, sect) -> (fromSeconds secs, Right $ sectionBody $ makeDisplaySection sect))
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0
    $ U.applyTempoTrack (F.s_tempos song)
    $ eventsSections (F.getEventsTrack $ F.s_tracks song)
  (leadTrack, leadPart  ) = makeGH3Part partLead
  (_        , rhythmPart) = makeGH3Part partRhythm
  -- Not sure if there's a real way to extend the end of the song past the last note.
  -- In WoR there's a special optional marker "\\L_ENDOFSONG" that does it.
  -- However it seems like if you just put a note on either lead or rhythm of
  -- whatever difficulty is being played, it will work to extend the song length.
  -- So we'll add a dummy note to Rhythm/Bass on each difficulty.
  rhythmWithEndHack = rhythmPart
    { gh3Easy   = addEndHack $ gh3Easy   rhythmPart
    , gh3Medium = addEndHack $ gh3Medium rhythmPart
    , gh3Hard   = addEndHack $ gh3Hard   rhythmPart
    , gh3Expert = addEndHack $ gh3Expert rhythmPart
    }
  addEndHack diff = diff { gh3Notes = gh3Notes diff <> endHack }
  endHack = [(fromSeconds $ U.applyTempoMap (F.s_tempos song) $ timingEnd timing, 1, 1)]
  makeFaceoff getPlayer
    = map (\(pos, len, _count) -> (pos, len))
    $ makeGH3Spans (F.s_tempos song) (getPlayer leadTrack)
    $ maybe RTB.empty Five.fiveGems
    $ Map.lookup Expert
    $ Five.fiveDifficulties leadTrack
  drumAnims = case getPart partDrummer songYaml >>= (.drums) of
    Nothing -> []
    Just pd -> let
      rbAnims = buildDrumAnimation pd (F.s_tempos song) $ F.getFlexPart partDrummer $ F.s_tracks song
      mapping = map swap gh3DrumMapping
      notes = RTB.mapMaybe (\(tdn, hand) -> lookup (tdn_gem tdn, hand) mapping) $ animationToTrueDrums rbAnims
      in map (\(secs, pitch) -> [floor $ secs * 1000, pitch, 96]) -- third number is probably original midi velocity
        $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ U.applyTempoTrack (F.s_tempos song) notes
  in (emptyMidQB
    { gh3Guitar          = leadPart
    , gh3Rhythm          = rhythmWithEndHack
    , gh3TimeSignatures  = timeSigs
    , gh3FretBars        = beats
    , gh3Markers         = sections
    , gh3P1FaceOff       = makeFaceoff Five.fivePlayer1
    , gh3P2FaceOff       = makeFaceoff Five.fivePlayer2
    , gh3BackgroundNotes = (GH3Background [] [] [] [] [] [] [] [])
      { gh3Drums = drumAnims
      }
    }, padSeconds)
