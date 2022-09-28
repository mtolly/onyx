{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Build.GuitarHero3 (gh3Rules) where

import           Audio
import           Build.Common
import           Config                         hiding (Difficulty)
import           Control.Monad                  (guard, when)
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Put                (putWord32be, runPut)
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.Conduit.Audio
import           Data.Hashable                  (Hashable, hash)
import qualified Data.Map                       as Map
import           Data.Maybe                     (catMaybes, fromMaybe)
import qualified Data.Text                      as T
import           Development.Shake              hiding (phony, (%>), (&%>))
import           Development.Shake.FilePath
import           Guitars                        (closeNotes', noOpenNotes',
                                                 strumHOPOTap')
import           Neversoft.Audio                (gh3Encrypt)
import           Neversoft.Checksum             (qbKeyCRC)
import           Neversoft.GH3
import qualified RockBand.Codec.File            as RBFile
import           RockBand.Codec.File            (shakeMIDI)
import qualified RockBand.Codec.Five            as Five
import           RockBand.Common                (Difficulty (..))
import qualified Sound.MIDI.Util                as U

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

  -- TODO we probably don't have to use "dlc*", can make a descriptive shortname instead
  let hashed = hashGH3 songYaml gh3
      songID = fromMaybe hashed $ gh3_SongID gh3
      -- this does probably have to be a number though (biggest dl number = the metadata that is read)
      dl = "dl" <> show (fromMaybe hashed $ gh3_DL gh3)

  let coopPart = case gh3_Coop gh3 of
        GH2Bass   -> gh3_Bass   gh3
        GH2Rhythm -> gh3_Rhythm gh3
      gh3Parts = [gh3_Guitar gh3, coopPart]
      loadOnyxMidi :: Staction (RBFile.Song (RBFile.OnyxFile U.Beats))
      loadOnyxMidi = shakeMIDI $ planDir </> "raw.mid"

  let pathGuitar  = dir </> "guitar.wav"
      pathRhythm  = dir </> "rhythm.wav"
      pathSong    = dir </> "song.wav"
      pathPreview = dir </> "preview.wav"
  pathGuitar %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo gh3Parts (gh3_Common gh3) mid 0 planName plan
      [(gh3_Guitar gh3, 1)]
    runAudio (clampIfSilent s) out
  pathRhythm %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo gh3Parts (gh3_Common gh3) mid 0 planName plan
      [(coopPart, 1)]
    runAudio (clampIfSilent s) out
  pathSong %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceSongCountin buildInfo (gh3_Common gh3) mid 0 True planName plan
      [ (gh3_Guitar gh3, 1)
      , (coopPart      , 1)
      ]
    runAudio (clampIfSilent s) out
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
    buildAudio previewExpr out

  let pathFsb = dir </> "audio.fsb"
      pathFsbXen = dir </> "gh3" </> ("DLC" <> show songID <> ".fsb.xen")
      pathDatXen = dir </> "gh3" </> ("dlc" <> show songID <> ".dat.xen")
  pathFsb %> \out -> do
    shk $ need [pathGuitar, pathPreview, pathRhythm, pathSong]
    hasGuitarAudio <- maybe False (/= 0) <$> audioLength pathGuitar
    hasRhythmAudio <- maybe False (/= 0) <$> audioLength pathRhythm
    makeFSB3 (catMaybes
      [ Just ("onyx_preview.xma", pathPreview)
      , Just ("onyx_song.xma", pathSong)
      , guard hasGuitarAudio >> Just ("onyx_guitar.xma", pathGuitar)
      , guard hasRhythmAudio >> Just ("onyx_rhythm.xma", pathRhythm)
      ]) out
  pathFsbXen %> \out -> do
    shk $ need [pathFsb]
    fsb <- stackIO $ BL.readFile pathFsb
    stackIO $ BL.writeFile out $ gh3Encrypt fsb
  pathDatXen %> \out -> do
    shk $ need [pathGuitar, pathRhythm]
    hasGuitarAudio <- maybe False (/= 0) <$> audioLength pathGuitar
    hasRhythmAudio <- maybe False (/= 0) <$> audioLength pathRhythm
    stackIO $ BL.writeFile out $ runPut $ do
      let count = 2 + (if hasGuitarAudio then 1 else 0) + (if hasRhythmAudio then 1 else 0)
      putWord32be count
      let datEntry slot index = do
            putWord32be $ qbKeyCRC $ B8.pack $ "dlc" <> show songID <> "_" <> slot
            putWord32be index
            putWord32be 0
            putWord32be 0
            putWord32be 0
      datEntry "preview" 0
      datEntry "song" 1
      when hasGuitarAudio $ datEntry "guitar" 2
      when hasRhythmAudio $ datEntry "rhythm" $ if hasGuitarAudio then 3 else 2

  phony (dir </> "gh3") $ do
    shk $ need [pathFsbXen, pathDatXen]

makeGH3MidQB
  :: RBFile.Song (RBFile.OnyxFile U.Beats)
  -> (RBFile.FlexPartName, Int)
  -> (RBFile.FlexPartName, Int)
  -> GH3MidQB
makeGH3MidQB song partLead partRhythm = let
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
      { gh3Notes       = makeGH3TrackNotes (RBFile.s_tempos song) sht
      , gh3StarPower   = [] -- TODO
      , gh3BattleStars = []
      }
  in emptyMidQB
    { gh3Guitar         = makeGH3Part partLead
    , gh3Rhythm         = makeGH3Part partRhythm
    , gh3TimeSignatures = undefined -- TODO
    , gh3FretBars       = undefined -- TODO
    }
