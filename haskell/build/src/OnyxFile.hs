{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module OnyxFile where

import           Audio                          (clampFloat)
import qualified C3
import           Codec.Picture
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace (printStackTraceIO)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toLower)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.Sndfile
import qualified Data.DTA                       as D
import qualified Data.DTA.Serialize             as D
import qualified Data.DTA.Serialize.Magma       as D
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Development.Shake
import qualified FretsOnFire                    as FoF
import           Image
import           PrettyDTA
import qualified Reaper.Base                    as RPP
import qualified Reaper.Parse                   as RPP
import qualified Reaper.Scan                    as RPP
import qualified RockBand.File                  as RBFile
import qualified Sound.File.Sndfile             as Snd
import qualified Sound.Jammit.Base              as J
import qualified Sound.MIDI.File.Load           as Load
import qualified Sound.MIDI.File.Save           as Save
import qualified Sound.MIDI.Util                as U
import           System.FilePath                (takeExtension, (<.>), (</>))

data Channel
  = ChannelGuitar
  | ChannelBass
  | ChannelDrums
  | ChannelKick
  | ChannelSnare
  | ChannelKeys
  | ChannelVocal
  | ChannelSong
  | ChannelCountin
  | ChannelSongCountin
  | ChannelCrowd
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ImageFormat
  = BMP
  | PNG
  | PNG_XBOX
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type PlanName = T.Text
type JammitSong = T.Text
type AudioStore = AudioSource (ResourceT IO) Float
type Is2x = Bool

data PSChannel
  = PSChannelGuitar
  | PSChannelBass
  | PSChannelDrums
  | PSChannelDrums1
  | PSChannelDrums2
  | PSChannelDrums3
  | PSChannelKeys
  | PSChannelVocal
  | PSChannelSong
  | PSChannelCrowd
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data MagmaChannel
  = MagmaChannelGuitar
  | MagmaChannelBass
  | MagmaChannelKeys
  | MagmaChannelVocal
  | MagmaChannelCrowd
  | MagmaChannelDrums
  | MagmaChannelSnare
  | MagmaChannelKick
  | MagmaChannelDryVox0
  | MagmaChannelDryVox1
  | MagmaChannelDryVox2
  | MagmaChannelDryVox3
  | MagmaChannelDryVoxSine
  | MagmaChannelSong
  | MagmaChannelDummyMono
  | MagmaChannelDummyStereo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

newtype MelodyTrack = MelodyTrack BL.ByteString
newtype MOGG = MOGG BL.ByteString
newtype Milo = Milo BL.ByteString
newtype CON = CON BL.ByteString
newtype RBA = RBA BL.ByteString
newtype Weights = Weights BL.ByteString
newtype Pan = Pan BL.ByteString

data OnyxFile a where
  OnyxReadme :: OnyxFile T.Text
  OnyxNotes :: OnyxFile (RBFile.Song U.Beats)
  OnyxJammit :: JammitSong -> J.AudioPart -> OnyxFile AudioStore
  OnyxAudio :: PlanName -> Channel -> OnyxFile AudioStore
  OnyxRBOGG :: PlanName -> OnyxFile AudioStore
  OnyxRBMOGG :: PlanName -> OnyxFile MOGG
  OnyxAlbumArt :: ImageFormat -> OnyxFile DynamicImage
  OnyxPSSongINI :: PlanName -> OnyxFile FoF.Song
  OnyxPSAudio :: PlanName -> PSChannel -> OnyxFile AudioStore
  OnyxPSAlbumArt :: PlanName -> OnyxFile DynamicImage
  OnyxPSNotes :: PlanName -> OnyxFile (RBFile.Song U.Beats)
  OnyxMelodyAudio :: PlanName -> OnyxFile AudioStore
  OnyxMelodyTrack :: PlanName -> OnyxFile MelodyTrack
  OnyxRB3DTA :: PlanName -> Is2x -> OnyxFile DTASingle
  OnyxRB3Notes :: PlanName -> Is2x -> String -> OnyxFile (RBFile.Song U.Beats)
  OnyxRB3MOGG :: PlanName -> Is2x -> String -> OnyxFile MOGG
  OnyxRB3AlbumArt :: PlanName -> Is2x -> String -> OnyxFile DynamicImage
  OnyxRB3Milo :: PlanName -> Is2x -> String -> OnyxFile Milo
  OnyxRB3CON :: PlanName -> Is2x -> OnyxFile CON
  OnyxMagmaAudio :: PlanName -> Is2x -> MagmaChannel -> OnyxFile AudioStore
  OnyxMagmaAlbumArt :: PlanName -> Is2x -> OnyxFile DynamicImage
  OnyxMagmaAlbumArtV1 :: PlanName -> Is2x -> OnyxFile DynamicImage
  OnyxMagmaNotes :: PlanName -> Is2x -> OnyxFile (RBFile.Song U.Beats)
  OnyxMagmaNotesV1 :: PlanName -> Is2x -> OnyxFile (RBFile.Song U.Beats)
  OnyxMagmaProject :: PlanName -> Is2x -> OnyxFile D.RBProj
  OnyxMagmaProjectV1 :: PlanName -> Is2x -> OnyxFile D.RBProj
  OnyxMagmaC3 :: PlanName -> Is2x -> OnyxFile C3.C3
  OnyxMagmaRBA :: PlanName -> Is2x -> OnyxFile RBA
  OnyxMagmaRBAV1 :: PlanName -> Is2x -> OnyxFile RBA
  OnyxMagmaNotesExport :: PlanName -> Is2x -> OnyxFile (RBFile.Song U.Beats)
  OnyxMagmaNotesAdded :: PlanName -> Is2x -> OnyxFile (RBFile.Song U.Beats)
  OnyxRB2OriginalDTA :: PlanName -> Is2x -> OnyxFile DTASingle
  OnyxRB2DTA :: PlanName -> Is2x -> OnyxFile DTASingle
  OnyxRB2Notes :: PlanName -> Is2x -> String -> OnyxFile (RBFile.Song U.Beats)
  OnyxRB2MOGG :: PlanName -> Is2x -> String -> OnyxFile MOGG
  OnyxRB2AlbumArt :: PlanName -> Is2x -> String -> OnyxFile DynamicImage
  OnyxRB2Weights :: PlanName -> Is2x -> String -> OnyxFile Weights
  OnyxRB2Milo :: PlanName -> Is2x -> String -> OnyxFile Milo
  OnyxRB2Pan :: PlanName -> Is2x -> String -> OnyxFile Pan
  OnyxRB2CON :: PlanName -> Is2x -> OnyxFile CON
  OnyxRPP :: PlanName -> OnyxFile RPP.Element

onyxFilePath :: OnyxFile a -> FilePath
onyxFilePath = \case
  OnyxReadme -> "README.md"
  OnyxNotes -> "gen/notes.mid"
  OnyxJammit jammitSong jammitAudio ->
    "gen/jammit" </> T.unpack jammitSong </> case jammitAudio of
      J.Only    part -> "only" </> map toLower (drop 4 $ show part) <.> "wav"
      J.Without inst -> "without" </> map toLower (show inst) <.> "wav"
  OnyxAudio planName channel -> dir planName </> case channel of
    ChannelGuitar -> "guitar.wav"
    ChannelBass -> "bass.wav"
    ChannelDrums -> "drums.wav"
    ChannelKick -> "kick.wav"
    ChannelSnare -> "snare.wav"
    ChannelKeys -> "keys.wav"
    ChannelVocal -> "vocal.wav"
    ChannelSong -> "song.wav"
    ChannelCountin -> "countin.wav"
    ChannelSongCountin -> "song-countin.wav"
    ChannelCrowd -> "crowd.wav"
  OnyxAlbumArt imageFormat -> "gen/cover" <.> map toLower (show imageFormat)
  OnyxPSSongINI planName -> dir planName </> "ps/song.ini"
  OnyxPSAudio planName psChannel -> dir planName </> "ps" </> case psChannel of
    PSChannelGuitar -> "guitar.ogg"
    PSChannelBass -> "bass.ogg"
    PSChannelDrums -> "drums.ogg"
    PSChannelDrums1 -> "drums_1.ogg"
    PSChannelDrums2 -> "drums_2.ogg"
    PSChannelDrums3 -> "drums_3.ogg"
    PSChannelKeys -> "keys.ogg"
    PSChannelVocal -> "vocal.ogg"
    PSChannelSong -> "song.ogg"
    PSChannelCrowd -> "crowd.ogg"
  OnyxPSAlbumArt planName -> dir planName </> "ps/album.png"
  OnyxPSNotes planName -> dir planName </> "ps/notes.mid"
  OnyxMelodyAudio planName -> dir planName </> "melody/audio.ogg"
  OnyxMelodyTrack planName -> dir planName </> "melody/song.track"
  OnyxRB3DTA planName is2x -> pedalDir planName is2x </> "rb3/songs/songs.dta"
  OnyxRB3Notes planName is2x pkg -> pedalDir planName is2x </> "rb3/songs" </> pkg </> pkg <.> "mid"
  OnyxRB3AlbumArt planName is2x pkg -> pedalDir planName is2x </> "rb3/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
  OnyxRB3Milo planName is2x pkg -> pedalDir planName is2x </> "rb3/songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
  OnyxRB3MOGG planName is2x pkg -> pedalDir planName is2x </> "rb3/songs" </> pkg </> pkg <.> "mogg"
  OnyxRB3CON planName is2x -> pedalDir planName is2x </> "rb3.con"
  OnyxRBOGG planName -> dir planName </> "audio.ogg"
  OnyxRBMOGG planName -> dir planName </> "audio.mogg"
  OnyxMagmaAudio planName is2x magmaChannel -> pedalDir planName is2x </> "magma" </> case magmaChannel of
    MagmaChannelGuitar -> "guitar.wav"
    MagmaChannelBass -> "bass.wav"
    MagmaChannelKeys -> "keys.wav"
    MagmaChannelVocal -> "vocal.wav"
    MagmaChannelDrums -> "drums.wav"
    MagmaChannelSnare -> "snare.wav"
    MagmaChannelKick -> "kick.wav"
    MagmaChannelCrowd -> "crowd.wav"
    MagmaChannelDryVox0 -> "dryvox0.wav"
    MagmaChannelDryVox1 -> "dryvox1.wav"
    MagmaChannelDryVox2 -> "dryvox2.wav"
    MagmaChannelDryVox3 -> "dryvox3.wav"
    MagmaChannelDryVoxSine -> "dryvox-sine.wav"
    MagmaChannelSong -> "song-countin.wav"
    MagmaChannelDummyMono -> "dummy-mono.wav"
    MagmaChannelDummyStereo -> "dummy-stereo.wav"
  OnyxMagmaAlbumArt planName is2x -> pedalDir planName is2x </> "magma/cover.bmp"
  OnyxMagmaAlbumArtV1 planName is2x -> pedalDir planName is2x </> "magma/cover-v1.bmp"
  OnyxMagmaNotes planName is2x -> pedalDir planName is2x </> "magma/notes.mid"
  OnyxMagmaNotesV1 planName is2x -> pedalDir planName is2x </> "magma/notes-v1.mid"
  OnyxMagmaProject planName is2x -> pedalDir planName is2x </> "magma/magma.rbproj"
  OnyxMagmaProjectV1 planName is2x -> pedalDir planName is2x </> "magma/magma-v1.rbproj"
  OnyxMagmaC3 planName is2x -> pedalDir planName is2x </> "magma/magma.c3"
  OnyxMagmaRBA planName is2x -> pedalDir planName is2x </> "magma.rba"
  OnyxMagmaRBAV1 planName is2x -> pedalDir planName is2x </> "magma-v1.rba"
  OnyxMagmaNotesExport planName is2x -> pedalDir planName is2x </> "notes-magma-export.mid"
  OnyxMagmaNotesAdded planName is2x -> pedalDir planName is2x </> "notes-magma-added.mid"
  OnyxRB2OriginalDTA planName is2x -> pedalDir planName is2x </> "rb2-original.dta"
  OnyxRB2DTA planName is2x -> pedalDir planName is2x </> "rb2/songs/songs.dta"
  OnyxRB2Notes planName is2x pkg -> pedalDir planName is2x </> "rb2/songs" </> pkg </> pkg <.> "mid"
  OnyxRB2MOGG planName is2x pkg -> pedalDir planName is2x </> "rb2/songs" </> pkg </> pkg <.> "mogg"
  OnyxRB2AlbumArt planName is2x pkg -> pedalDir planName is2x </> "rb2/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
  OnyxRB2Weights planName is2x pkg -> pedalDir planName is2x </> "rb2/songs" </> pkg </> "gen" </> (pkg ++ "_weights.bin")
  OnyxRB2Milo planName is2x pkg -> pedalDir planName is2x </> "rb2/songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
  OnyxRB2Pan planName is2x pkg -> pedalDir planName is2x </> "rb2/songs" </> pkg </> pkg <.> "pan"
  OnyxRB2CON planName is2x -> pedalDir planName is2x </> "rb2.con"
  OnyxRPP planName -> "notes-" ++ T.unpack planName ++ ".RPP"
  where pedalDir planName is2x = dir planName </> if is2x then "2p" else "1p"
        dir planName = "gen/plan" </> T.unpack planName

readOnyxFile :: OnyxFile a -> IO a
readOnyxFile f = case f of
  OnyxReadme{} -> TIO.readFile fp
  OnyxNotes{} -> readNotes
  OnyxJammit{} -> sourceSnd fp
  OnyxAudio{} -> sourceSnd fp
  OnyxAlbumArt imageFormat -> case imageFormat of
    BMP -> readImage'
    PNG -> readImage'
    PNG_XBOX -> ImageRGB8 . readPNGXbox <$> BL.readFile fp
  OnyxPSSongINI{} -> FoF.loadSong fp
  OnyxPSAudio{} -> sourceSnd fp
  OnyxPSAlbumArt{} -> readImage'
  OnyxPSNotes{} -> readNotes
  OnyxMagmaNotes{} -> readNotes
  OnyxMagmaNotesV1{} -> readNotes
  OnyxRB3Notes{} -> readNotes
  OnyxRB3AlbumArt{} -> ImageRGB8 . readPNGXbox <$> BL.readFile fp
  OnyxRB2AlbumArt{} -> ImageRGB8 . readPNGXbox <$> BL.readFile fp
  OnyxRB3Milo{} -> Milo <$> BL.readFile fp
  OnyxRB2Milo{} -> Milo <$> BL.readFile fp
  OnyxRB2Weights{} -> Weights <$> BL.readFile fp
  OnyxRB2Pan{} -> Pan <$> BL.readFile fp
  OnyxRBOGG{} -> sourceSnd fp
  OnyxMelodyAudio{} -> sourceSnd fp
  OnyxRBMOGG{} -> MOGG <$> BL.readFile fp
  OnyxRB2MOGG{} -> MOGG <$> BL.readFile fp
  OnyxRB3MOGG{} -> MOGG <$> BL.readFile fp
  OnyxMagmaRBA{} -> RBA <$> BL.readFile fp
  OnyxMagmaRBAV1{} -> RBA <$> BL.readFile fp
  OnyxMagmaAudio{} -> sourceSnd fp
  OnyxRB3CON{} -> CON <$> BL.readFile fp
  OnyxRB2CON{} -> CON <$> BL.readFile fp
  OnyxMelodyTrack{} -> MelodyTrack <$> BL.readFile fp
  OnyxMagmaAlbumArt{} -> readImage'
  OnyxMagmaAlbumArtV1{} -> readImage'
  OnyxMagmaNotesExport{} -> readNotes
  OnyxMagmaNotesAdded{} -> readNotes
  OnyxRB2Notes{} -> readNotes
  OnyxMagmaC3{} -> error "TODO: implement .c3 file reading"
  OnyxRB3DTA{} -> readDTASingle fp
  OnyxRB2DTA{} -> readDTASingle fp
  OnyxRB2OriginalDTA{} -> readDTASingle fp
  OnyxRPP{} -> RPP.parse . RPP.scan <$> readFile fp
  OnyxMagmaProject{} -> D.readFileDTA fp >>= \dta -> case D.unserialize dta of
    Left err -> error $ "readOnyxFile: failed to load Magma project " ++ show fp ++ "; error: " ++ err
    Right proj -> return proj
  OnyxMagmaProjectV1{} -> D.readFileDTA fp >>= \dta -> case D.unserialize dta of
    Left err -> error $ "readOnyxFile: failed to load Magma project " ++ show fp ++ "; error: " ++ err
    Right proj -> return proj
  where fp = onyxFilePath f
        readImage' = readImage fp >>= \case
          Left  err -> error $
            "readOnyxFile: failed to load image " ++ show fp ++ "; error: " ++ err
          Right img -> return img
        readNotes = Load.fromFile fp >>= printStackTraceIO . RBFile.readMIDIFile

loadOnyxFile :: OnyxFile a -> Action a
loadOnyxFile f = need [onyxFilePath f] >> liftIO (readOnyxFile f)

loadOnyxFiles :: [OnyxFile a] -> Action [a]
loadOnyxFiles fs = need (map onyxFilePath fs) >> liftIO (mapM readOnyxFile fs)

writeOnyxFile :: OnyxFile a -> a -> IO ()
writeOnyxFile f x = case f of
  OnyxReadme{} -> TIO.writeFile fp x
  OnyxNotes{} -> writeNotes x
  OnyxJammit{} -> writeAudio x
  OnyxAudio{} -> writeAudio x
  OnyxAlbumArt imageFormat -> case imageFormat of
    BMP -> saveBmpImage fp x
    PNG -> savePngImage fp x
    PNG_XBOX -> BL.writeFile fp $ toPNG_XBOX $ convertRGB8 x
  OnyxPSSongINI{} -> FoF.saveSong fp x
  OnyxPSAudio{} -> writeAudio x
  OnyxPSAlbumArt{} -> savePngImage fp x
  OnyxPSNotes{} -> writeNotes x
  OnyxRB3Notes{} -> writeNotes x
  OnyxRB2Notes{} -> writeNotes x
  OnyxMagmaNotes{} -> writeNotes x
  OnyxMagmaNotesV1{} -> writeNotes x
  OnyxRB3DTA{} -> writeFile fp $ writeDTASingle x
  OnyxRB2OriginalDTA{} -> writeFile fp $ writeDTASingle x
  OnyxRB2DTA{} -> writeFile fp $ writeDTASingle x
  OnyxRBOGG{} -> writeAudio x
  OnyxMelodyAudio{} -> writeAudio x
  OnyxRBMOGG{} -> case x of MOGG bs -> BL.writeFile fp bs
  OnyxRB2MOGG{} -> case x of MOGG bs -> BL.writeFile fp bs
  OnyxRB3MOGG{} -> case x of MOGG bs -> BL.writeFile fp bs
  OnyxRB3Milo{} -> case x of Milo bs -> BL.writeFile fp bs
  OnyxRB3CON{} -> case x of CON bs -> BL.writeFile fp bs
  OnyxRB2CON{} -> case x of CON bs -> BL.writeFile fp bs
  OnyxMagmaRBA{} -> case x of RBA bs -> BL.writeFile fp bs
  OnyxMagmaRBAV1{} -> case x of RBA bs -> BL.writeFile fp bs
  OnyxMelodyTrack{} -> case x of MelodyTrack bs -> BL.writeFile fp bs
  OnyxMagmaAudio{} -> writeAudio x
  OnyxMagmaAlbumArt{} -> saveBmpImage fp x
  OnyxMagmaAlbumArtV1{} -> saveBmpImage fp x
  OnyxMagmaC3{} -> writeFile fp $ C3.showC3 x
  OnyxMagmaNotesExport{} -> writeNotes x
  OnyxMagmaNotesAdded{} -> writeNotes x
  OnyxRB2Weights{} -> case x of Weights bs -> BL.writeFile fp bs
  OnyxRB2Milo{} -> case x of Milo bs -> BL.writeFile fp bs
  OnyxRB2Pan{} -> case x of Pan bs -> BL.writeFile fp bs
  OnyxRPP{} -> RPP.writeRPP fp x
  OnyxRB3AlbumArt{} -> BL.writeFile fp $ toPNG_XBOX $ convertRGB8 x
  OnyxRB2AlbumArt{} -> BL.writeFile fp $ toPNG_XBOX $ convertRGB8 x
  OnyxMagmaProject{} -> D.writeFileDTA_utf8 fp $ D.serialize x
  OnyxMagmaProjectV1{} -> D.writeFileDTA_utf8 fp $ D.serialize x
  where fp = onyxFilePath f
        writeAudio :: AudioStore -> IO ()
        writeAudio aud = let
          fmt = case takeExtension fp of
            ".ogg" -> Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile
            ".wav" -> Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
            _ -> error $ "writeOnyxFile: unknown audio output file format " ++ show fp
          in runResourceT $ sinkSnd fp fmt $ clampFloat aud
        writeNotes song = Save.toFile fp $ RBFile.showMIDIFile song

ruleOnyxFile :: OnyxFile a -> Action a -> Rules ()
ruleOnyxFile f act = onyxFilePath f %> \_ -> act >>= liftIO . writeOnyxFile f

copyOnyxFileTo :: OnyxFile a -> OnyxFile a -> Rules ()
x `copyOnyxFileTo` y = onyxFilePath y %> \out -> copyFile' (onyxFilePath x) out
