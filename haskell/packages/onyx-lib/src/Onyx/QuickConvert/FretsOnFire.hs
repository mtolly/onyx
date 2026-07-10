{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.QuickConvert.FretsOnFire where

import           Control.Applicative              ((<|>))
import           Control.Monad.Codec              (codecOut)
import           Control.Monad.Extra              (forM, forM_, guard,
                                                   mapMaybeM)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.State.Strict (execState)
import           Data.Bifunctor                   (second)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace, toLower)
import qualified Data.Conduit.Audio               as CA
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import           Data.Functor.Identity            (Identity, runIdentity)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import           Data.List.Extra                  (nubOrd, partition)
import           Data.List.HT                     (partitionMaybe)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Audio                       (Audio (..), applyVolsMono,
                                                   audioIO, audioLengthReadable,
                                                   audioRateReadable,
                                                   buildSource', crossCorrelate,
                                                   loadAudioInput, sameChannels,
                                                   standardRate)
import           Onyx.CloneHero.SNG
import           Onyx.Encore                      (EncoreInfo (..),
                                                   encoreMidiToFoF,
                                                   loadEncoreInfo)
import           Onyx.FeedBack.Load               (chartToBeats, chartToIni,
                                                   chartToMIDI, loadChartFile,
                                                   loadChartReadable)
import           Onyx.FFMPEG                      (ffSource)
import           Onyx.FretsOnFire                 (loadPSIni, songToIniContents,
                                                   writePSIni)
import qualified Onyx.Guitar                      as G
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   StrumHOPOTap (..),
                                                   isNoteEdge, isNoteEdgeCPV)
import           Onyx.MIDI.Parse                  (getMIDI)
import           Onyx.MIDI.Read                   (TrackCodec, parseTrack)
import           Onyx.MIDI.Track.Drums.Elite      (psRealToElite)
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as G5
import qualified Onyx.MIDI.Track.SixFret          as G6
import           Onyx.PhaseShift.Message          (PSMessage (..),
                                                   PhraseID (..), parsePSSysEx,
                                                   unparsePSSysEx)
import           Onyx.Reductions                  (limitFretsDifficulty)
import           Onyx.StackTrace
import           Onyx.Util.Binary                 (runGetM)
import           Onyx.Util.Files                  (fixFileCase)
import           Onyx.Util.Handle
import           Onyx.Util.Text.Decode            (decodeGeneral, encodeLatin1)
import           Optics                           (mapped, (%), (%~), (.~))
import qualified Sound.MIDI.File                  as File
import qualified Sound.MIDI.File.Event            as Event
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           System.Directory                 (createDirectoryIfMissing,
                                                   doesFileExist,
                                                   removePathForcibly,
                                                   renameDirectory, renameFile)
import           System.FilePath                  (dropTrailingPathSeparator,
                                                   splitExtension,
                                                   takeDirectory, takeExtension,
                                                   takeFileName, (<.>), (</>))
import           System.IO                        (hClose)
import           System.IO.Temp                   (withSystemTempFile)
import           Text.Read                        (readMaybe)
import           UnliftIO.Exception               (catchAny)

#if MIN_VERSION_base(4,18,0)
-- liftA2 from Prelude
#else
import           Control.Applicative              (liftA2)
#endif

data QuickFoF = QuickFoF
  { metadata :: [(T.Text, T.Text)]
  , files    :: [(T.Text, FoFFile)]
  , location :: FilePath
  , format   :: FoFFormatInput
  } deriving (Generic)

data FoFFormat
  = FoFFolder
  | FoFSNG
  deriving (Eq)

data FoFFormatInput
  = FoFEncore
  | FoFFormat FoFFormat
  deriving (Eq)

data FoFFile
  = FoFReadable Readable
  | FoFMIDI (File.T B.ByteString)

loadQuickFoF :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m (Maybe QuickFoF)
loadQuickFoF fin = inside ("Loading: " <> fin) let
  r = fileReadable fin
  in case map toLower $ takeFileName fin of
    "song.ini" -> do
      let dir = takeDirectory fin
      ini <- loadPSIni r
      folder <- stackIO $ crawlFolder dir
      return $ Just QuickFoF
        { metadata = ini
        , files    = flip mapMaybe (folderFiles folder) \(name, x) -> do
          guard $ T.toLower name /= "song.ini"
          Just (name, FoFReadable x)
        , location = dir
        , format   = FoFFormat FoFFolder
        }
    "notes.chart" -> do
      let dir = takeDirectory fin
      iniPath <- fixFileCase $ dir </> "song.ini"
      stackIO (doesFileExist iniPath) >>= \case
        True -> return Nothing -- load from song.ini instead
        False -> do
          ini <- songToIniContents . chartToIni <$> loadChartFile fin
          folder <- stackIO $ crawlFolder dir
          return $ Just QuickFoF
            { metadata = ini
            , files    = second FoFReadable <$> folderFiles folder
            , location = dir
            , format   = FoFFormat FoFFolder
            }
    "info.json" -> do
      info <- loadEncoreInfo fin
      stackIO $ Just <$> convertEncoreFoF fin info
      -- TODO some songs have a song.ini and info.json. in this case:
      -- * if the info.json midi is named "notes.mid", only load encore format.
      --   it can't be a well formed fof format song due to the track names
      -- * if there is no notes.mid or notes.chart, only load encore format
      -- * otherwise, load fof format
    _ -> if map toLower (takeExtension fin) == ".sng"
      then do
        hdr <- stackIO $ readSNGHeader r
        let folder = getSNGFolder False hdr r
        return $ Just QuickFoF
          { metadata = hdr.metadata
          , files    = second FoFReadable <$> folderFiles folder
          , location = fin
          , format   = FoFFormat FoFSNG
          }
      else do
        iniPath <- fixFileCase $ fin </> "song.ini"
        stackIO (doesFileExist iniPath) >>= \case
          True  -> loadQuickFoF iniPath
          False -> return Nothing

textExt :: T.Text -> (T.Text, T.Text)
textExt x = let
  (name, ext) = splitExtension $ T.unpack x
  in (T.pack name, T.toLower $ T.pack ext)

convertAudio :: [T.Text] -> T.Text -> QuickFoF -> IO QuickFoF
convertAudio convFrom convTo q = do
  newFiles <- forM q.files \pair@(f, file) -> case file of
    FoFReadable r -> case textExt f of
      (name, ext) | elem ext convFrom -> do
        let newName = name <> convTo
            newStr = T.unpack newName
        bs <- withSystemTempFile newStr \fout hout -> do
          withSystemTempFile (T.unpack name) \fin hin -> do
            hClose hin
            hClose hout
            saveReadable r fin
            src <- loadAudioInput fin
            -- TODO use preference for ogg quality
            audioIO Nothing src fout
            BL.fromStrict <$> B.readFile fout
        return (newName, FoFReadable $ makeHandle newStr $ byteStringSimpleHandle bs)
      _ -> return pair
    _ -> return pair
  return $ q & #files .~ newFiles

convertOgg, convertOpus :: QuickFoF -> IO QuickFoF
convertOgg  = convertAudio [".mp3", ".wav", ".opus", ".flac"] ".ogg"
convertOpus = convertAudio [".mp3", ".wav", ".ogg" , ".flac"] ".opus"

convertMIDI :: (SendMessage m, MonadIO m) => QuickFoF -> StackTraceT m QuickFoF
convertMIDI q = do
  newFiles <- forM q.files \pair@(f, file) -> case file of
    FoFReadable r -> case textExt f of
      (name, ".chart") -> do
        chart <- chartToBeats <$> loadChartReadable r
        mid <- chartToMIDI chart
        let newName = name <> ".mid"
        return (newName, FoFMIDI $ fmap TE.encodeUtf8 $ F.showMIDIFile' mid)
        -- TODO should also save .chart.bak
      _ -> return pair
    _ -> return pair
  return $ q & #files .~ newFiles

applyToMIDI
  :: (SendMessage m, MonadIO m)
  => (File.T B.ByteString -> StackTraceT m (File.T B.ByteString))
  -> QuickFoF -> StackTraceT m QuickFoF
applyToMIDI f q = do
  newFiles <- forM q.files \pair@(name, file) -> case T.toLower name of
    "notes.mid" -> do
      newMIDI <- getFoFMIDI file >>= f
      return (name, FoFMIDI newMIDI)
    _ -> return pair
  return $ q & #files .~ newFiles

quickHOPOThreshold :: QuickFoF -> U.Beats
quickHOPOThreshold q = case lookup "hopo_frequency" q.metadata >>= readMaybe . T.unpack of
  Just n -> fromInteger n / 480
  _      -> case T.toLower <$> lookup "eighthnote_hopo" q.metadata of
    Just "true" -> 250 / 480 -- don't know exact interpretation but this is what I went with in FoF import
    _           -> 170 / 480

psTapOpenFormats :: (SendMessage m, MonadIO m) => QuickFoF -> StackTraceT m QuickFoF
psTapOpenFormats q = applyToMIDI (psOpenFormat (quickHOPOThreshold q) . psTapFormat) q

psTapFormat :: File.T B.ByteString -> File.T B.ByteString
psTapFormat = (runIdentity .) $ modifyFiveFretTracks \_dvn trk -> return let
  (notes, trk1) = flip RTB.partitionMaybe trk \e -> do
    (104, b) <- isNoteEdge e
    return b
  (sysexes, trk2) = flip RTB.partitionMaybe trk1 \e -> do
    msg <- parsePSSysEx e
    guard $ msg.psPhraseID == TapNotes
    return msg
  -- overlay the note and sysex (expert / all difficulties) forms
  -- so we enable tap when either of them say to
  events = RTB.merge (('a',) <$> notes) $ flip RTB.mapMaybe sysexes \msg ->
    case msg.psDifficulty of
      Nothing     -> Just ('b', msg.psEdge)
      Just Expert -> Just ('b', msg.psEdge)
      _           -> Nothing
  in if RTB.null notes
    then trk -- nothing to do
    else RTB.merge trk2 $ G.applyStatus events events <&> \(active, _) ->
      -- only emit all difficulties sysex
      unparsePSSysEx PSMessage
        { psDifficulty = Nothing
        , psPhraseID   = TapNotes
        , psEdge       = not $ null active
        }

psOpenFormat :: (SendMessage m) => U.Beats -> File.T B.ByteString -> StackTraceT m (File.T B.ByteString)
psOpenFormat hopoThreshold = modifyFiveFretTracks \dvn trk -> let
  isEnhancedOpensEvent = \case
    Event.MetaEvent (Meta.TextEvent s) -> elem s ["[ENHANCED_OPENS]", "ENHANCED_OPENS"]
    _                                  -> False
  in if not $ any isEnhancedOpensEvent $ U.trackTakeZero trk
    then return trk -- nothing to do
    else flip (overParsedFiveFret dvn) trk let
      eachDiff = G.emitGuitar5PS . G.computeFiveFretSHT G.HOPOsRBGuitar hopoThreshold
      in #difficulties % mapped %~ eachDiff

-- WIP
tapToHOPO :: (SendMessage m) => U.Beats -> File.T B.ByteString -> StackTraceT m (File.T B.ByteString)
tapToHOPO hopoThreshold = modifyFiveFretTracks \dvn trk -> let
  hasTap = True -- TODO look for events
  in if not hasTap
    then return trk -- nothing to do
    else flip (overParsedFiveFret dvn) trk let
      eachDiff
        = G.emitGuitar5PS -- should this be an option? probably always PS format opens here
        . fmap (\((color, sht), len) -> ((color, case sht of Tap -> HOPO; _ -> sht), len))
        . G.computeFiveFretSHT G.HOPOsRBGuitar hopoThreshold
      in #difficulties % mapped %~ eachDiff

-- WIP
removeOpenNotes :: (SendMessage m) => U.Beats -> Bool -> File.T B.ByteString -> StackTraceT m (File.T B.ByteString)
removeOpenNotes hopoThreshold detectMuted = modifyFiveFretTracks \dvn trk -> let
  hasOpen = True -- TODO look for events
  in if not hasOpen
    then return trk -- nothing to do
    else flip (overParsedFiveFret dvn) trk let
      eachDiff
        = G.emitGuitar5 -- open format doesn't matter since there are none
        . fmap (\((color, sht), len) -> ((Just color, sht), len))
        . G.noOpenNotes detectMuted
        -- TODO we probably need to remove all extended sustains here
        . G.computeFiveFretSHT G.HOPOsRBGuitar hopoThreshold
      in #difficulties % mapped %~ eachDiff

limitEasyMediumFrets :: (SendMessage m, MonadIO m) => QuickFoF -> StackTraceT m QuickFoF
limitEasyMediumFrets q = applyToMIDI (limitEasyMediumFretsMIDI $ quickHOPOThreshold q) q

limitEasyMediumFretsMIDI :: (SendMessage m) => U.Beats -> File.T B.ByteString -> StackTraceT m (File.T B.ByteString)
limitEasyMediumFretsMIDI hopoThreshold
  = modifyFiveFretTracks \dvn trk ->
    flip (overParsedFiveFret dvn) trk
      $ #difficulties %~ Map.mapWithKey \diff fd -> case diff of
        Easy   -> limitFretsDifficulty 3 (Just hopoThreshold) fd
        Medium -> limitFretsDifficulty 4 (Just hopoThreshold) fd
        _      -> fd

overParsedFiveFret
  :: (SendMessage m)
  => File.Division -- midi resolution
  -> (G5.FiveTrack U.Beats -> G5.FiveTrack U.Beats)
  -> RTB.T Event.ElapsedTime (Event.T B.ByteString)
  -> StackTraceT m (RTB.T Event.ElapsedTime (Event.T B.ByteString))
overParsedFiveFret dvn f trk = do
  res <- case dvn of
    File.Ticks tks -> return tks
    _              -> fatal "Unsupported MIDI file using non-ticks (SMPTE?) timing"
  let trk'
        = RTB.mapTime (\tks -> realToFrac tks / realToFrac res)
        $ RTB.mapBody (fmap TE.decodeLatin1) trk
      name = U.trackName trk
      reencodeTrack
        = maybe id U.setTrackName name
        . RTB.discretize . RTB.mapTime (* realToFrac res)
        . RTB.mapBody (fmap encodeLatin1)
  five <- F.parseTrackReport $ F.stripTrack trk'
  let parseTrack' :: TrackCodec (PureLog Identity) U.Beats (G5.FiveTrack U.Beats)
      parseTrack' = parseTrack
  return
    $ reencodeTrack
    $ execState (codecOut parseTrack' $ f five) RTB.empty

modifyFiveFretTracks
  :: (Monad m)
  => (File.Division -> RTB.T Event.ElapsedTime (Event.T B.ByteString) -> m (RTB.T Event.ElapsedTime (Event.T B.ByteString)))
  -> File.T B.ByteString
  -> m (File.T B.ByteString)
modifyFiveFretTracks f (File.Cons typ dvn trks)
  = fmap (File.Cons typ dvn) $ forM trks \trk ->
    if isFiveFretTrack trk then f dvn trk else return trk

isFiveFretTrack :: (NNC.C t) => RTB.T t (Event.T B.ByteString) -> Bool
isFiveFretTrack trk = case U.trackName trk of
  Nothing   -> False
  Just name -> elem name
    [ "PART GUITAR"
    , "PART BASS"
    , "PART KEYS"
    , "PART RHYTHM"
    , "PART GUITAR COOP"
    ]

fillEliteFromPSRealMIDI :: (SendMessage m) => File.T B.ByteString -> StackTraceT m (Maybe (File.T B.ByteString))
fillEliteFromPSRealMIDI f = do
  song <- F.readMIDIFile $ decodeGeneral <$> f
  let (reals, notReal) = partition (\trk -> U.trackName trk == Just "PART REAL_DRUMS_PS") song.tracks
      (elites, rest) = partition (\trk -> U.trackName trk == Just "PART ELITE_DRUMS") notReal
  case (reals, elites) of
    (real : _, []) -> case F.parseTrackIgnoreWarnings real of
      Nothing -> return Nothing -- warn maybe?
      Just realParsed -> let
        elite = psRealToElite realParsed
        shownElite = F.showTrack elite
        in return $ Just $ TE.encodeUtf8 <$> F.showMIDIFile song
          { F.tracks = rest <> [U.setTrackName "PART ELITE_DRUMS" shownElite]
          }
    _ -> return Nothing

fillEliteFromPSReal :: (SendMessage m, MonadIO m) => QuickFoF -> StackTraceT m QuickFoF
fillEliteFromPSReal q = case lookup "notes.mid" q.files of
  Nothing   -> return q
  Just file -> do
    mid <- getFoFMIDI file
    fillEliteFromPSRealMIDI mid >>= \case
      Nothing   -> return q
      Just mid' -> do
        lg "Converted Phase Shift Real Drums to Elite Drums"
        return QuickFoF
          { metadata = let
            keep = filter (\(k, _) -> k /= "diff_drums_elite") q.metadata
            new = case lookup "diff_drums_real" q.metadata of
              Just d | d /= "-1" -> [("diff_drums_elite", d)]
              _                  -> []
            in keep <> new
          , files = ("notes.mid", FoFMIDI mid') : filter (\(name, _) -> name /= "notes.mid") q.files
          , location = q.location
          , format = q.format
          }

getFoFReadables :: QuickFoF -> [(T.Text, Readable)]
getFoFReadables q = q.files <&> \(name, file) -> let
  r = case file of
    FoFReadable x -> x
    FoFMIDI     m -> makeHandle (T.unpack name) $ byteStringSimpleHandle $ Save.toByteString m
  in (name, r)

getFoFMIDI :: (SendMessage m, MonadIO m) => FoFFile -> StackTraceT m (File.T B.ByteString)
getFoFMIDI = \case
  FoFMIDI     m -> return m
  FoFReadable r -> F.loadMIDIBytes r

saveQuickFoFSNG :: FilePath -> QuickFoF -> IO ()
saveQuickFoFSNG out q = do
  let temp = out <> ".temp"
  rs <- makeSNG q.metadata $ getFoFReadables q
  saveReadables rs temp
  renameFile temp out

saveQuickFoFFolder :: FilePath -> QuickFoF -> IO ()
saveQuickFoFFolder out q = do
  let temp = dropTrailingPathSeparator out <> ".temp"
  createDirectoryIfMissing False temp
  forM_ (getFoFReadables q) \(name, r) -> do
    saveReadable r $ temp </> T.unpack name
  writePSIni (temp </> "song.ini") q.metadata
  removePathForcibly out
  renameDirectory temp out

-- CH as of v1.1.0.4261-PTB seems to require song_length in .sng (not folder?), or you get:
-- "ERROR: These folders either have no audio files, the audio files are named incorrectly or the audio files are in the wrong format!"
fillRequiredMetadata :: QuickFoF -> IO QuickFoF
fillRequiredMetadata = let
  audioExts = [".ogg", ".mp3", ".wav", ".opus", ".flac"]
  toLengthMS :: Integer -> Int -> Integer
  toLengthMS frames rate = let
    secs = fromIntegral frames / fromIntegral rate :: Double
    in round $ secs * 1000
  withSongLength :: QuickFoF -> IO QuickFoF
  withSongLength q = case lookup "song_length" q.metadata of
    Just s | T.any (not . isSpace) s && T.strip s /= "0" -> return q
    _ -> do
      lens <- fmap catMaybes $ forM q.files \(name, file) -> case file of
        FoFReadable r -> let
          path = T.unpack $ T.toLower name
          in if elem (takeExtension path) audioExts
            then do
              mframes <- audioLengthReadable path r `catchAny` \_ -> return Nothing
              mrate   <- audioRateReadable   path r `catchAny` \_ -> return Nothing
              return $ liftA2 toLengthMS mframes mrate
            else return Nothing
        _ -> return Nothing
      let ms = case filter (/= 0) lens of
            []     -> 1 -- note, 0 does not work
            n : ns -> foldr max n ns
      return $ q & #metadata %~ \meta
        -> [ p | p@(k, _) <- meta, k /= "song_length" ]
        <> [ ("song_length", T.pack $ show ms) ]
  in withSongLength

convertEncoreFoF :: FilePath -> EncoreInfo FilePath -> IO QuickFoF
convertEncoreFoF f info = do
  newMidi <- generateCachedReadable do
    midPath <- fixFileCase info.midi
    bs <- useHandle (fileReadable midPath) handleToByteString
    (mid, _warnings) <- runGetM getMIDI bs
    return $ makeHandle "notes.mid" $ byteStringSimpleHandle $ Save.toByteString $ encoreMidiToFoF mid
  let audioMap =
        [ ("song", "backing")
        , ("guitar", "lead")
        , ("keys", "keys")
        , ("rhythm", "bass")
        , ("drums", "drums")
        , ("vocals", "vocals")
        ]
  newStems <- fmap catMaybes $ forM audioMap \(nameCH, keyEncore) -> do
    let paths = fromMaybe [] $ HM.lookup keyEncore info.stems
    existingPaths <- flip mapMaybeM paths \p -> do
      p' <- fixFileCase p
      e <- doesFileExist p'
      return $ guard e >> Just p'
    case existingPaths of
      [path] -> return $ Just (T.pack $ nameCH <.> takeExtension path, fileReadable path)
      p : ps@(_ : _) -> do
        let ext = "ogg" -- TODO use preference
        r <- generateCachedReadable do
          bs <- withSystemTempFile ("onyx-mix" <.> ext) \tmp h -> do
            hClose h
            src <- buildSource' $ Mix $ fmap Input $ p NE.:| ps
            -- TODO use preference for ogg quality
            audioIO Nothing src tmp
            useHandle (fileReadable tmp) handleToByteString
          return $ makeHandle ("onyx-mix" <.> ext) $ byteStringSimpleHandle bs
        return $ Just (T.pack $ nameCH <.> ext, r)
      [] -> return Nothing
  artPath <- mapM fixFileCase info.art
  return QuickFoF
    { metadata = concat
      [ [("name", info.title)]
      , [("artist", info.artist)]
      , ("album",) <$> toList info.album
      , ("loading_phrase",) <$> toList info.loading_phrase
      , [("multiplier_note", "116")]
      , toList info.length_ <&> \secs -> ("song_length", T.pack $ show $ secs * 1000) -- seconds in encore, milliseconds in fof
      , ("preview_start_time",) . T.pack . show <$> toList info.preview_start_time -- milliseconds in both
      , [("charter", T.intercalate ", " info.charters) | not $ null info.charters]
      , ("year",) <$> toList info.release_year
      , [("genre", T.intercalate ", " info.genres) | not $ null info.genres]
      , ("diff_guitar"    ,) . T.pack . show <$> toList (HM.lookup "plastic_guitar" info.diff <|> HM.lookup "pg" info.diff)
      , ("diff_bass"      ,) . T.pack . show <$> toList (HM.lookup "plastic_bass"   info.diff <|> HM.lookup "pb" info.diff)
      , ("diff_drums"     ,) . T.pack . show <$> toList (HM.lookup "plastic_drums"  info.diff <|> HM.lookup "pd" info.diff)
      , ("diff_vocals"    ,) . T.pack . show <$> toList (HM.lookup "pitched_vocals" info.diff <|> HM.lookup "pv" info.diff) -- just adding pv to be safe
      , ("diff_keys"      ,) . T.pack . show <$> toList (HM.lookup "plastic_keys"   info.diff                             )
      , ("diff_guitar_pad",) . T.pack . show <$> toList (HM.lookup "guitar"         info.diff <|> HM.lookup "gr" info.diff)
      , ("diff_bass_pad"  ,) . T.pack . show <$> toList (HM.lookup "bass"           info.diff <|> HM.lookup "ba" info.diff)
      , ("diff_drums_pad" ,) . T.pack . show <$> toList (HM.lookup "drums"          info.diff <|> HM.lookup "ds" info.diff)
      , ("diff_vocals_pad",) . T.pack . show <$> toList (HM.lookup "vocals"         info.diff <|> HM.lookup "vl" info.diff)
      , ("diff_keys_pad"  ,) . T.pack . show <$> toList (HM.lookup "keys"           info.diff                             )
      , case HM.lookup "plastic_drums"  info.diff <|> HM.lookup "pd" info.diff of
        Just n | n /= (-1) -> [("pro_drums", "True")] -- assuming drums are always pro
        _                  -> []
      ]
    , files = second FoFReadable <$> concat
      [ [("notes.mid", newMidi)]
      , case artPath of
        Nothing  -> []
        Just art -> [(T.pack $ "album" <.> takeExtension art, fileReadable art)]
      , newStems
      ]
    , location = takeDirectory f
    , format   = FoFEncore
    }

--------------------------------------------------------------------------------

loadMixedAudio :: (MonadResource m) => QuickFoF -> IO (CA.AudioSource m Float)
loadMixedAudio qfof = let
  fileList = do
    (filename, FoFReadable r) <- qfof.files
    let (name, ext) = textExt filename
    guard $ elem name
      [ "song"
      , "guitar"
      , "rhythm"
      , "bass"
      , "drums", "drums_1", "drums_2", "drums_3", "drums_4"
      , "vocals", "vocals_1", "vocals_2"
      , "keys"
      ]
    guard $ elem ext [".ogg", ".opus", ".mp3", ".wav", ".flac"]
    return r
  in case NE.nonEmpty fileList of
    Nothing -> return $ CA.silent (CA.Seconds 0) 44100 2
    Just files -> do
      src NE.:| srcs <- mapM ffSource files
      let mixed = foldl (\a b -> uncurry CA.mix $ sameChannels (a, b)) src srcs
      -- TODO apply offset/delay
      return mixed

combineCharts :: (SendMessage m, MonadIO m) => QuickFoF -> QuickFoF -> StackTraceT m QuickFoF
combineCharts songBase songNew = do
  let crossPrep = applyVolsMono [] . CA.takeStart (CA.Seconds 15) . standardRate
  srcBase <- stackIO $ crossPrep <$> loadMixedAudio songBase
  srcNew  <- stackIO $ crossPrep <$> loadMixedAudio songNew
  offset <- stackIO $ crossCorrelate srcBase srcNew
  stackIO $ print offset
  -- TODO maybe we should also try to correlate the ends of the song,
  -- and warn if the result is different from the start of the song,
  -- as this probably means the two charts have different speed audio or something.
  -- maybe also want to use smarter methods of detecting when two charts have
  -- very different lengths and handle that better to have a better shot at proper sync
  baseWithMidi <- convertMIDI songBase
  newWithMidi  <- convertMIDI songNew
  let findMidi :: (SendMessage m, MonadIO m) => QuickFoF -> StackTraceT m (File.T B.ByteString)
      findMidi song = case lookup "notes.mid" song.files of
        Nothing -> fatal $ "Couldn't find MIDI/.chart in song at " <> song.location
        Just f  -> getFoFMIDI f
  baseMidi  <- findMidi baseWithMidi
  newMidi   <- findMidi newWithMidi
  finalMidi <- combineMidi
    offset
    (quickHOPOThreshold newWithMidi)
    (songDetectTracks baseWithMidi.metadata baseMidi)
    baseMidi
    (songDetectTracks newWithMidi.metadata newMidi)
    newMidi
  let combinedMetadata = combineMetadata baseWithMidi.metadata newWithMidi.metadata
      finalSong :: QuickFoF
      finalSong = baseWithMidi
        & #files % mapped %~
          (\(name, f) -> (name, if name == "notes.mid" then FoFMIDI finalMidi else f))
        & #metadata .~ combinedMetadata
  return finalSong

data FoFTrackInfo = FoFTrackInfo
  { trackNames    :: [T.Text]
  , difficultyKey :: T.Text
  }

detectTrackMapping :: HM.HashMap F.PartName [FoFTrackInfo]
detectTrackMapping = HM.fromList
  [ (F.PartGuitar,
    [ FoFTrackInfo ["PART GUITAR"] "diff_guitar"
    , FoFTrackInfo ["PART GUITAR GHL"] "diff_guitarghl"
    , FoFTrackInfo ["PART REAL_GUITAR"] "diff_guitar_real"
    , FoFTrackInfo ["PART REAL_GUITAR_22"] "diff_guitar_real_22"
    , FoFTrackInfo ["PAD GUITAR"] "diff_guitar_pad"
    ])
  , (F.PartBass,
    [ FoFTrackInfo ["PART BASS"] "diff_bass"
    , FoFTrackInfo ["PART BASS GHL"] "diff_bassghl"
    , FoFTrackInfo ["PART REAL_BASS"] "diff_bass_real"
    , FoFTrackInfo ["PART REAL_BASS_22"] "diff_bass_real_22"
    , FoFTrackInfo ["PAD BASS"] "diff_bass_pad"
    ])
  , (F.PartDrums,
    [ FoFTrackInfo ["PART DRUMS", "PART DRUMS_2X", "PART DRUM"] "diff_drums"
    , FoFTrackInfo ["PART REAL_DRUMS_PS"] "diff_drums_real"
    , FoFTrackInfo ["PART ELITE_DRUMS"] "diff_drums_elite" -- diff key tentative
    , FoFTrackInfo ["PAD DRUMS"] "diff_drums_pad"
    ])
  , (F.PartKeys,
    [ FoFTrackInfo ["PART KEYS"] "diff_keys"
    , FoFTrackInfo ["PART REAL_KEYS_E", "PART REAL_KEYS_M", "PART REAL_KEYS_H", "PART REAL_KEYS_X"] "diff_keys_real"
    , FoFTrackInfo ["PAD KEYS"] "diff_keys_pad"
    ])
  , (F.PartVocal,
    [ FoFTrackInfo ["PART VOCALS"] "diff_vocals"
    , FoFTrackInfo ["HARM1", "HARM2", "HARM3"] "diff_vocals_harm"
    , FoFTrackInfo ["PAD VOCALS"] "diff_vocals_pad"
    ])
  , (F.PartName "rhythm",
    [ FoFTrackInfo ["PART RHYTHM"] "diff_rhythm"
    , FoFTrackInfo ["PART RHYTHM GHL"] "diff_rhythm_ghl"
    ])
  , (F.PartName "guitar-coop",
    [ FoFTrackInfo ["PART GUITAR COOP"] "diff_guitar_coop"
    , FoFTrackInfo ["PART GUITAR COOP GHL"] "diff_guitar_coop_ghl"
    ])
  ]

songDetectTracks :: [(T.Text, T.Text)] -> File.T B.ByteString -> [T.Text]
songDetectTracks metadata (File.Cons _ _ trks) = let
  metadataMap = HM.fromList metadata
  tracksWithNotes = nubOrd do
    trk <- trks
    name <- decodeGeneral <$> toList (U.trackName trk)
    guard $ any (\e -> isJust $ isNoteEdge e) trk
    return name
  nameToDiff = HM.fromList do
    FoFTrackInfo names diff <- concat $ toList detectTrackMapping
    name <- names
    return (name, diff)
  isAllowedInMetadata name = case HM.lookup name nameToDiff >>= \k -> HM.lookup k metadataMap of
    Just "-1" -> False
    _         -> True
  in filter isAllowedInMetadata tracksWithNotes

combineMetadata :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
combineMetadata baseMetadata newMetadata = let
  baseMap = HM.fromList baseMetadata
  newMap = HM.fromList newMetadata

  -- Helper to get a value from either map
  getValue key = HM.lookup key baseMap <|> HM.lookup key newMap

  -- Combine charter properties
  combineCharters = case (HM.lookup "charter" baseMap, HM.lookup "charter" newMap) of
    (Just base, Just new) | T.strip base /= "" && T.strip new /= "" ->
      if base == new then [("charter", base)] else [("charter", base <> "; " <> new)]
    (Just base, _) | T.strip base /= "" -> [("charter", base)]
    (_, Just new) | T.strip new /= "" -> [("charter", new)]
    _ -> []

  -- Handle diff_ properties
  combineDiffs = flip mapMaybe (nubOrd $ HM.keys baseMap ++ HM.keys newMap) \key ->
    if "diff_" `T.isPrefixOf` key
      then case (HM.lookup key baseMap, HM.lookup key newMap) of
        (Just base, _) | base /= "-1" -> Just (key, base)
        (_, Just new) | new /= "-1"   -> Just (key, new)
        -- TODO we may want smarter handling if it's -1 in one song and missing in the other!
        _                             -> Just (key, "-1")
      else Nothing

  -- Handle special properties (only from base song)
  specialProps = ["song_length", "preview_start_time", "preview_end_time", "delay"]
  baseOnlyProps = flip mapMaybe specialProps \key ->
    fmap (key,) (HM.lookup key baseMap)

  -- Handle other properties (excluding charter, diff_, and special properties)
  otherProps = flip mapMaybe (nubOrd $ HM.keys baseMap ++ HM.keys newMap) \key ->
    if not ("diff_" `T.isPrefixOf` key) && key /= "charter" && not (key `elem` specialProps)
      then fmap (key,) (getValue key)
      else Nothing

  in combineCharters ++ combineDiffs ++ baseOnlyProps ++ otherProps

-- If this is a 5-fret or 6-fret track, replaces just the force strum/hopo/tap events to cover all notes.
forceSHT :: U.Beats -> RTB.T U.Beats (Event.T T.Text) -> RTB.T U.Beats (Event.T T.Text)
forceSHT hopoThreshold track = let
  names5 = ["PART GUITAR", "PART BASS", "PART KEYS", "PART RHYTHM", "PART GUITAR COOP"]
  names6 = ["PART GUITAR GHL", "PART BASS GHL", "PART RHYTHM GHL", "PART GUITAR COOP GHL"]
  -- True if event is a force hopo/strum/tap note edge, or a force tap sysex event
  isForceEvent event = case isNoteEdge event of
    Just (p, _) -> elem p [65, 66, 77, 78, 89, 90, 101, 102, 104]
    Nothing     -> case parsePSSysEx event of
      Just msg -> msg.psPhraseID == TapNotes
      Nothing  -> False
  replaceEvents newTrack = RTB.merge
    (RTB.filter (not . isForceEvent) track   )
    (RTB.filter isForceEvent         newTrack)
  in case U.trackName track of
    Just name
      | elem name names5 -> case F.parseTrackIgnoreWarnings track of
        Nothing        -> track -- warn or something?
        Just fiveTrack -> let
          allForces = fiveTrack
            { G5.difficulties = reforce <$> fiveTrack.difficulties
            }
          -- probably don't care about using keys algo for PART KEYS
          reforce = G.emitGuitar5 . G.computeFiveFretSHT G.HOPOsRBGuitar hopoThreshold
          newTrack = F.showTrack allForces
          in replaceEvents newTrack
      | elem name names6 -> case F.parseTrackIgnoreWarnings track of
        Nothing        -> track -- warn or something?
        Just sixTrack -> let
          allForces = sixTrack
            { G6.difficulties = reforce <$> sixTrack.difficulties
            }
          reforce = G.emitGuitar6 . G.computeSixFretSHT G.HOPOsRBGuitar hopoThreshold
          newTrack = F.showTrack allForces
          in replaceEvents newTrack
    _ -> track

-- Pads the start with time, but pulls time-0 meta+sysex events back so they're still at time 0.
syncDelay :: (NNC.C t) => t -> RTB.T t (Event.T T.Text) -> RTB.T t (Event.T T.Text)
syncDelay t trk = let
  (zero, rest) = U.trackSplitZero trk
  (match, other) = partition isMetaSysEx zero
  isMetaSysEx = \case
    Event.MetaEvent      {} -> True
    Event.SystemExclusive{} -> True
    _                       -> False
  in U.trackGlueZero match $ RTB.delay t $ U.trackGlueZero other rest

condenseEvents :: (NNC.C t, Ord s) => RTB.T t (Event.T s) -> [Event.T s]
condenseEvents = go [] . toList . RTB.normalize where
  -- normalize should put note-off (both 0x8 and 0x9-velocity-0) before simultaneous note-on;
  -- I guess it doesn't guarantee that PS sysex will be sorted similarly
  -- if you do a weird mix of all-difficulty and specific-difficuly messages, but whatever
  go cur []       = cur
  go cur (x : xs) = case x of
    Event.MetaEvent{} -> go (x : cur) xs
    _ -> case parsePSSysEx x of
      Just msg | not msg.psEdge -> go
        (removePSMessage msg.psDifficulty msg.psPhraseID cur)
        xs
      _ -> case isNoteEdgeCPV x of
        Just (c, p, Nothing) -> go
          (filter (not . isNoteOnFor c p) cur)
          xs
        _ -> go (x : cur) xs -- could condense other stuff like control values, but not important for now
  isNoteOnFor c p e = case isNoteEdgeCPV e of
    Just (c', p', Just _) -> c == c' && p == p'
    _                     -> False
  removePSMessage diff phraseID events = let
    (currentDifficulties, rest) = flip partitionMaybe events \e -> do
      msg <- parsePSSysEx e
      guard $ msg.psPhraseID == phraseID && msg.psEdge
      return msg.psDifficulty
    withoutDifficulty toRemove = do
      d <- [Easy .. Expert]
      guard $ d /= toRemove && any (`elem` currentDifficulties) [Just d, Nothing]
      return $ unparsePSSysEx $ PSMessage (Just d) phraseID True
    in case diff of
      Nothing         -> rest
      Just removeDiff -> withoutDifficulty removeDiff <> rest

-- Drops time from the start, but moves meta+sysex events forward to time 0,
-- and if a note would be split, moves the note-on forward to just shrink the note length.
-- (probably similar for sysex edges, make sure on/off are processed right)
syncDrop :: (NNC.C t) => t -> RTB.T t (Event.T T.Text) -> RTB.T t (Event.T T.Text)
syncDrop t trk = let
  (dropped, rest) = U.trackSplit t trk
  in U.trackGlueZero (condenseEvents dropped) rest

combineMidi
  :: (SendMessage m)
  => Double
  -> U.Beats
  -> [T.Text]
  -> File.T B.ByteString
  -> [T.Text]
  -> File.T B.ByteString
  -> StackTraceT m (File.T B.ByteString)
combineMidi offset hopoThreshold baseTracks baseMidi newTracks newMidi = do

  baseSong <- F.readMIDIFile $ decodeGeneral <$> baseMidi
  newSong  <- F.readMIDIFile $ decodeGeneral <$> newMidi

  let baseTrackSet = HS.fromList $ "EVENTS" : "VENUE" : baseTracks
      addedTrackSet = HS.fromList newTracks `HS.difference` baseTrackSet
      allowedNewTrack  = maybe False (\name -> HS.member name addedTrackSet) . U.trackName
      allowedBaseTrack = maybe False (\name -> HS.member name baseTrackSet ) . U.trackName

  -- Apply tempo track to get new tracks in real time format
  let newTracksRealTime
        = filter allowedNewTrack
        $ map (U.applyTempoTrack newSong.tempos . forceSHT hopoThreshold) newSong.tracks

  -- Apply offset to each track, preserving track name events at position 0
  let applyOffsetToTrack trk = let
        name = U.trackName trk
        removeTrackName = RTB.filter \case Event.MetaEvent (Meta.TrackName _) -> False; _ -> True
        offsetTrack = if offset >= 0
          then syncDelay (U.Seconds $ realToFrac offset) $ removeTrackName trk
          else syncDrop (U.Seconds $ realToFrac $ negate offset) $ removeTrackName trk
        in maybe id U.setTrackName name offsetTrack

      offsetNewTracksRealTime = map applyOffsetToTrack newTracksRealTime

      -- Convert back to beats using base song's tempo map
      offsetNewTracks = map (U.unapplyTempoTrack baseSong.tempos) offsetNewTracksRealTime

  let combinedTracks = filter allowedBaseTrack baseSong.tracks ++ offsetNewTracks
      combinedSong = F.Song baseSong.tempos baseSong.timesigs combinedTracks

  return $ TE.encodeUtf8 <$> F.showMIDIFile combinedSong

--------------------------------------------------------------------------------

{-

TODO game downconversion steps

PS (free):
- convert taps and opens to sysex format (pull back opens)
- optional: no opens since no hopo opens
- bass.ogg -> rhythm.ogg (steam PS supports this)
- remove <tags> from metadata, loading phrase, lyrics

FoFiX:
- remove taps and opens, no sysex events allowed
- probably no extended sustains
- guitar.ogg is required
- guitar.ogg must be the song length or practice mode breaks
- for no stems, should just move song.ogg to guitar.ogg (otherwise no audio in practice mode)
- bunch of PS stem filenames not supported, combine

FoF:
- no square album art, convert to cassette format
- IIRC, guitar needs to be the first midi track

-}
