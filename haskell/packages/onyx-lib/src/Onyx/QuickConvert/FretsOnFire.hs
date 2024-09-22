{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.QuickConvert.FretsOnFire where

import           Control.Monad.Codec              (codecOut)
import           Control.Monad.Extra              (forM, forM_, guard,
                                                   mapMaybeM)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.State.Strict (execState)
import           Data.Bifunctor                   (second)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace, toLower)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (Identity, runIdentity)
import qualified Data.HashMap.Strict              as HM
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Audio                       (Audio (..), audioIO,
                                                   audioLengthReadable,
                                                   audioRateReadable,
                                                   buildSource', loadAudioInput)
import           Onyx.CloneHero.SNG
import           Onyx.FeedBack.Load               (chartToBeats, chartToIni,
                                                   chartToMIDI, loadChartFile,
                                                   loadChartReadable)
import           Onyx.FretsOnFire                 (loadPSIni, songToIniContents,
                                                   writePSIni)
import           Onyx.Guitar                      (HOPOsAlgorithm (HOPOsRBGuitar),
                                                   applyStatus,
                                                   computeFiveFretSHT,
                                                   emitGuitar5, emitGuitar5PS,
                                                   noOpenNotes)
import           Onyx.Import.Encore               (EncoreInfo (..),
                                                   encoreMidiToFoF,
                                                   loadEncoreInfo)
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   StrumHOPOTap (..),
                                                   isNoteEdge)
import           Onyx.MIDI.Parse                  (getMIDI)
import           Onyx.MIDI.Read                   (TrackCodec, parseTrack)
import           Onyx.MIDI.Track.File             (loadMIDIBytes,
                                                   parseTrackReport,
                                                   showMIDIFile', stripTrack)
import qualified Onyx.MIDI.Track.FiveFret         as G5
import           Onyx.PhaseShift.Message          (PSMessage (..),
                                                   PhraseID (..), parsePSSysEx,
                                                   unparsePSSysEx)
import           Onyx.StackTrace
import           Onyx.Util.Binary                 (runGetM)
import           Onyx.Util.Files                  (fixFileCase)
import           Onyx.Util.Handle
import           Onyx.Util.Text.Decode            (encodeLatin1)
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
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
  }

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
  | FoFMIDI (F.T B.ByteString)

loadQuickFoF :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m (Maybe QuickFoF)
loadQuickFoF fin = inside ("Loading: " <> fin) $ let
  r = fileReadable fin
  in case map toLower $ takeFileName fin of
    "song.ini" -> do
      let dir = takeDirectory fin
      ini <- loadPSIni r
      folder <- stackIO $ crawlFolder dir
      return $ Just QuickFoF
        { metadata = ini
        , files    = flip mapMaybe (folderFiles folder) $ \(name, x) -> do
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
      else return Nothing

textExt :: T.Text -> (T.Text, T.Text)
textExt x = let
  (name, ext) = splitExtension $ T.unpack x
  in (T.pack name, T.toLower $ T.pack ext)

convertAudio :: [T.Text] -> T.Text -> QuickFoF -> IO QuickFoF
convertAudio convFrom convTo q = do
  newFiles <- forM q.files $ \pair@(f, file) -> case file of
    FoFReadable r -> case textExt f of
      (name, ext) | elem ext convFrom -> do
        let newName = name <> convTo
            newStr = T.unpack newName
        bs <- withSystemTempFile newStr $ \fout hout -> do
          withSystemTempFile (T.unpack name) $ \fin hin -> do
            hClose hin
            hClose hout
            saveReadable r fin
            src <- loadAudioInput fin
            audioIO Nothing src fout
            BL.fromStrict <$> B.readFile fout
        return (newName, FoFReadable $ makeHandle newStr $ byteStringSimpleHandle bs)
      _ -> return pair
    _ -> return pair
  return $ let QuickFoF{..} = q in QuickFoF { files = newFiles, .. }

convertOgg, convertOpus :: QuickFoF -> IO QuickFoF
convertOgg  = convertAudio [".mp3", ".wav", ".opus", ".flac"] ".ogg"
convertOpus = convertAudio [".mp3", ".wav", ".ogg" , ".flac"] ".opus"

convertMIDI :: (SendMessage m, MonadIO m) => QuickFoF -> StackTraceT m QuickFoF
convertMIDI q = do
  newFiles <- forM q.files $ \pair@(f, file) -> case file of
    FoFReadable r -> case textExt f of
      (name, ".chart") -> do
        chart <- chartToBeats <$> loadChartReadable r
        mid <- chartToMIDI chart
        let newName = name <> ".mid"
        return (newName, FoFMIDI $ fmap TE.encodeUtf8 $ showMIDIFile' mid)
        -- TODO should also save .chart.bak
      _ -> return pair
    _ -> return pair
  return $ let QuickFoF{..} = q in QuickFoF { files = newFiles, .. }

applyToMIDI
  :: (SendMessage m, MonadIO m)
  => (F.T B.ByteString -> StackTraceT m (F.T B.ByteString))
  -> QuickFoF -> StackTraceT m QuickFoF
applyToMIDI f q = do
  newFiles <- forM q.files $ \pair@(name, file) -> case T.toLower name of
    "notes.mid" -> do
      newMIDI <- getFoFMIDI file >>= f
      return (name, FoFMIDI newMIDI)
    _ -> return pair
  return $ let QuickFoF{..} = q in QuickFoF { files = newFiles, .. }

quickHOPOThreshold :: QuickFoF -> U.Beats
quickHOPOThreshold q = case lookup "hopo_frequency" q.metadata >>= readMaybe . T.unpack of
  Just n -> fromInteger n / 480
  _      -> case T.toLower <$> lookup "eighthnote_hopo" q.metadata of
    Just "true" -> 250 / 480 -- don't know exact interpretation but this is what I went with in FoF import
    _           -> 170 / 480

psTapOpenFormats :: (SendMessage m, MonadIO m) => QuickFoF -> StackTraceT m QuickFoF
psTapOpenFormats q = applyToMIDI (psOpenFormat (quickHOPOThreshold q) . psTapFormat) q

psTapFormat :: F.T B.ByteString -> F.T B.ByteString
psTapFormat = (runIdentity .) $ modifyFiveFretTracks $ \_dvn trk -> return $ let
  (notes, trk1) = flip RTB.partitionMaybe trk $ \e -> do
    (104, b) <- isNoteEdge e
    return b
  (sysexes, trk2) = flip RTB.partitionMaybe trk1 $ \e -> do
    msg <- parsePSSysEx e
    guard $ msg.psPhraseID == TapNotes
    return msg
  -- overlay the note and sysex (expert / all difficulties) forms
  -- so we enable tap when either of them say to
  events = RTB.merge (('a',) <$> notes) $ flip RTB.mapMaybe sysexes $ \msg ->
    case msg.psDifficulty of
      Nothing     -> Just ('b', msg.psEdge)
      Just Expert -> Just ('b', msg.psEdge)
      _           -> Nothing
  in if RTB.null notes
    then trk -- nothing to do
    else RTB.merge trk2 $ flip fmap (applyStatus events events) $ \(active, _) ->
      -- only emit all difficulties sysex
      unparsePSSysEx PSMessage
        { psDifficulty = Nothing
        , psPhraseID   = TapNotes
        , psEdge       = not $ null active
        }

psOpenFormat :: (SendMessage m) => U.Beats -> F.T B.ByteString -> StackTraceT m (F.T B.ByteString)
psOpenFormat hopoThreshold = modifyFiveFretTracks $ \dvn trk -> let
  isEnhancedOpensEvent = \case
    E.MetaEvent (Meta.TextEvent s) -> elem s ["[ENHANCED_OPENS]", "ENHANCED_OPENS"]
    _                              -> False
  in if not $ any isEnhancedOpensEvent $ U.trackTakeZero trk
    then return trk -- nothing to do
    else flip (overParsedFiveFret dvn) trk $ \ft -> let
      G5.FiveTrack{..} = ft
      eachDiff = emitGuitar5PS . computeFiveFretSHT HOPOsRBGuitar hopoThreshold
      in G5.FiveTrack
        { fiveDifficulties = fmap eachDiff ft.fiveDifficulties
        , ..
        }

-- WIP
tapToHOPO :: (SendMessage m) => U.Beats -> F.T B.ByteString -> StackTraceT m (F.T B.ByteString)
tapToHOPO hopoThreshold = modifyFiveFretTracks $ \dvn trk -> let
  hasTap = True -- TODO look for events
  in if not hasTap
    then return trk -- nothing to do
    else flip (overParsedFiveFret dvn) trk $ \ft -> let
      G5.FiveTrack{..} = ft
      eachDiff
        = emitGuitar5PS -- should this be an option? probably always PS format opens here
        . fmap (\((color, sht), len) -> ((color, case sht of Tap -> HOPO; _ -> sht), len))
        . computeFiveFretSHT HOPOsRBGuitar hopoThreshold
      in G5.FiveTrack
        { fiveDifficulties = fmap eachDiff ft.fiveDifficulties
        , ..
        }

-- WIP
removeOpenNotes :: (SendMessage m) => U.Beats -> Bool -> F.T B.ByteString -> StackTraceT m (F.T B.ByteString)
removeOpenNotes hopoThreshold detectMuted = modifyFiveFretTracks $ \dvn trk -> let
  hasOpen = True -- TODO look for events
  in if not hasOpen
    then return trk -- nothing to do
    else flip (overParsedFiveFret dvn) trk $ \ft -> let
      G5.FiveTrack{..} = ft
      eachDiff
        = emitGuitar5 -- open format doesn't matter since there are none
        . fmap (\((color, sht), len) -> ((Just color, sht), len))
        . noOpenNotes detectMuted
        -- TODO we probably need to remove all extended sustains here
        . computeFiveFretSHT HOPOsRBGuitar hopoThreshold
      in G5.FiveTrack
        { fiveDifficulties = fmap eachDiff ft.fiveDifficulties
        , ..
        }

overParsedFiveFret
  :: (SendMessage m)
  => F.Division -- midi resolution
  -> (G5.FiveTrack U.Beats -> G5.FiveTrack U.Beats)
  -> RTB.T E.ElapsedTime (E.T B.ByteString)
  -> StackTraceT m (RTB.T E.ElapsedTime (E.T B.ByteString))
overParsedFiveFret dvn f trk = do
  res <- case dvn of
    F.Ticks tks -> return tks
    _           -> fatal "Unsupported MIDI file using non-ticks (SMPTE?) timing"
  let trk'
        = RTB.mapTime (\tks -> realToFrac tks / realToFrac res)
        $ RTB.mapBody (fmap TE.decodeLatin1) trk
      name = U.trackName trk
      reencodeTrack
        = maybe id U.setTrackName name
        . RTB.discretize . RTB.mapTime (* realToFrac res)
        . RTB.mapBody (fmap encodeLatin1)
  five <- parseTrackReport $ stripTrack trk'
  let parseTrack' :: TrackCodec (PureLog Identity) U.Beats (G5.FiveTrack U.Beats)
      parseTrack' = parseTrack
  return
    $ reencodeTrack
    $ execState (codecOut parseTrack' $ f five) RTB.empty

modifyFiveFretTracks
  :: (Monad m)
  => (F.Division -> RTB.T E.ElapsedTime (E.T B.ByteString) -> m (RTB.T E.ElapsedTime (E.T B.ByteString)))
  -> F.T B.ByteString
  -> m (F.T B.ByteString)
modifyFiveFretTracks f (F.Cons typ dvn trks)
  = fmap (F.Cons typ dvn) $ forM trks $ \trk ->
    if isFiveFretTrack trk then f dvn trk else return trk

isFiveFretTrack :: (NNC.C t) => RTB.T t (E.T B.ByteString) -> Bool
isFiveFretTrack trk = case U.trackName trk of
  Nothing   -> False
  Just name -> elem name
    [ "PART GUITAR"
    , "PART BASS"
    , "PART KEYS"
    , "PART RHYTHM"
    , "PART GUITAR COOP"
    ]

getFoFReadables :: QuickFoF -> [(T.Text, Readable)]
getFoFReadables q = flip map q.files $ \(name, file) -> let
  r = case file of
    FoFReadable x -> x
    FoFMIDI     m -> makeHandle (T.unpack name) $ byteStringSimpleHandle $ Save.toByteString m
  in (name, r)

getFoFMIDI :: (SendMessage m, MonadIO m) => FoFFile -> StackTraceT m (F.T B.ByteString)
getFoFMIDI = \case
  FoFMIDI     m -> return m
  FoFReadable r -> loadMIDIBytes r

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
  forM_ (getFoFReadables q) $ \(name, r) -> do
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
  withSongLength q = case lookup "song_length" q.metadata of
    Just s | T.any (not . isSpace) s -> return q
    _ -> do
      lens <- fmap catMaybes $ forM q.files $ \(name, file) -> case file of
        FoFReadable r -> let
          path = T.unpack $ T.toLower name
          in if elem (takeExtension path) audioExts
            then do
              mframes <- audioLengthReadable path r `catchAny` \_ -> return Nothing
              mrate   <- audioRateReadable   path r `catchAny` \_ -> return Nothing
              return $ liftA2 toLengthMS mframes mrate
            else return Nothing
        _ -> return Nothing
      let ms = case lens of
            []     -> 1 -- note, 0 does not work
            n : ns -> foldr max n ns
      return $ let QuickFoF{..} = q in QuickFoF
        { metadata
          =  [ p | p@(k, _) <- q.metadata, k /= "song_length" ]
          <> [ ("song_length", T.pack $ show ms) ]
        , ..
        }
  in withSongLength

convertEncoreFoF :: FilePath -> EncoreInfo FilePath -> IO QuickFoF
convertEncoreFoF f info = do
  newMidi <- generateCachedReadable $ do
    midPath <- fixFileCase info.midi
    bs <- useHandle (fileReadable midPath) handleToByteString
    (mid, _warnings) <- runGetM getMIDI bs
    return $ makeHandle "notes.mid" $ byteStringSimpleHandle $ Save.toByteString $ encoreMidiToFoF mid
  let audioMap =
        [ ("song", "backing")
        , ("guitar", "lead")
        , ("rhythm", "bass")
        , ("drums", "drums")
        , ("vocals", "vocals")
        ]
  newStems <- fmap catMaybes $ forM audioMap $ \(nameCH, keyEncore) -> do
    let paths = fromMaybe [] $ HM.lookup keyEncore info.stems
    existingPaths <- flip mapMaybeM paths $ \p -> do
      p' <- fixFileCase p
      e <- doesFileExist p'
      return $ guard e >> Just p'
    case existingPaths of
      [path] -> return $ Just (T.pack $ nameCH <.> takeExtension path, fileReadable path)
      p : ps@(_ : _) -> do
        let ext = "ogg" -- TODO use preference
        r <- generateCachedReadable $ do
          bs <- withSystemTempFile ("onyx-mix" <.> ext) $ \tmp h -> do
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
      , flip map (toList info.length_) $ \secs -> ("song_length", T.pack $ show $ secs * 1000)
      , ("preview_start_time",) . T.pack . show <$> toList info.preview_start_time
      , [("charter", T.intercalate ", " info.charters) | not $ null info.charters]
      , ("year",) <$> toList info.release_year
      , [("genre", T.intercalate ", " info.genres) | not $ null info.genres]
      , ("diff_guitar",) . T.pack . show <$> toList (HM.lookup "plastic_guitar" info.diff)
      , ("diff_bass"  ,) . T.pack . show <$> toList (HM.lookup "plastic_bass"   info.diff)
      , ("diff_drums" ,) . T.pack . show <$> toList (HM.lookup "plastic_drums"  info.diff)
      , ("diff_vocals",) . T.pack . show <$> toList (HM.lookup "pitched_vocals" info.diff)
      , case HM.lookup "plastic_drums" info.diff of
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
