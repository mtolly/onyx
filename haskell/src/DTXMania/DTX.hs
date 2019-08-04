{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module DTXMania.DTX where

import           Audio                            (applyPansVols, mixMany)
import           Control.Applicative              ((<|>))
import           Control.Concurrent.MVar          (newEmptyMVar, tryPutMVar,
                                                   tryReadMVar)
import           Control.Monad                    (forM, forM_, guard, void)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Resource     (MonadResource, runResourceT)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Binary.Get
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isAlphaNum, isDigit, toLower)
import           Data.Conduit
import           Data.Conduit.Audio
import           Data.Conduit.Audio.Mpg123        (sourceMpg)
import           Data.Conduit.Audio.SampleRate    (ConverterType (SincMediumQuality),
                                                   resampleTo)
import           Data.Conduit.Audio.Sndfile       (sourceSnd)
import qualified Data.Conduit.List                as CL
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe
import qualified Data.Text                        as T
import qualified Data.Vector.Storable             as V
import           DTXMania.ShiftJIS                (decodeShiftJIS)
import           DTXMania.XA                      (sourceXA)
import           Numeric
import           OSFiles                          (fixFileCase)
import qualified RockBand.Codec.Five              as Five
import           RockBand.Common                  (pattern RNil, pattern Wait)
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath
import           System.IO
import           Text.Decode                      (decodeWithDefault)
import           Text.Read                        (readMaybe)

loadDTXLines :: FilePath -> IO [(T.Text, T.Text)]
loadDTXLines f = do
  -- Shift-JIS is the usual encoding, but I've seen UTF-16 (with BOM) in the wild
  lns <- T.lines . decodeWithDefault (T.pack . decodeShiftJIS) <$> B.readFile f
  return $ flip mapMaybe lns $ \ln -> case T.uncons ln of
    Just ('#', rest) -> case T.span isAlphaNum rest of
      (x, y) -> Just (T.strip x, T.strip $ T.takeWhile (/= ';') $ T.dropWhile (== ':') y)
    _ -> Nothing

type BarNumber = Int
type Channel = T.Text
type Chip = T.Text

data DTX = DTX
  { dtx_TITLE         :: Maybe T.Text
  , dtx_ARTIST        :: Maybe T.Text
  , dtx_PREIMAGE      :: Maybe FilePath
  , dtx_COMMENT       :: Maybe T.Text
  , dtx_GENRE         :: Maybe T.Text
  , dtx_PREVIEW       :: Maybe FilePath
  , dtx_STAGEFILE     :: Maybe FilePath
  , dtx_DLEVEL        :: Maybe Int
  , dtx_GLEVEL        :: Maybe Int
  , dtx_BLEVEL        :: Maybe Int
  , dtx_DLVDEC        :: Maybe Int
  , dtx_GLVDEC        :: Maybe Int
  , dtx_BLVDEC        :: Maybe Int
  , dtx_WAV           :: HM.HashMap Chip FilePath
  , dtx_VOLUME        :: HM.HashMap Chip Int
  , dtx_PAN           :: HM.HashMap Chip Int
  , dtx_MeasureMap    :: U.MeasureMap
  , dtx_TempoMap      :: U.TempoMap
  , dtx_Drums         :: RTB.T U.Beats (DrumLane, Chip)
  , dtx_DrumsDummy    :: RTB.T U.Beats (DrumLane, Chip)
  , dtx_Guitar        :: RTB.T U.Beats ([Five.Color], Chip)
  , dtx_GuitarWailing :: RTB.T U.Beats ()
  , dtx_Bass          :: RTB.T U.Beats ([Five.Color], Chip)
  , dtx_BassWailing   :: RTB.T U.Beats ()
  , dtx_BGM           :: RTB.T U.Beats Chip
  , dtx_BGMExtra      :: HM.HashMap Channel (RTB.T U.Beats Chip)
  } deriving (Show)

textFloat :: (RealFrac a) => T.Text -> Maybe a
textFloat = fmap fst . listToMaybe . readFloat . map (\case ',' -> '.'; c -> c) . T.unpack

textHex :: (Eq a, Num a) => T.Text -> Maybe a
textHex = fmap fst . listToMaybe . readHex . T.unpack

splitChips :: T.Text -> [T.Text]
splitChips t = if T.length t >= 2
  then case T.splitAt 2 t of
    (x, y) -> x : splitChips y
  else []

data DrumLane
  = HihatClose
  | Snare
  | BassDrum
  | HighTom
  | LowTom
  | Cymbal
  | FloorTom
  | HihatOpen
  | RideCymbal
  | LeftCymbal
  | LeftPedal
  | LeftBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

drumChar :: DrumLane -> Char
drumChar = \case
  HihatClose -> '1'
  Snare      -> '2'
  BassDrum   -> '3'
  HighTom    -> '4'
  LowTom     -> '5'
  Cymbal     -> '6'
  FloorTom   -> '7'
  HihatOpen  -> '8'
  RideCymbal -> '9'
  LeftCymbal -> 'A'
  LeftPedal  -> 'B'
  LeftBass   -> 'C'

drumGDA :: DrumLane -> Maybe T.Text
drumGDA = \case
  HihatClose -> Just "HH"
  Snare      -> Just "SD"
  BassDrum   -> Just "BD" -- this conflicts with .dtx, so only read from .gda
  HighTom    -> Just "HT"
  LowTom     -> Just "LT"
  Cymbal     -> Just "CY"
  FloorTom   -> Just "FT" -- guess from DTXC
  HihatOpen  -> Nothing -- unknown
  RideCymbal -> Just "RD" -- guess from DTXC
  LeftCymbal -> Just "LC" -- guess from DTXC
  LeftPedal  -> Just "LP" -- guess from DTXC
  LeftBass   -> Just "LB" -- guess from DTXC

guitarMapping :: [(Channel, [Five.Color])]
guitarMapping = execWriter $ do
  let x chan cols = tell [(chan, cols)]
      n1 = Five.Green  -- red in GF
      n2 = Five.Red    -- green in GF
      n3 = Five.Yellow -- blue in GF
      n4 = Five.Blue   -- yellow in GF
      n5 = Five.Orange -- purple in GF
  forM_ ['2', 'G'] $ \c -> do
    x (T.cons c "0") []
    x (T.cons c "1") [n3]
    x (T.cons c "2") [n2]
    x (T.cons c "3") [n2, n3]
    x (T.cons c "4") [n1]
    x (T.cons c "5") [n1, n3]
    x (T.cons c "6") [n1, n2]
    x (T.cons c "7") [n1, n2, n3]
    -- 28 / G8 / GW is wailing
  x "93" [n4]
  x "94" [n3, n4]
  x "95" [n2, n4]
  x "96" [n2, n3, n4]
  x "97" [n1, n4]
  x "98" [n1, n3, n4]
  x "99" [n1, n2, n4]
  x "9A" [n1, n2, n3, n4]
  x "9B" [n5]
  x "9C" [n3, n5]
  x "9D" [n2, n5]
  x "9E" [n2, n3, n5]
  x "9F" [n1, n5]
  x "A9" [n1, n3, n5]
  x "AA" [n1, n2, n5]
  x "AB" [n1, n2, n3, n5]
  x "AC" [n4, n5]
  x "AD" [n3, n4, n5]
  x "AE" [n2, n4, n5]
  x "AF" [n2, n3, n4, n5]
  x "D0" [n1, n4, n5]
  x "D1" [n1, n3, n4, n5]
  x "D2" [n1, n2, n4, n5]
  x "D3" [n1, n2, n3, n4, n5]

bassMapping :: [(Channel, [Five.Color])]
bassMapping = execWriter $ do
  let x chan cols = tell [(chan, cols)]
      n1 = Five.Green  -- red in GF
      n2 = Five.Red    -- green in GF
      n3 = Five.Yellow -- blue in GF
      n4 = Five.Blue   -- yellow in GF
      n5 = Five.Orange -- purple in GF
  x "A0" []
  x "A1" [n3]
  x "A2" [n2]
  x "A3" [n2, n3]
  x "A4" [n1]
  x "A5" [n1, n3]
  x "A6" [n1, n2]
  x "A7" [n1, n2, n3]
  -- A8 is wailing
  x "C5" [n4]
  x "C6" [n3, n4]
  x "C8" [n2, n4]
  x "C9" [n2, n3, n4]
  x "CA" [n1, n4]
  x "CB" [n1, n3, n4]
  x "CC" [n1, n2, n4]
  x "CD" [n1, n2, n3, n4]
  x "CE" [n5]
  x "CF" [n3, n5]
  x "DA" [n2, n5]
  x "DB" [n2, n3, n5]
  x "DC" [n1, n5]
  x "DD" [n1, n3, n5]
  x "DE" [n1, n2, n5]
  x "DF" [n1, n2, n3, n5]
  x "E1" [n4, n5]
  x "E2" [n3, n4, n5]
  x "E3" [n2, n4, n5]
  x "E4" [n2, n3, n4, n5]
  x "E5" [n1, n4, n5]
  x "E6" [n1, n3, n4, n5]
  x "E7" [n1, n2, n4, n5]
  x "E8" [n1, n2, n3, n4, n5]

data DTXFormat = FormatDTX | FormatGDA
  deriving (Eq, Show)

readDTXLines :: DTXFormat -> [(T.Text, T.Text)] -> DTX
readDTXLines fmt lns = DTX
  { dtx_TITLE      = lookup "TITLE" lns
  , dtx_ARTIST     = lookup "ARTIST" lns
  , dtx_PREIMAGE   = T.unpack <$> lookup "PREIMAGE" lns
  , dtx_COMMENT    = lookup "COMMENT" lns
  , dtx_GENRE      = lookup "GENRE" lns
  , dtx_PREVIEW    = T.unpack <$> lookup "PREVIEW" lns
  , dtx_STAGEFILE  = T.unpack <$> lookup "STAGEFILE" lns
  , dtx_DLEVEL     = lookup "DLEVEL" lns >>= readMaybe . T.unpack
  , dtx_GLEVEL     = lookup "GLEVEL" lns >>= readMaybe . T.unpack
  , dtx_BLEVEL     = lookup "BLEVEL" lns >>= readMaybe . T.unpack
  , dtx_DLVDEC     = lookup "DLVDEC" lns >>= readMaybe . T.unpack
  , dtx_GLVDEC     = lookup "GLVDEC" lns >>= readMaybe . T.unpack
  , dtx_BLVDEC     = lookup "BLVDEC" lns >>= readMaybe . T.unpack
  , dtx_WAV        = fmap T.unpack $ HM.fromList $ getReferences "WAV"
  , dtx_VOLUME     = HM.mapMaybe (readMaybe . T.unpack) $ HM.fromList $
    getReferences "VOLUME" ++ getReferences "WAVVOL"
  , dtx_PAN        = HM.mapMaybe (readMaybe . T.unpack) $ HM.fromList $
    getReferences "PAN" ++ getReferences "WAVPAN"
  , dtx_MeasureMap = mmap
  , dtx_TempoMap   = tmap
  , dtx_Drums      = readDrums [Just '1', Nothing]
  , dtx_DrumsDummy = readDrums [Just '3']
  , dtx_Guitar     = readGuitar guitarMapping
  , dtx_GuitarWailing = void $ foldr RTB.merge RTB.empty $ map getChannel ["28", "G8", "GW"]
  , dtx_Bass       = readGuitar bassMapping
  , dtx_BassWailing = void $ getChannel "A8"
  , dtx_BGM        = getChannel "01"
  , dtx_BGMExtra   = HM.fromList $ filter (not . RTB.null . snd) $ do
    chan <- map (T.pack . show) ([61..69] ++ [70..79] ++ [80..89] ++ [90..92] :: [Int])
    return (chan, getChannel chan)
  } where

    getReferences :: T.Text -> [(Chip, T.Text)]
    getReferences typ = flip mapMaybe lns $ \(k, v) -> (, v) <$> T.stripPrefix typ k

    isObjects :: T.Text -> Maybe (BarNumber, Channel)
    isObjects k = if T.length k == 5 && T.all isDigit (T.take 3 k)
      then Just (read $ T.unpack $ T.take 3 k, T.drop 3 k)
      else Nothing
    objects :: Map.Map BarNumber (HM.HashMap Channel [T.Text])
    objects
      = foldr (Map.unionWith $ HM.unionWith (++)) Map.empty
      $ flip mapMaybe lns
      $ \(k, v) -> flip fmap (isObjects k)
        $ \(bar, chan) -> Map.singleton bar $ HM.singleton chan [v]
    maxBar = maybe 0 fst $ Map.lookupMax objects

    mmap
      = U.measureMapFromLengths U.Error
      $ assembleMMap
      $ getBarLengths 4 0
    getBarLengths len n = if n > maxBar
      then []
      else let
        len' = maybe len (* 4) $ do
          obj <- Map.lookup n objects
          lens <- HM.lookup "02" obj <|> HM.lookup "BL" obj -- BL = bar length, in .gda
          listToMaybe lens >>= textFloat
        in len' : getBarLengths len' (n + 1)
    assembleMMap lens = foldr ($) RNil $ zipWith Wait (0 : lens) lens

    getChannel :: Channel -> RTB.T U.Beats Chip
    getChannel chans = foldr RTB.merge RTB.empty $ map (`readBar` chans) [0 .. maxBar]

    readBar :: BarNumber -> Channel -> RTB.T U.Beats Chip
    readBar bar chan = let
      barStart = U.unapplyMeasureMap mmap (bar, 0)
      barEnd = U.unapplyMeasureMap mmap (bar + 1, 0)
      barLen = barEnd - barStart
      placeLine xs = let
        chips = splitChips xs
        subdiv = barLen / fromIntegral (length chips)
        in  RTB.filter (/= "00")
          $ foldr ($) RNil
          $ zipWith Wait (barStart : repeat subdiv) chips
      in foldr RTB.merge RTB.empty
        $ map placeLine
        $ fromMaybe []
        $ Map.lookup bar objects >>= HM.lookup chan

    tmap
      = U.tempoMapFromBPS
      $ fmap (\bpm -> U.BPS $ bpm / 60)
      $ addStartBPM
      $ fmap (+ baseBPM)
      $ RTB.merge bpmHexes bpmRefs
    addStartBPM bpms = case bpms of
      Wait 0 _ _ -> bpms -- already a tempo set at time 0
      _          -> case lookup "BPM" lns >>= textFloat of
        Just n | n /= 0 -> Wait 0 n   bpms
        _               -> Wait 0 120 bpms
    bpmHexes = RTB.mapMaybe textHex $ RTB.merge (getChannel "03") (getChannel "TC") -- TC = tempo change, in .gda
    bpmRefs = RTB.mapMaybe (`HM.lookup` refBPMs) $ getChannel "08"
    refBPMs = HM.mapMaybe textFloat $ HM.fromList $ getReferences "BPM"
    baseBPM = fromMaybe 0 $ lookup "BASEBPM" lns >>= textFloat

    readDrums cols = foldr RTB.merge RTB.empty $ do
      lane <- [minBound .. maxBound]
      col <- cols
      Just chan <- return $ case col of
        Just c  -> Just $ T.pack [c, drumChar lane]
        Nothing -> guard (fmt == FormatGDA) >> drumGDA lane
      return $ fmap (lane,) $ getChannel chan

    readGuitar mapping = foldr RTB.merge RTB.empty $ do
      (chan, frets) <- mapping
      return $ (frets,) <$> getChannel chan

getAudio :: (MonadResource m, MonadIO f, SendMessage f) =>
  Bool -> RTB.T U.Beats Chip -> FilePath -> DTX -> StackTraceT f (AudioSource m Float)
getAudio overlap chips dtxPath dtx = do
  let usedChips = nubOrd $ RTB.getBodies chips
      wavs = HM.filterWithKey (\k _ -> elem k usedChips) $ dtx_WAV dtx
  srcs <- fmap (HM.mapMaybe id) $ forM wavs $ \fp -> do
    fp' <- fixFileCase $ takeDirectory dtxPath </> map (\case '¥' -> '/'; '\\' -> '/'; c -> c) fp
    -- ¥ is the backslash when Shift-JIS decoded
    stackIO (Dir.doesFileExist fp') >>= \case
      False -> do
        warn $ "Couldn't find audio file at: " <> fp'
        return Nothing
      True -> stackIO $ Just <$> case map toLower $ takeExtension fp' of
        ".mp3" -> sourceMpg fp'
        ".xa"  -> mapSamples fractionalSample <$> sourceXA fp'
        ".wav" -> sourceCompressedWAV fp'
        _      -> sourceSnd fp'
  cachedSrcs <- forM srcs $ \src -> if fromIntegral (frames src) < rate src * 5
    then stackIO $ cacheAudio src
    else return src
  let outOf100 n = realToFrac n / 100
      r = 44100
      lookupChip chip = flip fmap (HM.lookup chip cachedSrcs) $ \src -> let
        stereo = applyPansVols
          (case channels src of
            2 -> [-1, 1]
            1 -> [maybe 0 outOf100 $ HM.lookup chip $ dtx_PAN dtx]
            n -> replicate n 0
          )
          (replicate (channels src) $ maybe 0 outOf100 $ HM.lookup chip $ dtx_VOLUME dtx)
          src
        in if rate stereo == r
          then stereo
          else resampleTo r SincMediumQuality stereo
  return
    $ mixMany overlap r 2
    $ U.applyTempoTrack (dtx_TempoMap dtx)
    $ RTB.mapMaybe lookupChip chips

dropBytes :: (MonadResource m) => Integer -> FilePath -> FilePath -> (FilePath -> m a) -> m a
dropBytes n template f cb = tempDir "onyx-dtx-import" $ \dir -> do
  temp <- liftIO $ withBinaryFile f ReadMode $ \h -> do
    let temp = dir </> template
    hSeek h AbsoluteSeek n
    BL.hGetContents h >>= BL.writeFile temp
    return temp
  cb temp

findOggS :: Handle -> IO (Maybe Integer)
findOggS h = do
  hSeek h AbsoluteSeek 0
  bs <- B.hGet h 0x1000
  return $ let
    (x, y) = B.breakSubstring "OggS" bs
    in guard (not $ B.null y) >> Just (fromIntegral $ B.length x)

cacheAudio :: (MonadIO m) => AudioSource m Float -> IO (AudioSource m Float)
cacheAudio src = do
  var <- newEmptyMVar
  return src
    { source = liftIO (tryReadMVar var) >>= \case
      Just v  -> yield v
      Nothing -> do
        xs <- source src .| CL.consume
        let v = V.concat xs
        _ <- liftIO $ tryPutMVar var v
        yield v
    }

sourceCompressedWAV :: (MonadIO f, MonadResource m) => FilePath -> f (AudioSource m Float)
sourceCompressedWAV f = liftIO $ do
  maybeCompressedStart <- withBinaryFile f ReadMode $ \h -> do
    let findChunk magic = hSeek h AbsoluteSeek 12 >> findChunk' magic
        findChunk' magic = do
          thisChunk <- BL.hGet h 4
          size <- BL.hGet h 4 >>= \bs -> case runGetOrFail getWord32le bs of
            Left  (_, _, err) -> liftIO $ ioError $ userError $ "[" <> f <> "] " <> err
            Right (_, _, x  ) -> return x
          if magic == thisChunk
            then return ()
            else do
              hSeek h RelativeSeek $ fromIntegral size
              findChunk' magic
    findChunk "fmt "
    audioType <- BL.hGet h 2
    findChunk "data"
    dataStart <- hTell h
    let oggOrData = do
          firstOggS <- findOggS h
          return $ Just (sourceSnd, "dtx-audio.ogg", fromMaybe dataStart firstOggS)
    case audioType of
      "Og"  -> oggOrData
      "pg"  -> oggOrData
      "U\0" -> return $ Just (sourceMpg, "dtx-audio.mp3", dataStart)
      _     -> return $ Nothing
  case maybeCompressedStart of
    Nothing                       -> sourceSnd f
    Just (getSource, name, inner) -> do
      src <- runResourceT $ dropBytes inner name f $ liftIO . getSource
      return AudioSource
        { frames   = frames   src
        , rate     = rate     src
        , channels = channels src
        , source   = dropBytes inner name f $ \temp -> do
          src' <- liftIO $ getSource temp
          source src'
        }
