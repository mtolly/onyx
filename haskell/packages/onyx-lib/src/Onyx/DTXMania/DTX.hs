{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.DTXMania.DTX where

import           Control.Applicative              ((<|>))
import           Control.Arrow                    (first, second)
import           Control.Monad                    (forM_, guard, void)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Resource     (MonadResource, runResourceT)
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Binary.Get
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isAlphaNum, isDigit, toLower)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.Mpg123        (sourceMpg)
import           Data.Conduit.Audio.Sndfile       (sourceSnd)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Pico)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (groupOn, nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Ratio                       (denominator)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Data.Tuple                       (swap)
import           Numeric
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DTXMania.XA                 (sourceXA)
import           Onyx.MIDI.Common                 (pattern RNil, pattern Wait)
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.Drums.Full       as FD
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.StackTrace
import           Onyx.Util.Files                  (fixFileCase)
import           Onyx.Util.ShiftJIS               (decodeShiftJIS)
import           Onyx.Util.Text.Decode            (decodeWithDefault)
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath
import           System.IO
import           Text.Read                        (readMaybe)

loadDTXLines' :: Bool -> FilePath -> IO [(T.Text, T.Text)]
loadDTXLines' semicolonComments f = do
  -- Shift-JIS is the usual encoding, but I've seen UTF-16 (with BOM) in the wild
  lns <- T.lines . decodeWithDefault (T.pack . decodeShiftJIS) <$> B.readFile f
  return $ flip mapMaybe lns $ \ln -> case T.uncons ln of
    Just ('#', rest) -> case T.span isAlphaNum rest of
      (x, y) -> Just
        ( T.strip x
        , T.strip
          $ (if semicolonComments then T.takeWhile (/= ';') else id)
          $ T.dropWhile (== ':') y
        )
    _ -> Nothing

-- don't think bms supports semicolon comment in the middle of a line like dtx.
-- see Options_semicolon.bms with "#TITLE Options [;]"

loadDTXLines, loadBMSLines :: FilePath -> IO [(T.Text, T.Text)]
loadDTXLines = loadDTXLines' True
loadBMSLines = loadDTXLines' False

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
  , dtx_AVI           :: HM.HashMap Chip FilePath
  , dtx_MeasureMap    :: U.MeasureMap
  , dtx_TempoMap      :: U.TempoMap
  , dtx_Drums         :: RTB.T U.Beats (DrumLane, Chip)
  , dtx_DrumsDummy    :: RTB.T U.Beats (DrumLane, Chip)
  , dtx_Guitar        :: RTB.T U.Beats ([Five.Color], Chip)
  , dtx_GuitarWailing :: RTB.T U.Beats ()
  , dtx_GuitarLong    :: RTB.T U.Beats ()
  , dtx_Bass          :: RTB.T U.Beats ([Five.Color], Chip)
  , dtx_BassWailing   :: RTB.T U.Beats ()
  , dtx_BassLong      :: RTB.T U.Beats ()
  , dtx_BGM           :: RTB.T U.Beats Chip
  , dtx_BGMExtra      :: HM.HashMap Channel (RTB.T U.Beats Chip)
  , dtx_Video         :: RTB.T U.Beats Chip
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
  deriving (Eq, Ord, Show, Enum, Bounded)

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

guitarMapping :: DTXFormat -> [(Channel, [Five.Color])]
guitarMapping fmt = execWriter $ do
  let x chan cols = tell [(chan, cols)]
      n1 = Five.Green  -- red in GF
      n2 = Five.Red    -- green in GF
      n3 = Five.Yellow -- blue in GF
      n4 = Five.Blue   -- yellow in GF
      n5 = Five.Orange -- purple in GF
      lowFretChars = case fmt of
        FormatDTX -> ['2']
        FormatGDA -> ['2', 'G']
  forM_ lowFretChars $ \c -> do
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

getReferences :: T.Text -> [(T.Text, T.Text)] -> [(Chip, T.Text)]
getReferences typ = mapMaybe $ \(k, v) -> do
  k' <- T.stripPrefix typ k
  guard $ T.length k' == 2
  return (k', v)

getObjects :: [(T.Text, T.Text)] -> Map.Map BarNumber (HM.HashMap Channel [T.Text])
getObjects lns = let
  isObjects :: T.Text -> Maybe (BarNumber, Channel)
  isObjects k = if T.length k == 5 && T.all isDigit (T.take 3 k)
    then Just (read $ T.unpack $ T.take 3 k, T.drop 3 k)
    else Nothing
  in foldr (Map.unionWith $ HM.unionWith (++)) Map.empty
    $ flip mapMaybe lns
    $ \(k, v) -> flip fmap (isObjects k)
      $ \(bar, chan) -> Map.singleton bar $ HM.singleton chan [v]

getChannelGeneral
  :: Channel
  -> Map.Map BarNumber (HM.HashMap Channel [T.Text])
  -> U.MeasureMap
  -> RTB.T U.Beats Chip
getChannelGeneral chan objects mmap = let
  maxBar = maybe 0 fst $ Map.lookupMax objects
  readBar :: BarNumber -> RTB.T U.Beats Chip
  readBar bar = let
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
  in foldr RTB.merge RTB.empty $ map readBar [0 .. maxBar]

-- DTXMania appears to "jump" very slightly for each BPM change,
-- so to fix songs with many BPM changes, we need to increase BPMs
-- slightly on import and decrease them on export.
hackFixTempoMap :: Bool -> RTB.T U.Beats U.BPS -> RTB.T U.Beats U.BPS
hackFixTempoMap isImport = \case
  Wait b1 bps1 rest@(Wait b2 _ _) -> let
    hackAdjust :: U.Seconds
    hackAdjust = 0.000582978
    nominalSeconds = U.applyTempo bps1 b2
    fixedSeconds = if isImport
      then nominalSeconds NNC.-| hackAdjust
      else nominalSeconds +      hackAdjust
    newBPS = if fixedSeconds == 0
      then bps1
      else U.makeTempo b2 fixedSeconds
    in Wait b1 newBPS $ hackFixTempoMap isImport rest
  rest -> rest

readDTXLines :: DTXFormat -> [(T.Text, T.Text)] -> DTX
readDTXLines fmt lns = DTX
  { dtx_TITLE      = lookup "TITLE" lns
  , dtx_ARTIST     = lookup "ARTIST" lns
  , dtx_PREIMAGE   = fixPath . T.unpack <$> lookup "PREIMAGE" lns
  , dtx_COMMENT    = lookup "COMMENT" lns
  , dtx_GENRE      = lookup "GENRE" lns
  , dtx_PREVIEW    = fixPath . T.unpack <$> lookup "PREVIEW" lns
  , dtx_STAGEFILE  = fixPath . T.unpack <$> lookup "STAGEFILE" lns
  , dtx_DLEVEL     = lookup "DLEVEL" lns >>= readMaybe . T.unpack
  , dtx_GLEVEL     = lookup "GLEVEL" lns >>= readMaybe . T.unpack
  , dtx_BLEVEL     = lookup "BLEVEL" lns >>= readMaybe . T.unpack
  , dtx_DLVDEC     = lookup "DLVDEC" lns >>= readMaybe . T.unpack
  , dtx_GLVDEC     = lookup "GLVDEC" lns >>= readMaybe . T.unpack
  , dtx_BLVDEC     = lookup "BLVDEC" lns >>= readMaybe . T.unpack
  , dtx_WAV        = fmap (fixPath . T.unpack) $ HM.fromList $ getReferences "WAV" lns
  , dtx_AVI        = fmap (fixPath . T.unpack) $ HM.fromList $ getReferences "AVI" lns <> getReferences "VIDEO" lns
  , dtx_VOLUME     = HM.mapMaybe (readMaybe . T.unpack) $ HM.fromList $
    getReferences "VOLUME" lns ++ getReferences "WAVVOL" lns
  , dtx_PAN        = HM.mapMaybe (readMaybe . T.unpack) $ HM.fromList $
    getReferences "PAN" lns ++ getReferences "WAVPAN" lns
  , dtx_MeasureMap = mmap
  , dtx_TempoMap   = tmap
  , dtx_Drums      = readDrums [Just '1', Nothing]
  , dtx_DrumsDummy = readDrums [Just '3']
  , dtx_Guitar     = readGuitar $ guitarMapping fmt
  , dtx_GuitarWailing = void $ foldr RTB.merge RTB.empty $ map getChannel ["28", "G8", "GW"]
  , dtx_GuitarLong = void $ getChannel "2C"
  , dtx_Bass       = readGuitar bassMapping
  , dtx_BassWailing = void $ getChannel "A8"
  , dtx_BassLong   = void $ getChannel "2D"
  , dtx_BGM        = getChannel "01"
  , dtx_BGMExtra   = HM.fromList $ filter (not . RTB.null . snd) $ do
    chan <- map (T.pack . show) ([61..69] ++ [70..79] ++ [80..89] ++ [90..92] :: [Int])
    return (chan, getChannel chan)
  , dtx_Video      = getChannel "54"
  } where

    -- ¥ is the backslash when Shift-JIS decoded
    fixPath = map $ \case '¥' -> '/'; '\\' -> '/'; c -> c

    objects :: Map.Map BarNumber (HM.HashMap Channel [T.Text])
    objects = getObjects lns

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
    getChannel chan = getChannelGeneral chan objects mmap

    tmap
      = U.tempoMapFromBPS
      $ hackFixTempoMap True
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
    refBPMs = HM.mapMaybe textFloat $ HM.fromList $ getReferences "BPM" lns
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

dtxAudioSource :: (MonadResource m, MonadIO f, SendMessage f) =>
  FilePath -> StackTraceT f (Maybe (AudioSource m Float))
dtxAudioSource fp' = do
  fp <- fixFileCase fp'
  stackIO (Dir.doesFileExist fp) >>= \case
    True -> stackIO $ Just <$> case map toLower $ takeExtension fp of
      ".mp3" -> sourceMpg fp
      ".xa"  -> mapSamples fractionalSample <$> sourceXA fp
      ".wav" -> sourceCompressedWAV fp
      _      -> sourceSnd fp
    False -> do
      tryOgg <- case map toLower $ takeExtension fp of
        ".ogg" -> return Nothing
        _      -> dtxAudioSource $ fp -<.> "ogg"
      case tryOgg of
        Nothing -> warn $ "Couldn't find audio file at: " <> fp
        Just _  -> return ()
      return tryOgg

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

makeDTX :: DTX -> T.Text
makeDTX DTX{..} = T.unlines $ execWriter $ do

  let gap = tell [""]
      pair k v = tell ["#" <> k <> ": " <> v]
      opt k = mapM_ $ pair k
      bar n channel = pair $ T.takeEnd 3 ("000" <> T.pack (show n)) <> channel

  tell ["; Created by Onyx"] -- TODO add version
  gap

  opt "TITLE" dtx_TITLE
  opt "ARTIST" dtx_ARTIST
  opt "PREIMAGE" $ T.pack <$> dtx_PREIMAGE
  opt "COMMENT" dtx_COMMENT
  opt "GENRE" dtx_GENRE
  opt "PREVIEW" $ T.pack <$> dtx_PREVIEW
  opt "STAGEFILE" $ T.pack <$> dtx_STAGEFILE
  gap

  opt "DLEVEL" $ T.pack . show <$> dtx_DLEVEL
  opt "GLEVEL" $ T.pack . show <$> dtx_GLEVEL
  opt "BLEVEL" $ T.pack . show <$> dtx_BLEVEL
  opt "DLVDEC" $ T.pack . show <$> dtx_DLVDEC
  opt "GLVDEC" $ T.pack . show <$> dtx_GLVDEC
  opt "BLVDEC" $ T.pack . show <$> dtx_BLVDEC
  gap

  let allWAVs = Set.toList $ Set.fromList $ HM.keys dtx_WAV <> HM.keys dtx_VOLUME <> HM.keys dtx_PAN
  forM_ allWAVs $ \chip -> do
    opt ("WAV" <> chip) $ T.pack <$> HM.lookup chip dtx_WAV
    opt ("VOLUME" <> chip) $ T.pack . show <$> HM.lookup chip dtx_VOLUME
    opt ("PAN" <> chip) $ T.pack . show <$> HM.lookup chip dtx_PAN
  gap

  forM_ (HM.toList dtx_AVI) $ \(chip, f) -> pair ("AVI" <> chip) (T.pack f)
  gap

  let showDecimal r = T.dropWhileEnd (== '.') $ T.dropWhileEnd (== '0') $ T.pack $ show (realToFrac r :: Pico)

  tell ["; Time signatures"]
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 $ U.measureMapToLengths dtx_MeasureMap) $ \(sigPosn, len) -> let
    (sigBar, _sigBeats) = U.applyMeasureMap dtx_MeasureMap sigPosn -- _sigBeats should be 0
    fraction44 = len / 4
    in bar sigBar "02" $ showDecimal fraction44
  gap

  let alphaNumChips = do
        let chars = ['0' .. '9'] <> ['A' .. 'Z']
        x <- chars
        y <- chars
        guard $ x /= '0' || y /= '0'
        return $ T.pack [x, y]
      fixedTempoBPS = hackFixTempoMap False $ U.tempoMapToBPS dtx_TempoMap
      tempoRefs = zip alphaNumChips $ Set.toList $ Set.fromList $ RTB.getBodies fixedTempoBPS
      tempoLookup = Map.fromList $ map swap tempoRefs
  forM_ tempoRefs $ \(chip, bps) -> pair ("BPM" <> chip) $ showDecimal $ bps * 60
  gap

  let eachBar rtb f = let
        barGroups
          = groupOn (\((msr, _), _) -> msr)
          $ map (first $ U.applyMeasureMap dtx_MeasureMap)
          $ ATB.toPairList
          $ RTB.toAbsoluteEventList 0 rtb
        in forM_ barGroups $ \barGroup -> case barGroup of
          [] -> return () -- shouldn't happen
          ((i, _), _) : _ -> let
            barLength = U.unapplyMeasureMap dtx_MeasureMap (i + 1, 0) - U.unapplyMeasureMap dtx_MeasureMap (i, 0)
            barOffsetToFraction :: U.Beats -> Rational
            barOffsetToFraction bts = realToFrac $ bts / barLength
            in f i $ map (\((_, bts), x) -> (barOffsetToFraction bts, x)) barGroup
      makeBar :: [(Rational, Chip)] -> T.Text
      makeBar chips = let
        denom = foldr lcm 1 $ map (denominator . fst) chips
        chips' = map (first $ round . (* realToFrac denom)) chips
        in T.concat $ do
          num <- [0 .. denom - 1]
          return $ fromMaybe "00" $ lookup num chips'

  tell ["; BPM changes"]
  eachBar fixedTempoBPS $ \barNumber changes -> do
    bar barNumber "08" $ makeBar $ map (second $ \bps -> fromMaybe "ZZ" $ Map.lookup bps tempoLookup) changes
  gap

  let splitEvents events = do
        group <- nubOrd $ map (\(_, (g, _)) -> g) events
        return (group, mapMaybe (\(t, (g, x)) -> guard (g == group) >> Just (t, x)) events)

  eachBar dtx_Drums $ \barNumber chips -> do
    tell ["; Drums bar " <> T.pack (show barNumber)]
    forM_ (splitEvents chips) $ \(lane, laneChips) -> do
      bar barNumber (T.pack ['1', drumChar lane]) $ makeBar laneChips
    gap

  eachBar dtx_DrumsDummy $ \barNumber chips -> do
    tell ["; Drums (dummy) bar " <> T.pack (show barNumber)]
    forM_ (splitEvents chips) $ \(lane, laneChips) -> do
      bar barNumber (T.pack ['3', drumChar lane]) $ makeBar laneChips
    gap

  -- TODO maybe ensure sort before lookup
  let guitarTable = Map.fromList $ map swap $ guitarMapping FormatDTX
  eachBar dtx_Guitar $ \barNumber chips -> do
    tell ["; Guitar bar " <> T.pack (show barNumber)]
    forM_ (splitEvents chips) $ \(colors, colorsChips) -> do
      bar barNumber (fromMaybe "ZZ" $ Map.lookup colors guitarTable) $ makeBar colorsChips
    gap

  eachBar dtx_GuitarWailing $ \barNumber wails -> do
    tell ["; Guitar wailing bar " <> T.pack (show barNumber)]
    bar barNumber "28" $ makeBar $ map (\(t, _) -> (t, "01")) wails
    gap

  eachBar dtx_GuitarLong $ \barNumber longs -> do
    tell ["; Guitar long bar " <> T.pack (show barNumber)]
    bar barNumber "2C" $ makeBar $ map (\(t, _) -> (t, "01")) longs
    gap

  -- TODO maybe ensure sort before lookup
  let bassTable = Map.fromList $ map swap bassMapping
  eachBar dtx_Bass $ \barNumber chips -> do
    tell ["; Bass bar " <> T.pack (show barNumber)]
    forM_ (splitEvents chips) $ \(colors, colorsChips) -> do
      bar barNumber (fromMaybe "ZZ" $ Map.lookup colors bassTable) $ makeBar colorsChips
    gap

  eachBar dtx_BassWailing $ \barNumber wails -> do
    tell ["; Bass wailing bar " <> T.pack (show barNumber)]
    bar barNumber "A8" $ makeBar $ map (\(t, _) -> (t, "01")) wails
    gap

  eachBar dtx_BassLong $ \barNumber longs -> do
    tell ["; Bass long bar " <> T.pack (show barNumber)]
    bar barNumber "2D" $ makeBar $ map (\(t, _) -> (t, "01")) longs
    gap

  tell ["; BGM"]
  eachBar dtx_BGM $ \barNumber chips -> do
    bar barNumber "01" $ makeBar chips
  gap

  forM_ (HM.toList dtx_BGMExtra) $ \(chan, extra) -> do
    tell ["; BGM extra channel " <> chan]
    eachBar extra $ \barNumber chips -> do
      bar barNumber chan $ makeBar chips
    gap

  eachBar dtx_Video $ \barNumber chips -> do
    tell ["; Video bar " <> T.pack (show barNumber)]
    bar barNumber "54" $ makeBar chips
  gap

-- System for hooking up a template (e.g. from APPROVED) to auto-keysound Full Drums

data DTXMapping = DTXMapping FilePath [DTXCondition] [DTXOverride]
  deriving (Read)

data DTXCondition
  = MatchNote     FD.FullGem     DTXCondition
  | MatchType     FD.FullGemType DTXCondition
  | MatchVelocity D.DrumVelocity DTXCondition
  | Chip          Chip
  | Branch        [DTXCondition]
  deriving (Read)

data DTXOverride = DTXOverride T.Text [DTXCondition]
  deriving (Read)

lookupDTXMapping :: [DTXCondition] -> FD.FullDrumNote -> Maybe Chip
lookupDTXMapping conds fdn = go $ Branch conds where
  go (Chip chip)           = Just chip
  go (MatchNote     n sub) = if n == FD.fdn_gem      fdn then go sub else Nothing
  go (MatchType     t sub) = if t == FD.fdn_type     fdn then go sub else Nothing
  go (MatchVelocity v sub) = if v == FD.fdn_velocity fdn then go sub else Nothing
  go (Branch subs)         = listToMaybe $ mapMaybe go subs
