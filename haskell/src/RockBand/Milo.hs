{-
Thanks to PyMilo, LibForge, and MiloMod for information on these structures.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module RockBand.Milo where

import qualified Codec.Compression.GZip           as GZ
import qualified Codec.Compression.Zlib.Internal  as Z
import           Control.Monad                    (forM, forM_, guard,
                                                   replicateM)
import           Control.Monad.ST.Lazy
import           Control.Monad.Trans.StackTrace   (logStdout, stackIO)
import           Data.Binary.Get
import           Data.Binary.IEEE754              (getFloat32be)
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (foldl', sort)
import           Data.List.Split                  (keepDelimsR, onSublist,
                                                   split)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust, isNothing)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Data.Word
import           DryVox                           (vocalTubes)
import           Resources                        (CMUPhoneme (..), cmuDict)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Lipsync           (LipsyncTrack (..),
                                                   MagmaViseme (..))
import           RockBand.Codec.Vocal
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U

data MiloCompression
  = MILO_A
  | MILO_B
  | MILO_C
  | MILO_D
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- decompresses zlib stream, but ignores "input ended prematurely" error
zlibTruncate :: BL.ByteString -> BL.ByteString
zlibTruncate bs = runST $ let
  go input = \case
    Z.DecompressInputRequired f              -> case input of
      []     -> f B.empty >>= go []
      x : xs -> f x       >>= go xs
    Z.DecompressOutputAvailable out getNext  -> do
      next <- getNext
      (BL.fromStrict out <>) <$> go input next
    Z.DecompressStreamEnd _unread            -> return BL.empty
    Z.DecompressStreamError Z.TruncatedInput -> return BL.empty
    Z.DecompressStreamError err              ->
      error $ "Milo Zlib decompression error: " <> show err
  in go (BL.toChunks bs) $ Z.decompressST Z.zlibFormat Z.defaultDecompressParams

decompressBlock :: MiloCompression -> BL.ByteString -> BL.ByteString
decompressBlock comp bs = case comp of
  MILO_A -> bs
  MILO_B -> zlibTruncate $ zlib_info <> bs
  MILO_C -> GZ.decompress bs
  MILO_D -> zlibTruncate $ zlib_info <> BL.drop 4 (BL.take (BL.length bs - 1) bs)
  where zlib_info = BL.pack [0x78, 0x9C]

decompressMilo :: Get BL.ByteString
decompressMilo = do
  startingOffset <- bytesRead
  comp <- getWord32le >>= \case
    0xCABEDEAF -> return MILO_A
    0xCBBEDEAF -> return MILO_B
    0xCCBEDEAF -> return MILO_C
    0xCDBEDEAF -> return MILO_D
    n          -> fail $ "Unrecognized .milo compression: " <> show n
  offset <- getWord32le
  blockCount <- getWord32le
  _largestBlock <- getWord32le -- max uncompressed size
  let maxSize = 1 `shiftL` 24
  blockInfo <- replicateM (fromIntegral blockCount) $ do
    size <- getWord32le
    let (compressed, size') = case comp of
          MILO_A -> (False, size)
          MILO_D ->
            ( size .&. maxSize == 0
            , size .&. complement maxSize
            )
          _      -> (True, size)
    return (size', compressed)
  posn <- bytesRead
  skip $ fromIntegral offset - fromIntegral (posn - startingOffset)
  fmap BL.concat $ forM blockInfo $ \(size, compressed) -> do
    bs <- getLazyByteString $ fromIntegral size
    return $ if compressed then decompressBlock comp bs else bs

addMiloHeader :: BL.ByteString -> BL.ByteString
addMiloHeader bs = let
  barrier = [0xAD, 0xDE, 0xAD, 0xDE]
  headerSize = 0x810
  chunks = map (fromIntegral . length) $ filter (not . null) $
    case split (keepDelimsR $ onSublist barrier) $ BL.unpack bs of
      []           -> []
      [c]          -> [c]
      c1 : c2 : cs -> (c1 ++ c2) : cs
  header = runPut $ do
    putWord32le 0xCABEDEAF
    putWord32le headerSize
    putWord32le $ fromIntegral $ length chunks
    putWord32le $ foldl' max 0 chunks
    mapM_ putWord32le chunks
  in BL.concat
    [ header
    , BL.replicate (fromIntegral headerSize - BL.length header) 0
    , bs
    ]

data MagmaLipsync
  = MagmaLipsync1 Lipsync
  | MagmaLipsync2 Lipsync Lipsync
  | MagmaLipsync3 Lipsync Lipsync Lipsync
  deriving (Eq, Show)

magmaMilo :: MagmaLipsync -> BL.ByteString
magmaMilo ml = addMiloHeader $ runPut $ do
  putWord32be 0x1C
  putStringBE "ObjectDir"
  putStringBE "lipsync"
  case ml of
    MagmaLipsync1{} -> do
      putWord32be 4
      putWord32be 0x15
      putWord32be 1
    MagmaLipsync2{} -> do
      putWord32be 6
      putWord32be 0x23
      putWord32be 2
    MagmaLipsync3{} -> do
      putWord32be 8
      putWord32be 0x31
      putWord32be 3
  case ml of
    MagmaLipsync1{} -> return ()
    _ -> do
      putStringBE "CharLipSync"
      putStringBE "part2.lipsync"
  case ml of
    MagmaLipsync3{} -> do
      putStringBE "CharLipSync"
      putStringBE "part3.lipsync"
    _ -> return ()
  putStringBE "CharLipSync"
  putStringBE "song.lipsync"
  putByteString magmaMiloSuffix
  let putThenBarrier x = putLipsync x >> putWord32be 0xADDEADDE
  case ml of
    MagmaLipsync1 h1 -> do
      putThenBarrier h1
    MagmaLipsync2 h1 h2 -> do
      putThenBarrier h2
      putThenBarrier h1
    MagmaLipsync3 h1 h2 h3 -> do
      putThenBarrier h2
      putThenBarrier h3
      putThenBarrier h1

magmaMiloSuffix :: B.ByteString
magmaMiloSuffix = B.pack
  [ 0x00, 0x00, 0x00, 0x1B, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0x3F, 0x35, 0x04, 0xF3, 0xBF, 0x35, 0x04, 0xF3
  , 0x00, 0x00, 0x00, 0x00, 0x3F, 0x13, 0xCD, 0x3A, 0x3F, 0x13, 0xCD, 0x3A, 0xBF, 0x13, 0xCD, 0x3A
  , 0x3E, 0xD1, 0x05, 0xEB, 0x3E, 0xD1, 0x05, 0xEB, 0x3F, 0x51, 0x05, 0xEB, 0xC3, 0xDD, 0xB3, 0xD7
  , 0xC3, 0xDD, 0xB3, 0xD7, 0x43, 0xDD, 0xB3, 0xD7, 0x00, 0x00, 0x00, 0x00, 0xBF, 0x80, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0xC4, 0x40, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0xBF, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x44, 0x40, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xBF, 0x80, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x44, 0x40, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0xBF, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0xC4, 0x40, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0xC4, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xBF, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xBF, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x44, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xAD, 0xDE, 0xAD, 0xDE
  ]

data Lipsync = Lipsync
  { lipsyncVersion    :: Word32 -- 1 from magma v2
  , lipsyncSubversion :: Word32 -- 2 from magma v2
  , lipsyncDTAImport  :: B.ByteString -- empty string from magma v2
  , lipsyncVisemes    :: [B.ByteString]
  , lipsyncKeyframes  :: [Keyframe]
  } deriving (Eq, Show)

newtype Keyframe = Keyframe
  { keyframeEvents :: [VisemeEvent]
  } deriving (Eq, Show)

data VisemeEvent = VisemeEvent
  { visemeIndex  :: Int
  , visemeWeight :: Word8
  } deriving (Eq, Ord, Show)

getStringBE :: Get B.ByteString
getStringBE = do
  len <- getWord32be
  getByteString $ fromIntegral len

putStringBE :: B.ByteString -> Put
putStringBE bs = do
  putWord32be $ fromIntegral $ B.length bs
  putByteString bs

parseLipsync :: Get Lipsync
parseLipsync = do
  lipsyncVersion <- getWord32be
  lipsyncSubversion <- getWord32be
  lipsyncDTAImport <- getStringBE
  dtb <- getWord8
  case dtb of
    0 -> return ()
    _ -> fail "Parsing of Lipsync files with embedded DTB is not currently supported"
  skip 4 -- skips zeroes
  visemeCount <- getWord32be
  lipsyncVisemes <- replicateM (fromIntegral visemeCount) getStringBE
  keyframeCount <- getWord32be
  _followingSize <- getWord32be
  lipsyncKeyframes <- replicateM (fromIntegral keyframeCount) $ do
    eventCount <- getWord8
    keyframeEvents <- replicateM (fromIntegral eventCount) $ do
      visemeIndex <- fromIntegral <$> getWord8
      visemeWeight <- getWord8
      return VisemeEvent{..}
    return Keyframe{..}
  return Lipsync{..}

putLipsync :: Lipsync -> Put
putLipsync lip = do
  putWord32be $ lipsyncVersion lip
  putWord32be $ lipsyncSubversion lip
  putStringBE $ lipsyncDTAImport lip
  putWord8 0
  putWord32be 0
  putWord32be $ fromIntegral $ length $ lipsyncVisemes lip
  mapM_ putStringBE $ lipsyncVisemes lip
  putWord32be $ fromIntegral $ length $ lipsyncKeyframes lip
  let keyframeBS = runPut $ forM_ (lipsyncKeyframes lip) $ \key -> do
        putWord8 $ fromIntegral $ length $ keyframeEvents key
        forM_ (keyframeEvents key) $ \evt -> do
          putWord8 $ fromIntegral $ visemeIndex evt
          putWord8 $ visemeWeight evt
  putWord32be $ fromIntegral $ BL.length keyframeBS
  putLazyByteString keyframeBS
  putWord32be 0

lipsyncToMIDI :: U.TempoMap -> U.MeasureMap -> Lipsync -> RBFile.Song (RBFile.RawFile U.Beats)
lipsyncToMIDI tmap mmap lip = RBFile.Song tmap mmap $ RBFile.RawFile $ (:[])
  $ U.setTrackName "LIPSYNC"
  $ U.unapplyTempoTrack tmap
  $ RTB.flatten
  $ RTB.fromPairList
  $ do
    (dt, key) <- zip (0 : repeat (1/30 :: U.Seconds)) $ lipsyncKeyframes lip
    let evts = do
          vis <- keyframeEvents key
          let str = "[viseme " <> B8.unpack (lipsyncVisemes lip !! visemeIndex vis) <> " " <> show (visemeWeight vis) <> "]"
          return $ E.MetaEvent $ Meta.TextEvent str
    return (dt, evts)

cmuToVisemes :: CMUPhoneme -> [MagmaViseme]
cmuToVisemes = \case
  -- ipa and examples from https://en.wikipedia.org/wiki/ARPABET

  CMU_AA -> [Viseme_Ox_hi, Viseme_Ox_lo] -- ɑ : balm bot
  CMU_AH -> [Viseme_If_hi, Viseme_If_lo] -- ʌ : butt
  CMU_AY -> [Viseme_Ox_hi, Viseme_Ox_lo] -- aɪ : bite
  -- probably should be a diphthong
  CMU_EH -> [Viseme_Cage_hi, Viseme_Cage_lo] -- ɛ : bet
  CMU_ER -> [Viseme_Church_hi, Viseme_Church_lo] -- ɝ : bird
  CMU_EY -> [Viseme_Cage_hi, Viseme_Cage_lo] -- eɪ : bait
  CMU_IH -> [Viseme_If_hi, Viseme_If_lo] -- ɪ : bit
  CMU_IY -> [Viseme_Eat_hi, Viseme_Eat_lo] -- i : beat
  CMU_OW -> [Viseme_Earth_hi, Viseme_Earth_lo] -- oʊ : boat
  CMU_UW -> [Viseme_Wet_hi, Viseme_Wet_lo] -- u : boot
  CMU_AE -> [Viseme_Cage_hi, Viseme_Cage_lo] -- æ : bat
  CMU_AO -> [Viseme_Earth_hi, Viseme_Earth_lo] -- ɔ : story
  CMU_AW -> [Viseme_If_hi, Viseme_If_lo] -- aʊ : bout
  -- probably should be a diphthong
  CMU_OY -> [Viseme_Oat_hi, Viseme_Oat_lo] -- ɔɪ : boy
  -- probably should be a diphthong
  CMU_UH -> [Viseme_Though_hi, Viseme_Though_lo] -- ʊ : book

  _      -> [] -- probably shouldn't happen

englishVowels :: RTB.T t (Maybe T.Text) -> RTB.T t (Maybe CMUPhoneme)
englishVowels = let
  splitFirstWord evts = let
    (x, y) = flip span evts $ \case
      (_, Nothing   ) -> True
      (_, Just lyric) -> elem
        (T.takeEnd 1 $ T.dropWhileEnd (`elem` ['$', '#', '^']) lyric)
        ["-", "="]
    in (x <> take 1 y, drop 1 y)
  go [] = []
  go evts = case splitFirstWord evts of
    (wordEvents, rest) -> let
      numSyllables = length [ () | (_, Just _) <- wordEvents ]
      isVowel phone = elem phone
        [ CMU_AA, CMU_AE, CMU_AH, CMU_AO, CMU_AW
        , CMU_AY, CMU_EH, CMU_ER, CMU_EY, CMU_IH
        , CMU_IY, CMU_OW, CMU_OY, CMU_UH, CMU_UW
        ]
      filterLyric = maybe ""
        $ T.map (\case '=' -> '-'; c -> c)
        . T.filter (`notElem` ("-#^$!?" :: String))
      word = B8.pack $ T.unpack $ T.toUpper $ T.concat $ map (filterLyric . snd) wordEvents
      phones = case filter ((== numSyllables) . length) $ map (filter isVowel) $ fromMaybe [] $ HM.lookup word cmuDict of
        match : _ -> applyPhonemes match wordEvents
        []        -> guessPhonemes wordEvents
      in phones ++ go rest
  applyPhonemes phones           ((t, Nothing) : events) = (t, Nothing    ) : applyPhonemes phones events
  applyPhonemes (phone : phones) ((t, Just _ ) : events) = (t, Just phone ) : applyPhonemes phones events
  applyPhonemes _                []                      = []
  applyPhonemes []               ((t, Just _ ) : events) = (t, Just CMU_AH) : applyPhonemes []     events -- shouldn't happen
  guessPhonemes = map $ \case
    (t, Nothing) -> (t, Nothing)
    (t, Just _lyric) -> let
      phone = CMU_AH -- TODO
      in (t, Just phone)
  in RTB.fromPairList . go . RTB.toPairList

redundantZero :: (Eq a) => [[(a, Word8)]] -> [[(a, Word8)]]
redundantZero []               = []
redundantZero [x]              = [x, x]
redundantZero (x : xs@(y : _)) = x : case [ vis | (vis, 0) <- x, isNothing $ lookup vis y ] of
  []      -> redundantZero xs
  visemes -> case redundantZero xs of
    []      -> [] -- shouldn't happen
    y' : ys -> (map (, 0) visemes ++ y') : ys

autoLipsync :: VocalTrack U.Seconds -> Lipsync
autoLipsync vt = let
  fullWeight = 140
  weightDelta = 35
  ahs cur next = do
    vis <- Set.toList $ Map.keysSet cur <> Map.keysSet next
    return (vis, fromMaybe 0 $ Map.lookup vis next)
  makeKeyframes cur goal rest = let
    next = Map.fromList $ filter (\(_, n) -> n /= 0) $ do
      vis <- Map.keys cur <> Map.keys goal
      return (vis, crawlFrame (Map.lookup vis cur) (Map.lookup vis goal))
    crawlFrame mx my = case compare x y of
      EQ -> x
      LT -> if x + weightDelta < x then y else min y $ x + weightDelta
      GT -> if x - weightDelta > x then y else max y $ x - weightDelta
      where x = fromMaybe 0 mx
            y = fromMaybe 0 my
    in ahs cur next : if RTB.null rest
      then if cur == next then [] else makeKeyframes next goal RTB.empty
      else let
        (frame, after) = U.trackSplit (1/30 :: U.Seconds) rest
        goal' = case RTB.viewR frame of
          Just (_, (_, phone)) -> Map.fromList $ map (, fullWeight) $ maybe [] cmuToVisemes phone
          Nothing              -> goal
        in makeKeyframes next goal' after
  in Lipsync
    { lipsyncVersion    = 1
    , lipsyncSubversion = 2
    , lipsyncDTAImport  = B.empty
    , lipsyncVisemes    = map (B8.pack . drop 7 . show) [minBound :: MagmaViseme .. maxBound]
    , lipsyncKeyframes
      = map (Keyframe . sort . map ((\(vis, n) -> VisemeEvent (fromEnum vis) n)))
      $ redundantZero
      $ drop 2
      $ makeKeyframes Map.empty Map.empty
      $ englishVowels
      $ vocalTubes vt
    }

autoLipsyncAh :: VocalTrack U.Seconds -> Lipsync
autoLipsyncAh vt = let
  ah n = [(Viseme_Ox_hi, n), (Viseme_Ox_lo, n)]
  makeKeyframes cur goal rest = let
    next = case compare cur goal of
      EQ -> cur
      LT -> if cur + 20 < cur then goal else min goal $ cur + 20
      GT -> if cur - 20 > cur then goal else max goal $ cur - 20
    in ah next : if RTB.null rest
      then if cur == next then [] else makeKeyframes next goal RTB.empty
      else let
        (frame, after) = U.trackSplit (1/30 :: U.Seconds) rest
        goal' = case RTB.viewR frame of
          Just (_, (_, bool)) -> if bool then 100 else 0
          Nothing             -> goal
        in makeKeyframes next goal' after
  in Lipsync
    { lipsyncVersion    = 1
    , lipsyncSubversion = 2
    , lipsyncDTAImport  = B.empty
    , lipsyncVisemes    = map (B8.pack . drop 7 . show) [minBound :: MagmaViseme .. maxBound]
    , lipsyncKeyframes
      = drop 1
      $ map (Keyframe . map ((\(vis, n) -> VisemeEvent (fromEnum vis) n)))
      $ makeKeyframes 0 0
      $ fmap isJust
      $ vocalTubes vt
    }

lipsyncFromMidi :: LipsyncTrack U.Seconds -> Lipsync
lipsyncFromMidi _ = let
  -- TODO
  in Lipsync
    { lipsyncVersion    = 1
    , lipsyncSubversion = 2
    , lipsyncDTAImport  = B.empty
    , lipsyncVisemes    = map (B8.pack . drop 7 . show) [minBound :: MagmaViseme .. maxBound]
    , lipsyncKeyframes  = undefined
    }

data Venue = Venue
  { venueVersion    :: Word32
  , venueSubversion :: Word32
  , venueDTAImport  :: B.ByteString
  , venueMystery    :: B.ByteString
  , venueTracks     :: [Track]
  } deriving (Eq, Show)

data Track = Track
  { trackVersion    :: Word32
  , trackSubversion :: Word32
  , trackDomain     :: B.ByteString
  , trackMystery    :: B.ByteString
  , trackName       :: B.ByteString
  , trackMystery2   :: Word32
  , trackName2      :: B.ByteString
  , trackMystery3   :: B.ByteString
  , trackEvents     :: ATB.T U.Seconds B.ByteString
  } deriving (Eq, Show)

data VenueEvent = VenueEvent
  { venueEvent :: B.ByteString
  , venueTime  :: U.Seconds
  } deriving (Eq, Show)

parseVenue :: Get Venue
parseVenue = do
  venueVersion <- getWord32be -- 0xD
  venueSubversion <- getWord32be -- 0x2
  venueDTAImport <- getStringBE -- "song_anim"
  venueMystery <- getByteString 17
    {-
      00
      00 00 00 00
      00 00 00 04
      46 6D F5 79 -- probably end timestamp
      00 00 00 01
    -}
  trackCount <- getWord32be
  venueTracks <- replicateM (fromIntegral trackCount) $ do
    trackVersion <- getWord32be -- usually 6, 2 in postproc track
    trackSubversion <- getWord32be -- usually 6, 2 in postproc track
    trackDomain <- getStringBE -- "BandDirector"
    trackMystery <- getByteString 11 -- 01 00 01 00 00 00 00 00 00 00 05
    trackName <- getStringBE -- like "bass_intensity"
    trackMystery2 <- getWord32be
    trackName2 <- getStringBE -- like "lightpreset_interp" but usually ""
    trackMystery3 <- getByteString 5
    eventCount <- getWord32be
    trackEvents <- fmap ATB.fromPairList $ replicateM (fromIntegral eventCount) $ do
      event <- getStringBE
      -- see "postproc" track where each event has 4 extra bytes of 0
      event' <- if B.null event then getStringBE else return event
      frames <- getFloat32be
      return (realToFrac $ frames / 30, event')
    return Track{..}
  return Venue{..}

venueToMIDI :: U.TempoMap -> U.MeasureMap -> Venue -> RBFile.Song (RBFile.RawFile U.Beats)
venueToMIDI tmap mmap venue = RBFile.Song tmap mmap $ RBFile.RawFile $ do
  trk <- venueTracks venue
  return
    $ U.setTrackName (B8.unpack $ trackName trk)
    $ U.unapplyTempoTrack tmap
    $ RTB.fromAbsoluteEventList
    $ fmap (E.MetaEvent . Meta.TextEvent . B8.unpack)
    $ trackEvents trk

testConvertVenue :: FilePath -> FilePath -> FilePath -> IO ()
testConvertVenue fmid fven fout = do
  res <- logStdout $ stackIO (Load.fromFile fmid) >>= RBFile.readMIDIFile'
  mid <- case res of
    Left err  -> error $ show err
    Right mid -> return mid
  ven <- fmap (runGet parseVenue) $ BL.readFile fven
  let raw = venueToMIDI (RBFile.s_tempos mid) (RBFile.s_signatures mid) ven `asTypeOf` mid
  Save.toFile fout $ RBFile.showMIDIFile' raw

testConvertLipsync :: FilePath -> FilePath -> FilePath -> IO ()
testConvertLipsync fmid fvoc fout = do
  res <- logStdout $ stackIO (Load.fromFile fmid) >>= RBFile.readMIDIFile'
  mid <- case res of
    Left err  -> error $ show err
    Right mid -> return mid
  voc <- fmap (runGet parseLipsync) $ BL.readFile fvoc
  let raw = lipsyncToMIDI (RBFile.s_tempos mid) (RBFile.s_signatures mid) voc `asTypeOf` mid
  Save.toFile fout $ RBFile.showMIDIFile' raw

-- | Does simple matching of the different RB3 milo content sets.
recognizeMilo :: B.ByteString -> Maybe [(FilePath, B.ByteString)]
recognizeMilo bs = let
  breakPiece b = case B.breakSubstring (B.pack [0xAD, 0xDE, 0xAD, 0xDE]) b of
    (x, y) -> if B.null y
      then [b]
      else x : breakPiece (B.drop 4 y)
  pieces = breakPiece bs
  filesInHeader header = let
    indexOfString s = case B.breakSubstring s header of
      (x, y) -> do
        guard $ not $ B.null y
        Just $ B.length x
    knownFiles = ["song.lipsync", "part2.lipsync", "part3.lipsync", "part4.lipsync", "song.anim"]
    in map snd $ sort [ (i, B8.unpack f) | f <- knownFiles, Just i <- [indexOfString f] ]
  in case pieces of
    [] -> Nothing
    header : rest -> let
      files = filesInHeader header
      nonzero = filter (B.any (/= 0)) rest
      in do
        guard $ length files == length nonzero
        realOrder <- case files of
          "song.anim" : restFiles -> Just $ restFiles ++ ["song.anim"]
          _ -> do
            guard $ "song.anim" `notElem` files
            Just files
        Just $ zip realOrder nonzero
