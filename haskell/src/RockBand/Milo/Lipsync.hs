{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module RockBand.Milo.Lipsync where

import           Control.Arrow                    (first)
import           Control.Monad                    (forM, forM_, guard,
                                                   replicateM, void)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace   (StackTraceT, fatal, inside,
                                                   logStdout, stackIO)
import           Data.Aeson                       ((.:), (.:?))
import qualified Data.Aeson                       as A
import qualified Data.Aeson.Types                 as A
import           Data.Binary.Codec.Class
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isAlpha)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (foldl', nubOrd, sort, (\\))
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isNothing,
                                                   listToMaybe, mapMaybe)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Data.Vector                      as V
import qualified Data.Yaml                        as Y
import           DryVox                           (vocalTubes)
import           Guitars                          (applyStatus1)
import qualified Numeric.NonNegative.Class        as NNC
import           Resources                        (CMUConsonant (..),
                                                   CMUPhoneme (..),
                                                   CMUVowel (..), cmuDict,
                                                   getResourcesPath)
import           RockBand.Codec                   (mapTrack)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Lipsync           (GH2Viseme (..),
                                                   LipsyncTrack (..),
                                                   LyricLanguage (..),
                                                   VisemeEvent (..),
                                                   VisemeGraph (..))
import           RockBand.Codec.Vocal
import           RockBand.Common                  (pattern RNil, pattern Wait,
                                                   noRedundantStatus)
import           RockBand.Milo.Compression
import           RockBand.Milo.Dir
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension)
import           Text.Read                        (readMaybe)

data MagmaLipsync
  = MagmaLipsync1 Lipsync
  | MagmaLipsync2 Lipsync Lipsync
  | MagmaLipsync3 Lipsync Lipsync Lipsync
  deriving (Eq, Show)

magmaMiloDir :: MagmaLipsync -> MiloDir
magmaMiloDir ml = MiloDir
  { miloVersion = 28
  , miloType = "ObjectDir"
  , miloName = "lipsync"
  , miloU1 = case ml of
    MagmaLipsync1{} -> 4
    MagmaLipsync2{} -> 6
    MagmaLipsync3{} -> 8
  , miloU2 = case ml of
    MagmaLipsync1{} -> 0x15
    MagmaLipsync2{} -> 0x23
    MagmaLipsync3{} -> 0x31
  , miloEntryNames = concat
    [ case ml of
      MagmaLipsync1{} -> []
      _               -> [("CharLipSync", "part2.lipsync")]
    , case ml of
      MagmaLipsync3{} -> [("CharLipSync", "part3.lipsync")]
      _               -> []
    , [("CharLipSync", "song.lipsync")]
    ]
  , miloU3 = 0x1B
  , miloU4 = Just 2
  , miloSubname = Just ""
  , miloU5 = Just 0
  , miloU6 = Just 0
  , miloMatrices = let
    float a b c d = runGet getFloatbe $ BL.pack [a, b, c, d]
    in
      [ [ float 0x3F 0x35 0x04 0xF3, float 0xBF 0x35 0x04 0xF3, float 0x00 0x00 0x00 0x00
        , float 0x3F 0x13 0xCD 0x3A, float 0x3F 0x13 0xCD 0x3A, float 0xBF 0x13 0xCD 0x3A
        , float 0x3E 0xD1 0x05 0xEB, float 0x3E 0xD1 0x05 0xEB, float 0x3F 0x51 0x05 0xEB
        , float 0xC3 0xDD 0xB3 0xD7, float 0xC3 0xDD 0xB3 0xD7, float 0x43 0xDD 0xB3 0xD7
        ]
      , [ float 0x00 0x00 0x00 0x00, float 0xBF 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x3F 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x3F 0x80 0x00 0x00
        , float 0xC4 0x40 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00
        ]
      , [ float 0x00 0x00 0x00 0x00, float 0x3F 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0xBF 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x3F 0x80 0x00 0x00
        , float 0x44 0x40 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00
        ]
      , [ float 0x3F 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0xBF 0x80 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x3F 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x44 0x40 0x00 0x00
        ]
      , [ float 0x3F 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x3F 0x80 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0xBF 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0xC4 0x40 0x00 0x00
        ]
      , [ float 0x3F 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x3F 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x3F 0x80 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0xC4 0x40 0x00 0x00, float 0x00 0x00 0x00 0x00
        ]
      , [ float 0xBF 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0xBF 0x80 0x00 0x00, float 0x00 0x00 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x00 0x00 0x00 0x00, float 0x3F 0x80 0x00 0x00
        , float 0x00 0x00 0x00 0x00, float 0x44 0x40 0x00 0x00, float 0x00 0x00 0x00 0x00
        ]
      ]
  , miloU7 = 0
  , miloU8 = 1
  , miloU9 = 0
  , miloParents = []
  , miloU10 = 0
  , miloChildren = []
  , miloU11 = Nothing
  , miloSubdirs = []
  , miloUnknownBytes = BL.replicate 13 0
  , miloFiles = map (runPut . putLipsync) $ case ml of
    MagmaLipsync1 h1       -> [h1]
    MagmaLipsync2 h1 h2    -> [h2, h1]
    MagmaLipsync3 h1 h2 h3 -> [h2, h3, h1]
  }

magmaMilo :: MagmaLipsync -> BL.ByteString
magmaMilo = addMiloHeader . runPut . void . codecOut bin . magmaMiloDir

data Lipsync = Lipsync
  { lipsyncVersion    :: Word32 -- 1 from magma v2
  , lipsyncSubversion :: Word32 -- 2 from magma v2
  , lipsyncDTAImport  :: B.ByteString -- empty string from magma v2
  , lipsyncVisemes    :: [B.ByteString]
  , lipsyncKeyframes  :: [Keyframe]
  } deriving (Eq, Show)

newtype Keyframe = Keyframe
  { keyframeEvents :: [VisemeEvent () Int]
  } deriving (Eq, Show)

type LipsyncStates = [Map.Map B.ByteString Word8] -- each map is an absolute state of visemes

lipsyncToStates :: Lipsync -> LipsyncStates
lipsyncToStates l = go Map.empty $ lipsyncKeyframes l where
  go _ [] = []
  go m (kf : rest) = let
    m' = Map.filter (/= 0) $
      foldr (\(VisemeEvent i w ()) -> Map.insert (lipsyncVisemes l !! i) w) m (keyframeEvents kf)
    in m' : go m' rest

lipsyncFromStates :: LipsyncStates -> Lipsync
lipsyncFromStates ms = let
  visemes = Set.unions $ map Map.keysSet ms
  changes _   []            = []
  changes cur (next : rest) = let
    this = do
      vis <- Set.toList $ Map.keysSet cur <> Map.keysSet next
      return (vis, fromMaybe 0 $ Map.lookup vis next)
    in this : changes next rest
  in Lipsync
    { lipsyncVersion = 1
    , lipsyncSubversion = 2
    , lipsyncDTAImport = ""
    , lipsyncVisemes = Set.toList visemes
    , lipsyncKeyframes = do
      frame <- redundantZero $ changes Map.empty ms
      return $ Keyframe $ sort $ flip map frame $ \(bs, w) -> VisemeEvent
        { visemeKey = Set.findIndex bs visemes
        , visemeWeight = w
        , visemeGraph = ()
        }
    }

addLipsyncStates :: LipsyncStates -> LipsyncStates -> LipsyncStates
addLipsyncStates = let
  addList []       ys       = ys
  addList xs       []       = xs
  addList [x]      ys       = map (addSingle x) ys
  addList xs       [y]      = map (addSingle y) xs
  addList (x : xs) (y : ys) = addSingle x y : addList xs ys
  addSingle mx my = Map.filter (/= 0) $ Map.unionWith addWeight mx my
  addWeight w1 w2 = let
    summed = fromIntegral w1 + fromIntegral w2 :: Int
    in if summed > 255 then 255 else fromIntegral summed
  in addList

addLipsync :: Lipsync -> Lipsync -> Lipsync
addLipsync lx ly = lipsyncFromStates $
  addLipsyncStates (lipsyncToStates lx) (lipsyncToStates ly)

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
      visemeKey <- fromIntegral <$> getWord8
      visemeWeight <- getWord8
      let visemeGraph = ()
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
          putWord8 $ fromIntegral $ visemeKey evt
          putWord8 $ visemeWeight evt
  putWord32be $ fromIntegral $ BL.length keyframeBS
  putLazyByteString keyframeBS
  putWord32be 0

lipsyncToMIDITrack :: Lipsync -> LipsyncTrack U.Seconds
lipsyncToMIDITrack lip
  = LipsyncTrack RTB.empty RTB.empty RTB.empty
  $ RTB.flatten
  $ RTB.fromPairList
  $ do
    (dt, key) <- zip (0 : repeat (1/30 :: U.Seconds)) $ lipsyncKeyframes lip
    let lookupViseme i = TE.decodeLatin1 $ lipsyncVisemes lip !! i
        modifyEvent (VisemeEvent i w ()) = VisemeEvent (lookupViseme i) w GraphHold
    return (dt, map modifyEvent $ keyframeEvents key)

data VisemeMap vis = VisemeMap
  { vmVowels     :: CMUVowel     -> (vis, Maybe vis)
  , vmConsonants :: CMUConsonant -> [vis]
  , vmDefault    :: vis
  } deriving (Functor)

applyVisemeMap :: VisemeMap vis -> CMUSyllable -> VisemeSyllable vis
applyVisemeMap vmap cmu = Syllable
  { sylInitial = concatMap (vmConsonants vmap) $ sylInitial cmu
  , sylVowel   = vmVowels vmap $ sylVowel cmu
  , sylFinal   = concatMap (vmConsonants vmap) $ sylFinal cmu
  }

loadVisemesRB3 :: (MonadIO m) => StackTraceT m (VisemeMap [(T.Text, Word8)])
loadVisemesRB3 = stackIO (getResourcesPath "visemes/rb3.yml") >>= loadVisemeMap

loadVisemesTBRB :: (MonadIO m) => StackTraceT m (VisemeMap [(T.Text, Word8)])
loadVisemesTBRB = stackIO (getResourcesPath "visemes/beatles.yml") >>= loadVisemeMap

loadVisemeMap :: (MonadIO m) => FilePath -> StackTraceT m (VisemeMap [(T.Text, Word8)])
loadVisemeMap f = inside ("Loading viseme map from: " <> f) $ do
  stackIO (Y.decodeFileEither f) >>= \case
    Right v -> return v
    Left  e -> fatal $ Y.prettyPrintParseException e

instance A.FromJSON (VisemeMap [(T.Text, Word8)]) where
  parseJSON = A.withObject "VisemeMap" $ \o -> do
    let readMap m = forM (HM.toList m) $ \(k, v) -> do
          v' <- A.parseJSON v
          return (k, v')
        readVowel A.Null = return ([], Nothing)
        readVowel (A.Object obj) = (obj .:? "main") >>= \case
          Just objMain -> do
            objEnd <- obj .: "end"
            vMain <- A.withObject "vowel main part" readMap objMain
            vEnd <- A.withObject "vowel end part" readMap objEnd
            return (vMain, Just vEnd)
          Nothing -> do
            vMain <- readMap obj
            return (vMain, Nothing)
        readVowel v = A.typeMismatch "object/null" v
        readConsonant A.Null = return []
        readConsonant (A.Array xs) = mapM (A.withObject "consonant viseme sequence" readMap) $ V.toList xs
        readConsonant (A.Object obj) = (: []) <$> readMap obj
        readConsonant v = A.typeMismatch "object/array/null" v
    objVowels <- o .: "vowels"
    vmVowels <- do
      m <- fmap Map.fromList $ forM (HM.toList objVowels) $ \(k, v) -> do
        cmuv <- case readMaybe $ T.unpack k of
          Just cmuv -> return cmuv
          Nothing   -> fail $ "Unrecognized vowel: " <> show k
        vm <- readVowel v
        return (cmuv, vm)
      return $ \vowel -> fromMaybe ([], Nothing) $ Map.lookup vowel m
    objConsonants <- o .: "consonants"
    vmConsonants <- do
      m <- fmap Map.fromList $ forM (HM.toList objConsonants) $ \(k, v) -> do
        cmuc <- case readMaybe $ T.unpack k of
          Just cmuc -> return cmuc
          Nothing   -> fail $ "Unrecognized consonant: " <> show k
        cm <- readConsonant v
        return (cmuc, cm)
      return $ \consonant -> fromMaybe [] $ Map.lookup consonant m
    objDefault <- o .: "default"
    vmDefault <- A.withObject "default viseme setting" readMap objDefault
    return VisemeMap{..}

-- TODO add diphthongs and consonants
cmuToGH2Viseme :: CMUVowel -> Maybe GH2Viseme
cmuToGH2Viseme = \case
  CMU_AA -> Just GH2_Ox -- ɑ : balm bot
  CMU_AH -> Just GH2_If -- ʌ : butt
  CMU_AY -> Just GH2_Ox -- aɪ : bite
  CMU_EH -> Just GH2_Cage -- ɛ : bet
  CMU_ER -> Just GH2_Church -- ɝ : bird
  CMU_EY -> Just GH2_Cage -- eɪ : bait
  CMU_IH -> Just GH2_If -- ɪ : bit
  CMU_IY -> Just GH2_Eat -- i : beat
  CMU_OW -> Just GH2_Earth -- oʊ : boat
  CMU_UW -> Just GH2_Wet -- u : boot
  CMU_AE -> Just GH2_Cage -- æ : bat
  CMU_AO -> Just GH2_Earth -- ɔ : story
  CMU_AW -> Just GH2_If -- aʊ : bout
  CMU_OY -> Just GH2_Oat -- ɔɪ : boy
  CMU_UH -> Just GH2_Though -- ʊ : book

data Syllable c v = Syllable
  { sylInitial :: [c]
  , sylVowel   :: v
  , sylFinal   :: [c]
  }
type CMUSyllable        = Syllable CMUConsonant CMUVowel
type VisemeSyllable vis = Syllable vis          (vis, Maybe vis)

justVowel :: v -> Syllable c v
justVowel v = Syllable [] v []

mapVisemeSyllable :: (a -> b) -> VisemeSyllable [(a, Word8)] -> VisemeSyllable [(b, Word8)]
mapVisemeSyllable f syl = Syllable
  { sylInitial = map f' $ sylInitial syl
  , sylVowel = case sylVowel syl of
    (v1, Nothing) -> (f' v1, Nothing)
    (v1, Just v2) -> (f' v1, Just $ f' v2)
  , sylFinal = map f' $ sylFinal syl
  } where f' = map $ first f

type Transcribe = [T.Text] -> [CMUSyllable]

spanishVowels :: Transcribe
spanishVowels = fmap $ \t -> let
  vowels = flip mapMaybe (T.unpack $ T.toLower t) $ \case
    'á' -> Just 'a'
    'é' -> Just 'e'
    'í' -> Just 'i'
    'ó' -> Just 'o'
    'ú' -> Just 'u'
    -- don't need to handle 'ü' since should always be followed by 'i'
    c   -> guard (elem c ("aeiou" :: String)) >> Just c
  sound = \case
    'a' -> CMU_AA
    'e' -> CMU_EY -- could be CMU_EH
    'i' -> CMU_IY
    'o' -> CMU_OW
    'u' -> CMU_UW
    _   -> CMU_AH -- shouldn't happen
  in justVowel $ case vowels of
    [v] -> sound v
    _   -> case filter (\c -> c /= 'i' && c /= 'u') vowels of
      v : _ -> sound v
      []    -> case vowels of
        "ui"  -> CMU_IY
        v : _ -> sound v
        []    -> if T.any (\c -> c == 'y' || c == 'Y') t
          then CMU_IY
          else CMU_AH -- default

germanVowels :: Transcribe
germanVowels = fmap $ \t -> let
  vowels = T.filter (`elem` ("aeiouäöü" :: String)) $ T.toLower t
  in justVowel $ case vowels of
    "ei" -> CMU_AY
    "ie" -> CMU_IY
    "eu" -> CMU_OY
    "eue" -> CMU_OY
    "oo" -> case T.filter isAlpha $ T.toLower t of
      "ooh" -> CMU_UW -- for english "ooh"
      _     -> CMU_OW
    "ea" -> CMU_AE -- for english "yeah"
    _ -> case T.take 1 vowels of
      -- these aren't entirely accurate, long vs short should depend on consonants
      "a" -> CMU_AA
      "e" -> CMU_EH -- should be CMU_AH when unstressed, especially -e at end of word
      "i" -> CMU_IH
      "o" -> CMU_AO
      "u" -> CMU_UW
      "ä" -> CMU_EH
      "ö" -> CMU_ER
      "ü" -> CMU_UW
      _   -> CMU_AH -- default

splitSyllables :: [CMUPhoneme] -> [CMUSyllable]
splitSyllables [] = []
splitSyllables (CMUConsonant c : ps) = case splitSyllables ps of
  []         -> []
  syl : syls -> syl { sylInitial = c : sylInitial syl } : syls
splitSyllables (CMUVowel v : ps) = case ps of
  CMUConsonant _ : CMUVowel _ : _ -> noFinal
  -- TODO smarter rules for consuming final consonants
  CMUConsonant c : rest           -> Syllable
    { sylInitial = []
    , sylVowel   = v
    , sylFinal   = [c]
    } : splitSyllables rest
  _                               -> noFinal
  where noFinal = Syllable
          { sylInitial = []
          , sylVowel = v
          , sylFinal = []
          } : splitSyllables ps

englishSyllables :: Transcribe
englishSyllables syllables = let
  numSyllables = length syllables
  filterLyric
    = T.map (\case '=' -> '-'; c -> c)
    . T.filter (`notElem` ("-#^$!?" :: String))
  word = B8.pack $ T.unpack $ T.toUpper $ T.concat $ map filterLyric syllables
  in case filter ((== numSyllables) . length) $ map splitSyllables $ fromMaybe [] $ HM.lookup word cmuDict of
    match : _ -> match
    []        -> const (justVowel CMU_AH) <$> syllables -- TODO better default syllables

runTranscribe :: RTB.T t (Maybe (Transcribe, T.Text)) -> RTB.T t (Maybe CMUSyllable)
runTranscribe = let
  splitFirstWord _         []            = ([], [])
  splitFirstWord passedEnd xs@(x : rest) = let
    (passedEnd', continue) = case x of
      (_, Nothing) -> (passedEnd, True)
      (_, Just (_, lyric)) -> if passedEnd
        then (passedEnd, False)
        else let
          doesWordContinue = elem
            (T.takeEnd 1 $ T.dropWhileEnd (`elem` ['$', '#', '^']) lyric)
            ["-", "="]
          in (not doesWordContinue, True)
    in if continue
      then case splitFirstWord passedEnd' rest of
        (ys, zs) -> (x : ys, zs)
      else ([], xs)
  go [] = []
  go evts = case splitFirstWord False evts of
    (wordEvents, rest) -> let
      syllablePairs = mapMaybe snd wordEvents
      phones = case syllablePairs of
        []             -> [] -- shouldn't happen
        (trans, _) : _ -> applyPhonemes (trans $ map snd syllablePairs) wordEvents
      in phones ++ go rest
  applyPhonemes phones           ((t, Nothing) : events) = (t, Nothing   ) : applyPhonemes phones events
  applyPhonemes (phone : phones) ((t, Just _ ) : events) = (t, Just phone) : applyPhonemes phones events
  applyPhonemes _                []                      = []
  applyPhonemes []               ((t, Just _ ) : events) = (t, Just ah   ) : applyPhonemes []     events -- shouldn't happen
  ah = justVowel CMU_AH
  in RTB.fromPairList . go . RTB.toPairList

-- for some reason you sometimes need an extra zero for a viseme to be totally shut off.
-- official lipsync files usually have this as well
redundantZero :: (Eq a) => [[(a, Word8)]] -> [[(a, Word8)]]
redundantZero []               = []
redundantZero [x]              = [x, x]
redundantZero (x : xs@(y : _)) = x : case [ vis | (vis, 0) <- x, isNothing $ lookup vis y ] of
  []      -> redundantZero xs
  visemes -> case redundantZero xs of
    []      -> [] -- shouldn't happen
    y' : ys -> (map (, 0) visemes ++ y') : ys

data VisemeAnimation v
  = VisemeHold v
  | VisemeLine v v -- initial/final transitions, linear
  | VisemeFall v v -- diphthong with main vowel then secondary one, "easeInExpo"
  deriving (Show, Functor, Foldable)

-- each list in the event list is an absolute set of visemes, not just changes
simpleAnimations
  :: U.Seconds
  -> RTB.T U.Seconds [pair]
  -> RTB.T U.Seconds (VisemeAnimation [pair])
simpleAnimations transition = go [] where
  halfTransition = transition / 2
  go prev = \case
    RNil -> RNil
    Wait t1 s (Wait t2 x rest) -> let
      front = min halfTransition t1
      back = min halfTransition (t2 / 2)
      in Wait (t1 - front) (VisemeLine prev s) $ if back == t2 / 2
        then go s $ Wait (t2 - back) x rest -- don't need to hold, will immediately do another transition
        else Wait (front + back) (VisemeHold s)
          $ go s $ Wait (t2 - back) x rest
    Wait t1 s RNil -> let
      front = min halfTransition t1
      back = halfTransition
      in Wait (t1 - front) (VisemeLine prev s)
        $ Wait (front + back) (VisemeHold s) RNil

syllablesToAnimations
  :: U.Seconds
  -> VisemeMap [pair]
  -> RTB.T U.Seconds (Maybe (VisemeSyllable [pair]))
  -> RTB.T U.Seconds (VisemeAnimation [pair])
syllablesToAnimations transition vmap = removeCancelled . Wait 0 (VisemeHold $ vmDefault vmap) . go where
  halfTransition = transition / 2
  go = \case
    RNil -> RNil
    Wait t1 (Just syl) (Wait t2 Nothing rest) -> let
      initialFront = min halfTransition t1
      initialBack = min halfTransition (t2 / 2)
      finalFront = min halfTransition (t2 / 2)
      finalBack = case rest of
        RNil        -> halfTransition
        Wait t3 _ _ -> min halfTransition (t3 / 2)
      vowelBody = case sylVowel syl of
        (v1, Nothing) -> VisemeHold v1
        (v1, Just v2) -> VisemeFall v1 v2
      vowelStart = fst $ sylVowel syl
      vowelEnd = case sylVowel syl of
        (v1, Nothing) -> v1
        (_, Just v2)  -> v2
      lineSequence len x [] z continue = Wait 0 (VisemeLine x z) $ RTB.delay len continue
      lineSequence len x (y : ys) z continue = let
        segments = length ys + 2
        firstLen = len / fromIntegral segments
        in Wait 0 (VisemeLine x y) $ RTB.delay firstLen
          $ lineSequence (len - firstLen) y ys z continue
      in RTB.delay (t1 - initialFront)
        $ lineSequence (initialFront + initialBack) (vmDefault vmap) (sylInitial syl) vowelStart
        $ Wait 0 vowelBody
        $ RTB.delay (t2 - initialBack - finalFront)
        $ lineSequence (finalFront + finalBack) vowelEnd (sylFinal syl) (vmDefault vmap)
        $ Wait 0 (VisemeHold $ vmDefault vmap)
        $ go $ U.trackDrop finalBack rest
    Wait t1 Nothing rest -> RTB.delay t1 $ go rest -- shouldn't happen
    Wait t1 (Just syl1) (Wait t2 (Just syl2) rest) -- shouldn't happen
      -> go $ Wait t1 (Just syl1) $ Wait t2 Nothing $ Wait 0 (Just syl2) rest
    Wait _t1 (Just _syl1) RNil -> RNil -- shouldn't happen
  removeCancelled = \case
    Wait t _ (Wait 0 anim rest) -> removeCancelled $ Wait t anim rest
    Wait t x rest -> Wait t x $ removeCancelled rest
    RNil -> RNil

singleAnimsToStates
  :: T.Text
  -> RTB.T U.Seconds (VisemeAnimation Word8)
  -> LipsyncStates
singleAnimsToStates vis anims = let
  makeVisemeEvent weight = VisemeEvent vis weight ()
  animate animFunction len thisViseme1 thisViseme2 rest = let
    transitionSteps = ceiling $ len * 30 :: Int
    normalStep = len / fromIntegral transitionSteps
    inBetween i = makeVisemeEvent $ let
      animProgress = animFunction $ realToFrac i / realToFrac transitionSteps
      in thisViseme1 + round ((realToFrac thisViseme2 - realToFrac thisViseme1) * animProgress)
    steps = do
      (i, step) <- zip [0 .. transitionSteps - 1] (0 : repeat normalStep)
      return $ Wait step [inBetween i]
    in foldr ($) (go $ RTB.delay normalStep rest) steps
  easeInExpo :: Double -> Double
  easeInExpo t = if t == 0 then 0 else 2 ** (10 * t - 10)
  go = \case
    RNil -> RNil
    Wait t1 (VisemeHold v) rest -> Wait t1 [makeVisemeEvent v] $ go rest
    Wait t1 (VisemeLine v1 v2) (Wait t2 x xs) -> if t2 /= 0
      then RTB.delay t1 $ animate id t2 v1 v2 $ Wait 0 x xs
      else go $ Wait t1 (VisemeHold v1) $ let -- probably shouldn't happen
        x' = case x of
          VisemeLine next1 next2 | next1 == v2 -> VisemeLine v1 next2
          VisemeFall next1 next2 | next1 == v2 -> VisemeFall v1 next2
          _                                    -> x
        in Wait t2 x' xs
    Wait t1 (VisemeFall v1 v2) (Wait t2 x xs) -> if t2 /= 0
      then RTB.delay t1 $ animate easeInExpo t2 v1 v2 $ Wait 0 x xs
      else go $ Wait t1 (VisemeHold v1) $ let -- can happen if a diphthong has zero time to do its transition
        x' = case x of
          VisemeLine next1 next2 | next1 == v2 -> VisemeLine v1 next2
          VisemeFall next1 next2 | next1 == v2 -> VisemeFall v1 next2
          _                                    -> x
        in Wait t2 x' xs
    Wait t1 (VisemeLine v1 _) RNil -> Wait t1 [makeVisemeEvent v1] RNil -- shouldn't happen
    Wait t1 (VisemeFall v1 _) RNil -> Wait t1 [makeVisemeEvent v1] RNil -- shouldn't happen
  in lipEventsStates $ RTB.flatten $ go anims

animationsToEvents
  :: (NNC.C t)
  => RTB.T t (VisemeAnimation [(T.Text, Word8)])
  -> RTB.T t (VisemeEvent VisemeGraph T.Text)
animationsToEvents = let
  go _  RNil             = RNil
  go on (Wait dt x rest) = let
    (startState, graph) = case x of
      VisemeHold va    -> (va                , GraphHold      )
      VisemeLine va vb -> (va <> zeroes va vb, GraphLinear    )
      VisemeFall va vb -> (va <> zeroes va vb, GraphEaseInExpo)
    zeroes va vb = map (, 0) $ map fst vb \\ map fst va
    x' = map (\(a, w) -> VisemeEvent a w graph) startState
    needToClear = Set.toList $ Set.difference on (Set.fromList $ map fst startState)
    clears = map (\a -> VisemeEvent a 0 GraphHold) needToClear
    on' = Set.fromList $ map fst startState
    in Wait dt (x' <> clears) $ go on' rest
  in RTB.flatten . go Set.empty

animationsToStates
  :: RTB.T U.Seconds (VisemeAnimation [(T.Text, Word8)])
  -> LipsyncStates
animationsToStates anims = let
  allVisemes :: [T.Text]
  allVisemes = nubOrd $ toList anims >>= toList >>= map fst
  makeVisemeEvent pairs = do
    vis <- allVisemes
    return $ VisemeEvent vis (fromMaybe 0 $ lookup vis pairs) ()
  animate animFunction len v1 v2 rest = let
    transitionSteps = ceiling $ len * 30 :: Int
    normalStep = len / fromIntegral transitionSteps
    inBetween i = makeVisemeEvent $ do
      let animProgress = animFunction $ realToFrac i / realToFrac transitionSteps
      k <- nubOrd $ map fst v1 <> map fst v2
      let thisViseme1 = fromMaybe 0 $ lookup k v1
          thisViseme2 = fromMaybe 0 $ lookup k v2
      return (k, thisViseme1 + round ((realToFrac thisViseme2 - realToFrac thisViseme1) * animProgress))
    steps = do
      (i, step) <- zip [0 .. transitionSteps - 1] (0 : repeat normalStep)
      return $ Wait step $ inBetween i
    in foldr ($) (go $ RTB.delay normalStep rest) steps
  easeInExpo :: Double -> Double
  easeInExpo t = if t == 0 then 0 else 2 ** (10 * t - 10)
  go = \case
    RNil -> RNil
    Wait t1 (VisemeHold v) rest -> Wait t1 (makeVisemeEvent v) $ go rest
    Wait t1 (VisemeLine v1 v2) (Wait t2 x xs) -> if t2 /= 0
      then RTB.delay t1 $ animate id t2 v1 v2 $ Wait 0 x xs
      else go $ Wait t1 (VisemeHold v1) $ let -- probably shouldn't happen
        x' = case x of
          VisemeLine next1 next2 | next1 == v2 -> VisemeLine v1 next2
          VisemeFall next1 next2 | next1 == v2 -> VisemeFall v1 next2
          _                                    -> x
        in Wait t2 x' xs
    Wait t1 (VisemeFall v1 v2) (Wait t2 x xs) -> if t2 /= 0
      then RTB.delay t1 $ animate easeInExpo t2 v1 v2 $ Wait 0 x xs
      else go $ Wait t1 (VisemeHold v1) $ let -- can happen if a diphthong has zero time to do its transition
        x' = case x of
          VisemeLine next1 next2 | next1 == v2 -> VisemeLine v1 next2
          VisemeFall next1 next2 | next1 == v2 -> VisemeFall v1 next2
          _                                    -> x
        in Wait t2 x' xs
    Wait t1 (VisemeLine v1 _) RNil -> Wait t1 (makeVisemeEvent v1) RNil -- shouldn't happen
    Wait t1 (VisemeFall v1 _) RNil -> Wait t1 (makeVisemeEvent v1) RNil -- shouldn't happen
  in lipEventsStates $ RTB.flatten $ go anims

visemesToLipsync :: U.Seconds -> RTB.T U.Seconds [(T.Text, Word8)] -> Lipsync
visemesToLipsync transition = lipsyncFromStates . animationsToStates . simpleAnimations transition

defaultTransition :: U.Seconds
defaultTransition = 0.12

autoLipsync :: U.Seconds -> VisemeMap [(T.Text, Word8)] -> Transcribe -> VocalTrack U.Seconds -> Lipsync
autoLipsync transition vmap trans vt = let
  eyes
    = animationsToStates
    $ simpleAnimations transition
    $ fmap (\b -> guard b >> [("Blink", 255)])
    $ vocalEyesClosed vt
  mouth
    = animationsToStates
    $ syllablesToAnimations transition vmap
    $ fmap (fmap $ applyVisemeMap vmap)
    $ runTranscribe
    $ fmap (fmap (trans,))
    $ vocalTubes vt
  in lipsyncFromStates $ addLipsyncStates eyes mouth

setRB3 :: Lipsync -> Lipsync
setRB3 lip = lip
  { lipsyncVersion = 1
  , lipsyncSubversion = 2
  , lipsyncDTAImport = ""
  }

setBeatles :: Lipsync -> Lipsync
setBeatles lip = lip
  { lipsyncVersion = 0
  , lipsyncSubversion = 2
  , lipsyncDTAImport = "proj9"
  }

beatlesLipsync :: U.Seconds -> VisemeMap [(T.Text, Word8)] -> Transcribe -> VocalTrack U.Seconds -> Lipsync
beatlesLipsync transition vmap trans vt = let
  eyes
    = animationsToStates
    $ simpleAnimations transition
    $ fmap (\b -> guard b >> [("l_lids", 255), ("r_lids", 255)])
    $ vocalEyesClosed vt
  mouth
    = animationsToStates
    $ syllablesToAnimations transition vmap
    $ fmap (fmap $ applyVisemeMap vmap)
    $ runTranscribe
    $ fmap (fmap (trans,))
    $ vocalTubes vt
  in setBeatles $ lipsyncFromStates $ addLipsyncStates eyes mouth

beatlesLipsync' :: U.Seconds -> VisemeMap [(T.Text, Word8)] -> Transcribe -> VocalTrack U.Seconds
  -> RTB.T U.Seconds (VisemeEvent VisemeGraph T.Text)
beatlesLipsync' transition vmap trans vt = let
  eyes
    = animationsToEvents
    $ simpleAnimations transition
    $ fmap (\b -> guard b >> [("l_lids", 255), ("r_lids", 255)])
    $ vocalEyesClosed vt
  mouth
    = animationsToEvents
    $ syllablesToAnimations transition vmap
    $ fmap (fmap $ applyVisemeMap vmap)
    $ runTranscribe
    $ fmap (fmap (trans,))
    $ vocalTubes vt
  in RTB.merge eyes mouth

gh2Lipsync :: Transcribe -> VocalTrack U.Seconds -> VocFile
gh2Lipsync trans
  = visemesToVoc
  . fmap (\msyl -> case msyl >>= cmuToGH2Viseme . sylVowel of
    Nothing  -> []
    Just vis -> [(T.replace "_" " " $ T.pack $ drop 4 $ show vis, 0.5)]
    )
  . runTranscribe
  . fmap (fmap (trans,))
  . vocalTubes

lipEventsStates :: RTB.T U.Seconds (VisemeEvent () T.Text) -> LipsyncStates
lipEventsStates = let
  makeKeyframes cur rest = let
    (frame, after) = U.trackSplit (1/30 :: U.Seconds) rest
    next = Map.filter (/= 0) $ foldl' (\m (VisemeEvent k w ()) -> Map.insert k w m) cur frame
    in next : if RTB.null after
      then []
      else makeKeyframes next after
  in map (Map.mapKeys TE.encodeUtf8)
    . makeKeyframes Map.empty
    . RTB.delay (1/60 :: U.Seconds) -- this is so we can process the first 1/30 and end up in the center of the first frame

lipEventsStates' :: RTB.T U.Seconds (VisemeEvent VisemeGraph T.Text) -> LipsyncStates
lipEventsStates' events = let
  allVisemes :: [T.Text]
  allVisemes = nubOrd $ map visemeKey $ toList events
  getSingleStates k = singleAnimsToStates k $ toAnimations $ RTB.filter (\e -> visemeKey e == k) events
  toAnimations = \case
    RNil -> RNil
    Wait t (VisemeEvent _ w _) RNil -> Wait t (VisemeHold w) RNil
    Wait t (VisemeEvent _ w1 graph) rest@(Wait _ (VisemeEvent _ w2 _) _) -> case graph of
      GraphHold       -> Wait t (VisemeHold w1)    $ toAnimations rest
      GraphLinear     -> Wait t (VisemeLine w1 w2) $ toAnimations rest
      GraphEaseInExpo -> Wait t (VisemeFall w1 w2) $ toAnimations rest
  in foldr addLipsyncStates [] $ map getSingleStates allVisemes

data LipsyncTarget
  = LipsyncRB3
  | LipsyncTBRB

lipLyricsStates :: VisemeMap [(T.Text, Word8)] -> LipsyncTrack U.Seconds -> LipsyncStates
lipLyricsStates vmap lip = let
  tubes = vocalTubes mempty
    { vocalLyrics = lipLyrics lip
    , vocalNotes  = lipNotes  lip
    }
  withLang = applyStatus1 LyricEnglish (lipLanguage lip) tubes
  transcribed = runTranscribe $ flip fmap withLang $ \(lang, mtext) -> let
    trans = case lang of
      LyricEnglish -> englishSyllables
      LyricGerman  -> germanVowels
      LyricSpanish -> spanishVowels
    in (trans,) <$> mtext
  fromCMU = fmap $ applyVisemeMap vmap
  in animationsToStates
    $ syllablesToAnimations defaultTransition vmap
    $ fmap fromCMU transcribed

lipsyncFromMIDITrack' :: LipsyncTarget -> VisemeMap [(T.Text, Word8)] -> LipsyncTrack U.Seconds -> Lipsync
lipsyncFromMIDITrack' tgt vmap lip
  = (case tgt of LipsyncTBRB -> setBeatles; LipsyncRB3 -> setRB3)
  $ lipsyncFromStates
  $ addLipsyncStates (lipEventsStates' $ lipEvents lip) (lipLyricsStates vmap lip)

lipsyncFromMIDITrack :: VisemeMap [(T.Text, Word8)] -> LipsyncTrack U.Seconds -> Lipsync
lipsyncFromMIDITrack = lipsyncFromMIDITrack' LipsyncRB3

testConvertLipsync :: FilePath -> [FilePath] -> FilePath -> IO ()
testConvertLipsync fmid fvocs fout = do
  res <- logStdout $ RBFile.loadMIDI fmid
  mid <- case res of
    Left err  -> error $ show err
    Right mid -> return mid
  trks <- forM fvocs $ \fvoc -> do
    trk <- BL.readFile fvoc >>= return . case takeExtension fvoc of
      ".voc" -> vocToMIDITrack     . runGet parseVocFile
      _      -> lipsyncToMIDITrack . runGet parseLipsync
    return $ mapTrack (U.unapplyTempoTrack $ RBFile.s_tempos mid) trk
  Save.toFile fout $ RBFile.showMIDIFile' mid
    { RBFile.s_tracks = (RBFile.s_tracks mid)
      { RBFile.onyxParts = let
        orig = RBFile.onyxParts $ RBFile.s_tracks mid
        fn vox = Just (fromMaybe mempty vox)
          { RBFile.onyxLipsync1 = fromMaybe mempty $ listToMaybe trks
          , RBFile.onyxLipsync2 = fromMaybe mempty $ listToMaybe $ drop 1 trks
          , RBFile.onyxLipsync3 = fromMaybe mempty $ listToMaybe $ drop 2 trks
          }
        in Map.alter fn RBFile.FlexVocal orig
      }
    }

data VocFile = VocFile
  { vocMystery1  :: Word32 -- RB: 1500, GH2: 1200
  , vocMystery2  :: Word16 -- RB: 1, GH2: 0
  , vocCompany   :: B.ByteString -- "Harmonix"
  , vocMystery3  :: Word16 -- 1
  , vocComment   :: B.ByteString
    -- RB: "5 projects developed before 5/7/2007"
    -- GH2: "Karaoke Revolution Vol 4"
  -- boundaries between next few are uncertain
  , vocMystery4  :: Word32 -- 1000
  , vocMystery5  :: Word32 -- 0
  , vocMystery6  :: Word16 -- 0
  , vocMystery7  :: Word16 -- RB: 1, GH2: 0
  , vocName      :: B.ByteString -- "alright_dryvox"
  , vocMystery8  :: Word16 -- 3
  , vocFileSize  :: Word32 -- total size of the file. 123525 (0x1E285)
  , vocMystery9  :: Word16 -- 0
  , vocVisemes   :: [VocViseme]
  -- again, boundaries uncertain
  , vocMystery10 :: Word32 -- 0
  , vocMystery11 :: Word16 -- 0
  -- pretty sure these are floats. could be weight transition times
  , vocMystery12 :: Float -- 0.16
  , vocMystery13 :: Float -- 0.22
  -- GH2 file ends here. RB file continues
  , vocMystery14 :: Maybe Word32 -- 0
  , vocMystery15 :: Maybe Word16 -- 0
  , vocMystery16 :: Maybe Word32 -- 1
  , vocMystery17 :: Maybe Word16 -- 0
  , vocMystery18 :: Maybe Word32 -- 1
  , vocMystery19 :: Maybe Word16 -- 0
  , vocMystery20 :: Maybe Int32 -- -1
  } deriving (Show)

data VocViseme = VocViseme
  { vvMystery1 :: Word32 -- 0
  , vvMystery2 :: Word16 -- 0
  , vvMystery3 :: Word16 -- RB: 1, GH2: 0
  , vvName     :: B.ByteString -- "Eat", "If", etc.
  , vvMystery4 :: Word32 -- 0
  , vvMystery5 :: Word32 -- 0
  , vvEvents   :: [VocEvent] -- each is 18 bytes
  , vvMystery6 :: Word16 -- 0
  } deriving (Show)

data VocEvent = VocEvent
  { veMystery1 :: Word32 -- 0
  , veTime     :: Float -- timestamp in seconds
  , veWeight   :: Float -- range appears to vary per property. 0 to 1 is typical but some are negative or higher than 1
  , veMystery2 :: Word32 -- 0
  , veMystery3 :: Word16 -- 0
  } deriving (Show)

parseVocFile :: Get VocFile
parseVocFile = do
  "FACE" <- getByteString 4
  vocMystery1 <- getWord32le
  vocMystery2 <- getWord16le
  vocCompany <- getStringLE
  vocMystery3 <- getWord16le
  vocComment <- getStringLE
  vocMystery4 <- getWord32le
  vocMystery5 <- getWord32le
  vocMystery6 <- getWord16le
  vocMystery7 <- getWord16le
  vocName <- getStringLE
  vocMystery8 <- getWord16le
  vocFileSize <- getWord32le
  vocMystery9 <- getWord16le
  visemeCount <- getWord32le
  vocVisemes <- replicateM (fromIntegral visemeCount) $ do
    vvMystery1 <- getWord32le
    vvMystery2 <- getWord16le
    vvMystery3 <- getWord16le
    vvName <- getStringLE
    vvMystery4 <- getWord32le
    vvMystery5 <- getWord32le
    eventCount <- getWord16le
    vvEvents <- replicateM (fromIntegral eventCount) $ do
      veMystery1 <- getWord32le
      veTime <- getFloatle
      veWeight <- getFloatle
      veMystery2 <- getWord32le
      veMystery3 <- getWord16le
      return VocEvent{..}
    vvMystery6 <- getWord16le
    return VocViseme{..}
  vocMystery10 <- getWord32le
  vocMystery11 <- getWord16le
  vocMystery12 <- getFloatle
  vocMystery13 <- getFloatle
  isGH <- isEmpty
  vocMystery14 <- if isGH then return Nothing else Just <$> getWord32le
  vocMystery15 <- if isGH then return Nothing else Just <$> getWord16le
  vocMystery16 <- if isGH then return Nothing else Just <$> getWord32le
  vocMystery17 <- if isGH then return Nothing else Just <$> getWord16le
  vocMystery18 <- if isGH then return Nothing else Just <$> getWord32le
  vocMystery19 <- if isGH then return Nothing else Just <$> getWord16le
  vocMystery20 <- if isGH then return Nothing else Just <$> getInt32le
  return VocFile{..}

putVocFileRaw :: VocFile -> Put
putVocFileRaw VocFile{..} = do
  putByteString "FACE"
  putWord32le vocMystery1
  putWord16le vocMystery2
  putStringLE vocCompany
  putWord16le vocMystery3
  putStringLE vocComment
  putWord32le vocMystery4
  putWord32le vocMystery5
  putWord16le vocMystery6
  putWord16le vocMystery7
  putStringLE vocName
  putWord16le vocMystery8
  putWord32le vocFileSize
  putWord16le vocMystery9
  putWord32le $ fromIntegral $ length vocVisemes
  forM_ vocVisemes $ \VocViseme{..} -> do
    putWord32le vvMystery1
    putWord16le vvMystery2
    putWord16le vvMystery3
    putStringLE vvName
    putWord32le vvMystery4
    putWord32le vvMystery5
    putWord16le $ fromIntegral $ length vvEvents
    forM_ vvEvents $ \VocEvent{..} -> do
      putWord32le veMystery1
      putFloatle veTime
      putFloatle veWeight
      putWord32le veMystery2
      putWord16le veMystery3
    putWord16le vvMystery6
  putWord32le vocMystery10
  putWord16le vocMystery11
  putFloatle vocMystery12
  putFloatle vocMystery13
  mapM_ putWord32le vocMystery14
  mapM_ putWord16le vocMystery15
  mapM_ putWord32le vocMystery16
  mapM_ putWord16le vocMystery17
  mapM_ putWord32le vocMystery18
  mapM_ putWord16le vocMystery19
  mapM_ putInt32le vocMystery20

putVocFile :: VocFile -> Put
putVocFile voc = let
  len = fromIntegral $ BL.length $ runPut $ putVocFileRaw voc
  in putVocFileRaw voc { vocFileSize = len }

getStringLE :: Get B.ByteString
getStringLE = do
  len <- getWord32le
  getByteString $ fromIntegral len

putStringLE :: B.ByteString -> Put
putStringLE bs = do
  putWord32le $ fromIntegral $ B.length bs
  putByteString bs

vocToMIDITrack :: VocFile -> LipsyncTrack U.Seconds
vocToMIDITrack voc
  = LipsyncTrack RTB.empty RTB.empty RTB.empty
  $ foldr RTB.merge RTB.empty
  $ flip map (vocVisemes voc)
  $ \vis -> let
    name = TE.decodeLatin1 $ vvName vis
    in RTB.fromAbsoluteEventList
      $ ATB.fromPairList
      $ flip map (vvEvents vis) $ \evt -> let
        -- arterialblack.voc has a veTime of -2.3666643e-2 so we clamp to 0
        time = realToFrac $ if veTime evt < 0 then 0 else veTime evt
        -- TODO extend lipsync track to support the full range
        weight
          | veWeight evt < 0 = 0
          | veWeight evt > 1 = 255
          | otherwise        = round $ veWeight evt * 255
        in (time, VisemeEvent name weight GraphHold)

visemesToVoc :: RTB.T U.Seconds [(T.Text, Float)] -> VocFile
visemesToVoc visemes = VocFile
  { vocMystery1 = 1200
  , vocMystery2 = 0
  , vocCompany = "Harmonix"
  , vocMystery3 = 0
  , vocComment = "Karaoke Revolution Vol 4"
  , vocMystery4 = 1000
  , vocMystery5 = 0
  , vocMystery6 = 0
  , vocMystery7 = 0
  , vocName = "onyx_toolkit_lipsync"
  , vocMystery8 = 0
  , vocFileSize = 0xDEADBEEF -- calculated later
  , vocMystery9 = 0
  , vocVisemes = flip map (nubOrd $ map fst $ concat $ RTB.getBodies visemes) $ \vis -> VocViseme
    { vvMystery1 = 0
    , vvMystery2 = 0
    , vvMystery3 = 0
    , vvName = TE.encodeUtf8 vis
    , vvMystery4 = 0
    , vvMystery5 = 0
    , vvEvents
      = map (\(t, weight) -> VocEvent
        { veMystery1 = 0
        , veTime = realToFrac t
        , veWeight = weight
        , veMystery2 = 0
        , veMystery3 = 0
        })
      $ ATB.toPairList
      $ RTB.toAbsoluteEventList 0
      $ noRedundantStatus
      $ fmap (\set -> fromMaybe 0 $ lookup vis set) visemes
    , vvMystery6 = 0
    }
  , vocMystery10 = 0
  , vocMystery11 = 0
  , vocMystery12 = 0.16
  , vocMystery13 = 0.22
  , vocMystery14 = Nothing
  , vocMystery15 = Nothing
  , vocMystery16 = Nothing
  , vocMystery17 = Nothing
  , vocMystery18 = Nothing
  , vocMystery19 = Nothing
  , vocMystery20 = Nothing
  }
