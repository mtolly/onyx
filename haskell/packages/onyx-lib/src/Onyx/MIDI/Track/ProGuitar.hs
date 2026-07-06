{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.MIDI.Track.ProGuitar where

import           Control.Arrow                    (first)
import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import           Data.Default.Class               (Default (..))
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import           Data.List.Extra                  (sort, unsnoc)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.Guitar                      (applyBlipStatus, applyStatus,
                                                   applyStatus1, guitarify,
                                                   trackState)
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Optics                           (mapped, (%), (.~))
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data NoteType
  = NormalNote
  | ArpeggioForm
  | Bent
  | Muted
  | Tapped
  | Harmonic
  | PinchHarmonic
  deriving (Eq, Ord, Show, Enum, Bounded)

data SlideType = NormalSlide | ReversedSlide
  deriving (Eq, Ord, Show, Enum, Bounded)

data StrumArea = High | Mid | Low | MysteryStrum0
  deriving (Eq, Ord, Show, Enum, Bounded)

type GtrFret = Int

-- Used in the following way:
-- 4 strings: S6 S5 S4 S3
-- 5 strings: S6 S5 S4 S3 S2
-- 6 strings: S6 S5 S4 S3 S2 S1
-- 7 strings: S7 S6 S5 S4 S3 S2 S1
-- 8 strings: S8 S7 S6 S5 S4 S3 S2 S1
data GtrString = S8 | S7 | S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getStringIndex :: Int -> GtrString -> Int
getStringIndex 8 s = fromEnum s
getStringIndex 7 s = fromEnum s - 1
getStringIndex _ s = fromEnum s - 2

indexTuning :: [Int] -> GtrString -> Int
indexTuning tuning str = tuning !! getStringIndex (length tuning) str

data GtrBase
  = Guitar6
  | Guitar7
  | Guitar8
  | Bass4
  | Bass5
  | Bass6
  | GtrCustom [Int] -- List of MIDI pitches from low to high
  deriving (Eq, Ord, Show)

data GtrTuning = GtrTuning
  { gtrBase    :: GtrBase
  , gtrOffsets :: [Int]
  , gtrGlobal  :: Int
  , gtrCapo    :: Int
  , gtrName    :: Maybe T.Text
  } deriving (Eq, Ord, Show)

instance Default GtrTuning where
  def = GtrTuning Guitar6 [] 0 0 Nothing

tuningBasePitches :: GtrBase -> [Int]
tuningBasePitches = \case
  Guitar6      -> [40, 45, 50, 55, 59, 64]
  Guitar7      -> [35, 40, 45, 50, 55, 59, 64]
  Guitar8      -> [30, 35, 40, 45, 50, 55, 59, 64]
  Bass4        -> [28, 33, 38, 43]
  Bass5        -> [23, 28, 33, 38, 43]
  Bass6        -> [23, 28, 33, 38, 43, 48]
  GtrCustom ps -> ps

tuningPitches :: GtrTuning -> [Int]
tuningPitches t
  = map (+ (t.gtrGlobal + t.gtrCapo))
  $ zipWith (+) (t.gtrOffsets ++ repeat 0)
  $ tuningBasePitches t.gtrBase

-- True if this should use string colors starting from purple (5/6-string bass or similar)
lowBassTuning :: GtrTuning -> Bool
lowBassTuning tuning = case tuning.gtrBase of
  Bass5             -> True
  Bass6             -> True
  GtrCustom (n : _) -> n < 28
  _                 -> False

-- | RB3 style (does not include 'gtrGlobal' or 'gtrCapo').
encodeTuningOffsets :: GtrTuning -> GuitarType -> [Int]
encodeTuningOffsets tun typ = let
  std  = tuningPitches case typ of
    TypeGuitar -> GtrTuning Guitar6 [] 0 0 Nothing
    TypeBass   -> GtrTuning Bass4   [] 0 0 Nothing
  n = case typ of
    TypeGuitar -> 6
    TypeBass   -> 4
  this = take n $ tuningPitches tun { gtrGlobal = 0, gtrCapo = 0 }
  in map fromIntegral $ zipWith (-) this std

eadgbeOffsetsToTuning :: GuitarType -> [Int] -> GtrTuning
eadgbeOffsetsToTuning TypeGuitar ns = GtrTuning
  { gtrBase = Guitar6 -- not supporting 7/8 this route
  , gtrOffsets = ns
  , gtrGlobal = 0
  , gtrCapo = 0
  , gtrName = Nothing
  }
eadgbeOffsetsToTuning TypeBass ns = let
  base = case length ns of
    5 -> Bass5
    n -> if n <= 4 then Bass4 else Bass6
  reference = map (subtract 12) $ tuningBasePitches Guitar6
  pitches = zipWith (+) reference ns
  in GtrTuning
    { gtrBase = base
    , gtrOffsets = zipWith subtract (tuningBasePitches base) pitches
    , gtrGlobal = 0
    , gtrCapo = 0
    , gtrName = Nothing
    }

instance ChannelType NoteType where
  encodeChannel = fromEnum

instance ChannelType SlideType where
  encodeChannel = \case
    NormalSlide   -> 0
    ReversedSlide -> 11
    -- I've also seen slides on other channels (2 and 3), but I think those are
    -- because HMX forgot to reset after making notes on those channels

instance ChannelType StrumArea where
  encodeChannel = \case
    High          -> 13
    Mid           -> 14
    Low           -> 15
    MysteryStrum0 -> 0

data GuitarType = TypeGuitar | TypeBass
  deriving (Eq, Ord, Show, Enum, Bounded)

data ProGuitarTrack t = ProGuitarTrack
  { difficulties   :: Map.Map Difficulty (ProGuitarDifficulty t)
  , trainer        :: RTB.T t (GuitarType, Trainer)
  , tremolo        :: RTB.T t (Maybe LaneDifficulty)
  , trill          :: RTB.T t (Maybe LaneDifficulty)
  , overdrive      :: RTB.T t Bool
  , bre            :: RTB.T t (GuitarType, Bool)
  , solo           :: RTB.T t Bool
  , handPosition   :: RTB.T t GtrFret
  , chordRoot      :: RTB.T t Key
  , noChordNames   :: RTB.T t Bool
  , slashChords    :: RTB.T t Bool
  , swapAccidental :: RTB.T t Bool
  -- ^ according to Ruggy, default sharp/flat is according to usual sheet music
  -- rules for the key + tonality
  , onyxOctave     :: RTB.T t GtrFret -- "move these notes 12 frets down if needed" section
  , onyxString     :: RTB.T t [GtrString] -- which strings RB should use (default S6..S1)
  , mystery45      :: RTB.T t Bool
  , mystery69      :: RTB.T t Bool
  , mystery93      :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ProGuitarTrack t)

instance ChopTrack ProGuitarTrack where
  chopTake t pg = ProGuitarTrack
    { difficulties   = mapTrack (U.trackTake t) <$> pg.difficulties -- TODO
    , trainer        = U.trackTake    t pg.trainer -- TODO
    , tremolo        = chopTakeMaybe  t pg.tremolo
    , trill          = chopTakeMaybe  t pg.trill
    , overdrive      = chopTakeBool   t pg.overdrive
    , bre            = U.trackTake    t pg.bre -- TODO
    , solo           = chopTakeBool   t pg.solo
    , handPosition   = U.trackTake    t pg.handPosition
    , chordRoot      = U.trackTake    t pg.chordRoot
    , noChordNames   = chopTakeBool   t pg.noChordNames
    , slashChords    = chopTakeBool   t pg.slashChords
    , swapAccidental = chopTakeBool   t pg.swapAccidental
    , onyxOctave     = U.trackTake    t pg.onyxOctave
    , onyxString     = U.trackTake    t pg.onyxString
    , mystery45      = chopTakeBool   t pg.mystery45
    , mystery69      = chopTakeBool   t pg.mystery69
    , mystery93      = chopTakeBool   t pg.mystery93
    }
  chopDrop t pg = ProGuitarTrack
    { difficulties   = mapTrack (U.trackDrop t) <$> pg.difficulties -- TODO
    , trainer        = U.trackDrop    t pg.trainer -- TODO
    , tremolo        = chopDropMaybe  t pg.tremolo
    , trill          = chopDropMaybe  t pg.trill
    , overdrive      = chopDropBool   t pg.overdrive
    , bre            = U.trackDrop    t pg.bre -- TODO
    , solo           = chopDropBool   t pg.solo
    , handPosition   = chopDropStatus t pg.handPosition
    , chordRoot      = chopDropStatus t pg.chordRoot
    , noChordNames   = chopDropBool   t pg.noChordNames
    , slashChords    = chopDropBool   t pg.slashChords
    , swapAccidental = chopDropBool   t pg.swapAccidental
    , onyxOctave     = chopDropStatus t pg.onyxOctave
    , onyxString     = chopDropStatus t pg.onyxString
    , mystery45      = chopDropBool   t pg.mystery45
    , mystery69      = chopDropBool   t pg.mystery69
    , mystery93      = chopDropBool   t pg.mystery93
    }

nullPG :: ProGuitarTrack t -> Bool
nullPG = all (RTB.null . (.notes)) . toList . (.difficulties)

instance TraverseTrack ProGuitarTrack where
  traverseTrack fn (ProGuitarTrack a b c d e f g h i j k l m n o p q) = ProGuitarTrack
    <$> traverse (traverseTrack fn) a <*> fn b <*> fn c <*> fn d <*> fn e
    <*> fn f <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k
    <*> fn l <*> fn m <*> fn n <*> fn o <*> fn p <*> fn q

data ProGuitarDifficulty t = ProGuitarDifficulty
  { chordName    :: RTB.T t (Maybe T.Text)
  , force        :: RTB.T t (Edge () PGForce) -- Found by Ruggy, ch13 (1-based) is force strum
  , slide        :: RTB.T t SlideType -- TODO test to make sure this is a blip (has to coincide with the note, can't cover multiple)
  , arpeggio     :: RTB.T t Bool
  , partialChord :: RTB.T t StrumArea
  , allFrets     :: RTB.T t Bool
  , mysteryBFlat :: RTB.T t Bool
  -- TODO EOF format sysexes
  , notes        :: RTB.T t (Edge GtrFret (GtrString, NoteType))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ProGuitarDifficulty t)

instance TraverseTrack ProGuitarDifficulty where
  traverseTrack fn (ProGuitarDifficulty a b c d e f g h) = ProGuitarDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d
    <*> fn e <*> fn f <*> fn g <*> fn h

data PGForce = PGForceStrum | PGForceHOPO
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ChannelType PGForce where
  encodeChannel PGForceHOPO  = 0
  encodeChannel PGForceStrum = 12
  channelMap = [ (c, if c == 12 then PGForceStrum else PGForceHOPO) | c <- [0..15] ]

instance ParseTrack ProGuitarTrack where
  parseTrack = do
    trainer      <- (.trainer)=. let
      parse = toCommand >=> \case
        (t, k) | k == T.pack "pg" -> Just (TypeGuitar, t)
               | k == T.pack "pb" -> Just (TypeBass  , t)
        _ -> Nothing
      unparse (TypeGuitar, t) = fromCommand (t, T.pack "pg")
      unparse (TypeBass  , t) = fromCommand (t, T.pack "pb")
      in commandMatch' parse unparse
    tremolo      <- (.tremolo) =. edgesLanes 126
    trill        <- (.trill) =. edgesLanes 127
    overdrive    <- (.overdrive) =. edges 116
    -- note, we explicitly do TypeGuitar before TypeBass
    -- because we want to try to parse the 6-note version first
    bre          <- (=.) (.bre) $ condenseMap $ eachKey [TypeGuitar, TypeBass] $ edgesBRE . \case
      TypeGuitar -> [120 .. 125]
      TypeBass   -> [120 .. 123]
    solo         <- (.solo) =. edges 115
    handPosition <- (.handPosition) =. let
      fs fret = (0, fret + 100)
      fp (_c, v) = v - 100
      in fatBlips (1/8) $ dimap (fmap fs) (fmap fp) $ blipCV 108
    onyxOctave   <- (.onyxOctave) =. let
      parse = \case
        ["onyx", "octave", x] -> readMaybe $ T.unpack x
        _                     -> Nothing
      unparse n = ["onyx", "octave", T.pack $ show n]
      in commandMatch' parse unparse
    onyxString   <- (.onyxString) =. let
      parse = \case
        "onyx" : "string" : xs -> mapM (readMaybe . ('S' :) . T.unpack) xs
        _                      -> Nothing
      unparse strs = "onyx" : "string" : map (T.pack . drop 1 . show) strs
      in commandMatch' parse unparse
    mystery45    <- (.mystery45) =. edges 45
    mystery69    <- (.mystery69) =. edges 69
    mystery93    <- (.mystery93) =. edges 93
    difficulties <- (=.) (.difficulties) $ eachKey each \diff -> fatBlips (1/8) do
      let base = case diff of
            Easy   -> 24
            Medium -> 48
            Hard   -> 72
            Expert -> 96
      chordSnap [base - 2 .. base + 5]
      notes        <- (.notes) =. let
        fs = \case
          EdgeOn fret (str, nt) -> (str, (nt, Just $ fret + 100))
          EdgeOff (str, nt)     -> (str, (nt, Nothing))
        fp = \case
          (str, (nt, Just v))  -> EdgeOn (v - 100) (str, nt)
          (str, (nt, Nothing)) -> EdgeOff (str, nt)
        in dimap (fmap fs) (fmap fp) $ condenseMap $ eachKey each \str -> channelEdges $ base + getStringIndex 6 str
      force       <- (.force) =. let
        fs = \case
          EdgeOn () sh -> (sh, True )
          EdgeOff   sh -> (sh, False)
        fp (sh, b) = (if b then EdgeOn () else EdgeOff) sh
        in dimap (fmap fs) (fmap fp) $ channelEdges_ (base + 6)
      slide        <- (.slide       ) =. channelBlip_ (base + 7)
      arpeggio     <- (.arpeggio    ) =. edges (base + 8)
      partialChord <- (.partialChord) =. channelBlip_ (base + 9)
      mysteryBFlat <- (.mysteryBFlat) =. edges (base + 10)
      allFrets     <- (.allFrets    ) =. edges (base + 11)
      chordName    <- (.chordName   ) =. let
        cmd = T.pack $ "chrd" ++ show (fromEnum diff)
        parse = \case
          k : cname | k == cmd -> Just case cname of
            []    -> Nothing
            _ : _ -> Just $ T.unwords cname
          _                    -> Nothing
        unparse cname = cmd : toList cname
        in commandMatch' parse unparse
      return ProGuitarDifficulty{..}
    chordRoot    <- (=.) (.chordRoot) $ statusBlips $ condenseMap_ $ eachKey each $ blip . \case
      E  -> 4
      F  -> 5
      Fs -> 6
      G  -> 7
      Gs -> 8
      A  -> 9
      As -> 10
      B  -> 11
      C  -> 12
      Cs -> 13
      D  -> 14
      Ds -> 15
    noChordNames   <- (.noChordNames  ) =. edges 17
    slashChords    <- (.slashChords   ) =. edges 16
    swapAccidental <- (.swapAccidental) =. edges 18
    return ProGuitarTrack{..}

standardGuitar :: [Int]
standardGuitar = [40, 45, 50, 55, 59, 64]

-- | Replicates the Pro Guitar chord name algorithm from RB3.
-- This has been verified to match RB3 for each of the 1486 possible chords.
makeChordName :: Key -> Set.Set Key -> Bool -> T.Text
makeChordName root notes flat = let
  s n = toEnum $ (fromEnum root + n) `rem` 12
  only n = Set.toList (Set.delete root notes) == [n]

  dim2 = Set.member (s  1) notes
  nat2 = Set.member (s  2) notes
  min3 = Set.member (s  3) notes
  maj3 = Set.member (s  4) notes
  nat4 = Set.member (s  5) notes
  dim5 = Set.member (s  6) notes
  aug5 = Set.member (s  8) notes
  nat6 = Set.member (s  9) notes
  min7 = Set.member (s 10) notes
  maj7 = Set.member (s 11) notes

  (base, super)

    | only (s  1) = ("(b2)", "")
    | only (s  2) = ("(2)" , "")
    | only (s  5) = ("(4)" , "")
    | only (s  6) = ("(b5)", "")
    | only (s  7) = ("5"   , "")
    | only (s  8) = ("(b6)", "")
    | only (s  9) = ("(6)" , "")
    | only (s 10) = ("(b7)", "")
    | only (s 11) = ("(7)" , "")

    | maj3 || min3 = let

      sharp9 = if maj3 && min3 then "#9" else ""
      four   = if nat4         then "4"  else ""
      six    = if nat6         then "6"  else ""

      in if min7 || maj7

        then let
          b = (if maj3 then "" else "m") <> if
            | maj7 && not min7           -> "M7"
            | not (nat2 || nat4 || nat6) -> "7"
            | maj7                       -> "M7"
            | otherwise                  -> ""
          flat9  = if dim2 then "b9"  else ""
          nine   = if nat2 then "9"   else ""
          flat13 = if aug5 then "b13" else ""
          fives
            | aug5 && dim5 = "+-5"
            | dim5         = "b5"
            | otherwise    = ""
          in (b, T.concat [fives, nine, flat9, sharp9, four, six, flat13])

        else let
          (b, start) = if dim5
            then if
              | maj3      -> ("", "#4" )
              | aug5      -> ("", "0#5")
              | otherwise -> ("", "0"  )
            else if
              | maj3 && aug5 -> ("" , "+" )
              | maj3         -> ("" , ""  )
              | aug5         -> ("m", "#5")
              | otherwise    -> ("m", ""  )
          two
            | dim2      = "b2"
            | nat2      = "2"
            | otherwise = ""
          in (b, T.concat [start, sharp9, two, four, six])

    | otherwise =
      ( if      min7 then "7"
        else if maj7 then "M7"
        else              ""
      , if nat4 && nat6 then "sus4/6"
        else if nat4 then "sus4"
        else if nat6 then "sus6"
        else if dim5 then "sus#4"
        else if nat2 then "sus2"
        else if dim2 then "susb2"
        else ""
      )

  in showKey flat root <> base <> case super of
    "" -> ""
    _  -> "<gtr>" <> super <> "</gtr>"

data ChordData t = ChordData
  { frets  :: Map.Map GtrString GtrFret
  , name   :: T.Text
  , muted  :: Bool
  , length :: Maybe t
  } deriving (Eq, Ord, Generic)

data ChordModifier = ModSwapAcc | ModNoName | ModSlash
  deriving (Eq, Ord)

computeChordNames :: Difficulty -> [Int] -> Bool -> ProGuitarTrack U.Beats -> RTB.T U.Beats (LongNote T.Text ())
computeChordNames diff tuning flatDefault pg = let

  pgd = fromMaybe mempty $ Map.lookup diff pg.difficulties
  notes
    = applyStatus1 E pg.chordRoot
    $ applyStatus
      (RTB.merge ((ModSwapAcc,) <$> pg.swapAccidental)
        (RTB.merge ((ModNoName,) <$> pg.noChordNames)
          ((ModSlash,) <$> pg.slashChords)))
    $ RTB.collectCoincident
    $ fmap (\(fret, (str, nt), mlen) -> (str, (nt, fret, mlen)))
    $ edgeBlips minSustainLengthRB pgd.notes

  chords :: RTB.T U.Beats (Maybe (ChordData U.Beats))
  chords = notes <&> \(root, (mods, chord)) -> case sort chord of
    low : rest@(_ : _) -> Just ChordData
      { frets = Map.fromList [ (str, fret) | (str, (_, fret, _)) <- chord ]
      , name = if elem ModNoName mods then "" else let
        (slash, chord') = if elem ModSlash mods
          then (Just low, rest)
          else (Nothing, chord)
        flat = flatDefault /= elem ModSwapAcc mods
        keys = Set.fromList $ map getKey chord'
        getKey (str, (_, fret, _)) = toEnum $ (indexTuning tuning str + fret) `rem` 12
        name = makeChordName root keys flat
        in case fmap getKey slash of
          -- Somebody to Love has a Bb chord, with leftmost note Bb,
          -- marked as slash for no reason. It's ignored in game, so we do that
          Just k | k /= root -> name <> "/" <> showKey flat k
          _                  -> name
      , muted = any (\(_, (nt, _, _)) -> nt == Muted) chord
      , length = minimum [ len | (_, (_, _, len)) <- chord ]
      }
    _ -> Nothing

  -- Slide sustains that are the last repetition of their chord don't stick the name.
  -- But it'll still stick if there's another repetition after.
  slides :: RTB.T U.Beats (Maybe (ChordData U.Beats)) -> RTB.T U.Beats (Maybe (ChordData U.Beats))
  slides = let
    go (slideTypes, mcd) = if not $ null slideTypes
      then (\cd -> cd { length = Nothing }) <$> mcd
      else mcd
    in fmap go . applyBlipStatus pgd.slide

  -- For an arpeggio, get the chord computed at the start,
  -- and stretch it for the length of the arpeggio section.
  arps :: RTB.T U.Beats (Maybe (ChordData U.Beats)) -> RTB.T U.Beats (Maybe (ChordData U.Beats))
  arps = let
    go rtb = case RTB.viewL rtb of
      Nothing -> RTB.empty
      Just ((dt, Right mcd), rtb') -> RTB.cons dt mcd $ go rtb'
      Just ((dt, Left arpLen), rtb') -> let
        fn = case catMaybes $ rights $ U.trackTakeZero rtb' of
          []      -> RTB.delay $ dt <> arpLen
          cd  : _ -> RTB.cons dt (Just cd{ length = Just arpLen }) . RTB.delay arpLen
        in fn $ go $ U.trackDrop arpLen rtb'
    in go . RTB.merge (Left <$> arpsList) . fmap Right
  arpsList = fmap (\((), (), t) -> t) $ joinEdgesSimple $ (\b -> if b then EdgeOn () () else EdgeOff ()) <$> pgd.arpeggio

  -- chrdX events override a single chord computed at a certain point.
  overrides :: RTB.T U.Beats (Maybe (ChordData U.Beats)) -> RTB.T U.Beats (Maybe (ChordData U.Beats))
  overrides = let
    go evs = case lefts evs of
      []           -> rights evs
      override : _ -> map (mapped % #name .~ fromMaybe "" override) $ rights evs
    in RTB.flatten . fmap go . RTB.collectCoincident . RTB.merge (Left <$> pgd.chordName) . fmap Right

  -- The same chord strummed multiple times will keep the chord name next to
  -- the strikeline between them. Also, a muted strum can continue a chord name
  -- as long as it has the same chord on both sides of it, and also has the same
  -- frets as the others under the mute channel (even though not shown in game).
  stick :: (NNC.C t) => RTB.T t (Maybe (ChordData t)) -> RTB.T t (ChordData t)
  stick rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, Nothing), rtb') -> RTB.delay dt $ stick rtb'
    Just ((dt, Just x), rtb')
      | x.muted -> RTB.delay dt $ stick rtb'
      | otherwise -> let
        stickThis next = case RTB.viewL next of
          Just ((t, Just y), next') -> if y.frets == x.frets
            then if y.muted
              then first (t <>) <$> stickThis next'
              else Just (t, (y.length, next'))
            else Nothing
          _ -> Nothing
        in case stickThis rtb' of
          Nothing -> RTB.cons dt x $ stick rtb'
          Just (t, (lastLen, next)) -> stick
            $ RTB.cons dt (Just x { length = Just $ fromMaybe NNC.zero lastLen <> t }) $ RTB.delay t next

  in splitEdges
    . RTB.mapMaybe (\cd -> guard (cd.name /= "") >> Just (cd.name, (), cd.length))
    . stick . overrides . arps . slides
    $ chords

-- | Renders all the chord names as explicit override events.
freezeChordNames :: [Int] -> Bool -> ProGuitarTrack U.Beats -> ProGuitarTrack U.Beats
freezeChordNames tuning flatDefault pg = pg
  { difficulties = flip Map.mapWithKey pg.difficulties \diff pgd -> let
    chords = computeChordNames diff tuning flatDefault pg
    in pgd & #chordName .~ flip RTB.mapMaybe chords \case
      NoteOff  () -> Nothing
      Blip   c () -> Just $ Just c
      NoteOn c () -> Just $ Just c
      -- TODO we might need to emit no-name events, often used on lower diffs
  }

-- | If there are no hand positions, adds one to every note.
autoHandPosition :: (NNC.C t) => ProGuitarTrack t -> ProGuitarTrack t
autoHandPosition pg = if RTB.null pg.handPosition
  then let
    frets = foldr RTB.merge RTB.empty do
      pgd <- Map.elems pg.difficulties
      -- note, we do take ArpeggioForm notes into account because Magma does too
      -- TODO do we need to take muted notes into account?
      return $ flip RTB.mapMaybe pgd.notes \case
        EdgeOn fret _ -> Just fret
        EdgeOff _     -> Nothing
    posns = RTB.collectCoincident frets <&> \fs ->
      case filter (/= 0) fs of
        []     -> 0
        f : ft -> foldr min f ft
    -- get rid of any zero positions except maybe the initial position
    posns' = case posns of
      RNil          -> RNil
      Wait t p rest -> Wait t p $ RTB.filter (/= 0) rest
    in pg { handPosition = noRedundantStatus posns' }
  else pg

-- | If there are no chord root notes, sets each chord to have its lowest
-- pitch as the root.
autoChordRoot :: (NNC.C t) => [Int] -> ProGuitarTrack t -> ProGuitarTrack t
autoChordRoot tuning pg = if RTB.null pg.chordRoot
  then let
    getPitch str fret = indexTuning tuning str + fret
    notes = foldr RTB.merge RTB.empty do
      (diff, pgd) <- Map.toList pg.difficulties
      return $ flip RTB.mapMaybe pgd.notes \case
        EdgeOn fret (str, _) -> Just (diff, getPitch str fret)
        EdgeOff _            -> Nothing
    roots = flip RTB.mapMaybe (RTB.collectCoincident notes) \ns -> let
      findChord diff = case map snd $ filter ((== diff) . fst) ns of
        p : ps@(_ : _) -> Just $ toEnum $ foldr min p ps `rem` 12
        _              -> case diff of
          Easy -> Nothing
          _    -> findChord $ pred diff
      in findChord Expert
    in pg { chordRoot = noRedundantStatus roots }
  else pg

-- | Basically like the GRYBO HOPO algorithm, with caveats:
-- * Phantom (arpeggio form) notes count for making a note into a "chord",
--   meaning it won't get auto-HOPO'd
-- * A single note after a chord (including the above kind of chord)
--   won't be auto-HOPO, even if it looks like it could
guitarifyHOPO :: U.Beats -> ProGuitarDifficulty U.Beats
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe U.Beats)
guitarifyHOPO threshold pgd = let
  gtr = joinEdges $ guitarify $ splitEdges
    $ (\(fret, (str, ntype), len) -> ((), (str, fret, ntype), len))
    <$> edgeBlips minSustainLengthRB pgd.notes
    -- TODO above logic is inefficient (join, split, join)
  withForce = applyStatus (forceEdge <$> pgd.force) gtr
  forceEdge = \case
    EdgeOn () PGForceStrum -> (Strum, True )
    EdgeOff   PGForceStrum -> (Strum, False)
    EdgeOn () PGForceHOPO  -> (HOPO , True )
    EdgeOff   PGForceHOPO  -> (HOPO , False)
  fn prev dt (forces, ((), gems, len)) = let
    ntype = if all (\(_, _, nt) -> elem nt [Tapped, ArpeggioForm]) gems
      then Tap
      else case forces of
        nt : _ -> nt
        [] -> if dt > threshold -- guessing this is > like 5-fret, but should test
          then Strum
          else case (prev, gems) of
            (Just [(prevStr, prevFret, prevType)], [(str, fret, typ)])
              |  prevType /= Muted
              && typ /= Muted
              && prevStr == str
              && prevFret /= fret
              -> HOPO
            _ -> Strum
    in (Just gems, Just (ntype, gems, len))
  in trackState Nothing fn withForce

data Slide = SlideUp | SlideDown
  deriving (Eq, Ord, Show, Enum, Bounded)

computeSlides
  :: RTB.T U.Beats SlideType
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe U.Beats)
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe (U.Beats, Maybe Slide))
computeSlides slideMarkers hopo = let
  marked = RTB.collectCoincident $ RTB.merge (Left <$> hopo) (Right <$> slideMarkers)
  marked' = RTB.flatten $ marked <&> \xs -> let
    notes = lefts xs
    slides = rights xs
    in map (, listToMaybe slides) notes
  go = \case
    RNil -> RNil
    Wait dt ((sht, notes, mlen), slide) rest -> let
      msust = case slide of
        Nothing -> (, Nothing) <$> mlen
        Just dir -> let
          lowFret = (\(_, fret, _) -> fret) . minimum -- TODO use NonEmpty for safe minimum
          connect :: Maybe (U.Beats, StrumHOPOTap, GtrFret)
          connect = case rest of
            RNil -> Nothing
            Wait dt' ((sht', chord, _), _) _ -> do
              guard $ (dt' <= 1/3) || (fromMaybe 0 mlen + (1/4) >= dt')
              Just (dt', sht', lowFret chord)
          smallSustain = case rest of
            RNil -> Nothing
            Wait dt' _ _ -> do
              guard $ dt' <= 1/3 -- TODO check exact number
              Just dt'
          computedDir = let
            natural = case connect of
              Nothing -> if lowFret notes < 8 then SlideUp else SlideDown
              Just (_, _, nextFret) -> if lowFret notes < nextFret
                then SlideUp
                else SlideDown
            in case (natural, dir) of
              (SlideDown, ReversedSlide) -> SlideUp
              (SlideUp  , ReversedSlide) -> SlideDown
              _                          -> natural
          in case (mlen, smallSustain) of
            (Nothing, Nothing) -> Nothing -- stub slide
            (Nothing, Just dt') -> Just (dt', Just computedDir) -- non-sustainable note becomes slide
            (Just len, _) -> Just case connect of
              Nothing -> (len, Just computedDir) -- no close next note, normal sustain length
              Just (dt', sht', _) -> case sht' of
                Strum -> (len, Just computedDir) -- next note is strum, no extension
                _     -> (dt', Just computedDir) -- extend sustain to next note (hopo/tap)
                -- TODO does extension happen even if next note is different string?
      in Wait dt (sht, notes, msust) $ go rest
  in go marked'

guitarifyFull :: U.Beats -> ProGuitarDifficulty U.Beats
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe (U.Beats, Maybe Slide))
guitarifyFull threshold pgd = computeSlides pgd.slide $ guitarifyHOPO threshold pgd

-- | Ensures that frets do not go above the given maximum,
-- first by lowering marked sections one octave and then by muting high notes.
pgFretLimit :: (NNC.C t) => Int -> ProGuitarTrack t -> ProGuitarTrack t
pgFretLimit maxFret pg = let
  shouldLower = fmap (>= maxFret) pg.onyxOctave
  doLower _    0 = 0
  doLower down n = if down && n >= 12 then n - 12 else n
  lowerDiff diff = diff
    { notes
      = splitEdgesSimple
      $ fmap (\(down, (fret, (str, nt), len)) -> let
          fret' = doLower down fret
          nt' = if nt /= ArpeggioForm && fret' > maxFret
            then Muted
            else nt
          in (fret', (str, nt'), len)
        )
      $ applyStatus1 False shouldLower
      $ joinEdgesSimple diff.notes
    }
  in pg
    { difficulties = fmap lowerDiff pg.difficulties
    , handPosition
      = fmap (uncurry doLower)
      $ applyStatus1 False shouldLower pg.handPosition
    , onyxOctave = RTB.empty -- fun fact: magma ignores these for some reason
    }

-- | Shifts notes between strings to shrink a 7/8-string part to 6 strings.
moveStrings :: (NNC.C t) => ProGuitarTrack t -> ProGuitarTrack t
moveStrings pg = let
  mappers = pg.onyxString <&> \setting -> let
    strs = case unsnoc setting of
      Nothing      -> [S6 .. S1]
      Just (xs, x) -> xs ++ [x ..]
    table = zip strs [S6 .. S1]
    in \oldString -> case lookup oldString table of
      Nothing -> error $ unwords
        [ "RockBand.Codec.ProGuitar.moveStrings: no string mapping found for"
        , show oldString
        , "in"
        , show setting
        ]
      Just newString -> newString
  moveDiff diff = diff
    { notes
      = splitEdgesSimple
      $ fmap (\(mapper, (fret, (oldString, ntype), len)) -> (fret, (mapper oldString, ntype), len))
      $ applyStatus1 id mappers
      $ joinEdgesSimple diff.notes
    }
  in pg
    { difficulties = fmap moveDiff pg.difficulties
    , onyxString = RTB.empty
    }
