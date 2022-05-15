{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module RockBand.Codec.ProGuitar where

import           Control.Arrow                    (first)
import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import           Data.Default.Class               (Default (..))
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List.Extra                  (sort, unsnoc)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           Guitars                          (applyBlipStatus, applyStatus,
                                                   applyStatus1, guitarify,
                                                   trackState)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
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
  } deriving (Eq, Ord, Show)

instance Default GtrTuning where
  def = GtrTuning Guitar6 [] 0 0

tuningPitches :: GtrTuning -> [Int]
tuningPitches t
  = map (+ (gtrGlobal t + gtrCapo t))
  $ zipWith (+) (gtrOffsets t ++ repeat 0)
  $ case gtrBase t of
    Guitar6      -> [40, 45, 50, 55, 59, 64]
    Guitar7      -> [35, 40, 45, 50, 55, 59, 64]
    Guitar8      -> [30, 35, 40, 45, 50, 55, 59, 64]
    Bass4        -> [28, 33, 38, 43]
    Bass5        -> [23, 28, 33, 38, 43]
    Bass6        -> [23, 28, 33, 38, 43, 48]
    GtrCustom ps -> ps

-- True if this should use string colors starting from purple (5/6-string bass or similar)
lowBassTuning :: GtrTuning -> Bool
lowBassTuning tuning = case gtrBase tuning of
  Bass5             -> True
  Bass6             -> True
  GtrCustom (n : _) -> n < 28
  _                 -> False

-- | RB3 style (does not include 'gtrGlobal' or 'gtrCapo').
encodeTuningOffsets :: GtrTuning -> GuitarType -> [Int]
encodeTuningOffsets tun typ = let
  std  = tuningPitches $ case typ of
    TypeGuitar -> GtrTuning Guitar6 [] 0 0
    TypeBass   -> GtrTuning Bass4   [] 0 0
  n = case typ of
    TypeGuitar -> 6
    TypeBass   -> 4
  this = take n $ tuningPitches tun { gtrGlobal = 0, gtrCapo = 0 }
  in map fromIntegral $ zipWith (-) this std

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
    High -> 13
    Mid  -> 14
    Low  -> 15
    MysteryStrum0 -> 0

data GuitarType = TypeGuitar | TypeBass
  deriving (Eq, Ord, Show, Enum, Bounded)

data ProGuitarTrack t = ProGuitarTrack
  { pgDifficulties   :: Map.Map Difficulty (ProGuitarDifficulty t)
  , pgTrainer        :: RTB.T t (GuitarType, Trainer)
  , pgTremolo        :: RTB.T t (Maybe LaneDifficulty)
  , pgTrill          :: RTB.T t (Maybe LaneDifficulty)
  , pgOverdrive      :: RTB.T t Bool
  , pgBRE            :: RTB.T t (GuitarType, Bool)
  , pgSolo           :: RTB.T t Bool
  , pgHandPosition   :: RTB.T t GtrFret
  , pgChordRoot      :: RTB.T t Key
  , pgNoChordNames   :: RTB.T t Bool
  , pgSlashChords    :: RTB.T t Bool
  , pgSwapAccidental :: RTB.T t Bool
  -- ^ according to Ruggy, default sharp/flat is according to usual sheet music
  -- rules for the key + tonality
  , pgOnyxOctave     :: RTB.T t GtrFret -- "move these notes 12 frets down if needed" section
  , pgOnyxString     :: RTB.T t [GtrString] -- which strings RB should use (default S6..S1)
  , pgMystery45      :: RTB.T t Bool
  , pgMystery69      :: RTB.T t Bool
  , pgMystery93      :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ProGuitarTrack t)

instance ChopTrack ProGuitarTrack where
  chopTake t pg = ProGuitarTrack
    { pgDifficulties   = mapTrack (U.trackTake t) <$> pgDifficulties pg -- TODO
    , pgTrainer        = U.trackTake    t $ pgTrainer        pg -- TODO
    , pgTremolo        = chopTakeMaybe  t $ pgTremolo        pg
    , pgTrill          = chopTakeMaybe  t $ pgTrill          pg
    , pgOverdrive      = chopTakeBool   t $ pgOverdrive      pg
    , pgBRE            = U.trackTake    t $ pgBRE            pg -- TODO
    , pgSolo           = chopTakeBool   t $ pgSolo           pg
    , pgHandPosition   = U.trackTake    t $ pgHandPosition   pg
    , pgChordRoot      = U.trackTake    t $ pgChordRoot      pg
    , pgNoChordNames   = chopTakeBool   t $ pgNoChordNames   pg
    , pgSlashChords    = chopTakeBool   t $ pgSlashChords    pg
    , pgSwapAccidental = chopTakeBool   t $ pgSwapAccidental pg
    , pgOnyxOctave     = U.trackTake    t $ pgOnyxOctave     pg
    , pgOnyxString     = U.trackTake    t $ pgOnyxString     pg
    , pgMystery45      = chopTakeBool   t $ pgMystery45      pg
    , pgMystery69      = chopTakeBool   t $ pgMystery69      pg
    , pgMystery93      = chopTakeBool   t $ pgMystery93      pg
    }
  chopDrop t pg = ProGuitarTrack
    { pgDifficulties   = mapTrack (U.trackDrop t) <$> pgDifficulties pg -- TODO
    , pgTrainer        = U.trackDrop    t $ pgTrainer        pg -- TODO
    , pgTremolo        = chopDropMaybe  t $ pgTremolo        pg
    , pgTrill          = chopDropMaybe  t $ pgTrill          pg
    , pgOverdrive      = chopDropBool   t $ pgOverdrive      pg
    , pgBRE            = U.trackDrop    t $ pgBRE            pg -- TODO
    , pgSolo           = chopDropBool   t $ pgSolo           pg
    , pgHandPosition   = chopDropStatus t $ pgHandPosition   pg
    , pgChordRoot      = chopDropStatus t $ pgChordRoot      pg
    , pgNoChordNames   = chopDropBool   t $ pgNoChordNames   pg
    , pgSlashChords    = chopDropBool   t $ pgSlashChords    pg
    , pgSwapAccidental = chopDropBool   t $ pgSwapAccidental pg
    , pgOnyxOctave     = chopDropStatus t $ pgOnyxOctave     pg
    , pgOnyxString     = chopDropStatus t $ pgOnyxString     pg
    , pgMystery45      = chopDropBool   t $ pgMystery45      pg
    , pgMystery69      = chopDropBool   t $ pgMystery69      pg
    , pgMystery93      = chopDropBool   t $ pgMystery93      pg
    }

nullPG :: ProGuitarTrack t -> Bool
nullPG = all (RTB.null . pgNotes) . toList . pgDifficulties

instance TraverseTrack ProGuitarTrack where
  traverseTrack fn (ProGuitarTrack a b c d e f g h i j k l m n o p q) = ProGuitarTrack
    <$> traverse (traverseTrack fn) a <*> fn b <*> fn c <*> fn d <*> fn e
    <*> fn f <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k
    <*> fn l <*> fn m <*> fn n <*> fn o <*> fn p <*> fn q

data ProGuitarDifficulty t = ProGuitarDifficulty
  { pgChordName    :: RTB.T t (Maybe T.Text)
  , pgForce        :: RTB.T t (Edge () PGForce) -- Found by Ruggy, ch13 (1-based?) is force strum
  , pgSlide        :: RTB.T t SlideType -- TODO test to make sure this is a blip (has to coincide with the note, can't cover multiple)
  , pgArpeggio     :: RTB.T t Bool
  , pgPartialChord :: RTB.T t StrumArea
  , pgAllFrets     :: RTB.T t Bool
  , pgMysteryBFlat :: RTB.T t Bool
  -- TODO EOF format sysexes
  , pgNotes        :: RTB.T t (Edge GtrFret (GtrString, NoteType))
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
  encodeChannel PGForceStrum = 14
  channelMap = [ (c, if c == 14 then PGForceStrum else PGForceHOPO) | c <- [0..15] ]

instance ParseTrack ProGuitarTrack where
  parseTrack = do
    pgTrainer   <- pgTrainer   =. let
      parse = toCommand >=> \case
        (t, k) | k == T.pack "pg" -> Just (TypeGuitar, t)
               | k == T.pack "pb" -> Just (TypeBass  , t)
        _ -> Nothing
      unparse (TypeGuitar, t) = fromCommand (t, T.pack "pg")
      unparse (TypeBass  , t) = fromCommand (t, T.pack "pb")
      in commandMatch' parse unparse
    pgTremolo      <- pgTremolo =. edgesLanes 126
    pgTrill        <- pgTrill =. edgesLanes 127
    pgOverdrive    <- pgOverdrive =. edges 116
    -- note, we explicitly do TypeGuitar before TypeBass
    -- because we want to try to parse the 6-note version first
    pgBRE          <- (pgBRE =.) $ condenseMap $ eachKey [TypeGuitar, TypeBass] $ edgesBRE . \case
      TypeGuitar -> [120 .. 125]
      TypeBass   -> [120 .. 123]
    pgSolo         <- pgSolo =. edges 115
    pgHandPosition <- pgHandPosition =. let
      fs fret = (0, fret + 100)
      fp (_c, v) = v - 100
      in fatBlips (1/8) $ dimap (fmap fs) (fmap fp) $ blipCV 108
    pgOnyxOctave   <- pgOnyxOctave =. let
      parse = \case
        ["onyx", "octave", x] -> readMaybe $ T.unpack x
        _                     -> Nothing
      unparse n = ["onyx", "octave", T.pack $ show n]
      in commandMatch' parse unparse
    pgOnyxString   <- pgOnyxString =. let
      parse = \case
        "onyx" : "string" : xs -> mapM (readMaybe . ('S' :) . T.unpack) xs
        _                      -> Nothing
      unparse strs = "onyx" : "string" : map (T.pack . drop 1 . show) strs
      in commandMatch' parse unparse
    pgMystery45    <- pgMystery45 =. edges 45
    pgMystery69    <- pgMystery69 =. edges 69
    pgMystery93    <- pgMystery93 =. edges 93
    pgDifficulties <- (pgDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 24
            Medium -> 48
            Hard   -> 72
            Expert -> 96
      pgNotes        <- (pgNotes =.) $ let
        fs = \case
          EdgeOn fret (str, nt) -> (str, (nt, Just $ fret + 100))
          EdgeOff (str, nt) -> (str, (nt, Nothing))
        fp = \case
          (str, (nt, Just v)) -> EdgeOn (v - 100) (str, nt)
          (str, (nt, Nothing)) -> EdgeOff (str, nt)
        in dimap (fmap fs) (fmap fp) $ condenseMap $ eachKey each $ \str -> channelEdges $ base + getStringIndex 6 str
      pgForce       <- pgForce =. let
        fs = \case
          EdgeOn () sh -> (sh, True )
          EdgeOff   sh -> (sh, False)
        fp (sh, b) = (if b then EdgeOn () else EdgeOff) sh
        in dimap (fmap fs) (fmap fp) $ channelEdges_ (base + 6)
      pgSlide        <- pgSlide =. channelBlip_ (base + 7)
      pgArpeggio     <- pgArpeggio =. edges (base + 8)
      pgPartialChord <- pgPartialChord =. channelBlip_ (base + 9)
      pgMysteryBFlat <- pgMysteryBFlat =. edges (base + 10)
      pgAllFrets     <- pgAllFrets =. edges (base + 11)
      pgChordName    <- pgChordName =. let
        cmd = T.pack $ "chrd" ++ show (fromEnum diff)
        parse = \case
          [k] | k == cmd -> Just Nothing
          [k, cname] | k == cmd -> Just $ Just cname
          _ -> Nothing
        unparse cname = cmd : toList cname
        in commandMatch' parse unparse
      return ProGuitarDifficulty{..}
    pgChordRoot    <- (pgChordRoot =.) $ statusBlips $ condenseMap_ $ eachKey each $ blip . \case
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
    pgNoChordNames   <- pgNoChordNames   =. edges 17
    pgSlashChords    <- pgSlashChords    =. edges 16
    pgSwapAccidental <- pgSwapAccidental =. edges 18
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
  { chordFrets  :: Map.Map GtrString GtrFret
  , chordName   :: T.Text
  , chordMuted  :: Bool
  , chordLength :: Maybe t
  } deriving (Eq, Ord)

data ChordModifier = ModSwapAcc | ModNoName | ModSlash
  deriving (Eq, Ord)

computeChordNames :: Difficulty -> [Int] -> Bool -> ProGuitarTrack U.Beats -> RTB.T U.Beats (LongNote T.Text ())
computeChordNames diff tuning flatDefault pg = let

  pgd = fromMaybe mempty $ Map.lookup diff $ pgDifficulties pg
  notes
    = applyStatus1 E (pgChordRoot pg)
    $ applyStatus
      (RTB.merge ((ModSwapAcc,) <$> pgSwapAccidental pg)
        (RTB.merge ((ModNoName,) <$> pgNoChordNames pg)
          ((ModSlash,) <$> pgSlashChords pg)))
    $ RTB.collectCoincident
    $ fmap (\(fret, (str, nt), mlen) -> (str, (nt, fret, mlen)))
    $ edgeBlips minSustainLengthRB
    $ pgNotes pgd

  chords :: RTB.T U.Beats (Maybe (ChordData U.Beats))
  chords = flip fmap notes $ \(root, (mods, chord)) -> case sort chord of
    low : rest@(_ : _) -> Just ChordData
      { chordFrets = Map.fromList [ (str, fret) | (str, (_, fret, _)) <- chord ]
      , chordName = if elem ModNoName mods then "" else let
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
      , chordMuted = any (\(_, (nt, _, _)) -> nt == Muted) chord
      , chordLength = minimum [ len | (_, (_, _, len)) <- chord ]
      }
    _ -> Nothing

  -- Slide sustains that are the last repetition of their chord don't stick the name.
  -- But it'll still stick if there's another repetition after.
  slides :: RTB.T U.Beats (Maybe (ChordData U.Beats)) -> RTB.T U.Beats (Maybe (ChordData U.Beats))
  slides = let
    go (slideTypes, mcd) = if not $ null slideTypes
      then (\cd -> cd { chordLength = Nothing }) <$> mcd
      else mcd
    in fmap go . applyBlipStatus (pgSlide pgd)

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
          cd  : _ -> RTB.cons dt (Just cd{ chordLength = Just arpLen }) . RTB.delay arpLen
        in fn $ go $ U.trackDrop arpLen rtb'
    in go . RTB.merge (Left <$> arpsList) . fmap Right
  arpsList = fmap (\((), (), t) -> t) $ joinEdgesSimple $ (\b -> if b then EdgeOn () () else EdgeOff ()) <$> pgArpeggio pgd

  -- chrdX events override a single chord computed at a certain point.
  overrides :: RTB.T U.Beats (Maybe (ChordData U.Beats)) -> RTB.T U.Beats (Maybe (ChordData U.Beats))
  overrides = let
    go evs = case lefts evs of
      []           -> rights evs
      override : _ -> map (fmap $ \cd -> cd { chordName = fromMaybe "" override }) $ rights evs
    in RTB.flatten . fmap go . RTB.collectCoincident . RTB.merge (Left <$> pgChordName pgd) . fmap Right

  -- The same chord strummed multiple times will keep the chord name next to
  -- the strikeline between them. Also, a muted strum can continue a chord name
  -- as long as it has the same chord on both sides of it, and also has the same
  -- frets as the others under the mute channel (even though not shown in game).
  stick :: (NNC.C t) => RTB.T t (Maybe (ChordData t)) -> RTB.T t (ChordData t)
  stick rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, Nothing), rtb') -> RTB.delay dt $ stick rtb'
    Just ((dt, Just x), rtb')
      | chordMuted x -> RTB.delay dt $ stick rtb'
      | otherwise -> let
        stickThis next = case RTB.viewL next of
          Just ((t, Just y), next') -> if chordFrets y == chordFrets x
            then if chordMuted y
              then first (t <>) <$> stickThis next'
              else Just (t, (chordLength y, next'))
            else Nothing
          _ -> Nothing
        in case stickThis rtb' of
          Nothing -> RTB.cons dt x $ stick rtb'
          Just (t, (lastLen, next)) -> stick
            $ RTB.cons dt (Just x { chordLength = Just $ fromMaybe NNC.zero lastLen <> t }) $ RTB.delay t next

  in splitEdges
    . RTB.mapMaybe (\cd -> guard (chordName cd /= "") >> Just (chordName cd, (), chordLength cd))
    . stick . overrides . arps . slides
    $ chords

-- | Renders all the chord names as explicit override events.
freezeChordNames :: [Int] -> Bool -> ProGuitarTrack U.Beats -> ProGuitarTrack U.Beats
freezeChordNames tuning flatDefault pg = pg
  { pgDifficulties = flip Map.mapWithKey (pgDifficulties pg) $ \diff pgd -> let
    chords = computeChordNames diff tuning flatDefault pg
    in pgd
      { pgChordName = flip RTB.mapMaybe chords $ \case
        NoteOff  () -> Nothing
        Blip   c () -> Just $ Just c
        NoteOn c () -> Just $ Just c
        -- TODO we might need to emit no-name events, often used on lower diffs
      }
  }

-- | If there are no hand positions, adds one to every note.
autoHandPosition :: (NNC.C t) => ProGuitarTrack t -> ProGuitarTrack t
autoHandPosition pg = if RTB.null $ pgHandPosition pg
  then let
    frets = foldr RTB.merge RTB.empty $ do
      pgd <- Map.elems $ pgDifficulties pg
      -- note, we do take ArpeggioForm notes into account because Magma does too
      -- TODO do we need to take muted notes into account?
      return $ flip RTB.mapMaybe (pgNotes pgd) $ \case
        EdgeOn fret _ -> Just fret
        EdgeOff _ -> Nothing
    posns = flip fmap (RTB.collectCoincident frets) $ \fs ->
      case filter (/= 0) fs of
        []     -> 0
        f : ft -> foldr min f ft
    -- get rid of any zero positions except maybe the initial position
    posns' = case posns of
      RNil          -> RNil
      Wait t p rest -> Wait t p $ RTB.filter (/= 0) rest
    in pg { pgHandPosition = noRedundantStatus posns' }
  else pg

-- | If there are no chord root notes, sets each chord to have its lowest
-- pitch as the root.
autoChordRoot :: (NNC.C t) => [Int] -> ProGuitarTrack t -> ProGuitarTrack t
autoChordRoot tuning pg = if RTB.null $ pgChordRoot pg
  then let
    getPitch str fret = indexTuning tuning str + fret
    notes = foldr RTB.merge RTB.empty $ do
      (diff, pgd) <- Map.toList $ pgDifficulties pg
      return $ flip RTB.mapMaybe (pgNotes pgd) $ \case
        EdgeOn fret (str, _) -> Just (diff, getPitch str fret)
        EdgeOff _ -> Nothing
    roots = flip RTB.mapMaybe (RTB.collectCoincident notes) $ \ns -> let
      findChord diff = case map snd $ filter ((== diff) . fst) ns of
        p : ps@(_ : _) -> Just $ toEnum $ foldr min p ps `rem` 12
        _              -> case diff of
          Easy -> Nothing
          _    -> findChord $ pred diff
      in findChord Expert
    in pg { pgChordRoot = noRedundantStatus roots }
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
    <$> edgeBlips minSustainLengthRB (pgNotes pgd)
    -- TODO above logic is inefficient (join, split, join)
  withForce = applyStatus (forceEdge <$> pgForce pgd) gtr
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
  :: ProGuitarDifficulty U.Beats
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe U.Beats)
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe (U.Beats, Maybe Slide))
computeSlides pgd hopo = let
  marked = RTB.collectCoincident $ RTB.merge (Left <$> hopo) (Right <$> pgSlide pgd)
  marked' = RTB.flatten $ flip fmap marked $ \xs -> let
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
            (Just len, _) -> Just $ case connect of
              Nothing -> (len, Just computedDir) -- no close next note, normal sustain length
              Just (dt', sht', _) -> case sht' of
                Strum -> (len, Just computedDir) -- next note is strum, no extension
                _     -> (dt', Just computedDir) -- extend sustain to next note (hopo/tap)
                -- TODO does extension happen even if next note is different string?
      in Wait dt (sht, notes, msust) $ go rest
  in go marked'

guitarifyFull :: U.Beats -> ProGuitarDifficulty U.Beats
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe (U.Beats, Maybe Slide))
guitarifyFull threshold pgd = computeSlides pgd $ guitarifyHOPO threshold pgd

-- | Ensures that frets do not go above the given maximum,
-- first by lowering marked sections one octave and then by muting high notes.
fretLimit :: (NNC.C t) => Int -> ProGuitarTrack t -> ProGuitarTrack t
fretLimit maxFret pg = let
  shouldLower = fmap (>= maxFret) $ pgOnyxOctave pg
  doLower _    0 = 0
  doLower down n = if down && n >= 12 then n - 12 else n
  lowerDiff diff = diff
    { pgNotes
      = splitEdgesSimple
      $ fmap (\(down, (fret, (str, nt), len)) -> let
          fret' = doLower down fret
          nt' = if nt /= ArpeggioForm && fret' > maxFret
            then Muted
            else nt
          in (fret', (str, nt'), len)
        )
      $ applyStatus1 False shouldLower
      $ joinEdgesSimple
      $ pgNotes diff
    }
  in pg
    { pgDifficulties = fmap lowerDiff $ pgDifficulties pg
    , pgHandPosition
      = fmap (uncurry doLower)
      $ applyStatus1 False shouldLower
      $ pgHandPosition pg
    , pgOnyxOctave = RTB.empty -- fun fact: magma ignores these for some reason
    }

-- | Shifts notes between strings to shrink a 7/8-string part to 6 strings.
moveStrings :: (NNC.C t) => ProGuitarTrack t -> ProGuitarTrack t
moveStrings pg = let
  mappers = flip fmap (pgOnyxString pg) $ \setting -> let
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
    { pgNotes
      = splitEdgesSimple
      $ fmap (\(mapper, (fret, (oldString, ntype), len)) -> (fret, (mapper oldString, ntype), len))
      $ applyStatus1 id mappers
      $ joinEdgesSimple
      $ pgNotes diff
    }
  in pg
    { pgDifficulties = fmap moveDiff $ pgDifficulties pg
    , pgOnyxString = RTB.empty
    }
