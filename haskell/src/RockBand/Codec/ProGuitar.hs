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
import           Data.List.Extra                  (nubOrd, sort, unsnoc)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           Guitars                          (applyStatus, applyStatus1,
                                                   guitarify, trackState)
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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SlideType = NormalSlide | ReversedSlide | MysterySlide3 | MysterySlide2
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data StrumArea = High | Mid | Low | MysteryStrum0
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  deriving (Eq, Ord, Show, Read)

data GtrTuning = GtrTuning
  { gtrBase    :: GtrBase
  , gtrOffsets :: [Int]
  , gtrGlobal  :: Int
  } deriving (Eq, Ord, Show, Read)

instance Default GtrTuning where
  def = GtrTuning Guitar6 [] 0

tuningPitches :: GtrTuning -> [Int]
tuningPitches t
  = map (+ gtrGlobal t)
  $ zipWith (+) (gtrOffsets t ++ repeat 0)
  $ case gtrBase t of
    Guitar6      -> [40, 45, 50, 55, 59, 64]
    Guitar7      -> [35, 40, 45, 50, 55, 59, 64]
    Guitar8      -> [30, 35, 40, 45, 50, 55, 59, 64]
    Bass4        -> [28, 33, 38, 43]
    Bass5        -> [23, 28, 33, 38, 43]
    Bass6        -> [23, 28, 33, 38, 43, 48]
    GtrCustom ps -> ps

-- | Does not include 'gtrGlobal'.
encodeTuningOffsets :: GtrTuning -> GuitarType -> [Int]
encodeTuningOffsets tun typ = let
  std  = tuningPitches $ case typ of
    TypeGuitar -> GtrTuning Guitar6 [] 0
    TypeBass   -> GtrTuning Bass4   [] 0
  n = case typ of
    TypeGuitar -> 6
    TypeBass   -> 4
  this = take n $ tuningPitches tun { gtrGlobal = 0 }
  in map fromIntegral $ zipWith (-) this std

instance ChannelType NoteType where
  encodeChannel = fromEnum

instance ChannelType SlideType where
  encodeChannel = \case
    NormalSlide   -> 0
    ReversedSlide -> 11
    MysterySlide3 -> 3
    MysterySlide2 -> 2

instance ChannelType StrumArea where
  encodeChannel = \case
    High -> 13
    Mid  -> 14
    Low  -> 15
    MysteryStrum0 -> 0

data GuitarType = TypeGuitar | TypeBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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

nullPG :: ProGuitarTrack t -> Bool
nullPG = all (RTB.null . pgNotes) . toList . pgDifficulties

instance TraverseTrack ProGuitarTrack where
  traverseTrack fn (ProGuitarTrack a b c d e f g h i j k l m n o p q) = ProGuitarTrack
    <$> traverse (traverseTrack fn) a <*> fn b <*> fn c <*> fn d <*> fn e
    <*> fn f <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k
    <*> fn l <*> fn m <*> fn n <*> fn o <*> fn p <*> fn q

data ProGuitarDifficulty t = ProGuitarDifficulty
  { pgChordName    :: RTB.T t (Maybe T.Text)
  , pgForceHOPO    :: RTB.T t Bool -- TODO Ruggy found that ch13 (1-based probably) is actually a force strum marker
  , pgSlide        :: RTB.T t (SlideType, Bool) -- TODO do these have to coincide with the note or can you cover a stretch of notes?
  , pgArpeggio     :: RTB.T t Bool
  , pgPartialChord :: RTB.T t (StrumArea, Bool) -- TODO these should be blips, need to be simultaneous with the note
  , pgAllFrets     :: RTB.T t Bool
  , pgMysteryBFlat :: RTB.T t Bool
  -- TODO EOF format sysexes
  , pgNotes        :: RTB.T t (GtrString, (NoteType, GtrFret, Maybe t))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ProGuitarDifficulty t)

instance TraverseTrack ProGuitarDifficulty where
  traverseTrack fn (ProGuitarDifficulty a b c d e f g h) = ProGuitarDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d
    <*> fn e <*> fn f <*> fn g <*> do
      fmap (fmap (\(fret, (str, nt), mlen) -> (str, (nt, fret, mlen))) . joinEdges)
        $ fn
        $ splitEdges
        $ fmap (\(str, (nt, fret, len)) -> (fret, (str, nt), len))
        $ h

instance ParseTrack ProGuitarTrack where
  parseTrack = do
    pgTrainer   <- pgTrainer   =. let
      parse = readCommand' >=> \case
        (t, k) | k == T.pack "pg" -> Just (TypeGuitar, t)
               | k == T.pack "pb" -> Just (TypeBass  , t)
        _ -> Nothing
      unparse (TypeGuitar, t) = showCommand' (t, T.pack "pg")
      unparse (TypeBass  , t) = showCommand' (t, T.pack "pb")
      in single parse unparse
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
      parse = readCommand' >=> \case
        ["onyx", "octave", x] -> readMaybe $ T.unpack x
        _                     -> Nothing
      unparse n = showCommand' ["onyx", "octave", T.pack $ show n]
      in single parse unparse
    pgOnyxString   <- pgOnyxString =. let
      parse = readCommand' >=> \case
        "onyx" : "string" : xs -> mapM (readMaybe . ('S' :) . T.unpack) xs
        _                      -> Nothing
      unparse strs = showCommand' $ "onyx" : "string" : map (T.pack . drop 1 . show) strs
      in single parse unparse
    pgMystery45    <- pgMystery45 =. edges 45
    pgMystery69    <- pgMystery69 =. edges 69
    pgMystery93    <- pgMystery93 =. edges 93
    pgDifficulties <- (pgDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 24
            Medium -> 48
            Hard   -> 72
            Expert -> 96
      pgNotes        <- (pgNotes =.) $ condenseMap $ eachKey each $ \str -> let
        fs (typ, fret, mlen) = (typ, fret + 100, fromMaybe (1/32) mlen)
        fp (typ, v, len) = (typ, v - 100, guard (len > (1/3)) >> Just len)
        in dimap (fmap fs) (fmap fp) $ matchEdgesCV $ channelEdges (base + getStringIndex 6 str)
      pgForceHOPO    <- pgForceHOPO =. edges (base + 6)
      pgSlide        <- pgSlide =. channelEdges_ (base + 7)
      pgArpeggio     <- pgArpeggio =. edges (base + 8)
      pgPartialChord <- pgPartialChord =. channelEdges_ (base + 9)
      pgMysteryBFlat <- pgMysteryBFlat =. edges (base + 10)
      pgAllFrets     <- pgAllFrets =. edges (base + 11)
      pgChordName    <- pgChordName =. let
        cmd = T.pack $ "chrd" ++ show (fromEnum diff)
        parse = readCommand' >=> \case
          [k] | k == cmd -> Just Nothing
          [k, cname] | k == cmd -> Just $ Just cname
          _ -> Nothing
        unparse cname = showCommand' $ cmd : toList cname
        in single parse unparse
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
makeChordName :: Key -> Set.Set Key -> Bool -> String
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

      sharp9 = guard (maj3 && min3) >> "#9"
      four   = guard nat4           >> "4"
      six    = guard nat6           >> "6"

      in if min7 || maj7

        then let
          b = (if maj3 then "" else "m") ++ if
            | maj7 && not min7           -> "M7"
            | not (nat2 || nat4 || nat6) -> "7"
            | maj7                       -> "M7"
            | otherwise                  -> ""
          flat9  = guard dim2 >> "b9"
          nine   = guard nat2 >> "9"
          flat13 = guard aug5 >> "b13"
          fives
            | aug5 && dim5 = "+-5"
            | dim5         = "b5"
            | otherwise    = ""
          in (b, concat [fives, nine, flat9, sharp9, four, six, flat13])

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
          in (b, concat [start, sharp9, two, four, six])

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

  in showKey flat root ++ base ++ case super of
    "" -> ""
    _  -> "<gtr>" ++ super ++ "</gtr>"

data ChordData t = ChordData
  { chordFrets  :: Map.Map GtrString GtrFret
  , chordName   :: T.Text
  , chordMuted  :: Bool
  , chordLength :: Maybe t
  } deriving (Eq, Ord)

data ChordModifier = ModSwapAcc | ModNoName | ModSlash
  deriving (Eq, Ord)

computeChordNames :: forall t. (NNC.C t) => Difficulty -> [Int] -> Bool -> ProGuitarTrack t -> RTB.T t (LongNote T.Text ())
computeChordNames diff tuning flatDefault pg = let

  pgd = fromMaybe mempty $ Map.lookup diff $ pgDifficulties pg
  notes
    = applyStatus1 E (pgChordRoot pg)
    $ applyStatus
      (RTB.merge ((ModSwapAcc,) <$> pgSwapAccidental pg)
        (RTB.merge ((ModNoName,) <$> pgNoChordNames pg)
          ((ModSlash,) <$> pgSlashChords pg)))
    $ RTB.collectCoincident $ pgNotes pgd

  chords :: RTB.T t (Maybe (ChordData t))
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
        name = T.pack $ makeChordName root keys flat
        in case fmap getKey slash of
          -- Somebody to Love has a Bb chord, with leftmost note Bb,
          -- marked as slash for no reason. It's ignored in game, so we do that
          Just k | k /= root -> name <> "/" <> T.pack (showKey flat k)
          _                  -> name
      , chordMuted = any (\(_, (nt, _, _)) -> nt == Muted) chord
      , chordLength = minimum [ len | (_, (_, _, len)) <- chord ]
      }
    _ -> Nothing

  -- Slide sustains that are the last repetition of their chord don't stick the name.
  -- But it'll still stick if there's another repetition after.
  slides :: RTB.T t (Maybe (ChordData t)) -> RTB.T t (Maybe (ChordData t))
  slides = let
    go (isSlide, mcd) = if isSlide
      then (\cd -> cd { chordLength = Nothing }) <$> mcd
      else mcd
    in fmap go . applyStatus1 False (snd <$> pgSlide pgd)

  -- For an arpeggio, get the chord computed at the start,
  -- and stretch it for the length of the arpeggio section.
  arps :: RTB.T t (Maybe (ChordData t)) -> RTB.T t (Maybe (ChordData t))
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
  arpsList = fmap (\((), (), t) -> t) $ joinEdgesSimple $ fmap (\b -> (guard b >> Just (), ())) $ pgArpeggio pgd

  -- chrdX events override a single chord computed at a certain point.
  overrides :: RTB.T t (Maybe (ChordData t)) -> RTB.T t (Maybe (ChordData t))
  overrides = let
    go evs = case lefts evs of
      []           -> rights evs
      override : _ -> map (fmap $ \cd -> cd { chordName = fromMaybe "" override }) $ rights evs
    in RTB.flatten . fmap go . RTB.collectCoincident . RTB.merge (Left <$> pgChordName pgd) . fmap Right

  -- The same chord strummed multiple times will keep the chord name next to
  -- the strikeline between them. Also, a muted strum can continue a chord name
  -- as long as it has the same chord on both sides of it, and also has the same
  -- frets as the others under the mute channel (even though not shown in game).
  stick :: RTB.T t (Maybe (ChordData t)) -> RTB.T t (ChordData t)
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
freezeChordNames :: (NNC.C t) => [Int] -> Bool -> ProGuitarTrack t -> ProGuitarTrack t
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
      return $ fmap (\(_, (_, fret, _)) -> fret) $ pgNotes pgd
    posns = flip fmap (RTB.collectCoincident frets) $ \fs ->
      case filter (/= 0) fs of
        []     -> 0
        f : ft -> foldr min f ft
    -- get rid of any zero positions except maybe the initial position
    posns' = case posns of
      RNil          -> RNil
      Wait t p rest -> Wait t p $ RTB.filter (/= 0) rest
    in pg { pgHandPosition = posns' }
  else pg

-- | If there are no chord root notes, sets each chord to have its lowest
-- pitch as the root.
autoChordRoot :: (NNC.C t) => [Int] -> ProGuitarTrack t -> ProGuitarTrack t
autoChordRoot tuning pg = if RTB.null $ pgChordRoot pg
  then let
    getPitch str fret = indexTuning tuning str + fret
    notes = foldr RTB.merge RTB.empty $ do
      pgd <- Map.elems $ pgDifficulties pg
      return $ fmap (\(str, (_, fret, _)) -> getPitch str fret) $ pgNotes pgd
    roots = flip RTB.mapMaybe (RTB.collectCoincident notes) $ \ns ->
      case nubOrd ns of
        p : ps@(_ : _) -> Just $ toEnum $ foldr min p ps `rem` 12
        _              -> Nothing
    -- TODO maybe remove duplicate roots
    in pg { pgChordRoot = roots }
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
    $ (\(str, (ntype, fret, len)) -> ((), (str, fret, ntype), len))
    <$> pgNotes pgd
  withForce = applyStatus ((HOPO,) <$> pgForceHOPO pgd) gtr
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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

computeSlides
  :: ProGuitarDifficulty U.Beats
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe U.Beats)
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe (U.Beats, Maybe Slide))
computeSlides pgd hopo = let
  slideMarkers = RTB.mapMaybe (\(dir, b) -> guard b >> return dir) $ pgSlide pgd
  marked = RTB.collectCoincident $ RTB.merge (Left <$> hopo) (Right <$> slideMarkers)
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
      = fmap (\(down, (str, (nt, fret, mt))) -> let
          fret' = doLower down fret
          nt' = if nt /= ArpeggioForm && fret' > maxFret
            then Muted
            else nt
          in (str, (nt', fret', mt))
        )
      $ applyStatus1 False shouldLower
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
      = fmap (\(mapper, (oldString, triple)) -> (mapper oldString, triple))
      $ applyStatus1 id mappers
      $ pgNotes diff
    }
  in pg
    { pgDifficulties = fmap moveDiff $ pgDifficulties pg
    , pgOnyxString = RTB.empty
    }
