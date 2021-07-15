{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module RockBand.Codec.Vocal where

import           Control.Monad                    ((>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.StackTrace
import           Data.Char                        (isAscii)
import           Data.DTA.Serialize.Magma         (Percussion (..))
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List.Extra                  (nubOrd)
import           Data.Maybe                       (fromMaybe, mapMaybe)
import qualified Data.Text                        as T
import           DeriveHelpers
import qualified FretsOnFire                      as FoF
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import qualified Sound.MIDI.Util                  as U

data Pitch
  = Octave36 Key
  | Octave48 Key
  | Octave60 Key
  | Octave72 Key
  | Octave84C
  deriving (Eq, Ord, Show, Generic)
  deriving (Enum, Bounded) via GenericFullEnum Pitch

pitchToKey :: Pitch -> Key
pitchToKey = \case
  Octave36 k -> k
  Octave48 k -> k
  Octave60 k -> k
  Octave72 k -> k
  Octave84C  -> C

parsePercAnimation :: (Monad m, NNC.C t) => TrackEvent m t (Percussion, Bool)
parsePercAnimation = let
  startEnd b = if b then "_start" else "_end"
  unparse :: (Percussion, Bool) -> [T.Text]
  unparse (typ, b) = case typ of
    Tambourine -> ["tambourine" <> startEnd b]
    Cowbell    -> ["cowbell"    <> startEnd b]
    Handclap   -> ["clap"       <> startEnd b]
  parse :: [T.Text] -> Maybe (Percussion, Bool)
  parse = reverseLookup ((,) <$> each <*> each) unparse
  in commandMatch' (toCommand >=> parse) (fromCommand . unparse)

data VocalTrack t = VocalTrack
  { vocalMood          :: RTB.T t Mood
  , vocalLyrics        :: RTB.T t T.Text
  , vocalPerc          :: RTB.T t () -- ^ playable percussion notes
  , vocalPercSound     :: RTB.T t () -- ^ nonplayable percussion, only triggers sound sample
  , vocalPercAnimation :: RTB.T t (Percussion, Bool)
  , vocalPhrase1       :: RTB.T t Bool -- ^ General phrase marker (RB3) or Player 1 phrases (pre-RB3)
  , vocalPhrase2       :: RTB.T t Bool -- ^ Pre-RB3, used for 2nd player phrases in Tug of War
  , vocalOverdrive     :: RTB.T t Bool
  , vocalLyricShift    :: RTB.T t ()
  , vocalRangeShift    :: RTB.T t Bool
  , vocalNotes         :: RTB.T t (Pitch, Bool)
  , vocalEyesClosed    :: RTB.T t Bool -- ^ Temporary event for lipsync generation (this will be moved to dedicated lipsync track)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (VocalTrack t)

nullVox :: VocalTrack t -> Bool
-- we look at lyrics also, so lyrics can be imported from PS/CH into vox tracks
nullVox t = RTB.null (vocalNotes t) && RTB.null (vocalLyrics t)

instance TraverseTrack VocalTrack where
  traverseTrack fn (VocalTrack a b c d e f g h i j k l) = VocalTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f
    <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k <*> fn l

instance ParseTrack VocalTrack where
  parseTrack = do
    vocalMood   <- vocalMood   =. command
    -- this also gets lyrics in non-lyric text events
    vocalLyrics <- vocalLyrics =. lyrics
    vocalPerc          <- vocalPerc          =. fatBlips (1/8) (blip 96)
    vocalPercSound     <- vocalPercSound     =. fatBlips (1/8) (blip 97)
    vocalPercAnimation <- vocalPercAnimation =. parsePercAnimation
    vocalPhrase1       <- vocalPhrase1       =. edges 105
    vocalPhrase2       <- vocalPhrase2       =. edges 106
    vocalOverdrive     <- vocalOverdrive     =. edges 116
    vocalLyricShift    <- vocalLyricShift    =. fatBlips (1/8) (blip 1)
    vocalRangeShift    <- vocalRangeShift    =. edges 0
    vocalNotes         <- (vocalNotes        =.)
      $ condenseMap $ eachKey each $ edges . (+ 36) . fromEnum
    vocalEyesClosed <- (vocalEyesClosed =.) $ condenseMap_ $ eachKey each $ \case
      True  -> commandMatch ["eyes", "close"]
      False -> commandMatch ["eyes", "open" ]
    return VocalTrack{..}

asciify :: T.Text -> T.Text
asciify = let
  oneToOne = zip
    "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝŸàáâãäåçèéêëìíîïðñòóôõö÷øùúûüýÿ"
    "AAAAAACEEEEIIIIDNOOOOOxOUUUUYYaaaaaaceeeeiiiidnooooo/ouuuuyy"
  f 'Æ' = "AE"
  f 'Þ' = "Th"
  f 'ß' = "ss"
  f 'æ' = "ae"
  f 'þ' = "th"
  f c   = T.singleton $ fromMaybe (if isAscii c then c else '?') $ lookup c oneToOne
  in T.concatMap f

-- | Phase Shift doesn't support non-ASCII chars in lyrics.
-- (RB text events are always Latin-1, even if .dta encoding is UTF-8.)
asciiLyrics :: VocalTrack t -> VocalTrack t
asciiLyrics vt = vt { vocalLyrics = fmap asciify $ vocalLyrics vt }

-- | Strips some CH-format tags used in lyric events.
stripTags :: VocalTrack t -> VocalTrack t
stripTags vt = vt
  { vocalLyrics = FoF.stripTags <$> vocalLyrics vt
  }

data TalkyDifficulty
  = TalkyNormal -- hash
  | TalkyEasy -- caret
  deriving (Show)

data Lyric = Lyric
  { lyricText      :: T.Text
  , lyricContinues :: Bool -- True if not the end of a word
  } deriving (Show)

data LyricNote
  = Pitched Pitch           Lyric
  | Talky   TalkyDifficulty Lyric
  | SlideTo Pitch
  deriving (Show)

joinMatchingTracks :: (NNC.C t) => RTB.T t a -> RTB.T t b -> Either (t, Bool) (RTB.T t (a, b))
joinMatchingTracks = go NNC.zero [] where
  go !elapsed pairs (Wait tx x xs) (Wait ty y ys) = case compare tx ty of
    EQ -> go (elapsed <> tx) ((tx, (x, y)) : pairs) xs ys
    LT -> Left (elapsed <> tx, False)
    GT -> Left (elapsed <> ty, True)
  go !elapsed _ (Wait tx _ _) RNil = Left (elapsed <> tx, False)
  go !elapsed _ RNil (Wait ty _ _) = Left (elapsed <> ty, True)
  go _ pairs RNil RNil = Right $ RTB.fromPairList $ reverse pairs

getLyricNotes :: (Monad m) => U.MeasureMap -> VocalTrack U.Beats -> StackTraceT m (RTB.T U.Beats (LyricNote, U.Beats))
getLyricNotes mmap vox = let
  joinedNotes = joinEdgesSimple $ flip fmap (vocalNotes vox) $ \(p, b) -> if b then EdgeOn () p else EdgeOff p
  in case joinMatchingTracks joinedNotes $ vocalLyrics vox of
    Left (errorTime, isLyricNoNote) -> fatal $ if isLyricNoNote
      then "Lyric without a corresponding note at " <> showPosition mmap errorTime
      else "Note without a corresponding lyric at " <> showPosition mmap errorTime
    Right joined -> return $ flip fmap joined $ \(((), pitch, len), lyric) -> let
      lyricNote = case lyric of
        "+" -> SlideTo pitch
        _   -> case T.stripSuffix "#" lyric of
          Just talky -> Talky TalkyNormal $ makeLyric talky
          Nothing -> case T.stripSuffix "^" lyric of
            Just talky -> Talky TalkyEasy $ makeLyric talky
            Nothing    -> Pitched pitch $ makeLyric lyric
      makeLyric str = case T.stripSuffix "-" str of
        Just str' -> Lyric { lyricText = str', lyricContinues = True  }
        Nothing   -> case T.stripSuffix "=" str of
          Just str' -> Lyric { lyricText = str' <> "-", lyricContinues = True  }
          Nothing   -> Lyric { lyricText = str        , lyricContinues = False }
      in (lyricNote, len)
  -- TODO handle the section-symbol-encoded spanish vowel elision char?
  -- TODO handle Footloose and Fancy Free style space hack (strip lyric)?

putLyricNotes :: (NNC.C t) => RTB.T t (LyricNote, t) -> VocalTrack t
putLyricNotes lnotes = mempty
  { vocalLyrics = flip fmap lnotes $ \(lnote, _) -> case lnote of
    Pitched _  (Lyric t cont) -> t <> if cont then "-" else ""
    Talky diff (Lyric t cont) -> T.concat
      [ case (T.stripSuffix "-" t, cont) of
        (Just undashed, True ) -> undashed <> "="
        (Nothing      , True ) -> t <> "-"
        (_            , False) -> t
      , case diff of TalkyNormal -> "#"; TalkyEasy -> "^"
      ]
    SlideTo _                 -> "+"
  , vocalNotes = U.trackJoin $ flip fmap lnotes $ \(lnote, dur) -> let
    pitch = case lnote of
      Pitched p _ -> p
      Talky   _ _ -> minBound
      SlideTo p   -> p
    in RTB.fromPairList [(NNC.zero, (pitch, True)), (dur, (pitch, False))]
  }

getPhraseLabels :: (Num t, NNC.C t) => RTB.T t Bool -> RTB.T t (LyricNote, t) -> RTB.T t T.Text
getPhraseLabels phrases lnotes = let
  eachPhrase lnoteList = let
    lyricList = flip mapMaybe lnoteList $ \case
      Pitched _ lyric -> Just lyric
      Talky   _ lyric -> Just lyric
      SlideTo _       -> Nothing
    in T.strip $ T.concat $ flip map lyricList $ \lyric ->
      if lyricContinues lyric
        then lyricText lyric
        else lyricText lyric <> " "
  joinedPhrases = joinEdgesSimple $ flip fmap phrases $ \b -> if b then EdgeOn () () else EdgeOff ()
  getPhrase start len = map fst $ RTB.getBodies $ U.trackTake len $ U.trackDrop start lnotes
  in RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ map (\(start, ((), (), len)) -> (start, eachPhrase $ getPhrase start len))
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0 joinedPhrases

vocalPhraseAll :: (NNC.C t) => VocalTrack t -> RTB.T t Bool
vocalPhraseAll vt = RTB.flatten $ fmap nubOrd $ RTB.collectCoincident $ RTB.merge
  (vocalPhrase1 vt)
  (vocalPhrase2 vt)
