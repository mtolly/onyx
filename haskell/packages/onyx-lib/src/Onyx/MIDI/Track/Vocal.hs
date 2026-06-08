{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns        #-}
module Onyx.MIDI.Track.Vocal where

import           Control.Monad                     ((>=>))
import           Control.Monad.Codec
import           Data.Char                         (isAscii)
import qualified Data.EventList.Absolute.TimeBody  as ATB
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Functor                      ((<&>))
import           Data.List.Extra                   (nubOrd)
import           Data.Maybe                        (fromMaybe, mapMaybe)
import qualified Data.Text                         as T
import           GHC.Generics                      (Generic)
import qualified Numeric.NonNegative.Class         as NNC
import           Onyx.DeriveHelpers
import           Onyx.FretsOnFire                  (stripTags)
import           Onyx.Harmonix.DTA.Serialize.Magma (Percussion (..))
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.StackTrace
import qualified Sound.MIDI.Util                   as U

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
  { mood                :: RTB.T t Mood
  , lyrics              :: RTB.T t T.Text
  , percussion          :: RTB.T t () -- ^ playable percussion notes
  , percussionSound     :: RTB.T t () -- ^ nonplayable percussion, only triggers sound sample
  , percussionAnimation :: RTB.T t (Percussion, Bool)
  , phrase1             :: RTB.T t Bool -- ^ General phrase marker (RB3) or Player 1 phrases (pre-RB3)
  , phrase2             :: RTB.T t Bool -- ^ Pre-RB3, used for 2nd player phrases in Tug of War
  , overdrive           :: RTB.T t Bool
  , lyricShift          :: RTB.T t ()
  , rangeShift          :: RTB.T t Bool
  , notes               :: RTB.T t (Pitch, Bool)
  , eyesClosed          :: RTB.T t Bool -- ^ Temporary event for lipsync generation (this will be moved to dedicated lipsync track)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (VocalTrack t)

nullVox :: VocalTrack t -> Bool
-- we look at lyrics also, so lyrics can be imported from PS/CH into vox tracks
nullVox t = RTB.null t.notes && RTB.null t.lyrics

instance TraverseTrack VocalTrack where
  traverseTrack fn (VocalTrack a b c d e f g h i j k l) = VocalTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f
    <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k <*> fn l

instance ParseTrack VocalTrack where
  parseTrack = do
    mood                <- (.mood               ) =. command
    -- this also gets lyrics in non-lyric text events
    lyrics              <- (.lyrics             ) =. lyricEvents
    percussion          <- (.percussion         ) =. fatBlips (1/8) (blip 96)
    percussionSound     <- (.percussionSound    ) =. fatBlips (1/8) (blip 97)
    percussionAnimation <- (.percussionAnimation) =. parsePercAnimation
    phrase1             <- (.phrase1            ) =. edges 105
    phrase2             <- (.phrase2            ) =. edges 106
    overdrive           <- (.overdrive          ) =. edges 116
    lyricShift          <- (.lyricShift         ) =. fatBlips (1/8) (blip 1)
    rangeShift          <- (.rangeShift         ) =. edges 0
    notes               <- (.notes              ) =. do
      condenseMap $ eachKey each $ edges . (+ 36) . fromEnum
    eyesClosed          <- (.eyesClosed         ) =. do
      condenseMap_ $ eachKey each \case
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
asciiLyrics vt = vt { lyrics = asciify <$> vt.lyrics }

-- | Strips CH-format tags used in lyric events.
stripLyricTags :: VocalTrack t -> VocalTrack t
stripLyricTags vt = vt
  { lyrics = stripTags <$> vt.lyrics
  }

data TalkyDifficulty
  = TalkyNormal -- hash
  | TalkyEasy -- caret
  deriving (Show)

data Lyric = Lyric
  { lyricText      :: T.Text
  , lyricContinues :: Bool -- True if not the end of a word
  } deriving (Eq, Ord, Show)

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

-- First tries to match notes and lyrics exactly on time.
-- Then if there are the same count of each, moves lyrics to match with notes.
-- Otherwise, error if there are different counts.
joinNotesLyrics :: (NNC.C t, SendMessage m) => (t -> String) -> RTB.T t a -> RTB.T t T.Text -> StackTraceT m (RTB.T t (a, T.Text))
joinNotesLyrics showTime notes lyrics = case joinMatchingTracks notes lyrics of
  Right perfect -> return perfect
  Left (errorTime, isLyricNoNote) -> let
    err = if isLyricNoNote
      then "Lyric without a corresponding note at " <> showTime errorTime
      else "Note without a corresponding lyric at " <> showTime errorTime
    in if length notes == length lyrics
      then do
        warn $ "Adjusting vocal lyric times to match notes; " <> err
        return $ RTB.fromPairList $ zipWith
          (\(dt, note) (_, lyric) -> (dt, (note, lyric)))
          (RTB.toPairList notes)
          (RTB.toPairList lyrics)
      else fatal err

fixLyricPositions :: (SendMessage m) => U.MeasureMap -> VocalTrack U.Beats -> StackTraceT m (VocalTrack U.Beats)
fixLyricPositions mmap vox = let
  joinedNotes = joinEdgesSimple $ vox.notes <&> \(p, b) -> if b then EdgeOn () p else EdgeOff p
  in do
    joined <- joinNotesLyrics (showPosition mmap) joinedNotes vox.lyrics
    return vox
      { notes = splitEdgesSimple (fst <$> joined) <&> \case
        EdgeOn () p -> (p, True )
        EdgeOff   p -> (p, False)
      , lyrics = snd <$> joined
      }

getLyricNotes :: (SendMessage m) => U.MeasureMap -> VocalTrack U.Beats -> StackTraceT m (RTB.T U.Beats (LyricNote, U.Beats))
getLyricNotes mmap vox = let
  joinedNotes = joinEdgesSimple $ vox.notes <&> \(p, b) -> if b then EdgeOn () p else EdgeOff p
  in do
    joined <- joinNotesLyrics (showPosition mmap) joinedNotes vox.lyrics
    return $ joined <&> \(((), pitch, len), lyric) -> let
      lyricNote = case lyric of
        "+" -> SlideTo pitch
        _   -> case T.stripSuffix "#" lyric of
          Just talky -> Talky TalkyNormal $ makeLyric talky
          Nothing -> case T.stripSuffix "^" lyric of
            Just talky -> Talky TalkyEasy $ makeLyric talky
            Nothing    -> Pitched pitch $ makeLyric lyric
      makeLyric (decodeSynalepha -> str) = case T.stripSuffix "-" str of
        Just str' -> Lyric { lyricText = str', lyricContinues = True  }
        Nothing   -> case T.stripSuffix "=" str of
          Just str' -> Lyric { lyricText = str' <> "-", lyricContinues = True  }
          Nothing   -> Lyric { lyricText = str        , lyricContinues = False }
      decodeSynalepha s = if T.elem '§' s
        then T.map (\case '§' -> '‿'; c -> c) s
        else s
      in (lyricNote, len)
  -- TODO handle Footloose and Fancy Free style space hack (strip lyric)?

putLyricNotes :: (NNC.C t) => RTB.T t (LyricNote, t) -> VocalTrack t
putLyricNotes lnotes = mempty
  { lyrics = lnotes <&> \(lnote, _) -> encodeSynalepha $ case lnote of
    Pitched _  (Lyric t cont) -> t <> if cont then "-" else ""
    Talky diff (Lyric t cont) -> T.concat
      [ case (T.stripSuffix "-" t, cont) of
        (Just undashed, True ) -> undashed <> "="
        (Nothing      , True ) -> t <> "-"
        (_            , False) -> t
      , case diff of TalkyNormal -> "#"; TalkyEasy -> "^"
      ]
    SlideTo _                 -> "+"
  , notes = U.trackJoin $ lnotes <&> \(lnote, dur) -> let
    pitch = case lnote of
      Pitched p _ -> p
      Talky   _ _ -> minBound
      SlideTo p   -> p
    in RTB.fromPairList [(NNC.zero, (pitch, True)), (dur, (pitch, False))]
  } where
    encodeSynalepha s = if T.elem '‿' s
      then T.map (\case '‿' -> '§'; c -> c) s
      else s

getPhraseLabels :: (Num t, NNC.C t) => RTB.T t Bool -> RTB.T t (LyricNote, t) -> RTB.T t T.Text
getPhraseLabels phrases lnotes = let
  eachPhrase lnoteList = let
    lyricList = flip mapMaybe lnoteList \case
      Pitched _ lyric -> Just lyric
      Talky   _ lyric -> Just lyric
      SlideTo _       -> Nothing
    in T.strip $ T.concat $ lyricList <&> \lyric ->
      if lyric.lyricContinues
        then lyric.lyricText
        else lyric.lyricText <> " "
  joinedPhrases = joinEdgesSimple $ phrases <&> \b -> if b then EdgeOn () () else EdgeOff ()
  getPhrase start len = map fst $ RTB.getBodies $ U.trackTake len $ U.trackDrop start lnotes
  in RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ map (\(start, ((), (), len)) -> (start, eachPhrase $ getPhrase start len))
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0 joinedPhrases

vocalPhraseAll :: (NNC.C t) => VocalTrack t -> RTB.T t Bool
vocalPhraseAll vt = RTB.flatten $ fmap nubOrd $ RTB.collectCoincident
  $ RTB.merge vt.phrase1 vt.phrase2
