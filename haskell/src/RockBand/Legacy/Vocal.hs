{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module RockBand.Legacy.Vocal
( Percussion(..), Pitch(..)
, Event(..)
, asciify
, asciiLyrics
, fixGHVocals
, vocalFromLegacy, vocalToLegacy
) where

import           Data.DTA.Serialize.Magma         (Percussion (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (partition)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Vocal
import           RockBand.Common
import qualified Sound.MIDI.Util                  as U

data Event
  = LyricShift
  | Mood Mood
  | Lyric T.Text
  | Percussion -- ^ playable percussion note
  | PercussionSound -- ^ nonplayable percussion note, only triggers sound sample
  | PercussionAnimation Percussion Bool
  | Phrase     Bool -- ^ General phrase marker (RB3) or Player 1 phrases (pre-RB3)
  | Phrase2    Bool -- ^ Pre-RB3, used for 2nd player phrases in Tug of War
  | Overdrive  Bool
  | RangeShift Bool
  | Note       Bool Pitch
  deriving (Eq, Ord, Show)

vocalFromLegacy :: (NNC.C t) => RTB.T t Event -> VocalTrack t
vocalFromLegacy leg = VocalTrack
  { vocalMood          = RTB.mapMaybe (\case Mood x -> Just x; _ -> Nothing) leg
  , vocalLyrics        = RTB.mapMaybe (\case Lyric x -> Just x; _ -> Nothing) leg
  , vocalPerc          = RTB.mapMaybe (\case Percussion -> Just (); _ -> Nothing) leg
  , vocalPercSound     = RTB.mapMaybe (\case PercussionSound -> Just (); _ -> Nothing) leg
  , vocalPercAnimation = RTB.mapMaybe (\case PercussionAnimation x y -> Just (x, y); _ -> Nothing) leg
  , vocalPhrase1       = RTB.mapMaybe (\case Phrase x -> Just x; _ -> Nothing) leg
  , vocalPhrase2       = RTB.mapMaybe (\case Phrase2 x -> Just x; _ -> Nothing) leg
  , vocalOverdrive     = RTB.mapMaybe (\case Overdrive x -> Just x; _ -> Nothing) leg
  , vocalLyricShift    = RTB.mapMaybe (\case LyricShift -> Just (); _ -> Nothing) leg
  , vocalRangeShift    = RTB.mapMaybe (\case RangeShift x -> Just x; _ -> Nothing) leg
  , vocalNotes         = RTB.mapMaybe (\case Note b p -> Just (p, b); _ -> Nothing) leg
  , vocalEyesClosed    = RTB.empty
  }

vocalToLegacy :: (NNC.C t) => VocalTrack t -> RTB.T t Event
vocalToLegacy o = foldr RTB.merge RTB.empty
  [ Mood                        <$> vocalMood          o
  , Lyric                       <$> vocalLyrics        o
  , const Percussion            <$> vocalPerc          o
  , const PercussionSound       <$> vocalPercSound     o
  , uncurry PercussionAnimation <$> vocalPercAnimation o
  , Phrase                      <$> vocalPhrase1       o
  , Phrase2                     <$> vocalPhrase2       o
  , Overdrive                   <$> vocalOverdrive     o
  , const LyricShift            <$> vocalLyricShift    o
  , RangeShift                  <$> vocalRangeShift    o
  , uncurry (flip Note)         <$> vocalNotes         o
  ]

{- |
Fix issues seen in Phase Shift conversions of Guitar Hero vocals charts:
1. + lyric at the end of a note that slides into the next
2. syllable connection on a + (so, +-) instead of the last non-+ lyric
3. talkies are just lyrics (with no #) that don't have notes
4. phrases sometimes end a few ticks early so notes go past the end
-}
fixGHVocals :: RTB.T U.Beats Event -> RTB.T U.Beats Event
fixGHVocals
  = RTB.flatten
  . fixEarlyPhraseEnd . fixTalkies
  . RTB.fromPairList
  . reverse
  . fixPlusDash . map (fmap fixEndPlus)
  . reverse
  . RTB.toPairList
  . RTB.collectCoincident
  where

  -- if the lyric "+" (or "GO!"?) appears with no note-on, remove it
  fixEndPlus evts = let
    noteOn  = [ () | Note True  _ <- evts ]
    (plus, notPlus) = partition (`elem` [Lyric "+", Lyric "GO!", Lyric "+-"]) evts
    in if null noteOn && not (null plus)
      then notPlus
      else evts
  -- if (in the reversed list) we find an instant with a +- lyric,
  -- remove it, and then put the dash back on the next real lyric
  -- (where next means back in time because list is reversed)
  fixPlusDash [] = []
  fixPlusDash ((t, evts) : rest) = case partition (== Lyric "+-") evts of
    ([], _) -> (t, evts) : fixPlusDash rest
    (_ : _, notPlusDash) -> let
      evts' = Lyric "+" : notPlusDash
      in (t, evts') : fixPlusDash (addDash rest)
  -- find the next non-plus lyric and add the dash
  addDash [] = [] -- shouldn't happen, whatever
  addDash ((t, evts) : rest) = let
    lyrics = [ s | Lyric s <- evts ]
    notLyrics = filter (\case Lyric _ -> False; _ -> True) evts
    in case lyrics of
      s : _ | T.take 1 s /= "+" -> let
        evts' = Lyric (s <> "-") : notLyrics
        in (t, evts') : rest
      _ -> (t, evts) : addDash rest
  -- in normal-order event-list, if we find a lyric with no note-on,
  -- add a tiny note to it, and make it a (tiny, lenient) talky
  fixTalkies rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evts), rtb') -> let
      noteOn = [ () | Note True _ <- evts ]
      lyrics = [ s | Lyric s <- evts ]
      notLyrics = filter (\case Lyric _ -> False; _ -> True) evts
      in case (lyrics, noteOn) of
        (s : _, []) -> let
          evts' = Lyric (s <> "^") : Note True (Octave60 C) : notLyrics
          rtb'' = RTB.insert (1/16) [Note False (Octave60 C)] rtb'
          in RTB.cons dt evts' $ fixTalkies rtb''
        _ -> RTB.cons dt evts $ fixTalkies rtb'
  -- fix phrases that end a few ticks before last note ends
  fixEarlyPhraseEnd rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evts), rtb') -> RTB.cons dt evts $ fixEarlyPhraseEnd
      $ if null [ () | Note True _ <- evts ]
        then rtb'
        else pushPhraseEnd False rtb'
  pushPhraseEnd False rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty -- probably shouldn't happen
    Just ((dt, evts), rtb') -> if not $ null [ () | Note False _ <- evts ]
      then rtb -- saw note end on or before phrase end, all good
      else case partition (== Phrase False) evts of
        ([], _)        -> RTB.cons dt evts  $ pushPhraseEnd False rtb' -- keep looking
        (_ : _, evts') -> RTB.cons dt evts' $ pushPhraseEnd True  rtb' -- time to push!
  pushPhraseEnd True rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty -- definitely shouldn't happen!
    Just ((dt, evts), rtb') -> if not $ null [ () | Note False _ <- evts ]
      then RTB.cons dt (Phrase False : evts) rtb' -- note off is here, push complete
      else RTB.cons dt evts $ pushPhraseEnd True rtb'
