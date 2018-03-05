{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
module RockBand.Vocals where

import           Data.Data
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (partition)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Parse
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U

data Event
  = LyricShift
  | Mood Mood
  | Lyric T.Text
  | Percussion -- ^ playable percussion note
  | PercussionSound -- ^ nonplayable percussion note, only triggers sound sample
  | PercussionAnimation PercussionType Bool
  | Phrase     Bool -- ^ General phrase marker (RB3) or Player 1 phrases (pre-RB3)
  | Phrase2    Bool -- ^ Pre-RB3, used for 2nd player phrases in Tug of War
  | Overdrive  Bool
  | RangeShift Bool
  | Note       Bool Pitch
  deriving (Eq, Ord, Show, Read, Typeable, Data)

data Pitch
  = Octave36 Key
  | Octave48 Key
  | Octave60 Key
  | Octave72 Key
  | Octave84C
  deriving (Eq, Ord, Show, Read, Typeable, Data)

pitchToKey :: Pitch -> Key
pitchToKey = \case
  Octave36 k -> k
  Octave48 k -> k
  Octave60 k -> k
  Octave72 k -> k
  Octave84C  -> C

instance Enum Pitch where
  fromEnum (Octave36 k) = fromEnum k
  fromEnum (Octave48 k) = fromEnum k + 12
  fromEnum (Octave60 k) = fromEnum k + 24
  fromEnum (Octave72 k) = fromEnum k + 36
  fromEnum Octave84C    = 48
  toEnum i = case divMod i 12 of
    (0, j) -> Octave36 $ toEnum j
    (1, j) -> Octave48 $ toEnum j
    (2, j) -> Octave60 $ toEnum j
    (3, j) -> Octave72 $ toEnum j
    (4, 0) -> Octave84C
    _      -> error $ "No vocals Pitch for: fromEnum " ++ show i

instance Bounded Pitch where
  minBound = Octave36 minBound
  maxBound = Octave84C

data PercussionType
  = Tambourine
  | Cowbell
  | Clap
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command (PercussionType, Bool) where
  fromCommand (typ, b) = [T.toLower (T.pack $ show typ) <> if b then "_start" else "_end"]
  toCommand = reverseLookup ((,) <$> each <*> each) fromCommand

instanceMIDIEvent [t| Event |] Nothing

  [ edge 0 $ applyB [p| RangeShift |]
  , blip 1 [p| LyricShift |]

  -- TODO: unknown notes on pitch 12, 13, 14

  , edgeRange [36..47] $ \_i _b -> [p| Note $(boolP _b) (Octave36 $(keyP $ _i - 36)) |]
  , edgeRange [48..59] $ \_i _b -> [p| Note $(boolP _b) (Octave48 $(keyP $ _i - 48)) |]
  , edgeRange [60..71] $ \_i _b -> [p| Note $(boolP _b) (Octave60 $(keyP $ _i - 60)) |]
  , edgeRange [72..83] $ \_i _b -> [p| Note $(boolP _b) (Octave72 $(keyP $ _i - 72)) |]
  , edge      84       $ \   _b -> [p| Note $(boolP _b) Octave84C                    |]

  , blip 96 [p| Percussion |]
  , blip 97 [p| PercussionSound |]
  , edge 105 $ applyB [p| Phrase |]
  , edge 106 $ applyB [p| Phrase2 |]
  , edge 116 $ applyB [p| Overdrive |]

  , ( [e| one $ mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| one $ mapParseOne (uncurry PercussionAnimation) parseCommand |]
    , [e| \case PercussionAnimation t b -> unparseCommand (t, b) |]
    )
  , ( [e| one $ firstEventWhich $ \case
        E.MetaEvent (Meta.Lyric s) -> Just $ Lyric $ T.pack s
        E.MetaEvent (Meta.TextEvent s) -> Just $ Lyric $ T.pack s
        -- unrecognized text events are lyrics by default.
        -- but note that this must come after the mood and perc-anim parsers!
        -- TODO maybe don't do this if the text parses as a command
        _ -> Nothing
      |]
    , [e| \case Lyric s -> RTB.singleton NNC.zero $ E.MetaEvent $ Meta.Lyric $ T.unpack s |]
    )
  ]

-- | Phase Shift doesn't support non-ASCII chars in lyrics.
-- (RB text events are always Latin-1, even if .dta encoding is UTF-8.)
asciiLyrics :: Event -> Event
asciiLyrics (Lyric t) = let
  oneToOne = zip
    "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõö÷øùúûüýÿ"
    "AAAAAACEEEEIIIIDNOOOOOxOUUUUYaaaaaaceeeeiiiidnooooo/ouuuuyy"
  f 'Æ' = "AE"
  f 'Þ' = "Th"
  f 'ß' = "ss"
  f 'æ' = "ae"
  f 'þ' = "th"
  f c   = T.singleton $ fromMaybe c $ lookup c oneToOne
  in Lyric $ T.concatMap f t
asciiLyrics e = e

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
