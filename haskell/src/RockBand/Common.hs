{- | Datatypes and functions used across multiple MIDI parsers. -}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
module RockBand.Common where

import           Control.Monad                    (guard)
import           Data.Bifunctor                   (Bifunctor (..))
import           Data.Char                        (isSpace)
import           Data.Data
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (stripPrefix)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Language.Haskell.TH
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

-- | Class for events which are stored as a @\"[x y z]\"@ text event.
class Command a where
  toCommand :: [T.Text] -> Maybe a
  fromCommand :: a -> [T.Text]

reverseLookup :: (Eq b) => [a] -> (a -> b) -> b -> Maybe a
reverseLookup xs f y = let
  pairs = [ (f x, x) | x <- xs ]
  in lookup y pairs

each :: (Enum a, Bounded a) => [a]
each = [minBound .. maxBound]

data Mood
  = Mood_idle_realtime
  | Mood_idle
  | Mood_idle_intense
  | Mood_play
  | Mood_mellow
  | Mood_intense
  | Mood_play_solo
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command Mood where
  fromCommand x = case T.stripPrefix "Mood_" $ T.pack $ show x of
    Nothing -> error "panic! couldn't strip Mood_ from event string"
    Just s  -> [s]
  toCommand = \case
    ["play", "solo"] -> Just Mood_play_solo
    cmd -> reverseLookup each fromCommand cmd

instance Command [T.Text] where
  toCommand   = Just
  fromCommand = id

data Difficulty = Easy | Medium | Hard | Expert
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

readCommand' :: (Command a) => E.T -> Maybe a
readCommand' (E.MetaEvent (Meta.TextEvent s)) = readCommand $ T.pack s
readCommand' (E.MetaEvent (Meta.Lyric s))     = readCommand $ T.pack s
readCommand' _                                = Nothing

readCommandList :: E.T -> Maybe [T.Text]
readCommandList = readCommand'

-- | Turns a string like @\"[foo bar baz]\"@ into some parsed type.
readCommand :: (Command a) => T.Text -> Maybe a
readCommand s =  case T.dropWhile isSpace s of
  (T.uncons -> Just ('[', s'))    -> case T.dropWhile isSpace $ T.reverse s' of
    (T.uncons -> Just (']', s'')) -> toCommand $ T.words $ T.reverse s''
    _                             -> Nothing
  _                               -> Nothing

showCommand' :: (Command a) => a -> E.T
showCommand' = E.MetaEvent . Meta.TextEvent . T.unpack . showCommand

-- | Opposite of 'readCommand'.
showCommand :: (Command a) => a -> T.Text
showCommand ws = "[" <> T.unwords (fromCommand ws) <> "]"

data Trainer
  = TrainerBegin Int
  | TrainerNorm Int
  | TrainerEnd Int
  deriving (Eq, Ord, Show, Read, Typeable, Data)

instance Command (Trainer, T.Text) where
  fromCommand (t, s) = case t of
    TrainerBegin i -> ["begin_" <> s, "song_trainer_" <> s <> "_" <> T.pack (show i)]
    TrainerNorm  i -> [ s <> "_norm", "song_trainer_" <> s <> "_" <> T.pack (show i)]
    TrainerEnd   i -> [  "end_" <> s, "song_trainer_" <> s <> "_" <> T.pack (show i)]
  toCommand [x, T.stripPrefix "song_trainer_" -> Just y] = case x of
    (T.stripPrefix "begin_" -> Just s) -> f s TrainerBegin
    (T.stripSuffix "_norm"  -> Just s) -> f s TrainerNorm
    (T.stripPrefix "end_"   -> Just s) -> f s TrainerEnd
    _                                  -> Nothing
    where f s con = case stripPrefix (T.unpack s) (T.unpack y) of
            Just ('_' : (readMaybe -> Just i)) -> Just (con i, s)
            Just (readMaybe -> Just i)         -> Just (con i, s)
            _                                  -> Nothing
  toCommand _ = Nothing

data Key = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

keyP :: Int -> Q Pat
keyP = \case
  0  -> [p| C  |]
  1  -> [p| Cs |]
  2  -> [p| D  |]
  3  -> [p| Ds |]
  4  -> [p| E  |]
  5  -> [p| F  |]
  6  -> [p| Fs |]
  7  -> [p| G  |]
  8  -> [p| Gs |]
  9  -> [p| A  |]
  10 -> [p| As |]
  11 -> [p| B  |]
  i  -> error $ "keyP: can't make Key pattern from " ++ show i

baseCopyExpert
  :: (NNC.C t, Ord a)
  => (Difficulty -> d -> a)
  -> (a -> Maybe (Difficulty, d))
  -> RTB.T t a
  -> RTB.T t a
baseCopyExpert differ undiffer rtb = let
  (diffEvents, rtb') = RTB.partitionMaybe undiffer rtb
  [e, m, h, x] = flip map [Easy, Medium, Hard, Expert] $ \diff ->
    flip RTB.mapMaybe diffEvents $ \(diff', evt) -> guard (diff == diff') >> return evt
  e' = fmap (differ Easy  ) $ if RTB.null e then x else e
  m' = fmap (differ Medium) $ if RTB.null m then x else m
  h' = fmap (differ Hard  ) $ if RTB.null h then x else h
  x' = fmap (differ Expert) x
  in foldr RTB.merge rtb' [e', m', h', x']

data LongNote s a
  = NoteOff     a
  | Blip      s a
  | NoteOn    s a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable, Data)

instance Bifunctor LongNote where
  first f = \case
    NoteOff  x -> NoteOff      x
    Blip   s x -> Blip   (f s) x
    NoteOn s x -> NoteOn (f s) x
  second = fmap

showEdgesNice :: (NNC.C t, Ord s, Ord a) => t -> RTB.T t (LongNote s a) -> RTB.T t (Maybe s, a)
showEdgesNice defLength = U.trackJoin . go . RTB.collectCoincident where
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, xs), rtb') -> let
      blipLength = case RTB.viewL ons of
        Just ((dt', _), _) -> min defLength dt'
        Nothing            -> defLength
      ons = RTB.filter (\case NoteOff{} -> False; _ -> True) $ RTB.flatten rtb'
      f = \case
        NoteOff  a -> RTB.singleton NNC.zero (Nothing, a)
        NoteOn s a -> RTB.singleton NNC.zero ( Just s, a)
        Blip   s a -> RTB.fromPairList
          [ (NNC.zero  , ( Just s, a))
          , (blipLength, (Nothing, a))
          ]
      in RTB.cons dt (foldr RTB.merge RTB.empty $ map f xs) $ go rtb'

showEdgesNice' :: (NNC.C t, Ord s, Ord a) => t -> RTB.T t (LongNote s a) -> RTB.T t (LongNote s a)
showEdgesNice' defLength trk = flip fmap (showEdgesNice defLength trk) $ \case
  (Nothing, a) -> NoteOff  a
  ( Just s, a) -> NoteOn s a

joinEdges :: (NNC.C t, Eq a) => RTB.T t (LongNote s a) -> RTB.T t (s, a, Maybe t)
joinEdges rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> case x of
    Blip s a -> RTB.cons dt (s, a, Nothing) $ joinEdges rtb'
    NoteOn s a -> let
      isNoteOff (NoteOff a') = guard (a == a') >> Just ()
      isNoteOff _            = Nothing
      in case U.extractFirst isNoteOff rtb' of
        Nothing -> RTB.delay dt $ joinEdges rtb' -- unmatched note on
        Just ((len, ()), rtb'') -> RTB.cons dt (s, a, Just len) $ joinEdges rtb''
    NoteOff _ -> RTB.delay dt $ joinEdges rtb' -- unmatched note off

fillJoinedBlips :: (NNC.C t) => t -> RTB.T t (s, a, Maybe t) -> RTB.T t (s, a, t)
fillJoinedBlips defLength rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, (s, a, len)), rtb') -> let
    len' = flip fromMaybe len $ case dropWhile (== NNC.zero) $ RTB.getTimes rtb' of
      []       -> defLength
      next : _ -> min defLength next
    in RTB.cons dt (s, a, len') $ fillJoinedBlips defLength rtb'

splitEdges :: (NNC.C t, Ord s, Ord a) => RTB.T t (s, a, Maybe t) -> RTB.T t (LongNote s a)
splitEdges = U.trackJoin . fmap f where
  f (s, a, len) = case len of
    Nothing -> RTB.singleton NNC.zero $ Blip s a
    Just t  -> RTB.fromPairList
      [ (NNC.zero, NoteOn s a)
      , (t       , NoteOff  a)
      ]
