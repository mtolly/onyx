{- | Datatypes and functions used across multiple MIDI parsers. -}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE ViewPatterns           #-}
module RockBand.Common where

import           Control.Monad                    (guard)
import           Data.Bifunctor                   (Bifunctor (..))
import           Data.Char                        (isSpace)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (stripPrefix)
import           Data.Maybe                       (fromMaybe, isJust)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Message.Channel       as C
import qualified Sound.MIDI.Message.Channel.Voice as V
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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Mood where
  fromCommand x = case T.stripPrefix "Mood_" $ T.pack $ show x of
    Nothing -> error "panic! couldn't strip Mood_ from event string"
    Just s  -> [s]
  toCommand = \case
    ["idle_mellow"] -> Just Mood_idle -- seen in RB4 midi converts
    ["play", "solo"] -> Just Mood_play_solo
    cmd -> reverseLookup each fromCommand cmd

instance Command [T.Text] where
  toCommand   = Just
  fromCommand = id

data Difficulty = Easy | Medium | Hard | Expert
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readCommand' :: (Command a) => E.T -> Maybe a
readCommand' (E.MetaEvent (Meta.TextEvent s)) = readCommand $ T.pack s
readCommand' (E.MetaEvent (Meta.Lyric s))     = readCommand $ T.pack s
readCommand' _                                = Nothing

readCommandList :: E.T -> Maybe [T.Text]
readCommandList = readCommand'

-- | Turns a string like @\"[foo bar baz]\"@ into some parsed type.
readCommand :: (Command a) => T.Text -> Maybe a
readCommand s =  case T.dropWhile isSpace s of
  (T.uncons -> Just ('[', s'))       -> case T.span (/= ']') s' of
    (s'', T.uncons -> Just (']', _)) -> toCommand $ let
      -- split into words, and also split before open parens.
      -- this makes it easier to handle [lighting (foo)] and [lighting(foo)]
      go t = case T.uncons t of
        Nothing      -> []
        Just (_, t') -> case T.findIndex (\c -> c == ' ' || c == '(') t' of
          Nothing -> [t]
          Just i  -> case T.splitAt (i + 1) t of
            (t1, t2) -> t1 : go t2
      in filter (not . T.null) $ map T.strip $ go s''
    _                                -> Nothing
  _                                  -> Nothing

showCommand' :: (Command a) => a -> E.T
showCommand' = E.MetaEvent . Meta.TextEvent . T.unpack . showCommand

-- | Opposite of 'readCommand'.
showCommand :: (Command a) => a -> T.Text
showCommand ws = "[" <> T.unwords (fromCommand ws) <> "]"

data Trainer
  = TrainerBegin Int
  | TrainerNorm Int
  | TrainerEnd Int
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

showKey :: Bool -> Key -> String
showKey flat = if flat
  then \case
    C  -> "C"
    Cs -> "Db"
    D  -> "D"
    Ds -> "Eb"
    E  -> "E"
    F  -> "F"
    Fs -> "Gb"
    G  -> "G"
    Gs -> "Ab"
    A  -> "A"
    As -> "Bb"
    B  -> "B"
  else map (\case 's' -> '#'; c -> c) . show

class HasDiffEvent d a | a -> d where
  makeDiffEvent :: Difficulty -> d -> a
  unmakeDiffEvent :: a -> Maybe (Difficulty, d)

eachDifficulty
  :: (NNC.C t, Ord a, HasDiffEvent d a)
  => (RTB.T t d -> RTB.T t d)
  -> RTB.T t a
  -> RTB.T t a
eachDifficulty f rtb = let
  (diffEvents, rtb') = RTB.partitionMaybe unmakeDiffEvent rtb
  [e, m, h, x] = flip map [Easy, Medium, Hard, Expert] $ \diff ->
    flip RTB.mapMaybe diffEvents $ \(diff', evt) -> guard (diff == diff') >> return evt
  e' = fmap (makeDiffEvent Easy  ) $ f e
  m' = fmap (makeDiffEvent Medium) $ f m
  h' = fmap (makeDiffEvent Hard  ) $ f h
  x' = fmap (makeDiffEvent Expert) $ f x
  in foldr RTB.merge rtb' [e', m', h', x']

data LongNote s a
  = NoteOff     a
  | Blip      s a
  | NoteOn    s a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

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

joinEdgesSimple :: (NNC.C t, Eq a) => RTB.T t (Maybe s, a) -> RTB.T t (s, a, t)
joinEdgesSimple rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> case x of
    (Just s, a) -> let
      isNoteOff (Nothing, a') = guard (a == a') >> Just ()
      isNoteOff _             = Nothing
      in case U.extractFirst isNoteOff rtb' of
        Nothing                 -> RTB.delay dt $ joinEdgesSimple rtb' -- unmatched note on
        Just ((len, ()), rtb'') -> RTB.cons dt (s, a, len) $ joinEdgesSimple rtb''
    (Nothing, _) -> RTB.delay dt $ joinEdgesSimple rtb' -- unmatched note off

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

splitEdgesSimple :: (NNC.C t, Ord s, Ord a) => RTB.T t (s, a, t) -> RTB.T t (Maybe s, a)
splitEdgesSimple = U.trackJoin . fmap f where
  f (s, a, t) = RTB.fromPairList
    [ (NNC.zero, (Just s , a))
    , (t       , (Nothing, a))
    ]

splitEdges :: (NNC.C t, Ord s, Ord a) => RTB.T t (s, a, Maybe t) -> RTB.T t (LongNote s a)
splitEdges = U.trackJoin . fmap f where
  f (s, a, len) = case len of
    Nothing -> RTB.singleton NNC.zero $ Blip s a
    Just t  -> RTB.fromPairList
      [ (NNC.zero, NoteOn s a)
      , (t       , NoteOff  a)
      ]

isNoteEdgeCPV :: E.T -> Maybe (Int, Int, Maybe Int)
isNoteEdgeCPV = \case
  E.MIDIEvent (C.Cons c (C.Voice (V.NoteOn  p v))) ->
    Just (C.fromChannel c, V.fromPitch p, case V.fromVelocity v of 0 -> Nothing; v' -> Just v')
  E.MIDIEvent (C.Cons c (C.Voice (V.NoteOff p _))) ->
    Just (C.fromChannel c, V.fromPitch p, Nothing)
  _ -> Nothing

isNoteEdge :: E.T -> Maybe (Int, Bool)
isNoteEdge e = isNoteEdgeCPV e >>= \(_c, p, v) -> return (p, isJust v)

unparseBlipCPV :: (Int, Int, Int) -> RTB.T U.Beats E.T
unparseBlipCPV (c, p, v) = RTB.fromPairList
  [ (0     , makeEdgeCPV c p $ Just v)
  , (1 / 32, makeEdgeCPV c p Nothing )
  ]

makeEdgeCPV :: Int -> Int -> Maybe Int -> E.T
makeEdgeCPV c p v = E.MIDIEvent $ C.Cons (C.toChannel c) $ C.Voice $
  V.NoteOn (V.toPitch p) $ maybe (V.toVelocity 0) V.toVelocity v

makeEdge :: Int -> Bool -> E.T
makeEdge p b = makeEdgeCPV 0 p $ guard b >> Just 96

data StrumHOPOTap = Strum | HOPO | Tap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
