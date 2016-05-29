{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module MelodysEscape where

import RockBand.Parse
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.List (intercalate, partition)
import Data.Maybe (mapMaybe)
import Control.Monad.Random
import qualified Numeric.NonNegative.Class as NNC

data Event
  = Intensity        Bool  Intensity
  | Note      (Maybe Bool) NoteType
  deriving (Eq, Ord, Show, Read)

data NoteType
  = Obstacle Direction
  | Color    Direction
  | Cutscene
  deriving (Eq, Ord, Show, Read)

data Intensity = Low | Neutral | High | ExtraHigh
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Direction = U | R | L | D
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instanceMIDIEvent [t| Event |]

  [ blipMax (1/4) 60 [p| Note Nothing (Obstacle D) |]
  , blipMax (1/4) 61 [p| Note Nothing (Obstacle L) |]
  , blipMax (1/4) 62 [p| Note Nothing (Obstacle R) |]
  , blipMax (1/4) 63 [p| Note Nothing (Obstacle U) |]
  , blipMax (1/4) 64 [p| Note Nothing  Cutscene    |]

  , blipMax (1/4) 72 [p| Note Nothing (Color D) |]
  , blipMax (1/4) 73 [p| Note Nothing (Color L) |]
  , blipMax (1/4) 74 [p| Note Nothing (Color R) |]
  , blipMax (1/4) 75 [p| Note Nothing (Color U) |]

  , edge 60 $ \_b -> [p| Note (Just $(boolP _b)) (Obstacle D) |]
  , edge 61 $ \_b -> [p| Note (Just $(boolP _b)) (Obstacle L) |]
  , edge 62 $ \_b -> [p| Note (Just $(boolP _b)) (Obstacle R) |]
  , edge 63 $ \_b -> [p| Note (Just $(boolP _b)) (Obstacle U) |]
  , edge 64 $ \_b -> [p| Note (Just $(boolP _b))  Cutscene    |]

  , edge 72 $ \_b -> [p| Note (Just $(boolP _b)) (Color D) |]
  , edge 73 $ \_b -> [p| Note (Just $(boolP _b)) (Color L) |]
  , edge 74 $ \_b -> [p| Note (Just $(boolP _b)) (Color R) |]
  , edge 75 $ \_b -> [p| Note (Just $(boolP _b)) (Color U) |]

  , edge 84 $ \_b -> [p| Intensity $(boolP _b) Low       |]
  , edge 85 $ \_b -> [p| Intensity $(boolP _b) Neutral   |]
  , edge 86 $ \_b -> [p| Intensity $(boolP _b) High      |]
  , edge 87 $ \_b -> [p| Intensity $(boolP _b) ExtraHigh |]
  ]

secondsToTicks :: U.Seconds -> Int
secondsToTicks = ceiling . (* (44100/1024))

intensityCharacter :: Intensity -> Char
intensityCharacter = \case
  Low       -> 'L'
  Neutral   -> 'N'
  High      -> 'H'
  ExtraHigh -> 'E'

noteTypeCharacter :: NoteType -> Char
noteTypeCharacter = \case
  Obstacle D -> 'D'
  Obstacle L -> 'L'
  Obstacle R -> 'R'
  Obstacle U -> 'U'
  Cutscene   -> 'A'
  Color    D -> '2'
  Color    L -> '4'
  Color    R -> '6'
  Color    U -> '8'

writeTransitions :: RTB.T U.Seconds Event -> String
writeTransitions = let
  writePair ((t1, i1), (t2, i2)) = concat
    [ [intensityCharacter i1]
    , ":"
    , show (max 1 $ secondsToTicks t1)
    , "-" 
    , [intensityCharacter i2]
    , ":"
    , show (max 1 $ secondsToTicks t2)
    ]
  makePairs ((t1, False, i1) : (t2, True, i2) : xs) =
    if i1 == i2
      then makePairs xs
      else ((t1, i1), (t2, i2)) : makePairs xs
  makePairs [(_, False, _)] = []
  makePairs xs = error $ "MelodysEscape.writeTransitions: invalid events: " ++ show xs
  onlyIntensity = mapMaybe $ \case
    (t, Intensity b i) -> Just (t, b, i)
    _                  -> Nothing
  in intercalate ";" . map writePair . makePairs . onlyIntensity .
    ((0, Intensity False Low) :) . ATB.toPairList . RTB.toAbsoluteEventList 0

writeNotes :: RTB.T U.Seconds Event -> String
writeNotes = intercalate ";" . go 0 where
  go :: U.Seconds -> RTB.T U.Seconds Event -> [String]
  go posn rtb = case RTB.viewL rtb of
    Nothing -> []
    Just ((dt, x), rtb') -> case x of
      Intensity _ _ -> go (posn + dt) rtb'
      Note Nothing nt -> let
        evt = show (secondsToTicks $ posn + dt) ++ ":" ++ [noteTypeCharacter nt]
        in evt : go (posn + dt) rtb'
      Note (Just True) nt -> let
        getNoteOff e = if e == Note (Just False) nt
          then Just ()
          else Nothing
        in case U.extractFirst getNoteOff rtb' of
          Just ((dt', ()), rtb'') -> let
            evt = show (secondsToTicks $ posn + dt) ++ ":" ++ [noteTypeCharacter nt] ++ "-" ++
              show (secondsToTicks dt')
            in evt : go (posn + dt) rtb''
          _ -> error $ "MelodysEscape.writeNotes: note on is not followed by matching note off: " ++ show (posn + dt, x)
      Note (Just False) _ -> error $ "MelodysEscape.writeNotes: found an unmatched note off: " ++ show (posn + dt, x)

-- | Any time 2 or more notes appear, randomly delete all but one of them.
randomNotes :: (NNC.C t, MonadRandom m) => RTB.T t Event -> m (RTB.T t Event)
randomNotes = fmap RTB.flatten . mapM go . RTB.collectCoincident where
  go evts = let
    (notes, notNotes) = partition (\case Note _ _ -> True; _ -> False) evts
    in case notes of
      [] -> return evts
      _  -> fmap (: notNotes) $ uniform notes

randomNotesIO :: (NNC.C t) => RTB.T t Event -> IO (RTB.T t Event)
randomNotesIO = evalRandIO . randomNotes
