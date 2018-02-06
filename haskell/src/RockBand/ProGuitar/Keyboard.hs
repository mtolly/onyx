{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module RockBand.ProGuitar.Keyboard where

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (forM_, guard)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.StackTrace (QueueLog, SendMessage,
                                                 StackTraceT, stracket)
import           Control.Monad.Trans.State      (StateT, evalStateT, get, put)
import           Data.Foldable                  (toList)
import           Data.List                      (sortOn)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Set                       as Set
import           RockBand.ProGuitar
import           RockBand.ProGuitar.Play
import           System.MIDI

type Pitch = Int

data Key
  = KeyString GtrString
  | KeyPitch Pitch
  | KeyStrum
  deriving (Eq, Ord, Show, Read)

data GtrSettings = GtrSettings
  { gtrTuning :: GtrString -> Maybe Pitch
  }

data GtrState = GtrState
  { heldStrings :: Set.Set GtrString
  , heldPitches :: Set.Set Pitch
  , heldStrum   :: Bool
  , strummed    :: Set.Set GtrString
  , gtrSettings :: GtrSettings
  }

initialState :: GtrSettings -> GtrState
initialState = GtrState Set.empty Set.empty False Set.empty

fretboardState :: GtrState -> [(GtrString, GtrFret)]
fretboardState gs = let
  strings = heldStrings gs
  pitches = heldPitches gs
  pitchOptions p = do
    str <- [S6 .. S1]
    open <- toList $ gtrTuning (gtrSettings gs) str
    let fret = p - open
    guard $ 0 <= fret && fret <= 22
    return (str, fret)
  chordOptions = let
    go !used notes = case notes of
      [] -> [[]]
      p : ps -> do
        pair@(str, _) <- pitchOptions p
        guard $ not $ Set.member str used
        map (pair :) $ go (Set.insert str used) ps
    in go Set.empty
  in case Set.toList pitches of
    [] -> []
    [p] -> let
      targetStr = fromMaybe S6 $ Set.lookupMin strings
      score (str, _) = fromEnum str + if str < targetStr then 10 else 0
      in case sortOn score $ pitchOptions p of
        []         -> []
        result : _ -> [result]
    _ -> [] -- TODO

processMessage :: (SendMessage m) => MidiMessage' -> StateT GtrState (StackTraceT m) [Message]
processMessage msg = let
  key = case msg of
    NoteOff p _ -> withPitch False p
    NoteOn p v  -> withPitch (v /= 0) p
    CC 64 v     -> Just (KeyStrum, v >= 64)
    _           -> Nothing
  withPitch b = \case
    24 -> Just (KeyString S6, b)
    25 -> Just (KeyStrum, b)
    26 -> Just (KeyString S5, b)
    27 -> Just (KeyStrum, b)
    28 -> Just (KeyString S4, b)
    29 -> Just (KeyString S3, b)
    31 -> Just (KeyString S2, b)
    33 -> Just (KeyString S1, b)
    n -> guard (n > 33) >> Just (KeyPitch n, b)
  in case key of
    Nothing -> return []
    Just (k, b) -> do
      s <- get
      let s' = case k of
            KeyString str -> s { heldStrings = (if b then Set.insert else Set.delete) str $ heldStrings s }
            KeyPitch p -> s { heldPitches = (if b then Set.insert else Set.delete) p $ heldPitches s }
            KeyStrum -> s { heldStrum = b }
          board = fretboardState s'
          newStrums = if heldStrum s'
            then Set.difference (Set.fromList $ map fst board) (strummed s')
            else Set.empty
          s'' = s'
            { strummed = if heldStrum s' && not (Set.null $ heldPitches s')
              then Set.union newStrums (strummed s')
              else Set.empty
            }
      put s''
      return
        $  [Fret str $ fromMaybe 0 $ lookup str board | str <- [S6 .. S1]]
        ++ [Strum str 64 | str <- Set.toList newStrums]

{-

DESIGN

Foot pedal is strum.

function fretboardState(strings, pitches)
  if single pitch:
    pick 2 strings to play that pitch on, starting from a specified string if there is one
  if multiple pitches:
    find a way to play all those notes
    (if multiple options, either start from the specified string,
      or use the set of all specified strings)

if pedal down
  when string/pitch down
    change frets to new fretboardState
    strum only strings that haven't been strummed already since all pitches were last released
  when string/pitch up
    change frets to new fretboardState
if pedal up
  when pedal down
    fret+strum the current fretboardState
  when string/pitch down/up
    change frets to new fretboardState

ideas for modifier keys:
- "strum everything" mode where each keypress does a new fret+strum of just that note, for solos

-}

runApp :: Source -> Destination -> GtrSettings -> StackTraceT (QueueLog IO) ()
runApp src dest sets = stracket (openSource src Nothing) close $ \src' -> do
  stracket (openDestination dest) close $ \dest' -> do
    liftIO $ start src'
    liftIO $ start dest'
    let loop = liftIO (getNextEvent src') >>= \case
          Nothing -> liftIO (threadDelay 1000) >> loop
          Just (MidiEvent _ msg) -> case msg of
            MidiMessage _ msg' -> do
              msgs <- processMessage msg'
              forM_ msgs $ \m -> do
                liftIO $ send dest' $ SysEx $ [0xF0] ++ sendCommand (Squier, m) ++ [0xF7]
                loop
            _ -> loop
    evalStateT loop $ initialState sets