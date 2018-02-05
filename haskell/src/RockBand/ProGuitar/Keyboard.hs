{-# LANGUAGE LambdaCase #-}
module RockBand.ProGuitar.Keyboard where

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.StackTrace (QueueLog, SendMessage,
                                                 StackTraceT, stracket)
import           Control.Monad.Trans.State      (StateT, evalStateT)
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

processMessage :: (SendMessage m) => MidiMessage' -> StateT GtrState (StackTraceT m) [Message]
processMessage = const $ return [] -- TODO

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
