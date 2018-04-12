{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.Codec.File where

import           Control.Monad                    (forM_)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State        (StateT, get, put, runStateT)
import           Control.Monad.Trans.Writer       (Writer, execWriter, tell)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (Identity)
import           Data.List.Extra                  (nubOrd, partition)
import qualified Data.Text                        as T
import           RockBand.Codec
import           RockBand.Codec.Beat
import           RockBand.Codec.Drums
import           RockBand.Codec.Events
import           RockBand.Codec.Five
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           RockBand.Codec.VenueRB3
import           RockBand.Codec.Vocal
import qualified RockBand.File                    as RBFile
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

type FileParser m t = StackTraceT (StateT [RTB.T t E.T] m)
type FileBuilder t = Writer [RTB.T t E.T]
type FileCodec m t a = Codec (FileParser m t) (FileBuilder t) a

class ParseFile f where
  parseFile :: (SendMessage m) => FileCodec m U.Beats (f U.Beats)

fileTrack :: (SendMessage m, ParseTrack trk) => T.Text -> FileCodec m U.Beats (trk U.Beats)
fileTrack name = Codec
  { codecIn = do
    trks <- lift get
    let (match, rest) = partition (\trk -> U.trackName trk == Just (T.unpack name)) trks
        match' = RBFile.stripTrack $ foldr RTB.merge RTB.empty match
    lift $ put rest
    inside ("Parsing track: " ++ T.unpack name) $ do
      (parsedTrk, unrec) <- flip mapStackTraceT (codecIn parseTrack) $ \input -> do
        (errorOrParsed, unrec) <- lift $ runStateT input match'
        case errorOrParsed of
          Left  msgs      -> return $ Left msgs
          Right parsedTrk -> return $ Right (parsedTrk, unrec)
      forM_ (nubOrd $ toList unrec) $ \e -> warn $ "Unrecognized MIDI event: " ++ show e
      return parsedTrk
  , codecOut
    = fmapArg
    $ tell
    . (: [])
    . U.setTrackName (T.unpack name)
    . runMergeTrack
    . execWriter
    . codecOut (forcePure parseTrack)
  } where
    forcePure
      :: TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
      -> TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
    forcePure = id

data RB3File t = RB3File
  { rb3PartDrums        :: DrumTrack t
  , rb3PartGuitar       :: FiveTrack t
  , rb3PartBass         :: FiveTrack t
  , rb3PartKeys         :: FiveTrack t
  , rb3PartRealGuitar   :: ProGuitarTrack t
  , rb3PartRealGuitar22 :: ProGuitarTrack t
  , rb3PartRealBass     :: ProGuitarTrack t
  , rb3PartRealBass22   :: ProGuitarTrack t
  , rb3PartRealKeysE    :: ProKeysTrack t
  , rb3PartRealKeysM    :: ProKeysTrack t
  , rb3PartRealKeysH    :: ProKeysTrack t
  , rb3PartRealKeysX    :: ProKeysTrack t
  , rb3PartKeysAnimLH   :: ProKeysTrack t
  , rb3PartKeysAnimRH   :: ProKeysTrack t
  , rb3PartVocals       :: VocalTrack t
  , rb3Harm1            :: VocalTrack t
  , rb3Harm2            :: VocalTrack t
  , rb3Harm3            :: VocalTrack t
  , rb3Events           :: EventsTrack t
  , rb3Beat             :: BeatTrack t
  , rb3Venue            :: VenueRB3 t
  } deriving (Eq, Ord, Show)

instance ParseFile RB3File where
  parseFile = do
    rb3PartDrums        <- rb3PartDrums        =. fileTrack "PART DRUMS"
    rb3PartGuitar       <- rb3PartGuitar       =. fileTrack "PART GUITAR"
    rb3PartBass         <- rb3PartBass         =. fileTrack "PART BASS"
    rb3PartKeys         <- rb3PartKeys         =. fileTrack "PART KEYS"
    rb3PartRealGuitar   <- rb3PartRealGuitar   =. fileTrack "PART REAL_GUITAR"
    rb3PartRealGuitar22 <- rb3PartRealGuitar22 =. fileTrack "PART REAL_GUITAR_22"
    rb3PartRealBass     <- rb3PartRealBass     =. fileTrack "PART REAL_BASS"
    rb3PartRealBass22   <- rb3PartRealBass22   =. fileTrack "PART REAL_BASS_22"
    rb3PartRealKeysE    <- rb3PartRealKeysE    =. fileTrack "PART REAL_KEYS_E"
    rb3PartRealKeysM    <- rb3PartRealKeysM    =. fileTrack "PART REAL_KEYS_M"
    rb3PartRealKeysH    <- rb3PartRealKeysH    =. fileTrack "PART REAL_KEYS_H"
    rb3PartRealKeysX    <- rb3PartRealKeysX    =. fileTrack "PART REAL_KEYS_X"
    rb3PartKeysAnimLH   <- rb3PartKeysAnimLH   =. fileTrack "PART KEYS_ANIM_LH"
    rb3PartKeysAnimRH   <- rb3PartKeysAnimRH   =. fileTrack "PART KEYS_ANIM_RH"
    rb3PartVocals       <- rb3PartVocals       =. fileTrack "PART VOCALS"
    rb3Harm1            <- rb3Harm1            =. fileTrack "HARM1"
    rb3Harm2            <- rb3Harm2            =. fileTrack "HARM2"
    rb3Harm3            <- rb3Harm3            =. fileTrack "HARM3"
    rb3Events           <- rb3Events           =. fileTrack "EVENTS"
    rb3Beat             <- rb3Beat             =. fileTrack "BEAT"
    rb3Venue            <- rb3Venue            =. fileTrack "VENUE"
    return RB3File{..}
