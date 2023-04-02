{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.PowerGig.MIDI where

import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (modify)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.MIDI.Track.Vocal            (Pitch)
import qualified Sound.MIDI.File                  as F
import           Sound.MIDI.Message.Channel.Voice (toPitch)
import qualified Sound.MIDI.Util                  as U

data GuitarDifficulty t = GuitarDifficulty
  { guitarGems             :: RTB.T t (Edge () (Maybe Five.Color))
  , guitarPowerChordsE     :: RTB.T t (Edge () (Maybe Five.Color))
  , guitarPowerChordsA     :: RTB.T t (Edge () (Maybe Five.Color))
  , guitarPowerChordsGRYBO :: RTB.T t (Edge () (Maybe Five.Color)) -- normal notes used in power chord sections
  , guitarHOPO             :: RTB.T t Bool
  , guitarPowerChordMode   :: RTB.T t Bool
  , guitarMojoDrummer      :: RTB.T t Bool
  , guitarMojoVocalist     :: RTB.T t Bool
  -- rest unknown
  , guitarController7      :: RTB.T t Int
  -- TODO others seen: controller 0, controller 32, controller 80
  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (GuitarDifficulty t)

instance ParseTrack GuitarDifficulty where
  parseTrack = do
    let allGems = Nothing : map Just each
    guitarGems <- (guitarGems =.) $ translateEdges $ condenseMap $ eachKey allGems $ edges . \case
      Nothing          -> 60
      Just Five.Green  -> 62
      Just Five.Red    -> 64
      Just Five.Yellow -> 65
      Just Five.Blue   -> 67
      Just Five.Orange -> 69
    guitarPowerChordsGRYBO <- (guitarPowerChordsGRYBO =.) $ translateEdges $ condenseMap $ eachKey allGems $ edges . \case
      Nothing          -> 96
      Just Five.Green  -> 98
      Just Five.Red    -> 100
      Just Five.Yellow -> 101
      Just Five.Blue   -> 103
      Just Five.Orange -> 105
    guitarPowerChordsE <- (guitarPowerChordsE =.) $ translateEdges $ condenseMap $ eachKey allGems $ edges . \case
      Nothing          -> 108
      Just Five.Green  -> 109
      Just Five.Red    -> 110
      Just Five.Yellow -> 111
      Just Five.Blue   -> 112
      Just Five.Orange -> 113
    guitarPowerChordsA <- (guitarPowerChordsA =.) $ translateEdges $ condenseMap $ eachKey allGems $ edges . \case
      Nothing          -> 114
      Just Five.Green  -> 115
      Just Five.Red    -> 116
      Just Five.Yellow -> 117
      Just Five.Blue   -> 118
      Just Five.Orange -> 119
    guitarHOPO           <- guitarHOPO           =. controllerBool 68 -- "legato pedal"
    guitarPowerChordMode <- guitarPowerChordMode =. controllerBool 86
    guitarMojoDrummer    <- guitarMojoDrummer    =. controllerBool 81
    guitarMojoVocalist   <- guitarMojoVocalist   =. controllerBool 82
    guitarController7    <- guitarController7    =. controller_ 7
    return GuitarDifficulty{..}

data DrumDifficulty t = DrumDifficulty
  { drumGems          :: RTB.T t (Edge () (D.Gem ()))
  , drumFreeGems      :: RTB.T t (Edge () (D.Gem ())) -- mostly in lower diffs but also tripping billies on all diffs
  , drumMojoGuitarist :: RTB.T t Bool
  , drumMojoVocalist  :: RTB.T t Bool
  , drumFreestyle     :: RTB.T t Bool -- surrounds drumFreeGems
  -- TODO program change (ch 9), probably switches between e.g. <kit kit_number="0" ...>
  -- TODO others seen: note pitch 61, note pitch 66, note pitch 84, controller 10, controller 7
  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DrumDifficulty t)

instance ParseTrack DrumDifficulty where
  parseTrack = do
    let allDrums = [D.Kick, D.Red, D.Pro D.Yellow (), D.Pro D.Blue (), D.Pro D.Green ()]
    drumGems <- (drumGems =.) $ translateEdges $ condenseMap $ eachKey allDrums $ edges . \case
      D.Pro D.Green  () -> 62
      D.Red             -> 64
      D.Pro D.Yellow () -> 65
      D.Pro D.Blue   () -> 67
      D.Kick            -> 69
      D.Orange          -> error "panic! orange case in powergig drums"
    drumFreeGems <- (drumFreeGems =.) $ translateEdges $ condenseMap $ eachKey allDrums $ edges . \case
      D.Pro D.Green  () -> 86
      D.Red             -> 88
      D.Pro D.Yellow () -> 89
      D.Pro D.Blue   () -> 91
      D.Kick            -> 93
      D.Orange          -> error "panic! orange case in powergig drums"
    drumMojoGuitarist <- drumMojoGuitarist =. controllerBool 80
    drumMojoVocalist  <- drumMojoVocalist  =. controllerBool 82
    drumFreestyle     <- drumFreestyle     =. controllerBool 64
    return DrumDifficulty{..}

data VocalDifficulty t = VocalDifficulty
  { vocalNotes             :: RTB.T t (Pitch, Bool) -- not sure of range
  , vocalTalkies           :: RTB.T t Bool
  , vocalLyrics            :: RTB.T t T.Text
  , vocalPhraseEnd         :: RTB.T t ()
  , vocalUnknownBackslashN :: RTB.T t ()
  , vocalFreestyle         :: RTB.T t Bool
  , vocalGlue              :: RTB.T t Bool
  , vocalMojoGuitarist     :: RTB.T t Bool
  , vocalMojoDrummer       :: RTB.T t Bool
  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (VocalDifficulty t)

instance ParseTrack VocalDifficulty where
  parseTrack = do
    vocalNotes             <- vocalNotes             =. condenseMap (eachKey each $ edges . (+ 36) . fromEnum)
    vocalTalkies           <- vocalTalkies           =. edges 0
    vocalLyrics            <- vocalLyrics            =. lyrics
    vocalPhraseEnd         <- vocalPhraseEnd         =. powerGigText "\\r"
    vocalUnknownBackslashN <- vocalUnknownBackslashN =. powerGigText "\\n"
    vocalFreestyle         <- vocalFreestyle         =. controllerBool 64 -- "hold pedal"
    vocalGlue              <- vocalGlue              =. controllerBool 68 -- "legato pedal"
    vocalMojoGuitarist     <- vocalMojoGuitarist     =. controllerBool 80
    vocalMojoDrummer       <- vocalMojoDrummer       =. controllerBool 81
    return VocalDifficulty{..}

data BeatTrack t = BeatTrack
  { beatLines :: RTB.T t ()
  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (BeatTrack t)

instance ParseTrack BeatTrack where
  parseTrack = do
    -- these can be any pitch apparently
    Codec
      { codecIn = lift $ modify $ \mt -> mt
        { midiNotes = Map.singleton (toPitch 60) $ mconcat $ Map.elems $ midiNotes mt
        }
      , codecOut = const $ return ()
      }
    beatLines <- beatLines =. blip 60
    return BeatTrack{..}

data PGFile t = PGFile

  { pgGuitarBeginner :: GuitarDifficulty t
  , pgGuitarEasy     :: GuitarDifficulty t
  , pgGuitarMedium   :: GuitarDifficulty t
  , pgGuitarHard     :: GuitarDifficulty t
  , pgGuitarExpert   :: GuitarDifficulty t
  , pgGuitarMaster   :: GuitarDifficulty t

  , pgDrumsBeginner  :: DrumDifficulty t
  , pgDrumsEasy      :: DrumDifficulty t
  , pgDrumsMedium    :: DrumDifficulty t
  , pgDrumsHard      :: DrumDifficulty t
  , pgDrumsExpert    :: DrumDifficulty t
  , pgDrumsMaster    :: DrumDifficulty t

  , pgVocalsBeginner :: VocalDifficulty t
  , pgVocalsEasy     :: VocalDifficulty t
  , pgVocalsMedium   :: VocalDifficulty t
  , pgVocalsHard     :: VocalDifficulty t
  , pgVocalsExpert   :: VocalDifficulty t
  , pgVocalsMaster   :: VocalDifficulty t

  , pgBeat           :: BeatTrack t

  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (PGFile t)

instance F.ParseFile PGFile where
  parseFile = do

    pgGuitarBeginner <- pgGuitarBeginner =. F.fileTrack (pure "guitar_1_beginner")
    pgGuitarEasy     <- pgGuitarEasy     =. F.fileTrack (pure "guitar_1_easy"    )
    pgGuitarMedium   <- pgGuitarMedium   =. F.fileTrack (pure "guitar_1_medium"  )
    pgGuitarHard     <- pgGuitarHard     =. F.fileTrack (pure "guitar_1_hard"    )
    pgGuitarExpert   <- pgGuitarExpert   =. F.fileTrack (pure "guitar_1_expert"  )
    pgGuitarMaster   <- pgGuitarMaster   =. F.fileTrack (pure "guitar_1_master"  )

    pgDrumsBeginner <- pgDrumsBeginner =. F.fileTrack (pure "drums_1_beginner")
    pgDrumsEasy     <- pgDrumsEasy     =. F.fileTrack (pure "drums_1_easy"    )
    pgDrumsMedium   <- pgDrumsMedium   =. F.fileTrack (pure "drums_1_medium"  )
    pgDrumsHard     <- pgDrumsHard     =. F.fileTrack (pure "drums_1_hard"    )
    pgDrumsExpert   <- pgDrumsExpert   =. F.fileTrack (pure "drums_1_expert"  )
    pgDrumsMaster   <- pgDrumsMaster   =. F.fileTrack (pure "drums_1_master"  )

    pgVocalsBeginner <- pgVocalsBeginner =. F.fileTrack (pure "vocals_1_beginner")
    pgVocalsEasy     <- pgVocalsEasy     =. F.fileTrack (pure "vocals_1_easy"    )
    pgVocalsMedium   <- pgVocalsMedium   =. F.fileTrack (pure "vocals_1_medium"  )
    pgVocalsHard     <- pgVocalsHard     =. F.fileTrack (pure "vocals_1_hard"    )
    pgVocalsExpert   <- pgVocalsExpert   =. F.fileTrack (pure "vocals_1_expert"  )
    pgVocalsMaster   <- pgVocalsMaster   =. F.fileTrack (pure "vocals_1_master"  )

    pgBeat <- pgBeat =. F.fileTrack (pure "beat")

    return PGFile{..}

-- This is needed for Cherub Rock and maybe others
fixLateTrackNames :: F.T s -> F.T s
fixLateTrackNames (F.Cons typ dvn trks) = F.Cons typ dvn $ flip map trks $ \trk -> case U.trackName trk of
  Just _  -> trk
  Nothing -> case RTB.partitionMaybe U.readTrackName trk of
    (names, rest) -> foldr (Wait 0 . U.showTrackName) rest names

{-

Cataloguing CherubRock_cue.mid:

camera1
notes 4 5 6 7 8 9 10 11 12 15 101 104 (blips)

L1Light1
notes 1 2 3 4 (blips)

Pyro
notes 1 (blips)

VenueEvents
notes 1 2 3 4 5 10 11 20 21 (blips)

GuitaristFullBody and GuitaristFace
notes 9 10 11 14 17 32 (long)

GuitaristStrum

GuitaristChord

GuitaristFret
notes 1 2 3 4 5 (long)
text event "easein 0.1 easeout 0.1"

VocalistFullBody

VocalistFace

DrummerFill

DrummerFace

-}
