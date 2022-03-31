{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module PowerGig.MIDI where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           RockBand.Codec
import qualified RockBand.Codec.Drums             as D
import           RockBand.Codec.File              (ParseFile (..), fileTrack)
import qualified RockBand.Codec.Five              as F
import           RockBand.Common

data GuitarDifficulty t = GuitarDifficulty
  { guitarGems         :: RTB.T t (Edge () (Maybe F.Color))
  , guitarOverride     :: RTB.T t (Edge () (Maybe F.Color)) -- I think these replace standard notes in PC mode
  , guitarPowerChordsE :: RTB.T t (Edge () (Maybe F.Color))
  , guitarPowerChordsA :: RTB.T t (Edge () (Maybe F.Color))
  -- TODO controllers 7, 81, 82, 86
  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (GuitarDifficulty t)

instance ParseTrack GuitarDifficulty where
  parseTrack = do
    let allGems = Nothing : map Just each
    guitarGems <- (guitarGems =.) $ translateEdges $ condenseMap $ eachKey allGems $ edges . \case
      Nothing     -> 60
      Just F.Green  -> 62
      Just F.Red    -> 64
      Just F.Yellow -> 65
      Just F.Blue   -> 67
      Just F.Orange -> 69
    guitarOverride <- (guitarOverride =.) $ translateEdges $ condenseMap $ eachKey allGems $ edges . \case
      Nothing     -> 96
      Just F.Green  -> 98
      Just F.Red    -> 100
      Just F.Yellow -> 101
      Just F.Blue   -> 103
      Just F.Orange -> 105
    guitarPowerChordsE <- (guitarPowerChordsE =.) $ translateEdges $ condenseMap $ eachKey allGems $ edges . \case
      Nothing     -> 108
      Just F.Green  -> 109
      Just F.Red    -> 110
      Just F.Yellow -> 111
      Just F.Blue   -> 112
      Just F.Orange -> 113
    guitarPowerChordsA <- (guitarPowerChordsA =.) $ translateEdges $ condenseMap $ eachKey allGems $ edges . \case
      Nothing     -> 114
      Just F.Green  -> 115
      Just F.Red    -> 116
      Just F.Yellow -> 117
      Just F.Blue   -> 118
      Just F.Orange -> 119
    return GuitarDifficulty{..}

data DrumDifficulty t = DrumDifficulty
  { drumGems      :: RTB.T t (D.Gem ())
  , drumFreestyle :: RTB.T t (Edge () (D.Gem ())) -- not sure what this is, just a guess. appears in lower difficulties?
  -- TODO controllers 80, 82, 64
  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DrumDifficulty t)

instance ParseTrack DrumDifficulty where
  parseTrack = do
    let allDrums = [D.Kick, D.Red, D.Pro D.Yellow (), D.Pro D.Blue (), D.Pro D.Green ()]
    drumGems <- (drumGems =.) $ fatBlips (1/8) $ condenseMap_ $ eachKey allDrums $ blip . \case
      D.Pro D.Green  () -> 62
      D.Red             -> 64
      D.Pro D.Yellow () -> 65
      D.Pro D.Blue   () -> 67
      D.Kick            -> 69
      D.Orange          -> error "panic! orange case in powergig drums"
    drumFreestyle <- (drumFreestyle =.) $ translateEdges $ condenseMap $ eachKey allDrums $ edges . \case
      D.Pro D.Green  () -> 86
      D.Red             -> 88
      D.Pro D.Yellow () -> 89
      D.Pro D.Blue   () -> 91
      D.Kick            -> 93
      D.Orange          -> error "panic! orange case in powergig drums"
    return DrumDifficulty{..}

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

  -- TODO vocals, beat

  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (PGFile t)

instance ParseFile PGFile where
  parseFile = do

    pgGuitarBeginner <- pgGuitarBeginner =. fileTrack (pure "guitar_1_beginner")
    pgGuitarEasy     <- pgGuitarEasy     =. fileTrack (pure "guitar_1_easy"    )
    pgGuitarMedium   <- pgGuitarMedium   =. fileTrack (pure "guitar_1_medium"  )
    pgGuitarHard     <- pgGuitarHard     =. fileTrack (pure "guitar_1_hard"    )
    pgGuitarExpert   <- pgGuitarExpert   =. fileTrack (pure "guitar_1_expert"  )
    pgGuitarMaster   <- pgGuitarMaster   =. fileTrack (pure "guitar_1_master"  )

    pgDrumsBeginner <- pgDrumsBeginner =. fileTrack (pure "drums_1_beginner")
    pgDrumsEasy     <- pgDrumsEasy     =. fileTrack (pure "drums_1_easy"    )
    pgDrumsMedium   <- pgDrumsMedium   =. fileTrack (pure "drums_1_medium"  )
    pgDrumsHard     <- pgDrumsHard     =. fileTrack (pure "drums_1_hard"    )
    pgDrumsExpert   <- pgDrumsExpert   =. fileTrack (pure "drums_1_expert"  )
    pgDrumsMaster   <- pgDrumsMaster   =. fileTrack (pure "drums_1_master"  )

    return PGFile{..}
