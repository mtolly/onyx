{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Difficulty where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.List.NonEmpty               as NE
import           Onyx.MIDI.Track.File             (FlexPartName (..))
import           Onyx.Mode
import           Onyx.Preferences                 (MagmaSetting (..),
                                                   RBEncoding (..))
import           Onyx.Project
import qualified Sound.MIDI.Util                  as U

rankToTier :: DiffMap -> Integer -> Integer
rankToTier dm rank = fromIntegral $ length $ takeWhile (<= rank) (1 : dm)

tierToRank :: DiffMap -> Integer -> Integer
tierToRank dm tier = let
  ranks = 0 NE.:| (1 : dm)
  in case NE.drop (fromIntegral tier) ranks of
    []       -> NE.last ranks -- if we wanted, could also keep extending upward
    rank : _ -> rank

type DiffMap = [Integer]

drumsDiffMap, vocalDiffMap, guitarDiffMap   , bassDiffMap    :: DiffMap
keysDiffMap , bandDiffMap , proGuitarDiffMap, proBassDiffMap :: DiffMap

drumsDiffMap     = [124, 151, 178, 242, 345, 448]
vocalDiffMap     = [132, 175, 218, 279, 353, 427]
bassDiffMap      = [135, 181, 228, 293, 364, 436]
guitarDiffMap    = [139, 176, 221, 267, 333, 409]
keysDiffMap      = [153, 211, 269, 327, 385, 443]
bandDiffMap      = [163, 215, 243, 267, 292, 345]
proGuitarDiffMap = [150, 205, 264, 323, 382, 442]
proBassDiffMap   = [150, 208, 267, 325, 384, 442]

data DifficultyRB3 = DifficultyRB3
  { rb3DrumsRank, rb3BassRank, rb3GuitarRank, rb3VocalRank, rb3KeysRank, rb3ProBassRank, rb3ProGuitarRank, rb3ProKeysRank, rb3BandRank :: Integer
  , rb3DrumsTier, rb3BassTier, rb3GuitarTier, rb3VocalTier, rb3KeysTier, rb3ProBassTier, rb3ProGuitarTier, rb3ProKeysTier, rb3BandTier :: Integer
  } deriving (Eq, Ord, Show, Read)

computeDrumRank :: FlexPartName -> SongYaml f -> DiffMap -> Integer
computeDrumRank flex songYaml dmap = case getPart flex songYaml >>= anyDrums of
  Nothing -> 0
  Just builder -> let
    result = builder DrumTargetRB2x ModeInput
      { tempo  = U.tempoMapFromBPS RTB.empty
      , events = mempty
      , part   = mempty
      }
    in case result.settings.difficulty of
      Rank r -> r
      Tier t -> tierToRank dmap t

computeFiveRank :: FlexPartName -> SongYaml f -> DiffMap -> Integer
computeFiveRank flex songYaml dmap = case getPart flex songYaml >>= anyFiveFret of
  Nothing -> 0
  Just builder -> let
    result = builder FiveTypeGuitarExt ModeInput
      { tempo  = U.tempoMapFromBPS RTB.empty
      , events = mempty
      , part   = mempty
      }
    in case result.settings.difficulty of
      Rank r -> r
      Tier t -> tierToRank dmap t

computeProKeysRank :: FlexPartName -> SongYaml f -> DiffMap -> Integer
computeProKeysRank flex songYaml dmap = case getPart flex songYaml >>= anyProKeys of
  Nothing -> 0
  Just builder -> let
    result = builder ModeInput
      { tempo  = U.tempoMapFromBPS RTB.empty
      , events = mempty
      , part   = mempty
      }
    in case result.settings.difficulty of
      Rank r -> r
      Tier t -> tierToRank dmap t

difficultyRB3 :: TargetRB3 f -> SongYaml f -> DifficultyRB3
difficultyRB3 rb3 songYaml = let

  simpleRank flex getMode dmap = case getPart flex songYaml >>= getMode of
    Nothing -> 0
    Just mode -> case mode.difficulty of
      Rank r -> r
      Tier t -> tierToRank dmap t

  rb3DrumsRank     = computeDrumRank rb3.drums  songYaml drumsDiffMap
  rb3BassRank'     = computeFiveRank rb3.bass   songYaml bassDiffMap
  rb3GuitarRank'   = computeFiveRank rb3.guitar songYaml guitarDiffMap
  rb3KeysRank'     = computeFiveRank rb3.keys   songYaml keysDiffMap
  rb3VocalRank     = simpleRank rb3.vocal (.vocal) vocalDiffMap
  rb3ProKeysRank   = computeProKeysRank rb3.keys songYaml  keysDiffMap
  rb3KeysRank      = if rb3KeysRank' == 0 then rb3ProKeysRank else rb3KeysRank'
  rb3ProBassRank   = simpleRank rb3.bass   (.proGuitar) proBassDiffMap
  rb3ProGuitarRank = simpleRank rb3.guitar (.proGuitar) proGuitarDiffMap
  rb3GuitarRank    = if rb3GuitarRank' == 0 then rb3ProGuitarRank else rb3GuitarRank'
  rb3BassRank      = if rb3BassRank' == 0 then rb3ProBassRank else rb3BassRank'
  rb3BandRank      = case songYaml.metadata.difficulty of
    Tier t -> tierToRank bandDiffMap t
    Rank r -> r
    -- TODO if no band difficulty, should default to average of instrument difficulties

  rb3DrumsTier     = rankToTier drumsDiffMap     rb3DrumsRank
  rb3BassTier      = rankToTier bassDiffMap      rb3BassRank
  rb3GuitarTier    = rankToTier guitarDiffMap    rb3GuitarRank
  rb3VocalTier     = rankToTier vocalDiffMap     rb3VocalRank
  rb3KeysTier      = rankToTier keysDiffMap      rb3KeysRank
  rb3ProKeysTier   = rankToTier keysDiffMap      rb3ProKeysRank
  rb3ProBassTier   = rankToTier proBassDiffMap   rb3ProBassRank
  rb3ProGuitarTier = rankToTier proGuitarDiffMap rb3ProGuitarRank
  rb3BandTier      = rankToTier bandDiffMap      rb3BandRank

  in DifficultyRB3{..}

data DifficultyPS = DifficultyPS
  { psDifficultyRB3  :: DifficultyRB3
  , psRhythmTier     :: Integer
  , psGuitarCoopTier :: Integer
  , psDanceTier      :: Integer
  , chGuitarGHLTier  :: Integer
  , chBassGHLTier    :: Integer
  } deriving (Eq, Ord, Show, Read)

difficultyPS :: TargetPS f -> SongYaml f -> DifficultyPS
difficultyPS ps songYaml = let
  rb3 = TargetRB3
    { common        = ps.common
    , drums         = ps.drums
    , guitar        = ps.guitar
    , keys          = ps.keys
    , vocal         = ps.vocal
    , bass          = ps.bass
    , is2xBassPedal = False
    , songID        = SongIDAutoSymbol
    , harmonix      = False
    , magma         = MagmaRequire
    , ps3Encrypt    = True
    , legalTempos   = True
    , encoding      = UTF8
    }
  psDifficultyRB3 = difficultyRB3 rb3 songYaml
  simpleTier flex getMode dmap = case getPart flex songYaml >>= getMode of
    Nothing -> 0
    Just mode -> case mode.difficulty of
      Tier t -> t
      Rank r -> rankToTier dmap r
  psRhythmTier     = simpleTier ps.rhythm     (.grybo) guitarDiffMap
  psGuitarCoopTier = simpleTier ps.guitarCoop (.grybo) guitarDiffMap
  psDanceTier      = simpleTier ps.dance      (.mania) drumsDiffMap
  chGuitarGHLTier  = simpleTier ps.guitar     (.ghl  ) guitarDiffMap
  chBassGHLTier    = simpleTier ps.bass       (.ghl  ) guitarDiffMap
  in DifficultyPS{..}

-- tiers go from 1 to 10, or 0 for no part
data DifficultyGH5 = DifficultyGH5
  { gh5GuitarTier :: Integer
  , gh5BassTier   :: Integer
  , gh5DrumsTier  :: Integer
  , gh5VocalsTier :: Integer
  }

difficultyGH5 :: TargetGH5 f -> SongYaml f -> DifficultyGH5
difficultyGH5 TargetGH5{..} songYaml = let
  rb3 = TargetRB3
    { common        = common
    , drums         = drums
    , guitar        = guitar
    , keys          = FlexExtra "undefined"
    , vocal         = vocal
    , bass          = bass
    , is2xBassPedal = False
    , songID        = SongIDAutoSymbol
    , harmonix      = False
    , magma         = MagmaRequire
    , ps3Encrypt    = True
    , legalTempos   = True
    , encoding      = UTF8
    }
  DifficultyRB3{..} = difficultyRB3 rb3 songYaml
  rb3RankToGH5 = \case
    0 -> 0
    n -> min 10 $ quot (n - 1) 50 + 1
  in DifficultyGH5
    { gh5GuitarTier = rb3RankToGH5 rb3GuitarRank
    , gh5BassTier   = rb3RankToGH5 rb3BassRank
    , gh5DrumsTier  = rb3RankToGH5 rb3DrumsRank
    , gh5VocalsTier = rb3RankToGH5 rb3VocalRank
    }
