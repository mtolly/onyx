{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Difficulty where

import qualified Data.EventList.Relative.TimeBody as RTB
import           Onyx.MIDI.Track.File             (FlexPartName (..))
import           Onyx.Mode
import           Onyx.Preferences                 (MagmaSetting (..))
import           Onyx.Project
import qualified Sound.MIDI.Util                  as U

rankToTier :: DiffMap -> Integer -> Integer
rankToTier dm rank = fromIntegral $ length $ takeWhile (<= rank) (1 : dm)

tierToRank :: DiffMap -> Integer -> Integer
tierToRank dm tier = (0 : 1 : dm) !! fromIntegral tier

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

difficultyRB3 :: TargetRB3 -> SongYaml f -> DifficultyRB3
difficultyRB3 rb3 songYaml = let

  simpleRank flex getMode dmap = case getPart flex songYaml >>= getMode of
    Nothing -> 0
    Just mode -> case mode.difficulty of
      Rank r -> r
      Tier t -> tierToRank dmap t
  fiveRank flex dmap = case getPart flex songYaml >>= anyFiveFret of
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

  rb3DrumsRank     = simpleRank rb3.drums  (.drums    ) drumsDiffMap
  rb3BassRank'     = fiveRank rb3.bass   bassDiffMap
  rb3GuitarRank'   = fiveRank rb3.guitar guitarDiffMap
  rb3KeysRank'     = fiveRank rb3.keys   keysDiffMap
  rb3VocalRank     = simpleRank rb3.vocal  (.vocal    ) vocalDiffMap
  rb3ProKeysRank'  = simpleRank rb3.keys   (.proKeys  ) keysDiffMap
  rb3KeysRank      = if rb3KeysRank' == 0 then rb3ProKeysRank' else rb3KeysRank'
  rb3ProKeysRank   = if rb3ProKeysRank' == 0 then rb3KeysRank' else rb3ProKeysRank'
  rb3ProBassRank   = simpleRank rb3.bass   (.proGuitar) proBassDiffMap
  rb3ProGuitarRank = simpleRank rb3.guitar (.proGuitar) proGuitarDiffMap
  rb3GuitarRank    = if rb3GuitarRank' == 0 then rb3ProGuitarRank else rb3GuitarRank'
  rb3BassRank      = if rb3BassRank' == 0 then rb3ProBassRank else rb3BassRank'
  rb3BandRank      = case songYaml.metadata.difficulty of
    Tier t -> tierToRank bandDiffMap t
    Rank r -> r

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

difficultyPS :: TargetPS -> SongYaml f -> DifficultyPS
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
    , version       = Nothing
    , harmonix      = False
    , magma         = MagmaRequire
    , ps3Encrypt    = True
    , legalTempos   = True
    }
  psDifficultyRB3 = difficultyRB3 rb3 songYaml
  simpleTier flex getMode dmap = case getPart flex songYaml >>= getMode of
    Nothing -> 0
    Just mode -> case mode.difficulty of
      Tier t -> t
      Rank r -> rankToTier dmap r
  psRhythmTier     = simpleTier ps.rhythm     (.grybo) guitarDiffMap
  psGuitarCoopTier = simpleTier ps.guitarCoop (.grybo) guitarDiffMap
  psDanceTier      = simpleTier ps.dance      (.dance) drumsDiffMap
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

difficultyGH5 :: TargetGH5 -> SongYaml f -> DifficultyGH5
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
    , version       = Nothing
    , harmonix      = False
    , magma         = MagmaRequire
    , ps3Encrypt    = True
    , legalTempos   = True
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
