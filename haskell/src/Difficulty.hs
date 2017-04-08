{-# LANGUAGE RecordWildCards #-}
module Difficulty where

import           Config
import           RockBand.File (FlexPartName (..))

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

difficultyRB3 :: TargetRB3 -> SongYaml -> DifficultyRB3
difficultyRB3 TargetRB3{..} SongYaml{..} = let

  simpleRank flex play dmap flexDef = if play `elem` playModes flex _instruments
    then case getDifficulty (Just (flex, play)) $ _difficulty _metadata of
      Tier t -> tierToRank dmap t
      Rank r -> if flex == flexDef then r else 1
    else 0

  rb3DrumsRank     = simpleRank rb3_Drums  PlayDrums drumsDiffMap  FlexDrums
  rb3BassRank      = simpleRank rb3_Bass   PlayGRYBO bassDiffMap   FlexBass
  rb3GuitarRank    = simpleRank rb3_Guitar PlayGRYBO guitarDiffMap FlexGuitar
  rb3VocalRank     = undefined
  rb3KeysRank      = undefined
  rb3ProKeysRank   = undefined
  rb3ProBassRank   = simpleRank rb3_Bass   PlayProGuitar bassDiffMap   FlexBass
  rb3ProGuitarRank = simpleRank rb3_Guitar PlayProGuitar guitarDiffMap FlexGuitar
  rb3BandRank      = case getDifficulty Nothing $ _difficulty _metadata of
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

{-
computeRanksTiers :: SongYaml -> RanksTiers
computeRanksTiers songYaml = let
  getRank has diff dmap = if has $ _instruments songYaml
    then case diff $ _difficulty $ _metadata songYaml of
      Nothing       -> 1
      Just (Rank r) -> r
      Just (Tier t) -> tierToRank dmap t
    else 0

  drumsRank     = getRank _hasDrums     _difficultyDrums     drumsDiffMap
  bassRank      = getRank _hasBass      _difficultyBass      bassDiffMap
  guitarRank    = getRank _hasGuitar    _difficultyGuitar    guitarDiffMap
  vocalRank     = getRank hasAnyVocal   _difficultyVocal     vocalDiffMap
  keysRank      = getRank hasAnyKeys    _difficultyKeys      keysDiffMap
  proKeysRank   = getRank hasAnyKeys    _difficultyProKeys   keysDiffMap
  proGuitarRank = getRank _hasProGuitar _difficultyProGuitar proGuitarDiffMap
  proBassRank   = getRank _hasProBass   _difficultyProBass   proBassDiffMap
  bandRank      = getRank (const True)  _difficultyBand      bandDiffMap

  drumsTier     = rankToTier drumsDiffMap     drumsRank
  bassTier      = rankToTier bassDiffMap      bassRank
  guitarTier    = rankToTier guitarDiffMap    guitarRank
  vocalTier     = rankToTier vocalDiffMap     vocalRank
  keysTier      = rankToTier keysDiffMap      keysRank
  proKeysTier   = rankToTier keysDiffMap      proKeysRank
  proGuitarTier = rankToTier proGuitarDiffMap proGuitarRank
  proBassTier   = rankToTier proBassDiffMap   proBassRank
  bandTier      = rankToTier bandDiffMap      bandRank

  in RanksTiers{..}
-}
