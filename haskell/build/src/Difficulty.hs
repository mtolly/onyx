{-# LANGUAGE RecordWildCards #-}
module Difficulty where

import Config

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

data RanksTiers = RanksTiers
  { drumsRank, bassRank, guitarRank, vocalRank, keysRank, proKeysRank, proGuitarRank, proBassRank, bandRank :: Integer
  , drumsTier, bassTier, guitarTier, vocalTier, keysTier, proKeysTier, proGuitarTier, proBassTier, bandTier :: Integer
  } deriving (Eq, Ord, Show, Read)

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
