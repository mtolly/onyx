{-# LANGUAGE RecordWildCards #-}
module Difficulty where

import           Config

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
difficultyRB3 TargetRB3{..} songYaml = let

  simpleRank flex getMode getDiff dmap = case getPart flex songYaml >>= getMode of
    Nothing -> 0
    Just mode -> case getDiff mode of
      Rank r -> r
      Tier t -> tierToRank dmap t

  rb3DrumsRank     = simpleRank rb3_Drums  partDrums     drumsDifficulty drumsDiffMap
  rb3BassRank      = simpleRank rb3_Bass   partGRYBO     gryboDifficulty bassDiffMap
  rb3GuitarRank    = simpleRank rb3_Guitar partGRYBO     gryboDifficulty guitarDiffMap
  rb3VocalRank     = simpleRank rb3_Vocal  partVocal     vocalDifficulty vocalDiffMap
  rb3KeysRank      = simpleRank rb3_Keys   partGRYBO     gryboDifficulty keysDiffMap
  rb3ProKeysRank   = simpleRank rb3_Keys   partProKeys   pkDifficulty    keysDiffMap
  rb3ProBassRank   = simpleRank rb3_Bass   partProGuitar pgDifficulty    proBassDiffMap
  rb3ProGuitarRank = simpleRank rb3_Guitar partProGuitar pgDifficulty    proBassDiffMap
  rb3BandRank      = case _difficulty $ _metadata songYaml of
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
