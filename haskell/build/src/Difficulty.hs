module Difficulty where

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
