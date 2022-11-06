{-# LANGUAGE RecordWildCards #-}
module Onyx.Harmonix.RockBand.Milo.SongPref where

import qualified Data.ByteString                 as B
import           Onyx.Codec.Binary
import           Onyx.Harmonix.RockBand.Milo.Dir (getStringBE, putStringBE)

data SongPref = SongPref
  { prefU1    :: Word32 -- 3
  , prefU2    :: Word32 -- 2
  -- bunch of zeroes, boundaries unclear
  , prefU3    :: Word32 -- 0
  , prefU4    :: Word32 -- 0
  , prefU5    :: Word8 -- 0
  -- lipsync assignments, parts are "guitar" "bass" "drum"
  , prefPart2 :: B.ByteString -- default "guitar"
  , prefPart3 :: B.ByteString -- default "bass"
  , prefPart4 :: B.ByteString -- default "drum"
  -- animation styles are
  -- "dramatic" (goth)
  -- "rocker" (rock)
  -- "spazz" (punk)
  -- "banger" (metal)
  -- default style is determined by genre
  , prefStyle :: B.ByteString
  } deriving (Eq, Show)

instance Bin SongPref where
  bin = do
    prefU1 <- prefU1 =. word32be
    prefU2 <- prefU2 =. word32be
    prefU3 <- prefU3 =. word32be
    prefU4 <- prefU4 =. word32be
    prefU5 <- prefU5 =. word8
    let stringBE = Codec getStringBE $ fmapArg putStringBE
    prefPart2 <- prefPart2 =. stringBE
    prefPart3 <- prefPart3 =. stringBE
    prefPart4 <- prefPart4 =. stringBE
    prefStyle <- prefStyle =. stringBE
    return SongPref{..}

{-

Default styles based on genre, list by StackOverflow0x:

(alternative rocker)
(blues rocker)
(classical dramatic)
(classicrock rocker)
(country rocker)
(emo dramatic)
(fusion rocker)
(glam dramatic)
(inspirational dramatic)
(jazz dramatic)
(jrock spazz)
(latin spazz)
(metal banger)
(novelty spazz)
(numetal banger)
(poprock rocker)
(prog rocker)
(punk spazz)
(rock rocker)
(southernrock rocker)
(grunge banger)
(indierock rocker)
(new_wave dramatic)
(reggaeska rocker)
(rbsoulfunk rocker)
(hiphoprap rocker)
(other rocker)
(popdanceelectronic dramatic)
(urban rocker)
(world rocker)

-}
