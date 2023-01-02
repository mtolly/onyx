{-
Ported from LibForge
https://github.com/maxton/LibForge
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Harmonix.RockBand.RB4.SongDTA where

import           Control.Monad          (replicateM_)
import qualified Data.ByteString        as B
import           Data.Profunctor        (dimap)
import           Onyx.Codec.Binary
import           Onyx.Rocksmith.Sng2014 (nullTerm)

data SongDTA = SongDTA
  { type_              :: Word32
  , songId             :: Word32
  , version            :: Int16
  , gameOrigin         :: B.ByteString
  , previewStart       :: Float
  , previewEnd         :: Float
  , name               :: B.ByteString
  , artist             :: B.ByteString
  , albumName          :: B.ByteString
  , albumTrackNumber   :: Int16
  , albumYear          :: Int32
  , originalYear       :: Int32
  , genre              :: B.ByteString
  , songLength         :: Float
  , guitarRank         :: Float
  , bassRank           :: Float
  , vocalsRank         :: Float
  , drumRank           :: Float
  , bandRank           :: Float
  , keysRank           :: Float
  , realKeysRank       :: Float
  , tutorial           :: Bool
  , albumArt           :: Bool
  , cover              :: Bool
  , vocalGender        :: Word8
  , animTempo          :: B.ByteString
  , hasFreestyleVocals :: Bool
  , vocalParts         :: Int32
  , flags              :: Int32
  , fake               :: Bool
  , shortname          :: B.ByteString
  } deriving (Show)

boolByte :: BinaryCodec Bool
boolByte = dimap (\b -> if b then 1 else 0) (/= 0) word8

skipBytes :: Int -> CodecFor Get PutM b ()
skipBytes n = replicateM_ n $ const 0 =. word8

instance Bin SongDTA where
  bin = do
    let ?endian = LittleEndian
    type_              <- (.type_             ) =. binEndian
    songId             <- (.songId            ) =. binEndian
    version            <- (.version           ) =. binEndian
    gameOrigin         <- (.gameOrigin        ) =. nullTerm 18
    previewStart       <- (.previewStart      ) =. binEndian
    previewEnd         <- (.previewEnd        ) =. binEndian
    name               <- (.name              ) =. nullTerm 256
    artist             <- (.artist            ) =. nullTerm 256
    albumName          <- (.albumName         ) =. nullTerm 256
    albumTrackNumber   <- (.albumTrackNumber  ) =. binEndian
    skipBytes 2
    albumYear          <- (.albumYear         ) =. binEndian
    originalYear       <- (.originalYear      ) =. binEndian
    genre              <- (.genre             ) =. nullTerm 64
    songLength         <- (.songLength        ) =. binEndian
    guitarRank         <- (.guitarRank        ) =. binEndian
    bassRank           <- (.bassRank          ) =. binEndian
    vocalsRank         <- (.vocalsRank        ) =. binEndian
    drumRank           <- (.drumRank          ) =. binEndian
    bandRank           <- (.bandRank          ) =. binEndian
    keysRank           <- (.keysRank          ) =. binEndian
    realKeysRank       <- (.realKeysRank      ) =. binEndian
    tutorial           <- (.tutorial          ) =. boolByte
    albumArt           <- (.albumArt          ) =. boolByte
    cover              <- (.cover             ) =. boolByte
    vocalGender        <- (.vocalGender       ) =. bin
    animTempo          <- (.animTempo         ) =. nullTerm 16
    hasFreestyleVocals <- (.hasFreestyleVocals) =. boolByte
    skipBytes 3
    vocalParts         <- (.vocalParts        ) =. binEndian
    flags              <- (.flags             ) =. binEndian
    fake               <- (.fake              ) =. boolByte
    shortname          <- (.shortname         ) =. nullTerm 256
    return SongDTA{..}
