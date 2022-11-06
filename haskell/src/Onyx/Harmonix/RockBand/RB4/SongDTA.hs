{-
Ported from LibForge
https://github.com/maxton/LibForge
-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE RecordWildCards #-}
module Onyx.Harmonix.RockBand.RB4.SongDTA where

import           Control.Monad          (replicateM_)
import qualified Data.ByteString        as B
import           Data.Profunctor        (dimap)
import           Onyx.Codec.Binary
import           Onyx.Rocksmith.Sng2014 (nullTerm)

data SongDTA = SongDTA
  { sdta_Type               :: Word32
  , sdta_SongId             :: Word32
  , sdta_Version            :: Int16
  , sdta_GameOrigin         :: B.ByteString
  , sdta_PreviewStart       :: Float
  , sdta_PreviewEnd         :: Float
  , sdta_Name               :: B.ByteString
  , sdta_Artist             :: B.ByteString
  , sdta_AlbumName          :: B.ByteString
  , sdta_AlbumTrackNumber   :: Int16
  , sdta_AlbumYear          :: Int32
  , sdta_OriginalYear       :: Int32
  , sdta_Genre              :: B.ByteString
  , sdta_SongLength         :: Float
  , sdta_GuitarRank         :: Float
  , sdta_BassRank           :: Float
  , sdta_VocalsRank         :: Float
  , sdta_DrumRank           :: Float
  , sdta_BandRank           :: Float
  , sdta_KeysRank           :: Float
  , sdta_RealKeysRank       :: Float
  , sdta_Tutorial           :: Bool
  , sdta_AlbumArt           :: Bool
  , sdta_Cover              :: Bool
  , sdta_VocalGender        :: Word8
  , sdta_AnimTempo          :: B.ByteString
  , sdta_HasFreestyleVocals :: Bool
  , sdta_VocalParts         :: Int32
  , sdta_Flags              :: Int32
  , sdta_Fake               :: Bool
  , sdta_Shortname          :: B.ByteString
  } deriving (Show)

boolByte :: BinaryCodec Bool
boolByte = dimap (\b -> if b then 1 else 0) (/= 0) word8

skipBytes :: Int -> CodecFor Get PutM b ()
skipBytes n = replicateM_ n $ const 0 =. word8

instance Bin SongDTA where
  bin = do
    let ?endian = LittleEndian
    sdta_Type               <- sdta_Type               =. binEndian
    sdta_SongId             <- sdta_SongId             =. binEndian
    sdta_Version            <- sdta_Version            =. binEndian
    sdta_GameOrigin         <- sdta_GameOrigin         =. nullTerm 18
    sdta_PreviewStart       <- sdta_PreviewStart       =. binEndian
    sdta_PreviewEnd         <- sdta_PreviewEnd         =. binEndian
    sdta_Name               <- sdta_Name               =. nullTerm 256
    sdta_Artist             <- sdta_Artist             =. nullTerm 256
    sdta_AlbumName          <- sdta_AlbumName          =. nullTerm 256
    sdta_AlbumTrackNumber   <- sdta_AlbumTrackNumber   =. binEndian
    skipBytes 2
    sdta_AlbumYear          <- sdta_AlbumYear          =. binEndian
    sdta_OriginalYear       <- sdta_OriginalYear       =. binEndian
    sdta_Genre              <- sdta_Genre              =. nullTerm 64
    sdta_SongLength         <- sdta_SongLength         =. binEndian
    sdta_GuitarRank         <- sdta_GuitarRank         =. binEndian
    sdta_BassRank           <- sdta_BassRank           =. binEndian
    sdta_VocalsRank         <- sdta_VocalsRank         =. binEndian
    sdta_DrumRank           <- sdta_DrumRank           =. binEndian
    sdta_BandRank           <- sdta_BandRank           =. binEndian
    sdta_KeysRank           <- sdta_KeysRank           =. binEndian
    sdta_RealKeysRank       <- sdta_RealKeysRank       =. binEndian
    sdta_Tutorial           <- sdta_Tutorial           =. boolByte
    sdta_AlbumArt           <- sdta_AlbumArt           =. boolByte
    sdta_Cover              <- sdta_Cover              =. boolByte
    sdta_VocalGender        <- sdta_VocalGender        =. bin
    sdta_AnimTempo          <- sdta_AnimTempo          =. nullTerm 16
    sdta_HasFreestyleVocals <- sdta_HasFreestyleVocals =. boolByte
    skipBytes 3
    sdta_VocalParts         <- sdta_VocalParts         =. binEndian
    sdta_Flags              <- sdta_Flags              =. binEndian
    sdta_Fake               <- sdta_Fake               =. boolByte
    sdta_Shortname          <- sdta_Shortname          =. nullTerm 256
    return SongDTA{..}
