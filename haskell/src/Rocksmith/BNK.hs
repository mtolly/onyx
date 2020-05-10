{-

Done with the help of
https://wiki.xentax.com/index.php/Wwise_SoundBank_(*.bnk)

-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Rocksmith.BNK where

import qualified Data.ByteString as B
import           Data.Word

data BKHD = BKHD
  { bkhd_version :: Word32
  , bkhd_id      :: Word32
  , bkhd_zeroes  :: Word32 -- wiki says 8 zeroes, but sample file has 20
  }

data DIDX = DIDX
  { didx_files :: [DIDXFile]
  }

data DIDXFile = DIDXFile
  { df_id     :: Word32
  , df_offset :: Word32
  , df_length :: Word32
  }

data DATA = DATA
  { data_bytes :: B.ByteString
  }

data HIRC = HIRC
  { hirc_objects :: [HIRCObject]
  }

data HIRCObject
  = HIRCSound Sound -- type 2
  | HIRCEventAction EventAction -- type 3
  | HIRCEvent Event -- type 4
  -- type 7 (actor-mixer) also seen
  | HIRCUnknown Word32 B.ByteString

data Sound = Sound
  { snd_id               :: Word32
  , snd_unknown          :: Word32
  , snd_location         :: Word32 -- 0, 1, 2
  , snd_audioFileID      :: Word32
  , snd_sourceID         :: Word32
  , snd_bankOffsetLength :: Maybe (Word32, Word32)
  , snd_type             :: Word8
  -- more stuff we don't care about
  }

data Event = Event
  { evt_id      :: Word32
  , evt_actions :: [Word32]
  }

data EventAction = EventAction
  { eact_id       :: Word32
  , eact_scope    :: Word8
  , eact_type     :: Word8
  , eact_objectID :: Word32
  , eact_zero1    :: Word8
  , eact_params   :: [(Word8, B.ByteString)]
  , eact_zero2    :: Word8
  -- more stuff we don't care about
  }
