{-
Ported from RawkSD
https://github.com/AerialX/rawksd/blob/f97aef64cbcc66/consolehaxx/ConsoleHaxx.Wii/WAD.cs
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Onyx.Nintendo.WAD where

import           Control.Monad
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           Onyx.Nintendo.U8
import           Onyx.Util.Handle

data WADHeader = WADHeader
  { wadHeaderSize           :: Word32
  , wadType                 :: Word32
  , wadCertificateChainSize :: Word32
  , wadReserved             :: Word32
  , wadTicketSize           :: Word32
  , wadTmdSize              :: Word32
  , wadDataSize             :: Word32
  , wadFooterSize           :: Word32
  } deriving (Show)

data WAD = WAD
  { wadHeader           :: WADHeader
  , wadCertificateChain :: B.ByteString
  , wadTicket           :: Ticket
  , wadTMD              :: TMD
  , wadEncrypted        :: B.ByteString
  , wadFooter           :: B.ByteString
  } deriving (Show)

data Ticket = Ticket
  { ticketSignature    :: Signature
  , ticketUnknown1     :: B.ByteString
  , ticketEncryptedKey :: B.ByteString
  , ticketUnknown2     :: Word8
  , ticketID           :: Word64
  , ticketConsoleID    :: Word32
  , ticketTitleID      :: Word64
  , ticketAccessMask   :: Word16
  , ticketUnknown3     :: B.ByteString
  , ticketContentMask  :: B.ByteString
  , ticketPadding      :: Word16
  , ticketLimits       :: [(Word32, Word32)]
  } deriving (Show)

data SignatureType
  = RSA4096
  | RSA2048
  | EllipticCurve
  deriving (Show)

signatureDataSize :: SignatureType -> Int
signatureDataSize = \case
  RSA2048       -> 0x100
  RSA4096       -> 0x200
  EllipticCurve -> 0x40

data Signature = Signature
  { sigType    :: SignatureType
  , sigData    :: B.ByteString
  , sigPadding :: B.ByteString
  , sigIssuer  :: B.ByteString
  } deriving (Show)

getSignature :: Get Signature
getSignature = do
  sigType <- getWord32be >>= \case
    0x00010000 -> return RSA4096
    0x00010001 -> return RSA2048
    0x00010002 -> return EllipticCurve
    _          -> fail "Unknown signature type"
  sigData <- getByteString $ signatureDataSize sigType
  sigPadding <- getByteString 0x3C
  sigIssuer <- getByteString 0x40
  return Signature{..}

getTicket :: Get Ticket
getTicket = do
  ticketSignature <- getSignature
  ticketUnknown1     <- getByteString 0x3F
  ticketEncryptedKey <- getByteString 0x10
  ticketUnknown2     <- getWord8
  ticketID           <- getWord64be
  ticketConsoleID    <- getWord32be
  ticketTitleID      <- getWord64be
  ticketAccessMask   <- getWord16be
  ticketUnknown3     <- getByteString 0x3C
  ticketContentMask  <- getByteString 0x40
  ticketPadding      <- getWord16be
  ticketLimits       <- replicateM 8 $ do
    tag   <- getWord32be
    value <- getWord32be
    return (tag, value)
  return Ticket{..}

data TMD = TMD
  { tmdSignature          :: Signature
  , tmdVersion            :: Word8
  , tmdCaCrlVersion       :: Word8
  , tmdSignerCaCrlVersion :: Word8
  , tmdPadding            :: Word8
  , tmdSystemVersion      :: Word64
  , tmdTitleID            :: Word64
  , tmdTitleType          :: Word32
  , tmdGroupID            :: Word16
  , tmdPadding2           :: B.ByteString
  , tmdAccessRights       :: Word32
  , tmdTitleVersion       :: Word16
  -- number of tmdContents goes here
  , tmdBootIndex          :: Word16
  , tmdPadding3           :: Word16
  , tmdContents           :: [TMDContent]
  } deriving (Show)

getTMD :: Get TMD
getTMD = do
  tmdSignature          <- getSignature
  tmdVersion            <- getWord8
  tmdCaCrlVersion       <- getWord8
  tmdSignerCaCrlVersion <- getWord8
  tmdPadding            <- getWord8
  tmdSystemVersion      <- getWord64be
  tmdTitleID            <- getWord64be
  tmdTitleType          <- getWord32be
  tmdGroupID            <- getWord16be
  tmdPadding2           <- getByteString 0x3E
  tmdAccessRights       <- getWord32be
  tmdTitleVersion       <- getWord16be
  countContents         <- fromIntegral <$> getWord16be
  tmdBootIndex          <- getWord16be
  tmdPadding3           <- getWord16be
  tmdContents           <- replicateM countContents getTMDContent
  return TMD{..}

data TMDContent = TMDContent
  { tcContentID :: Word32
  , tcIndex     :: Word16
  , tcType      :: Word16
  , tcSize      :: Int64
  , tcHash      :: B.ByteString
  } deriving (Show)

getTMDContent :: Get TMDContent
getTMDContent = do
  tcContentID <- getWord32be
  tcIndex     <- getWord16be
  tcType      <- getWord16be
  tcSize      <- getInt64be
  tcHash      <- getByteString 0x14
  return TMDContent{..}

getWADHeader :: Get WADHeader
getWADHeader = do
  wadHeaderSize           <- getWord32be
  wadType                 <- getWord32be
  wadCertificateChainSize <- getWord32be
  wadReserved             <- getWord32be
  wadTicketSize           <- getWord32be
  wadTmdSize              <- getWord32be
  wadDataSize             <- getWord32be
  wadFooterSize           <- getWord32be
  return WADHeader{..}

roundUpToMultiple :: (Integral a) => a -> a -> a
roundUpToMultiple align n = case rem n align of
  0       -> n
  partial -> n + (align - partial)

skipToMultiple :: Int64 -> Get ()
skipToMultiple align = do
  pos <- bytesRead
  skip $ fromIntegral $ roundUpToMultiple align pos - pos

getWAD :: Get WAD
getWAD = do
  wadHeader           <- getWADHeader
  skipToMultiple 0x40
  wadCertificateChain <- getByteString $ fromIntegral $ wadCertificateChainSize wadHeader
  skipToMultiple 0x40
  wadTicket           <- getTicket
  skipToMultiple 0x40
  wadTMD              <- getTMD
  skipToMultiple 0x40
  wadEncrypted        <- getByteString $ fromIntegral $ wadDataSize wadHeader
  skipToMultiple 0x40
  wadFooter           <- getByteString $ fromIntegral $ wadFooterSize wadHeader
  return WAD{..}

getKey :: (MonadFail m) => WAD -> m B.ByteString
getKey wad = do
  commonKey <- case B.unpack $ B.take 1 $ B.drop 0xB $ ticketUnknown3 $ wadTicket wad of
    -- common key
    [0] -> return $ B.pack [0xeb, 0xe4, 0x2a, 0x22, 0x5e, 0x85, 0x93, 0xe4, 0x48, 0xd9, 0xc5, 0x45, 0x73, 0x81, 0xaa, 0xf7]
    -- korean common key
    [1] -> return $ B.pack [0x63, 0xb8, 0x2b, 0xb4, 0xf4, 0x61, 0x4e, 0x2e, 0x13, 0xf2, 0xfe, 0xfb, 0xba, 0x4c, 0x9b, 0x7e]
    _   -> fail "Unrecognized key parameter"
  Just iv <- return $ makeIV $ BL.toStrict (runPut $ putWord64be $ ticketTitleID $ wadTicket wad) <> B.replicate 8 0
  CryptoPassed cipher <- return $ cipherInit commonKey
  return $ cbcDecrypt (cipher :: AES128) iv $ ticketEncryptedKey $ wadTicket wad

getDecryptedData :: (MonadFail m) => WAD -> m B.ByteString
getDecryptedData wad = do
  Just iv <- return $ makeIV $ B.replicate 0x10 0
  key <- getKey wad
  CryptoPassed cipher <- return $ cipherInit key
  return $ cbcDecrypt (cipher :: AES128) iv $ wadEncrypted wad

{-

in heart of the sunrise decrypted data:
0xBB00 is where the meta (songs.dta, preview audio, png_wii) U8 file starts.
but the first 0x10 bytes are random (obfuscated? hash?) instead of the expected first 16 U8 bytes.
first tmd content entry says size is 0xBAE0, if you go there then 0x40 align then you're at 0xBB00.
0x52880 is where the play (bik, mid, milo_wii) U8 file starts.
0x52980 is where the bink file starts. (KIBE magic)
0x15EA680 is where the midi file starts. (MThd magic)

meta U8 entries:
dir   0x0     0x0     0x8 # (root)
dir   0x1     0x0     0x8 # content
dir   0x9     0x1     0x8 # songs
file  0xF 0x3BD00   0x5B3 # songs.dta
dir  0x19     0x2     0x8 # heartofthesunrise
file 0x2B   0x100 0x3BBED # heartofthesunrise_prev.mogg, starts at 0xBC00
dir  0x47     0x4     0x8 # gen, why is the folder level 4 here? should be 3?
file 0x4B 0x3C2C0  0xAAA0 # heartofthesunrise_keep.png_wii, starts at 0x47DC0, ends at 0x52860

so the meta U8 is 290144 bytes long (0x52860 - 0xbb00), followed by 0x20 zeroes padding.
does that mean it is this tmd content?
TMDContent {tcContentID = 439, tcIndex = 386, tcType = 16385, tcSize = 290144, tcHash = "\203L\197\177\221\DELkX\180\180\229\194\147\SO3\190\ACK>\b\228"}

play U8 entries:
dir   0x0       0x0       0x8 # (root)
dir   0x1       0x0       0x8 # content
dir   0x9       0x1       0x8 # songs
dir   0xF       0x2       0x8 # heartofthesunrise
file 0x21     0x100 0x1597CF4 # heartofthesunrise.bik
file 0x37 0x1597E00   0x5484D # heartofthesunrise.mid
dir  0x4D       0x3       0x8 # gen
file 0x51 0x15EC660   0x1B485 # heartofthesunrise.milo_wii, starts at 0x163EEE0, ends at 0x165A365

play U8 is 0x165A365 - 0x52880 = 23100133 bytes long.
so it's this:
TMDContent {tcContentID = 440, tcIndex = 387, tcType = 16385, tcSize = 23100133, tcHash = ":\211^\157-\255\ETX/\161e\237\222\&6\178\246\137f\230\152L"}

thus, these are the only files that actually have data:
TMDContent {tcContentID = 569, tcIndex = 0, tcType = 1, tcSize = 47840, tcHash = "gE\208\SOH\190\&9(n\DC4`]\255$d\241\219\163q\162\222"}
TMDContent {tcContentID = 439, tcIndex = 386, tcType = 16385, tcSize = 290144, tcHash = "\203L\197\177\221\DELkX\180\180\229\194\147\SO3\190\ACK>\b\228"}
TMDContent {tcContentID = 440, tcIndex = 387, tcType = 16385, tcSize = 23100133, tcHash = ":\211^\157-\255\ETX/\161e\237\222\&6\178\246\137f\230\152L"}

According to https://gitlab.com/rock-band-customs/wii-vwii-dolphin/-/wikis/Official-Rock-Band-DLC-By-Generation/Rock-Band-3-DLC-Generation-B
the indexes 386/387 refer to Heart of the Sunrise specifically.

Our hack import process:
- Use first tmd content to get the size of the unknown first file to skip past.
- Skip to 0x40 alignment.
- Read first U8 file, up to wherever last file data ends.
- Skip to 0x40 alignment.
- Should now be at second U8 file. Read the same way.
- (Skip to 0x40 alignment, should now be at end of the decrypted data)

-}

-- Assumes the wad data is in the format
--   ( unknown first file which is first entry in tmd content
--   : sequence of zero or more U8 files
--   )
hackSplitU8s :: (MonadFail m) => WAD -> m (B.ByteString, [(B.ByteString, Folder B.ByteString BL.ByteString)])
hackSplitU8s wad = do
  dec <- getDecryptedData wad
  firstSize <- case tmdContents $ wadTMD wad of
    []    -> fail "No TMD contents"
    x : _ -> return $ tcSize x
  let firstData = B.take (fromIntegral firstSize) dec
      afterFirstData = B.drop (fromIntegral $ roundUpToMultiple 0x40 firstSize) dec
      getU8s bs = if B.null bs
        then return []
        else do
          (folder, usedBytes) <- readU8 $ BL.fromStrict bs
          rest <- getU8s $ B.drop (fromIntegral $ roundUpToMultiple 0x40 usedBytes) bs
          return $ (B.take (fromIntegral usedBytes) bs, folder) : rest
  u8s <- getU8s afterFirstData
  return (firstData, u8s)
