{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Neversoft.PS2 where

import           Control.Applicative   (liftA2)
import           Data.Bifunctor        (bimap)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL
import           Data.Int
import qualified Data.List.NonEmpty    as NE
import           Data.List.Split       (chunksOf)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Word
import           Onyx.Util.Handle
import           Onyx.Wii.WAD          (roundUpToMultiple, skipToMultiple)
import           Onyx.Xbox.STFS        (runGetM)

-- DATAP.HED + DATAP.WAD, contains most smaller files

-- Entry in DATAP.HED, found in GH3 PS2 and possibly others
data HedEntry = HedEntry
  { hedID   :: Word32
  , hedSize :: Word32
  , hedName :: B.ByteString
  } deriving (Show)

parseHed :: Get [HedEntry]
parseHed = go [] where
  go entries = do
    hedID <- getWord32le
    if hedID == 0xFFFFFFFF
      then return $ reverse entries
      else do
        hedSize <- getWord32le
        hedName <- getNullTerm
        skipToMultiple 4
        go $ HedEntry{..} : entries

getNullTerm :: Get B.ByteString
getNullTerm = let
  loop !n = do
    b <- getWord8
    if b == 0
      then return n
      else loop $ n + 1
  in do
    len <- lookAhead $ loop 0
    bs <- getByteString len
    skip 1 -- skip null byte
    return bs

applyHed :: [HedEntry] -> Folder B.ByteString (Word32, Word32)
applyHed entries = let
  findPositions _   []       = []
  findPositions !pos (e : es) = let
    -- 0x5C is backslash (seen in GH3), 0x2F is forward slash (seen in GH5).
    -- paths also start with a slash/backslash so we remove it before split
    isSlash c = c == 0x5C || c == 0x2F
    splitName = NE.fromList $ B.splitWith isSlash $ B.dropWhile isSlash $ hedName e
    -- TODO I think the jump values are different for GH5. Maybe always 0x8000?
    jumpNext = case es of
      next : _ -> if ".pak.ps2" `B8.isSuffixOf` hedName next
        then 0x8000
        else 0x800
      [] -> 0x800 -- doesn't matter
    nextPosition = roundUpToMultiple jumpNext $ pos + hedSize e
    in (splitName, (pos, hedSize e)) : findPositions nextPosition es
  in fromFiles $ findPositions 0 entries

hookUpWAD :: Readable -> Folder B.ByteString (Word32, Word32) -> Folder T.Text Readable
hookUpWAD wad = bimap TE.decodeLatin1
  -- TODO add label to handle
  $ \(pos, len) -> subHandle id (fromIntegral pos) (Just $ fromIntegral len) wad

-- .IMF + .ISF, main audio files (XA, like VGS but different interleaving)
-- note: SOUNDS/SFX.WAD also contains XA but looks like some differences

splitEvery :: Int64 -> BL.ByteString -> [BL.ByteString]
splitEvery n bs = if BL.null bs
  then []
  else let (x, y) = BL.splitAt n bs in x : splitEvery n y

detectChannels :: BL.ByteString -> Int
detectChannels = length . takeWhile ("MSVp" `BL.isPrefixOf`) . splitEvery 0x20000

splitChannels :: BL.ByteString -> [BL.ByteString]
splitChannels bs = do
  let blocks = splitEvery 0x20000 bs
      chans = detectChannels bs
      groups = chunksOf chans blocks
  i <- [0 .. chans - 1]
  return $ BL.concat $ groups >>= \group -> case drop i group of
    []      -> []
    blk : _ -> [blk]

convertChannelToVGS :: (MonadFail m) => BL.ByteString -> m BL.ByteString
convertChannelToVGS chan = do
  (len, rate) <- flip runGetM (BL.drop 12 chan) $ liftA2 (,) getWord32be getWord32be
  return $ runPut $ do
    putByteString $ B8.pack "VgS!"
    putWord32le 2
    let audio = BL.take (fromIntegral len) $ BL.drop 0x40 chan
    putWord32le $ fromIntegral rate
    putWord32le $ fromIntegral $ len `quot` 0x10
    putByteString $ B.replicate (0x80 - 16) 0
    putLazyByteString audio
