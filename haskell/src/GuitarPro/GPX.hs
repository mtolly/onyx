{-
Thanks to TuxGuitar:
https://github.com/phiresky/tuxguitar/blob/9f08ae077430f2543adbe3e60c81970ced346fa5/TuxGuitar-gpx/src/org/herac/tuxguitar/io/gpx/v6/GPXFileSystem.java
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarPro.GPX (gpxFiles) where

import           Control.Applicative     (optional)
import qualified Data.Binary.Bits.Get    as Bits
import           Data.Binary.Get
import           Data.Bits               (setBit)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import           Data.List               (foldl')
import           Data.Maybe              (mapMaybe)
import           STFS.Package            (runGetM)

gpxFiles :: (MonadFail m) => BL.ByteString -> m [(B.ByteString, BL.ByteString)]
gpxFiles gpx = do
  sectors <- gpxSectors gpx
  return $ flip mapMaybe sectors $ \sector -> do
    (name, size, sectorIDs) <- isFileSector sector
    let contents = BL.take (fromIntegral size) $ BL.concat $ map (sectors !!) sectorIDs
    return (name, contents)

isFileSector :: BL.ByteString -> Maybe (B.ByteString, Int, [Int])
isFileSector sect = case runGetM getWord32le sect of
  Just 2 -> do
    let name = BL.toStrict $ BL.takeWhile (/= 0) $ BL.drop 4 sect
    size <- fmap fromIntegral $ runGetM getWord32le $ BL.drop 0x8C sect
    sectorIDs <- flip runGetM (BL.drop 0x94 sect) $ let
      go = getWord32le >>= \case
        0 -> return []
        n -> (fromIntegral n :) <$> go
      in go
    return (name, size, sectorIDs)
  _ -> Nothing

gpxSectors :: (MonadFail m) => BL.ByteString -> m [BL.ByteString]
gpxSectors gpx = do
  gpx' <- decompressGPX gpx
  let splitSectors bs = if BL.null bs
        then []
        else case BL.splitAt 0x1000 bs of
          (x, y) -> x : splitSectors y
  case BL.splitAt 4 gpx' of
    ("BCFS", rest) -> return $ splitSectors rest
    (magic , _   ) -> fail $ "Unrecognized .gpx magic number after decompression: " <> show magic

decompressGPX :: (MonadFail m) => BL.ByteString -> m BL.ByteString
decompressGPX gpx = flip runGetM gpx $ do
  magic <- getByteString 4
  case magic of
    "BCFS" -> return gpx
    "BCFZ" -> do
      _expectLength <- getWord32le
      applyDecompress <$> Bits.runBitGet getDecompressSteps
    _ -> fail $ "Unrecognized .gpx magic number: " <> show magic

data DecompressStep
  = Raw B.ByteString
  | Repeat Int Int
  deriving (Show)

getDecompressSteps :: Bits.BitGet [DecompressStep]
getDecompressSteps = do
  -- this might miss some partial commands at the end?
  mstep <- optional $ do
    flag <- Bits.block Bits.bool
    if flag
      then do
        bits <- fromIntegral <$> Bits.getWord8 4
        offs <- Bits.block $ bitsLE bits
        size <- Bits.block $ bitsLE bits
        return $ Repeat offs size
      else do
        size <- Bits.block $ bitsLE 2
        Raw <$> Bits.getByteString size
  case mstep of
    Nothing   -> return []
    Just step -> (step :) <$> getDecompressSteps

bitsLE :: Int -> Bits.Block Int
bitsLE len = fmap
  (foldr (\(i, bool) n -> if bool then n `setBit` i else n) 0 . zip [0..])
  (sequenceA $ replicate len Bits.bool)

applyDecompress :: [DecompressStep] -> BL.ByteString
applyDecompress = BB.toLazyByteString . fst . foldl' step (mempty, BL.empty) where
  -- build is the builder for the final product, rev is a reversed bytestring for looking backwards
  step (build, rev) (Raw bs) = (build <> BB.byteString bs, BL.fromStrict (B.reverse bs) <> rev)
  step (build, rev) (Repeat offs size) = let
    newBytesRev = BL.drop (fromIntegral $ offs - min offs size) $ BL.take (fromIntegral offs) rev
    build' = build <> BB.lazyByteString (BL.reverse newBytesRev)
    rev' = newBytesRev <> rev
    in (build', rev')
