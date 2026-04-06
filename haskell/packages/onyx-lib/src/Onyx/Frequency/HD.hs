{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Frequency.HD where

import           Control.Monad   (forM, guard, replicateM, unless, (>=>))
import           Data.Binary.Get
import qualified Data.ByteString as B
import           Data.Char       (isSpace)
import           Data.List       (stripPrefix)
import           Data.Maybe      (mapMaybe)
import           Text.Read       (readMaybe)

data HD = HD
  { hdProg :: [Maybe ProgEntry]
  , hdSset :: [Maybe SsetEntry]
  , hdSmpl :: [Maybe SmplEntry]
  , hdVagi :: [Maybe VagiEntry]
  } deriving (Eq, Show)

getHD :: Get HD
getHD = do
  (locProg, locSset, locSmpl, locVagi) <- lookAhead do
    getSCEI "Vers" \_size -> return ()
    getSCEI "Head" \_size -> do
      _ <- getByteString 12
      _sizeHD <- getWord32le
      _sizeBD <- getWord32le
      (,,,) <$> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le
  prog <- lookAhead $ getByteString (fromIntegral locProg) >> getSCEIList "Prog" getProgEntry
  sset <- lookAhead $ getByteString (fromIntegral locSset) >> getSCEIList "Sset" getSsetEntry
  smpl <- lookAhead $ getByteString (fromIntegral locSmpl) >> getSCEIList "Smpl" getSmplEntry
  vagi <- lookAhead $ getByteString (fromIntegral locVagi) >> getSCEIList "Vagi" getVagiEntry
  return $ HD prog sset smpl vagi

matchByteString :: B.ByteString -> Get ()
matchByteString x' = do
  let x = B.reverse x'
  y <- getByteString $ B.length x
  unless (x == y) $ fail $ "Expected " ++ show x ++ " but found " ++ show y

getSCEI :: B.ByteString -> (Int -> Get a) -> Get a
getSCEI typ g = do
  size <- lookAhead do
    matchByteString "SCEI"
    matchByteString typ
    fromIntegral <$> getWord32le
  isolate size do
    x <- lookAhead $ g size
    _ <- getByteString size
    return x

getSCEIList :: B.ByteString -> Get a -> Get [Maybe a]
getSCEIList typ g = getSCEI typ \_size -> do
  posns <- lookAhead do
    _ <- getByteString 12
    count <- (+ 1) <$> getWord32le
    replicateM (fromIntegral count) getWord32le
  forM posns \case
    0xFFFFFFFF -> return Nothing
    posn       -> fmap Just $ lookAhead do
      _ <- getByteString $ fromIntegral posn
      g

data VagiEntry = VagiEntry
  { vagiFilePosition :: Int
  , vagiRate         :: Int
  , vagiBytes        :: B.ByteString
  } deriving (Eq, Show)

getVagiEntry :: Get VagiEntry
getVagiEntry = do
  bs <- lookAhead $ getByteString 8
  posn <- getWord32le
  rate <- getWord16le
  _ <- getByteString 2
  return $ VagiEntry (fromIntegral posn) (fromIntegral rate) bs

data SmplEntry = SmplEntry
  { smplVagiIndex   :: Int
  , smplBasePitch   :: Int
  , smplMaybeCutoff :: Int
  , smplBytes       :: B.ByteString
  } deriving (Eq, Show)

getSmplEntry :: Get SmplEntry
getSmplEntry = do
  bs <- lookAhead $ getByteString 42
  vagi <- getWord16le
  _ <- getByteString 40
  return $ SmplEntry
    { smplVagiIndex = fromIntegral vagi
    , smplBasePitch = fromIntegral $ B.index bs 11
    , smplMaybeCutoff = fromIntegral $ B.index bs 20
    , smplBytes = bs
    }

data SsetEntry = SsetEntry
  { ssetSmplIndex :: Int
  , ssetBytes     :: B.ByteString
  } deriving (Eq, Show)

getSsetEntry :: Get SsetEntry
getSsetEntry = do
  bs <- lookAhead $ getByteString 6
  _ <- getByteString 4
  smpl <- getWord16le
  return $ SsetEntry (fromIntegral smpl) bs

data ProgEntry = ProgEntry
  { progHeader :: B.ByteString
  , progRows   :: [ProgRow]
  } deriving (Eq, Show)

getProgEntry :: Get ProgEntry
getProgEntry = do
  hdr <- getByteString 0x24
  rows <- replicateM (fromIntegral $ B.index hdr 4) getProgRow
  return $ ProgEntry hdr rows

data ProgRow = ProgRow
  { progRowMinPitch  :: Int
  , progRowMaxPitch  :: Int
  , progRowVol       :: Int -- not sure of this. appears to be out of 80
  , progRowPan       :: Int -- assuming works like amplitude (00 L to 7F R, or maybe 80)
  , progRowSsetIndex :: Int
  , progRowBytes     :: B.ByteString
  } deriving (Eq, Show)

getProgRow :: Get ProgRow
getProgRow = do
  sset <- lookAhead getWord16le
  bs <- getByteString 0x14
  return $ ProgRow
    { progRowBytes = bs
    , progRowMinPitch = fromIntegral $ B.index bs 2
    , progRowMaxPitch = fromIntegral $ B.index bs 4
    , progRowVol = fromIntegral $ B.index bs 16
    , progRowPan = fromIntegral $ B.index bs 17
    , progRowSsetIndex = fromIntegral sset
    }

getBankSections :: String -> Maybe [Int]
getBankSections py = let
  noSpace = filter $ not . isSpace
  lns = map noSpace $ lines py
  in do
    guard $ noSpace "def use_bank_swapping (self): return 1" `elem` lns
    let getBoundaries = stripPrefix "self.section_boundaries=" >=> readMaybe
        defBoundaries = [8, 16, 24, 32] -- from defaults.py
    Just $ case reverse $ mapMaybe getBoundaries lns of
      []     -> defBoundaries
      xs : _ -> xs
