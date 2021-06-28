{- |
Written with much assistance from https://github.com/Nanook/Queen-Bee
-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Neversoft.QB where

import           Control.Applicative           ((<|>))
import           Control.Monad                 (forM, replicateM, unless, when)
import           Control.Monad.Codec.Onyx.JSON (pattern OneKey)
import           Data.Aeson
import           Data.Bifunctor
import           Data.Binary.Get
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Data.Word
import           Numeric                       (showHex)

data QBSection qs k
  = QBSectionInteger k k Word32
  | QBSectionArray k k (QBArray qs k)
  | QBSectionStruct k k [QBStructItem qs k]
  deriving (Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBSection qs k) where
  toJSON = \case
    QBSectionInteger x y z -> OneKey "QBSectionInteger" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionArray x y z -> OneKey "QBSectionArray" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionStruct x y z -> OneKey "QBSectionStruct" $ toJSON [toJSON x, toJSON y, toJSON z]

data QBArray qs k
  = QBArrayOfQbKey Bool [k] -- Bool is True if there is a pointer before the key list
  | QBArrayOfInteger [Word32]
  | QBArrayOfStruct [[QBStructItem qs k]]
  | QBArrayOfFloat [Float]
  | QBArrayOfQbKeyStringQs [qs]
  deriving (Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBArray qs k) where
  toJSON = \case
    QBArrayOfQbKey x y -> OneKey "QBArrayOfQbKey" $ toJSON [toJSON x, toJSON y]
    QBArrayOfInteger x -> OneKey "QBArrayOfInteger" $ toJSON x
    QBArrayOfStruct x -> OneKey "QBArrayOfStruct" $ toJSON x
    QBArrayOfFloat x -> OneKey "QBArrayOfFloat" $ toJSON x
    QBArrayOfQbKeyStringQs x -> OneKey "QBArrayOfQbKeyStringQs" $ toJSON x

data QBStructItem qs k
  = QBStructHeader -- empty
  | QBStructItemStruct k [QBStructItem qs k]
  | QBStructItemQbKey k k
  | QBStructItemString k B.ByteString
  | QBStructItemQbKeyString k k
  | QBStructItemQbKeyStringQs k qs
  | QBStructItemInteger k Word32
  | QBStructItemFloat k Float
  | QBStructItemArray k (QBArray qs k)
  deriving (Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBStructItem qs k) where
  toJSON = \case
    QBStructHeader -> "QBStructHeader"
    QBStructItemStruct x y -> OneKey "QBStructItemStruct" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKey x y -> OneKey "QBStructItemQbKey" $ toJSON [toJSON x, toJSON y]
    QBStructItemString x y -> OneKey "QBStructItemString" $ toJSON [toJSON x, toJSON $ B8.unpack y]
    QBStructItemQbKeyString x y -> OneKey "QBStructItemQbKeyString" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKeyStringQs x y -> OneKey "QBStructItemQbKeyStringQs" $ toJSON [toJSON x, toJSON y]
    QBStructItemInteger x y -> OneKey "QBStructItemInteger" $ toJSON [toJSON x, toJSON y]
    QBStructItemFloat x y -> OneKey "QBStructItemFloat" $ toJSON [toJSON x, toJSON y]
    QBStructItemArray x y -> OneKey "QBStructItemArray" $ toJSON [toJSON x, toJSON y]

instance Bifunctor QBSection where
  first f = \case
    QBSectionInteger x y n -> QBSectionInteger x y n
    QBSectionArray x y arr -> QBSectionArray x y $ first f arr
    QBSectionStruct x y items -> QBSectionStruct x y $ map (first f) items
  second = fmap

instance Bifunctor QBArray where
  first f = \case
    QBArrayOfQbKey b ks -> QBArrayOfQbKey b ks
    QBArrayOfInteger ns -> QBArrayOfInteger ns
    QBArrayOfStruct structs -> QBArrayOfStruct $ map (map $ first f) structs
    QBArrayOfFloat ns -> QBArrayOfFloat ns
    QBArrayOfQbKeyStringQs qs -> QBArrayOfQbKeyStringQs $ map f qs
  second = fmap

instance Bifunctor QBStructItem where
  first f = \case
    QBStructHeader -> QBStructHeader
    QBStructItemStruct x items -> QBStructItemStruct x $ map (first f) items
    QBStructItemQbKey x y -> QBStructItemQbKey x y
    QBStructItemString x y -> QBStructItemString x y
    QBStructItemQbKeyString x y -> QBStructItemQbKeyString x y
    QBStructItemQbKeyStringQs x qs -> QBStructItemQbKeyStringQs x $ f qs
    QBStructItemInteger x y -> QBStructItemInteger x y
    QBStructItemFloat x y -> QBStructItemFloat x y
    QBStructItemArray x arr -> QBStructItemArray x $ first f arr
  second = fmap

shouldBeAt :: Word32 -> Get ()
shouldBeAt w = do
  p <- fromIntegral <$> bytesRead
  unless (p == w) $ fail $ unwords
    [ "QB parser position expected to be"
    , "0x" <> showHex w ""
    , "but we're at"
    , "0x" <> showHex p ""
    ]

parseQBArray :: Get (QBArray Word32 Word32, Word32)
parseQBArray = do
  p1 <- getWord32be
  p2 <- getWord32be
  shouldBeAt p1
  arrayType <- getWord32be
  array <- case arrayType of
    0x00010D00 -> do
      len <- fromIntegral <$> getWord32be
      -- TODO figure out why this pointer is sometimes absent
      hasPointer <- lookAhead $ do
        p3 <- getWord32be
        (shouldBeAt p3 >> return True) <|> return False
      when hasPointer $ skip 4
      QBArrayOfQbKey hasPointer <$> replicateM len getWord32be
    0x00010100 -> do
      len <- fromIntegral <$> getWord32be
      p3 <- getWord32be
      shouldBeAt p3
      QBArrayOfInteger <$> replicateM len getWord32be
    0x00010A00 -> do
      len <- fromIntegral <$> getWord32be
      p3 <- getWord32be
      shouldBeAt p3
      lookAhead getWord32be >>= \case
        0x100 -> skip 4 -- ???
        _     -> return ()
      structStarts <- replicateM len getWord32be
      fmap QBArrayOfStruct $ forM structStarts $ \p4 -> do
        shouldBeAt p4
        parseQBStruct
    0x00010000 -> do
      len <- fromIntegral <$> getWord32be
      skip 4 -- 0?
      QBArrayOfFloat <$> replicateM len getFloatbe
    0x00011C00 -> do
      len <- fromIntegral <$> getWord32be
      p3 <- getWord32be
      shouldBeAt p3
      QBArrayOfQbKeyStringQs <$> replicateM len getWord32be
    _ -> fail $ "Unrecognized array type: 0x" <> showHex arrayType ""
  return (array, p2)

parseQBStruct :: Get [QBStructItem Word32 Word32]
parseQBStruct = do
  itemType <- getWord32be
  (item, nextPosition) <- case itemType of
    0x00000100 -> do
      p <- getWord32be
      -- assuming these are always empty?
      return (QBStructHeader, p)
    0x00010D00 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemQbKey x y, p)
    0x00011C00 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemQbKeyStringQs x y, p)
    0x00011A00 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemQbKeyString x y, p)
    0x00010100 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemInteger x y, p)
    0x00010300 -> do
      x <- getWord32be
      start <- getWord32be
      p <- getWord32be
      shouldBeAt start
      b <- let
        -- get a null-terminated string, then jump to the next 4-divisible position
        getNullTerm = do
          c <- getWord8
          if c == 0
            then do
              posn <- bytesRead
              case rem posn 4 of
                0 -> return () -- we already read the one zero
                1 -> skip 3
                2 -> skip 2
                _ -> skip 1
              return []
            else (c :) <$> getNullTerm
        in B.pack <$> getNullTerm
      return (QBStructItemString x b, p)
    0x00010200 -> do
      x <- getWord32be
      f <- getFloatbe
      p <- getWord32be
      return (QBStructItemFloat x f, p)
    0x00010C00 -> do
      x <- getWord32be
      (array, p) <- parseQBArray
      return (QBStructItemArray x array, p)
    0x00010A00 -> do
      x <- getWord32be
      p1 <- getWord32be
      p2 <- getWord32be
      shouldBeAt p1
      items <- parseQBStruct
      return (QBStructItemStruct x items, p2)
    _ -> fail $ "Unrecognized struct item type: 0x" <> showHex itemType ""
  case nextPosition of
    0 -> return [item] -- this is the last item
    _ -> do
      shouldBeAt nextPosition
      (item :) <$> parseQBStruct

parseQBSection :: Get (QBSection Word32 Word32)
parseQBSection = do
  sectionType <- getWord32be
  case sectionType of
    0x00200100 {- SectionInteger -} -> do
      itemQbKeyCrc <- getWord32be
      fileId <- getWord32be
      n1 <- getWord32be
      n2 <- getWord32be
      when (n2 /= 0) $ fail "SectionInteger: expected 0 for second number"
      return $ QBSectionInteger itemQbKeyCrc fileId n1
    0x00200C00 {- SectionArray -} -> do
      itemQbKeyCrc <- getWord32be
      fileId <- getWord32be
      (array, _) <- parseQBArray
      -- the snd above should be 0, I think
      return $ QBSectionArray itemQbKeyCrc fileId array
    0x00200A00 {- SectionStruct -} -> do
      itemQbKeyCrc <- getWord32be
      fileId <- getWord32be
      p1 <- getWord32be
      _reserved <- getWord32be
      shouldBeAt p1
      QBSectionStruct itemQbKeyCrc fileId <$> parseQBStruct
    _ -> fail $ "Unrecognized section type: 0x" <> showHex sectionType ""

parseQB :: Get [QBSection Word32 Word32]
parseQB = do
  _magic <- getWord32be
  fileSize <- getWord32be
  _unknown <- getByteString 20
  let parseSections = do
        pos <- fromIntegral <$> bytesRead
        if pos >= fileSize
          then return []
          else (:) <$> parseQBSection <*> parseSections
  parseSections

data QSResult
  = UnknownQS Word32
  | KnownQS Word32 T.Text
  deriving (Show)

instance ToJSON QSResult where
  toJSON = \case
    UnknownQS qs -> OneKey "UnknownQS" $ toJSON qs
    KnownQS qs t -> OneKey "KnownQS" $ toJSON [toJSON qs, toJSON t]

data QBResult
  = UnknownQB Word32
  | KnownQB B.ByteString
  deriving (Show)

instance ToJSON QBResult where
  toJSON = \case
    UnknownQB qb -> OneKey "UnknownQB" $ toJSON qb
    KnownQB b -> OneKey "KnownQB" $ toJSON $ B8.unpack b

lookupQS :: (Bifunctor obj) => HM.HashMap Word32 T.Text -> obj Word32 k -> obj QSResult k
lookupQS mapping = first $ \qs -> case HM.lookup qs mapping of
  Nothing -> UnknownQS qs
  Just t  -> KnownQS qs t

lookupQB :: (Bifunctor obj) => HM.HashMap Word32 B.ByteString -> obj qs Word32 -> obj qs QBResult
lookupQB mapping = second $ \qb -> case HM.lookup qb mapping of
  Nothing -> UnknownQB qb
  Just b  -> KnownQB b
