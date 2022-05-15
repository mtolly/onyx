{- |
Written with much assistance from https://github.com/Nanook/Queen-Bee
-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Neversoft.QB where

import           Control.Exception             (evaluate, try)
import           Control.Monad                 (forM, forM_, replicateM, unless,
                                                when)
import           Control.Monad.Codec.Onyx.JSON (pattern OneKey)
import           Control.Monad.ST              (ST)
import           Control.Monad.Trans.State     (StateT, execStateT, get, gets,
                                                put)
import           Data.Aeson
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Text.Encoding.Error      (UnicodeException (..),
                                                strictDecode)
import qualified Data.Vector.Storable          as V
import qualified Data.Vector.Storable.Mutable  as MV
import           Data.Word
import           Neversoft.Checksum            (qbKeyCRC)
import           Numeric                       (showHex)
import           System.IO.Unsafe              (unsafePerformIO)

data QBSection qs k
  = QBSectionInteger k k Word32 -- TODO all these "Integer" data should probably be signed, e.g. vocals_pitch_score_shift
  | QBSectionArray k k (QBArray qs k)
  | QBSectionStruct k k [QBStructItem qs k]
  | QBSectionScript k k Word32 B.ByteString -- decompressed size, then compressed bytestring (not bothering with decompression)
  | QBSectionStringW k k T.Text -- seen in gh3
  deriving (Eq, Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBSection qs k) where
  toJSON = \case
    QBSectionInteger x y z -> OneKey "SectionInteger" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionArray x y z -> OneKey "SectionArray" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionStruct x y z -> OneKey "SectionStruct" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionScript w x y z -> OneKey "SectionScript" $ toJSON [toJSON w, toJSON x, toJSON y, toJSON $ B.unpack z]
    QBSectionStringW x y z -> OneKey "SectionStringW" $ toJSON [toJSON x, toJSON y, toJSON z]

instance (FromJSON qs, FromJSON k) => FromJSON (QBSection qs k) where
  parseJSON = \case
    OneKey "SectionInteger" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionInteger <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionArray" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionArray <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionStruct" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionStruct <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionScript" xs -> parseJSON xs >>= \case
      [w, x, y, z] -> QBSectionScript <$> parseJSON w <*> parseJSON x <*> parseJSON y <*> fmap B.pack (parseJSON z)
      _ -> fail "QB json error"
    OneKey "SectionStringW" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionStringW <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    _ -> fail "QB json error"

data QBArray qs k
  = QBArrayOfQbKey [k]
  | QBArrayOfInteger [Word32]
  | QBArrayOfStruct [[QBStructItem qs k]]
  | QBArrayOfFloat [Float]
  | QBArrayOfFloatRaw [Float] -- QueenBee says "this isn't really an array, just a simple type", I don't know what that means
  | QBArrayOfQbKeyStringQs [qs]
  | QBArrayOfArray [QBArray qs k]
  deriving (Eq, Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBArray qs k) where
  toJSON = \case
    QBArrayOfQbKey x         -> OneKey "ArrayOfQbKey" $ toJSON x
    QBArrayOfInteger x       -> OneKey "ArrayOfInteger" $ toJSON x
    QBArrayOfStruct x        -> OneKey "ArrayOfStruct" $ toJSON x
    QBArrayOfFloat x         -> OneKey "ArrayOfFloat" $ toJSON x
    QBArrayOfFloatRaw x      -> OneKey "ArrayOfFloatRaw" $ toJSON x
    QBArrayOfQbKeyStringQs x -> OneKey "ArrayOfQbKeyStringQs" $ toJSON x
    QBArrayOfArray x         -> OneKey "ArrayOfArray" $ toJSON x

instance (FromJSON qs, FromJSON k) => FromJSON (QBArray qs k) where
  parseJSON = \case
    OneKey "ArrayOfQbKey" x         -> QBArrayOfQbKey <$> parseJSON x
    OneKey "ArrayOfInteger" x       -> QBArrayOfInteger <$> parseJSON x
    OneKey "ArrayOfStruct" x        -> QBArrayOfStruct <$> parseJSON x
    OneKey "ArrayOfFloat" x         -> QBArrayOfFloat <$> parseJSON x
    OneKey "ArrayOfFloatRaw" x      -> QBArrayOfFloatRaw <$> parseJSON x
    OneKey "ArrayOfQbKeyStringQs" x -> QBArrayOfQbKeyStringQs <$> parseJSON x
    _                               -> fail "QB json error"

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
  -- remaining ones are seen in older games like gh3.
  -- should come up with better naming once more is known
  | QBStructItemInteger810000 k Word32
  | QBStructItemQbKeyString9A0000 k k
  | QBStructItemQbKey8D0000 k k
  | QBStructItemStruct8A0000 k [QBStructItem qs k]
  | QBStructItemFloat820000 k Float
  | QBStructItemString830000 k B.ByteString
  | QBStructItemStringW k T.Text
  | QBStructItemArray8C0000 k (QBArray qs k)
  deriving (Eq, Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBStructItem qs k) where
  toJSON = \case
    QBStructHeader -> "StructHeader"
    QBStructItemStruct x y -> OneKey "StructItemStruct" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKey x y -> OneKey "StructItemQbKey" $ toJSON [toJSON x, toJSON y]
    QBStructItemString x y -> OneKey "StructItemString" $ toJSON [toJSON x, toJSON $ B8.unpack y]
    QBStructItemQbKeyString x y -> OneKey "StructItemQbKeyString" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKeyStringQs x y -> OneKey "StructItemQbKeyStringQs" $ toJSON [toJSON x, toJSON y]
    QBStructItemInteger x y -> OneKey "StructItemInteger" $ toJSON [toJSON x, toJSON y]
    QBStructItemFloat x y -> OneKey "StructItemFloat" $ toJSON [toJSON x, toJSON y]
    QBStructItemArray x y -> OneKey "StructItemArray" $ toJSON [toJSON x, toJSON y]
    QBStructItemInteger810000 x y -> OneKey "StructItemInteger810000" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKeyString9A0000 x y -> OneKey "StructItemQbKeyString9A0000" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKey8D0000 x y -> OneKey "StructItemQbKey8D0000" $ toJSON [toJSON x, toJSON y]
    QBStructItemStruct8A0000 x y -> OneKey "StructItemStruct8A0000" $ toJSON [toJSON x, toJSON y]
    QBStructItemFloat820000 x y -> OneKey "StructItemFloat820000" $ toJSON [toJSON x, toJSON y]
    QBStructItemString830000 x y -> OneKey "StructItemString830000" $ toJSON [toJSON x, toJSON $ B8.unpack y]
    QBStructItemStringW x y -> OneKey "StructItemStringW" $ toJSON [toJSON x, toJSON y]
    QBStructItemArray8C0000 x y -> OneKey "StructItemArray8C0000" $ toJSON [toJSON x, toJSON y]

instance (FromJSON qs, FromJSON k) => FromJSON (QBStructItem qs k) where
  parseJSON = \case
    "StructHeader" -> return QBStructHeader
    OneKey "StructItemStruct" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemStruct <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemQbKey" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemQbKey <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemString" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemString <$> parseJSON x <*> fmap B8.pack (parseJSON y)
      _ -> fail "QB json error"
    OneKey "StructItemQbKeyString" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemQbKeyString <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemQbKeyStringQs" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemQbKeyStringQs <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemInteger" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemInteger <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemFloat" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemFloat <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemArray" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemArray <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemInteger810000" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemInteger810000 <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemQbKeyString9A0000" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemQbKeyString9A0000 <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemQbKey8D0000" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemQbKey8D0000 <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemStruct8A0000" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemStruct8A0000 <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    _ -> fail "QB json error"

instance Bifunctor QBSection where
  first f = \case
    QBSectionInteger x y n    -> QBSectionInteger x y n
    QBSectionArray x y arr    -> QBSectionArray x y $ first f arr
    QBSectionStruct x y items -> QBSectionStruct x y $ map (first f) items
    QBSectionScript w x y z   -> QBSectionScript w x y z
    QBSectionStringW x y s    -> QBSectionStringW x y s
  second = fmap

instance Bifunctor QBArray where
  first f = \case
    QBArrayOfQbKey ks         -> QBArrayOfQbKey ks
    QBArrayOfInteger ns       -> QBArrayOfInteger ns
    QBArrayOfStruct structs   -> QBArrayOfStruct $ map (map $ first f) structs
    QBArrayOfFloat ns         -> QBArrayOfFloat ns
    QBArrayOfFloatRaw ns      -> QBArrayOfFloatRaw ns
    QBArrayOfQbKeyStringQs qs -> QBArrayOfQbKeyStringQs $ map f qs
    QBArrayOfArray arrs       -> QBArrayOfArray $ map (first f) arrs
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
    QBStructItemInteger810000 x y -> QBStructItemInteger810000 x y
    QBStructItemQbKeyString9A0000 x y -> QBStructItemQbKeyString9A0000 x y
    QBStructItemQbKey8D0000 x y -> QBStructItemQbKey8D0000 x y
    QBStructItemStruct8A0000 x items -> QBStructItemStruct8A0000 x $ map (first f) items
    QBStructItemFloat820000 x y -> QBStructItemFloat820000 x y
    QBStructItemString830000 x y -> QBStructItemString830000 x y
    QBStructItemStringW x y -> QBStructItemStringW x y
    QBStructItemArray8C0000 x arr -> QBStructItemArray8C0000 x $ first f arr
  second = fmap

instance Bifoldable QBSection where
  bifoldMap f g = \case
    QBSectionInteger x y _     -> g x <> g y
    QBSectionArray   x y arr   -> g x <> g y <> bifoldMap f g arr
    QBSectionStruct  x y items -> g x <> g y <> mconcat (map (bifoldMap f g) items)
    QBSectionScript  x y _ _   -> g x <> g y
    QBSectionStringW x y _     -> g x <> g y

instance Bifoldable QBArray where
  bifoldMap f g = \case
    QBArrayOfQbKey         ks      -> mconcat (map g ks)
    QBArrayOfInteger       _       -> mempty
    QBArrayOfStruct        structs -> mconcat $ mconcat $ map (map $ bifoldMap f g) structs
    QBArrayOfFloat         _       -> mempty
    QBArrayOfFloatRaw      _       -> mempty
    QBArrayOfQbKeyStringQs qs      -> mconcat $ map f qs
    QBArrayOfArray         arrs    -> mconcat $ map (bifoldMap f g) arrs

instance Bifoldable QBStructItem where
  bifoldMap f g = \case
    QBStructHeader                    -> mempty
    QBStructItemStruct        x items -> g x <> mconcat (map (bifoldMap f g) items)
    QBStructItemQbKey         x y     -> g x <> g y
    QBStructItemString        x _     -> g x
    QBStructItemQbKeyString   x y     -> g x <> g y
    QBStructItemQbKeyStringQs x qs    -> g x <> f qs
    QBStructItemInteger       x _     -> g x
    QBStructItemFloat         x _     -> g x
    QBStructItemArray         x arr   -> g x <> bifoldMap f g arr
    QBStructItemInteger810000 x _     -> g x
    QBStructItemQbKeyString9A0000 x y -> g x <> g y
    QBStructItemQbKey8D0000   x y     -> g x <> g y
    QBStructItemStruct8A0000  x items -> g x <> mconcat (map (bifoldMap f g) items)
    QBStructItemFloat820000   x _     -> g x
    QBStructItemString830000  x _     -> g x
    QBStructItemStringW       x _     -> g x
    QBStructItemArray8C0000   x arr   -> g x <> bifoldMap f g arr

allQS :: (Bifoldable obj) => obj qs k -> [qs]
allQS = bifoldMap (: []) (const [])

shouldBeAt :: Word32 -> Get ()
shouldBeAt w = do
  p <- fromIntegral <$> bytesRead
  unless (p == w) $ fail $ unwords
    [ "QB parser position expected to be"
    , "0x" <> showHex w ""
    , "but we're at"
    , "0x" <> showHex p ""
    ]

parseQBSubArray :: Get (QBArray Word32 Word32)
parseQBSubArray = do
  arrayType <- getWord32be
  len <- fromIntegral <$> getWord32be
  case len of
    0 -> skip 4
    1 -> return ()
    _ -> do
      p3 <- getWord32be
      shouldBeAt p3
  case arrayType of
    0x00010D00 -> QBArrayOfQbKey <$> replicateM len getWord32be
    0x00010100 -> QBArrayOfInteger <$> replicateM len getWord32be
    0x00010A00 -> do
      structStarts <- replicateM len getWord32be
      fmap QBArrayOfStruct $ forM structStarts $ \p4 -> do
        shouldBeAt p4
        parseQBStruct
    0x00010200 -> QBArrayOfFloat <$> replicateM len getFloatbe
    0x00010000 -> QBArrayOfFloatRaw <$> replicateM len getFloatbe
    0x00011C00 -> QBArrayOfQbKeyStringQs <$> replicateM len getWord32be
    0x00010C00 -> do
      arrayStarts <- replicateM len getWord32be
      fmap QBArrayOfArray $ forM arrayStarts $ \p4 -> do
        shouldBeAt p4
        parseQBSubArray
    _ -> fail $ "Unrecognized array type: 0x" <> showHex arrayType ""

parseQBArray :: Get (QBArray Word32 Word32, Word32)
parseQBArray = do
  p1 <- getWord32be
  p2 <- getWord32be
  shouldBeAt p1
  array <- parseQBSubArray
  return (array, p2)

-- Skip to next position divisible by 4
jumpTo4 :: Get ()
jumpTo4 = do
  posn <- bytesRead
  case rem posn 4 of
    0 -> return ()
    1 -> skip 3
    2 -> skip 2
    _ -> skip 1

parseQBStruct :: Get [QBStructItem Word32 Word32]
parseQBStruct = do
  itemType <- getWord32be
  (item, nextPosition) <- case itemType of
    0x00000100 -> do
      p <- getWord32be
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
              jumpTo4
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
    0x00810000 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemInteger810000 x y, p)
    0x009A0000 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemQbKeyString9A0000 x y, p)
    0x008D0000 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemQbKey8D0000 x y, p)
    0x008A0000 -> do
      x <- getWord32be
      p1 <- getWord32be
      p2 <- getWord32be
      shouldBeAt p1
      items <- parseQBStruct
      return (QBStructItemStruct8A0000 x items, p2)
    0x00820000 -> do
      x <- getWord32be
      f <- getFloatbe
      p <- getWord32be
      return (QBStructItemFloat820000 x f, p)
    0x00830000 -> do
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
              jumpTo4
              return []
            else (c :) <$> getNullTerm
        in B.pack <$> getNullTerm
      return (QBStructItemString830000 x b, p)
    0x00840000 -> do
      x <- getWord32be
      start <- getWord32be
      p <- getWord32be
      shouldBeAt start
      str <- getUtf16BE
      jumpTo4
      return (QBStructItemStringW x str, p)
    0x008C0000 -> do
      x <- getWord32be
      (array, p) <- parseQBArray
      return (QBStructItemArray8C0000 x array, p)
    _ -> fail $ "Unrecognized struct item type: 0x" <> showHex itemType ""
  case nextPosition of
    0 -> return [item] -- this is the last item
    _ -> do
      shouldBeAt nextPosition
      (item :) <$> parseQBStruct

getUtf16BE :: Get T.Text
getUtf16BE = do
  let go = do
        x <- getWord8
        y <- getWord8
        if x == 0 && y == 0
          then return []
          else ([x, y] <>) <$> go
  bytes <- B.pack <$> go
  case unsafePerformIO $ try $ evaluate $ TE.decodeUtf16BEWith strictDecode bytes of
    Left (DecodeError err _) -> fail $ "getUtf16BE: text decode failed; " <> err
    Left _                   -> fail "shouldn't happen!" -- deprecated EncodeError
    Right t                  -> return t

parseQBSection :: Get (QBSection Word32 Word32)
parseQBSection = do
  sectionType <- getWord32be
  itemQbKeyCrc <- getWord32be
  fileId <- getWord32be
  case sectionType of
    0x00200100 {- SectionInteger -} -> do
      n1 <- getWord32be
      n2 <- getWord32be
      when (n2 /= 0) $ fail "SectionInteger: expected 0 for second number"
      return $ QBSectionInteger itemQbKeyCrc fileId n1
    0x00200C00 {- SectionArray -} -> do
      (array, _) <- parseQBArray
      -- the snd above should be 0, I think
      return $ QBSectionArray itemQbKeyCrc fileId array
    0x00200A00 {- SectionStruct -} -> do
      p1 <- getWord32be
      _reserved <- getWord32be
      shouldBeAt p1
      QBSectionStruct itemQbKeyCrc fileId <$> parseQBStruct
    0x00200700 {- SectionScript -} -> do
      p1 <- getWord32be
      _reserved <- getWord32be -- 0
      shouldBeAt p1
      _unknown <- getWord32be -- 0xFFFFFFFF
      decompressedSize <- getWord32be
      compressedSize <- getWord32be
      bs <- getByteString $ fromIntegral compressedSize
      jumpTo4
      return $ QBSectionScript itemQbKeyCrc fileId decompressedSize bs
    0x00200400 {- SectionStringW -} -> do
      p1 <- getWord32be
      _reserved <- getWord32be -- 0
      shouldBeAt p1
      str <- getUtf16BE
      jumpTo4
      return $ QBSectionStringW itemQbKeyCrc fileId str
    _ -> fail $ "Unrecognized section type: 0x" <> showHex sectionType ""

parseQB :: Get [QBSection Word32 Word32]
parseQB = do
  _zero1 <- getWord32be
  fileSize <- getWord32be
  _headerSize <- getWord32le -- 0x1C, little endian? why?
  _zero2 <- getWord32be
  _contentSize <- getWord32be -- file size - header size
  _zero3 <- getWord32be
  _zero4 <- getWord32be
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

instance FromJSON QSResult where
  parseJSON = \case
    OneKey "UnknownQS" x -> UnknownQS <$> parseJSON x
    OneKey "KnownQS" xs -> parseJSON xs >>= \case
      [x, y] -> KnownQS <$> parseJSON x <*> parseJSON y
      _      -> fail "QB qs json error"
    _ -> fail "QB qs json error"

data QBResult
  = UnknownQB Word32
  | KnownQB B.ByteString
  deriving (Show)

instance ToJSON QBResult where
  toJSON = \case
    UnknownQB qb -> toJSON qb
    KnownQB b    -> toJSON $ B8.unpack b

instance FromJSON QBResult where
  parseJSON = \case
    Number n -> return $ UnknownQB $ round n
    String s -> return $ KnownQB $ B8.pack $ T.unpack s
    _        -> fail "QB key json error"

lookupQS :: (Bifunctor obj) => HM.HashMap Word32 T.Text -> obj Word32 k -> obj QSResult k
lookupQS mapping = first $ \qs -> case HM.lookup qs mapping of
  Nothing -> UnknownQS qs
  Just t  -> KnownQS qs t

lookupQB :: (Bifunctor obj) => HM.HashMap Word32 B.ByteString -> obj qs Word32 -> obj qs QBResult
lookupQB mapping = second $ \qb -> case HM.lookup qb mapping of
  Nothing -> UnknownQB qb
  Just b  -> KnownQB b

-----------------------------

type PutSeek s = StateT (MV.STVector s Word8, Int) (ST s)
type Pointer = Int

grow :: Int -> PutSeek s ()
grow need = do
  (v, used) <- get
  let available = MV.length v
  when (available < need) $ do
    -- at least double in size
    v' <- MV.grow v $ max available $ need - available
    put (v', used)

append :: B.ByteString -> PutSeek s ()
append b = do
  get >>= \(_, used) -> grow $ used + B.length b
  (v, used) <- get
  forM_ (zip [used ..] $ B.unpack b) $ \(i, c) -> MV.write v i c
  put (v, used + B.length b)

w32 :: Word32 -> PutSeek s ()
w32 = append . BL.toStrict . runPut . putWord32be

reservePointer :: PutSeek s Pointer
reservePointer = do
  p <- gets snd
  w32 0xDEADBEEF
  return p

setPointer :: Pointer -> Word32 -> PutSeek s ()
setPointer p w = do
  let bs = BL.toStrict $ runPut $ putWord32be w
  (v, _) <- get
  forM_ (zip [p ..] $ B.unpack bs) $ \(i, c) -> MV.write v i c

setPointerLE :: Pointer -> Word32 -> PutSeek s ()
setPointerLE p w = do
  let bs = BL.toStrict $ runPut $ putWord32le w
  (v, _) <- get
  forM_ (zip [p ..] $ B.unpack bs) $ \(i, c) -> MV.write v i c

fillPointer :: Pointer -> PutSeek s ()
fillPointer p = do
  posn <- gets snd
  setPointer p $ fromIntegral posn

putQBArray :: QBArray Word32 Word32 -> PutSeek s Pointer
putQBArray ary = do
  p1 <- reservePointer
  p2 <- reservePointer
  fillPointer p1
  putQBSubArray ary
  return p2

putQBSubArray :: QBArray Word32 Word32 -> PutSeek s ()
putQBSubArray ary = let
  writeLen xs = do
    let len = length xs
    w32 $ fromIntegral len
    case len of
      0 -> w32 0
      1 -> return ()
      _ -> reservePointer >>= fillPointer
  in case ary of
    QBArrayOfQbKey ks -> do
      w32 0x00010D00
      writeLen ks
      mapM_ w32 ks
    QBArrayOfInteger ns -> do
      w32 0x00010100
      writeLen ns
      mapM_ w32 ns
    QBArrayOfStruct structs -> do
      w32 0x00010A00
      writeLen structs
      ptrStructPairs <- mapM (\s -> (\p -> (p, s)) <$> reservePointer) structs
      forM_ ptrStructPairs $ \(p, s) -> do
        fillPointer p
        putQBStruct s
    QBArrayOfFloat floats -> do
      w32 0x00010200
      writeLen floats
      mapM_ (append . BL.toStrict . runPut . putFloatbe) floats
    QBArrayOfFloatRaw floats -> do
      w32 0x00010000
      writeLen floats
      mapM_ (append . BL.toStrict . runPut . putFloatbe) floats
    QBArrayOfQbKeyStringQs qs -> do
      w32 0x00011C00
      writeLen qs
      mapM_ w32 qs
    QBArrayOfArray arrs -> do
      w32 0x00010C00
      writeLen arrs
      ptrArrayPairs <- mapM (\a -> (\p -> (p, a)) <$> reservePointer) arrs
      forM_ ptrArrayPairs $ \(p, a) -> do
        fillPointer p
        putQBSubArray a

padTo4 :: B.ByteString -> B.ByteString
padTo4 bs = case rem (B.length bs) 4 of
  0 -> bs
  r -> bs <> B.replicate (4 - r) 0

putQBStruct :: [QBStructItem Word32 Word32] -> PutSeek s ()
putQBStruct = let
  go prevPointer [] = forM_ prevPointer $ \p -> setPointer p 0
  go prevPointer (item : items) = do
    mapM_ fillPointer prevPointer
    p <- case item of
      QBStructHeader -> do
        w32 0x00000100
        reservePointer
      QBStructItemQbKey x y -> do
        w32 0x00010D00
        w32 x
        w32 y
        reservePointer
      QBStructItemQbKeyStringQs x y -> do
        w32 0x00011C00
        w32 x
        w32 y
        reservePointer
      QBStructItemQbKeyString x y -> do
        w32 0x00011A00
        w32 x
        w32 y
        reservePointer
      QBStructItemInteger x y -> do
        w32 0x00010100
        w32 x
        w32 y
        reservePointer
      QBStructItemString x b -> do
        w32 0x00010300
        w32 x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        append $ padTo4 $ b <> B.singleton 0
        return p
      QBStructItemFloat x f -> do
        w32 0x00010200
        w32 x
        append $ BL.toStrict $ runPut $ putFloatbe f
        reservePointer
      QBStructItemArray x array -> do
        w32 0x00010C00
        w32 x
        putQBArray array
      QBStructItemStruct x sub -> do
        w32 0x00010A00
        w32 x
        p1 <- reservePointer
        p2 <- reservePointer
        fillPointer p1
        putQBStruct sub
        return p2
      QBStructItemInteger810000 x y -> do
        w32 0x00810000
        w32 x
        w32 y
        reservePointer
      QBStructItemQbKeyString9A0000 x y -> do
        w32 0x009A0000
        w32 x
        w32 y
        reservePointer
      QBStructItemQbKey8D0000 x y -> do
        w32 0x008D0000
        w32 x
        w32 y
        reservePointer
      QBStructItemStruct8A0000 x sub -> do
        w32 0x008A0000
        w32 x
        p1 <- reservePointer
        p2 <- reservePointer
        fillPointer p1
        putQBStruct sub
        return p2
      QBStructItemFloat820000 x f -> do
        w32 0x00820000
        w32 x
        append $ BL.toStrict $ runPut $ putFloatbe f
        reservePointer
      QBStructItemString830000 x b -> do
        w32 0x00830000
        w32 x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        append $ padTo4 $ b <> B.singleton 0
        return p
      QBStructItemStringW x str -> do
        w32 0x00840000
        w32 x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        append $ padTo4 $ TE.encodeUtf16BE $ str <> "\0"
        return p
      QBStructItemArray8C0000 x array -> do
        w32 0x008C0000
        w32 x
        putQBArray array
    go (Just p) items
  in go Nothing

putQBSection :: QBSection Word32 Word32 -> PutSeek s ()
putQBSection = \case
  QBSectionInteger itemQbKeyCrc fileId n1 -> do
    w32 0x00200100
    w32 itemQbKeyCrc
    w32 fileId
    w32 n1
    w32 0
  QBSectionArray itemQbKeyCrc fileId array -> do
    w32 0x00200C00
    w32 itemQbKeyCrc
    w32 fileId
    p <- putQBArray array
    setPointer p 0
  QBSectionStruct itemQbKeyCrc fileId struct -> do
    w32 0x00200A00
    w32 itemQbKeyCrc
    w32 fileId
    p <- reservePointer
    w32 0
    fillPointer p
    putQBStruct struct
  QBSectionScript itemQbKeyCrc fileId decompressedSize bs -> do
    w32 0x00200700
    w32 itemQbKeyCrc
    w32 fileId
    p <- reservePointer
    w32 0
    fillPointer p
    w32 0xFFFFFFFF
    w32 decompressedSize
    w32 $ fromIntegral $ B.length bs
    append $ padTo4 bs
  QBSectionStringW itemQbKeyCrc fileId str -> do
    w32 0x00200400
    w32 itemQbKeyCrc
    w32 fileId
    p <- reservePointer
    w32 0
    fillPointer p
    append $ padTo4 $ TE.encodeUtf16BE $ str <> "\0"

putQB :: [QBSection Word32 Word32] -> BL.ByteString
putQB sects = let
  go = do
    w32 0
    pFileSize <- reservePointer
    pHeaderSize <- reservePointer
    w32 0
    pContentSize <- reservePointer
    w32 0
    w32 0
    headerSize <- gets snd
    mapM_ putQBSection sects
    fileSize <- gets snd
    setPointer pFileSize $ fromIntegral fileSize
    setPointerLE pHeaderSize $ fromIntegral headerSize
    setPointer pContentSize $ fromIntegral $ fileSize - headerSize
  in BL.pack $ V.toList $ V.create $ do
    v <- MV.new 0
    (v', used) <- execStateT go (v, 0)
    return $ MV.take used v'

discardQS :: [QBSection QSResult k] -> [QBSection Word32 k]
discardQS = map $ first $ \case UnknownQS qs -> qs; KnownQS qs _ -> qs

discardQB :: [QBSection qs QBResult] -> [QBSection qs Word32]
discardQB = map $ second $ \case UnknownQB qb -> qb; KnownQB b -> qbKeyCRC b

discardStrings :: [QBSection QSResult QBResult] -> [QBSection Word32 Word32]
discardStrings = discardQS . discardQB
