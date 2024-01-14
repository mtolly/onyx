{- |
Written with much assistance from https://github.com/Nanook/Queen-Bee
-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Onyx.Neversoft.QB where

import           Control.Exception            (evaluate, try)
import           Control.Monad                (forM, forM_, replicateM, unless,
                                               when)
import           Control.Monad.ST             (ST)
import           Control.Monad.Trans.State    (StateT, execStateT, get, gets,
                                               put)
import           Data.Aeson
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import qualified Data.HashMap.Strict          as HM
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Text.Encoding.Error     (UnicodeException (..),
                                               strictDecode)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word
import           GHC.ByteOrder
import           Numeric                      (showHex)
import           Onyx.Codec.Binary            (binEndian, codecIn)
import           Onyx.Codec.JSON              (pattern OneKey)
import           Onyx.Neversoft.CRC           (qbKeyCRC)
import           System.IO.Unsafe             (unsafePerformIO)

data QBFormat
  = QBFormatPS2
  | QBFormatNew

data QBSection qs k
  = QBSectionInteger k k Word32 -- TODO all these "Integer" data should probably be signed, e.g. vocals_pitch_score_shift
  | QBSectionArray k k (QBArray qs k)
  | QBSectionQbKey k k k
  | QBSectionStruct k k [QBStructItem qs k]
  | QBSectionScript k k Word32 B.ByteString -- decompressed size, then compressed bytestring (not bothering with decompression)
  | QBSectionString k k B.ByteString -- seen in gh3 ps2
  | QBSectionStringW k k T.Text -- seen in gh3
  | QBSectionQbKeyStringQs k k qs
  | QBSectionQbKeyString k k k
  | QBSectionFloat k k Float
  | QBSectionFloatsX2 k k Float Float
  | QBSectionFloatsX3 k k Float Float Float
  deriving (Eq, Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBSection qs k) where
  toJSON = \case
    QBSectionInteger x y z -> OneKey "SectionInteger" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionArray x y z -> OneKey "SectionArray" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionQbKey x y z -> OneKey "SectionQbKey" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionStruct x y z -> OneKey "SectionStruct" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionScript w x y z -> OneKey "SectionScript" $ toJSON [toJSON w, toJSON x, toJSON y, toJSON $ B.unpack z]
    QBSectionString x y z -> OneKey "SectionString" $ toJSON [toJSON x, toJSON y, toJSON $ B8.unpack z]
    QBSectionStringW x y z -> OneKey "SectionStringW" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionQbKeyStringQs x y z -> OneKey "SectionQbKeyStringQs" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionQbKeyString x y z -> OneKey "SectionQbKeyString" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionFloat x y f1 -> OneKey "SectionFloat" $ toJSON [toJSON x, toJSON y, toJSON f1]
    QBSectionFloatsX2 x y f1 f2 -> OneKey "SectionFloatsX2" $ toJSON [toJSON x, toJSON y, toJSON f1, toJSON f2]
    QBSectionFloatsX3 x y f1 f2 f3 -> OneKey "SectionFloatsX3" $ toJSON [toJSON x, toJSON y, toJSON f1, toJSON f2, toJSON f3]

instance (FromJSON qs, FromJSON k) => FromJSON (QBSection qs k) where
  parseJSON = \case
    OneKey "SectionInteger" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionInteger <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionArray" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionArray <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionQbKey" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionQbKey <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionStruct" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionStruct <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionScript" xs -> parseJSON xs >>= \case
      [w, x, y, z] -> QBSectionScript <$> parseJSON w <*> parseJSON x <*> parseJSON y <*> fmap B.pack (parseJSON z)
      _ -> fail "QB json error"
    OneKey "SectionString" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionString <$> parseJSON x <*> parseJSON y <*> fmap B8.pack (parseJSON z)
      _ -> fail "QB json error"
    OneKey "SectionStringW" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionStringW <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionQbKeyStringQs" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionQbKeyStringQs <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionQbKeyString" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionQbKeyString <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    OneKey "SectionFloat" xs -> parseJSON xs >>= \case
      [x, y, f1] -> QBSectionFloat <$> parseJSON x <*> parseJSON y <*> parseJSON f1
      [x, y, f1, f2] -> QBSectionFloatsX2 <$> parseJSON x <*> parseJSON y <*> parseJSON f1 <*> parseJSON f2
      [x, y, f1, f2, f3] -> QBSectionFloatsX3 <$> parseJSON x <*> parseJSON y <*> parseJSON f1 <*> parseJSON f2 <*> parseJSON f3
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
  | QBArrayOfQbKeyString [k]
  | QBArrayOfFloatsX2 [(Float, Float)]
  | QBArrayOfFloatsX3 [(Float, Float, Float)]
  | QBArrayOfString [B.ByteString]
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
    QBArrayOfQbKeyString x   -> OneKey "ArrayOfQbKeyString" $ toJSON x
    QBArrayOfFloatsX2 x      -> OneKey "ArrayOfFloatsX2" $ toJSON x
    QBArrayOfFloatsX3 x      -> OneKey "ArrayOfFloatsX3" $ toJSON x
    QBArrayOfString x        -> OneKey "ArrayOfString" $ toJSON $ map B8.unpack x

instance (FromJSON qs, FromJSON k) => FromJSON (QBArray qs k) where
  parseJSON = \case
    OneKey "ArrayOfQbKey" x         -> QBArrayOfQbKey <$> parseJSON x
    OneKey "ArrayOfInteger" x       -> QBArrayOfInteger <$> parseJSON x
    OneKey "ArrayOfStruct" x        -> QBArrayOfStruct <$> parseJSON x
    OneKey "ArrayOfFloat" x         -> QBArrayOfFloat <$> parseJSON x
    OneKey "ArrayOfFloatRaw" x      -> QBArrayOfFloatRaw <$> parseJSON x
    OneKey "ArrayOfQbKeyStringQs" x -> QBArrayOfQbKeyStringQs <$> parseJSON x
    OneKey "ArrayOfQbKeyString" x   -> QBArrayOfQbKeyString <$> parseJSON x
    OneKey "ArrayOfFloatsX2" x      -> QBArrayOfFloatsX2 <$> parseJSON x
    OneKey "ArrayOfFloatsX3" x      -> QBArrayOfFloatsX3 <$> parseJSON x
    OneKey "ArrayOfString" x        -> QBArrayOfString . map B8.pack <$> parseJSON x
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
  | QBStructItemFloatsX2 k Float Float
  | QBStructItemFloatsX3 k Float Float Float
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
    QBStructItemFloatsX2 x y z -> OneKey "StructItemFloatsX2" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBStructItemFloatsX3 w x y z -> OneKey "StructItemFloatsX3" $ toJSON [toJSON w, toJSON x, toJSON y, toJSON z]
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
    OneKey "StructItemFloatsX2" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBStructItemFloatsX2 <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _      -> fail "QB json error"
    OneKey "StructItemFloatsX3" xs -> parseJSON xs >>= \case
      [w, x, y, z] -> QBStructItemFloatsX3 <$> parseJSON w <*> parseJSON x <*> parseJSON y <*> parseJSON z
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
    QBSectionInteger x y n         -> QBSectionInteger x y n
    QBSectionArray x y arr         -> QBSectionArray x y $ first f arr
    QBSectionQbKey x y z           -> QBSectionQbKey x y z
    QBSectionStruct x y items      -> QBSectionStruct x y $ map (first f) items
    QBSectionScript w x y z        -> QBSectionScript w x y z
    QBSectionString x y s          -> QBSectionString x y s
    QBSectionStringW x y s         -> QBSectionStringW x y s
    QBSectionQbKeyStringQs x y z   -> QBSectionQbKeyStringQs x y (f z)
    QBSectionQbKeyString x y z     -> QBSectionQbKeyString x y z
    QBSectionFloat    x y f1       -> QBSectionFloat x y f1
    QBSectionFloatsX2 x y f1 f2    -> QBSectionFloatsX2 x y f1 f2
    QBSectionFloatsX3 x y f1 f2 f3 -> QBSectionFloatsX3 x y f1 f2 f3
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
    QBArrayOfQbKeyString ks   -> QBArrayOfQbKeyString ks
    QBArrayOfFloatsX2 vs      -> QBArrayOfFloatsX2 vs
    QBArrayOfFloatsX3 vs      -> QBArrayOfFloatsX3 vs
    QBArrayOfString bs        -> QBArrayOfString bs
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
    QBStructItemFloatsX2 x y z -> QBStructItemFloatsX2 x y z
    QBStructItemFloatsX3 w x y z -> QBStructItemFloatsX3 w x y z
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
    QBSectionInteger x y _       -> g x <> g y
    QBSectionArray   x y arr     -> g x <> g y <> bifoldMap f g arr
    QBSectionQbKey   x y z       -> g x <> g y <> g z
    QBSectionStruct  x y items   -> g x <> g y <> mconcat (map (bifoldMap f g) items)
    QBSectionScript  x y _ _     -> g x <> g y
    QBSectionString  x y _       -> g x <> g y
    QBSectionStringW x y _       -> g x <> g y
    QBSectionQbKeyStringQs x y z -> g x <> g y <> f z
    QBSectionQbKeyString x y z   -> g x <> g y <> g z
    QBSectionFloat    x y _      -> g x <> g y
    QBSectionFloatsX2 x y _ _    -> g x <> g y
    QBSectionFloatsX3 x y _ _ _  -> g x <> g y

instance Bifoldable QBArray where
  bifoldMap f g = \case
    QBArrayOfQbKey         ks      -> mconcat (map g ks)
    QBArrayOfInteger       _       -> mempty
    QBArrayOfStruct        structs -> mconcat $ mconcat $ map (map $ bifoldMap f g) structs
    QBArrayOfFloat         _       -> mempty
    QBArrayOfFloatRaw      _       -> mempty
    QBArrayOfQbKeyStringQs qs      -> mconcat $ map f qs
    QBArrayOfArray         arrs    -> mconcat $ map (bifoldMap f g) arrs
    QBArrayOfQbKeyString   ks      -> mconcat (map g ks)
    QBArrayOfFloatsX2      _       -> mempty
    QBArrayOfFloatsX3      _       -> mempty
    QBArrayOfString        _       -> mempty

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
    QBStructItemFloatsX2      x _ _   -> g x
    QBStructItemFloatsX3      x _ _ _ -> g x
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

parseQBSubArray :: (?endian :: ByteOrder, ?format :: QBFormat) => Get (QBArray Word32 Word32)
parseQBSubArray = do
  arrayType <- getW32
  len <- fromIntegral <$> getW32
  case len of
    0 -> skip 4
    1 -> return ()
    _ -> do
      p3 <- getW32
      shouldBeAt p3
  let arrayOfArray = do
        arrayStarts <- replicateM len getW32
        fmap QBArrayOfArray $ forM arrayStarts $ \p4 -> do
          shouldBeAt p4
          parseQBSubArray
      arrayOfString = do
        stringStarts <- replicateM len getW32
        res <- fmap QBArrayOfString $ forM stringStarts $ \p4 -> do
          shouldBeAt p4
          qbNullTerm
        jumpTo4
        return res
      arrayOfStruct = do
        structStarts <- replicateM len getW32
        fmap QBArrayOfStruct $ forM structStarts $ \p4 -> do
          shouldBeAt p4
          parseQBStruct
      arrayOfFloatsX2 = do
        vectorStarts <- replicateM len getW32
        fmap QBArrayOfFloatsX2 $ forM vectorStarts $ \p4 -> do
          shouldBeAt p4
          parseQBFloatsX2
      arrayOfFloatsX3 = do
        vectorStarts <- replicateM len getW32
        fmap QBArrayOfFloatsX3 $ forM vectorStarts $ \p4 -> do
          shouldBeAt p4
          parseQBFloatsX3
  case ?format of
    QBFormatNew -> case arrayType of
      0x00010D00 -> QBArrayOfQbKey <$> replicateM len getW32
      0x00010100 -> QBArrayOfInteger <$> replicateM len getW32
      0x00010A00 -> arrayOfStruct
      0x00010200 -> QBArrayOfFloat <$> replicateM len getFloat
      0x00010000 -> QBArrayOfFloatRaw <$> replicateM len getFloat
      0x00011C00 -> QBArrayOfQbKeyStringQs <$> replicateM len getW32
      0x00010C00 -> arrayOfArray
      0x00011A00 -> QBArrayOfQbKeyString <$> replicateM len getW32
      0x00010500 -> arrayOfFloatsX2
      0x00010600 -> arrayOfFloatsX3
      0x00010300 -> arrayOfString
      _          -> fail $ "Unrecognized array type: 0x" <> showHex arrayType ""
    QBFormatPS2 -> case arrayType of
      0x000D0100 -> QBArrayOfQbKey <$> replicateM len getW32
      0x00010100 -> QBArrayOfInteger <$> replicateM len getW32
      0x000A0100 -> arrayOfStruct
      0x00020100 -> QBArrayOfFloat <$> replicateM len getFloat
      0x00000100 -> QBArrayOfFloatRaw <$> replicateM len getFloat
      0x001C0100 -> QBArrayOfQbKeyStringQs <$> replicateM len getW32
      0x000C0100 -> arrayOfArray
      0x00050100 -> arrayOfFloatsX2
      0x00060100 -> arrayOfFloatsX3
      0x001A0100 -> QBArrayOfQbKeyString <$> replicateM len getW32
      0x00030100 -> arrayOfString
      _          -> fail $ "Unrecognized array type: 0x" <> showHex arrayType ""

parseQBArray :: (?endian :: ByteOrder, ?format :: QBFormat) => Get (QBArray Word32 Word32, Word32)
parseQBArray = do
  p1 <- getW32
  p2 <- getW32
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

qbNullTerm :: Get B.ByteString
qbNullTerm = let
  getNullTerm = do
    c <- getWord8
    if c == 0
      then return []
      else (c :) <$> getNullTerm
  in B.pack <$> getNullTerm

parseQBFloatsX2 :: (?endian :: ByteOrder, ?format :: QBFormat) => Get (Float, Float)
parseQBFloatsX2 = do
  typ <- getW32
  let expectedType = case ?format of
        QBFormatNew -> 0x00010000
        QBFormatPS2 -> 0x00000100
  unless (typ == expectedType) $ fail "FloatsX2 does not have Floats subtype?"
  f1 <- getFloat
  f2 <- getFloat
  return (f1, f2)

parseQBFloatsX3 :: (?endian :: ByteOrder, ?format :: QBFormat) => Get (Float, Float, Float)
parseQBFloatsX3 = do
  typ <- getW32
  let expectedType = case ?format of
        QBFormatNew -> 0x00010000
        QBFormatPS2 -> 0x00000100
  unless (typ == expectedType) $ fail "FloatsX3 does not have Floats subtype?"
  f1 <- getFloat
  f2 <- getFloat
  f3 <- getFloat
  return (f1, f2, f3)

parseQBStruct :: (?endian :: ByteOrder, ?format :: QBFormat) => Get [QBStructItem Word32 Word32]
parseQBStruct = do
  itemType <- getW32
  let structHeader = do
        p <- getW32
        return (QBStructHeader, p)
      structItemQbKey = do
        x <- getW32
        y <- getW32
        p <- getW32
        return (QBStructItemQbKey x y, p)
      structItemQbKeyStringQs = do
        x <- getW32
        y <- getW32
        p <- getW32
        return (QBStructItemQbKeyStringQs x y, p)
      structItemQbKeyString = do
        x <- getW32
        y <- getW32
        p <- getW32
        return (QBStructItemQbKeyString x y, p)
      structItemInteger = do
        x <- getW32
        y <- getW32
        p <- getW32
        return (QBStructItemInteger x y, p)
      structItemString = do
        x <- getW32
        start <- getW32
        p <- getW32
        shouldBeAt start
        b <- qbNullTerm
        jumpTo4
        return (QBStructItemString x b, p)
      structItemFloat = do
        x <- getW32
        f <- getFloat
        p <- getW32
        return (QBStructItemFloat x f, p)
      structItemArray = do
        x <- getW32
        (array, p) <- parseQBArray
        return (QBStructItemArray x array, p)
      structItemFloatsX2 = do
        x <- getW32
        p1 <- getW32
        p2 <- getW32
        shouldBeAt p1
        (f1, f2) <- parseQBFloatsX2
        return (QBStructItemFloatsX2 x f1 f2, p2)
      structItemFloatsX3 = do
        x <- getW32
        p1 <- getW32
        p2 <- getW32
        shouldBeAt p1
        (f1, f2, f3) <- parseQBFloatsX3
        return (QBStructItemFloatsX3 x f1 f2 f3, p2)
      structItemStruct = do
        x <- getW32
        p1 <- getW32
        p2 <- getW32
        shouldBeAt p1
        items <- parseQBStruct
        return (QBStructItemStruct x items, p2)
      structItemInteger810000 = do
        x <- getW32
        y <- getW32
        p <- getW32
        return (QBStructItemInteger810000 x y, p)
      structItemQbKeyString9A0000 = do
        x <- getW32
        y <- getW32
        p <- getW32
        return (QBStructItemQbKeyString9A0000 x y, p)
      structItemQbKey8D0000 = do
        x <- getW32
        y <- getW32
        p <- getW32
        return (QBStructItemQbKey8D0000 x y, p)
      structItemStruct8A0000 = do
        x <- getW32
        p1 <- getW32
        p2 <- getW32
        shouldBeAt p1
        items <- parseQBStruct
        return (QBStructItemStruct8A0000 x items, p2)
      structItemFloat820000 = do
        x <- getW32
        f <- getFloat
        p <- getW32
        return (QBStructItemFloat820000 x f, p)
      structItemString830000 = do
        x <- getW32
        start <- getW32
        p <- getW32
        shouldBeAt start
        b <- qbNullTerm
        jumpTo4
        return (QBStructItemString830000 x b, p)
      structItemStringW = do
        x <- getW32
        start <- getW32
        p <- getW32
        shouldBeAt start
        str <- getUtf16BE
        jumpTo4
        return (QBStructItemStringW x str, p)
      structItemArray8C0000 = do
        x <- getW32
        (array, p) <- parseQBArray
        return (QBStructItemArray8C0000 x array, p)
  (item, nextPosition) <- case ?format of
    QBFormatNew -> case itemType of
      0x00000100 -> structHeader
      0x00010D00 -> structItemQbKey
      0x00011C00 -> structItemQbKeyStringQs
      0x00011A00 -> structItemQbKeyString
      0x00010100 -> structItemInteger
      0x00010300 -> structItemString
      0x00010200 -> structItemFloat
      0x00010C00 -> structItemArray
      0x00010500 -> structItemFloatsX2
      0x00010600 -> structItemFloatsX3
      0x00010A00 -> structItemStruct
      0x00810000 -> structItemInteger810000
      0x009A0000 -> structItemQbKeyString9A0000
      0x008D0000 -> structItemQbKey8D0000
      0x008A0000 -> structItemStruct8A0000
      0x00820000 -> structItemFloat820000
      0x00830000 -> structItemString830000
      0x00840000 -> structItemStringW
      0x008C0000 -> structItemArray8C0000
      _ -> fail $ "Unrecognized struct item type: 0x" <> showHex itemType ""
    QBFormatPS2 -> case itemType of
      0x00010000 -> structHeader
      0x000D0100 -> structItemQbKey
      0x001C0100 -> structItemQbKeyStringQs
      0x001A0100 -> structItemQbKeyString
      0x00010100 -> structItemInteger
      0x00030100 -> structItemString
      0x00020100 -> structItemFloat
      0x000C0100 -> structItemArray
      0x00050100 -> structItemFloatsX2
      0x00060100 -> structItemFloatsX3
      0x000A0100 -> structItemStruct
      0x00000300 -> structItemInteger810000
      0x00003500 -> structItemQbKeyString9A0000
      0x00001B00 -> structItemQbKey8D0000
      0x00001500 -> structItemStruct8A0000
      0x00000500 -> structItemFloat820000
      0x00000700 -> structItemString830000
      0x00000900 -> structItemStringW
      0x00001900 -> structItemArray8C0000
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

parseQBSection :: (?endian :: ByteOrder, ?format :: QBFormat) => Get (QBSection Word32 Word32)
parseQBSection = do
  sectionType <- getW32
  itemQbKeyCrc <- getW32
  fileId <- getW32
  let sectionInteger = do
        n1 <- getW32
        n2 <- getW32
        when (n2 /= 0) $ fail "SectionInteger: expected 0 for second number"
        return $ QBSectionInteger itemQbKeyCrc fileId n1
      sectionArray = do
        (array, _) <- parseQBArray
        -- the snd above should be 0, I think
        return $ QBSectionArray itemQbKeyCrc fileId array
      sectionQbKey = do
        n1 <- getW32
        n2 <- getW32
        when (n2 /= 0) $ fail "SectionQbKey: expected 0 for second number"
        return $ QBSectionQbKey itemQbKeyCrc fileId n1
      sectionStruct = do
        p1 <- getW32
        _reserved <- getW32
        shouldBeAt p1
        QBSectionStruct itemQbKeyCrc fileId <$> parseQBStruct
      sectionScript = do
        p1 <- getW32
        _reserved <- getW32 -- 0
        shouldBeAt p1
        _unknown <- getW32 -- 0xFFFFFFFF
        decompressedSize <- getW32
        compressedSize <- getW32
        bs <- getByteString $ fromIntegral compressedSize
        jumpTo4
        return $ QBSectionScript itemQbKeyCrc fileId decompressedSize bs
      sectionString = do
        p1 <- getW32
        _reserved <- getW32 -- 0
        shouldBeAt p1
        bs <- qbNullTerm
        jumpTo4
        return $ QBSectionString itemQbKeyCrc fileId bs
      sectionStringW = do
        p1 <- getW32
        _reserved <- getW32 -- 0
        shouldBeAt p1
        str <- getUtf16BE
        jumpTo4
        return $ QBSectionStringW itemQbKeyCrc fileId str
      sectionQbKeyStringQs = do
        n1 <- getW32
        n2 <- getW32
        when (n2 /= 0) $ fail "SectionQbKeyStringQs: expected 0 for second number"
        return $ QBSectionQbKeyStringQs itemQbKeyCrc fileId n1
      sectionQbKeyString = do
        n1 <- getW32
        n2 <- getW32
        when (n2 /= 0) $ fail "SectionQbKeyString: expected 0 for second number"
        return $ QBSectionQbKeyString itemQbKeyCrc fileId n1
      sectionFloat = do
        f1 <- getFloat
        n <- getW32
        when (n /= 0) $ fail "SectionFloat: expected 0 after float"
        return $ QBSectionFloat itemQbKeyCrc fileId f1
      sectionFloatsX2 = do
        p <- getW32
        0 <- getW32
        shouldBeAt p
        (f1, f2) <- parseQBFloatsX2
        return $ QBSectionFloatsX2 itemQbKeyCrc fileId f1 f2
      sectionFloatsX3 = do
        p <- getW32
        0 <- getW32
        shouldBeAt p
        (f1, f2, f3) <- parseQBFloatsX3
        return $ QBSectionFloatsX3 itemQbKeyCrc fileId f1 f2 f3
  case ?format of
    QBFormatNew -> case sectionType of
      0x00200100 -> sectionInteger
      0x00200C00 -> sectionArray
      0x00200D00 -> sectionQbKey
      0x00200A00 -> sectionStruct
      0x00200700 -> sectionScript
      0x00200300 -> sectionString
      0x00200400 -> sectionStringW
      0x00201C00 -> sectionQbKeyStringQs
      0x00201A00 -> sectionQbKeyString
      0x00200200 -> sectionFloat
      0x00200500 -> sectionFloatsX2
      0x00200600 -> sectionFloatsX3
      _ -> fail $ "Unrecognized section type: 0x" <> showHex sectionType ""
    QBFormatPS2 -> case sectionType of
      0x00010400 -> sectionInteger
      0x000C0400 -> sectionArray
      0x000D0400 -> sectionQbKey
      0x000A0400 -> sectionStruct
      0x00070400 -> sectionScript
      0x00030400 -> sectionString
      0x00040400 -> sectionStringW
      0x001C0400 -> sectionQbKeyStringQs
      0x00041A00 -> sectionQbKeyString
      0x00020400 -> sectionFloat
      0x00050400 -> sectionFloatsX2
      0x00060400 -> sectionFloatsX3
      _ -> fail $ "Unrecognized section type: 0x" <> showHex sectionType ""

parseQB :: (?endian :: ByteOrder) => Get [QBSection Word32 Word32]
parseQB = do
  -- for now, just use endian to determine type keys
  let ?format = case ?endian of
        LittleEndian -> QBFormatPS2
        BigEndian    -> QBFormatNew
  _zero1 <- getW32
  fileSize <- getW32
  _headerSize <- getWord8 -- 0x1C
  -- note, rest of the zero fields aren't zero on GH3 PS2
  _zero2 <- getByteString 7
  _contentSize <- getW32 -- file size - header size
  _zero3 <- getW32
  _zero4 <- getW32
  let parseSections = do
        pos <- fromIntegral <$> bytesRead
        if pos >= fileSize
          then return []
          else (:) <$> parseQBSection <*> parseSections
  parseSections

-- weird format that looks like a QB but is just the contents of a struct
parseSGHStruct :: Get [QBStructItem Word32 Word32]
parseSGHStruct = do
  let ?endian = BigEndian
      ?format = QBFormatNew
  _ <- getByteString 28 -- sort of normal qb header but don't need it
  parseQBStruct

getW32 :: (?endian :: ByteOrder) => Get Word32
getW32 = codecIn binEndian

getFloat :: (?endian :: ByteOrder) => Get Float
getFloat = codecIn binEndian

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

padOutputTo4 :: PutSeek s ()
padOutputTo4 = do
  (_, used) <- get
  case rem used 4 of
    0 -> return ()
    1 -> append $ B.replicate 3 0
    2 -> append $ B.replicate 2 0
    _ -> append $ B.singleton 0

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
    QBArrayOfQbKeyString ks -> do
      w32 0x00011A00
      writeLen ks
      mapM_ w32 ks
    QBArrayOfFloatsX2 vs -> do
      w32 0x00010500
      writeLen vs
      ptrVectorPairs <- mapM (\v -> (\p -> (p, v)) <$> reservePointer) vs
      forM_ ptrVectorPairs $ \(p, (f1, f2)) -> do
        fillPointer p
        w32 0x00010000
        append $ BL.toStrict $ runPut $ do
          putFloatbe f1
          putFloatbe f2
    QBArrayOfFloatsX3 vs -> do
      w32 0x00010600
      writeLen vs
      ptrVectorPairs <- mapM (\v -> (\p -> (p, v)) <$> reservePointer) vs
      forM_ ptrVectorPairs $ \(p, (f1, f2, f3)) -> do
        fillPointer p
        w32 0x00010000
        append $ BL.toStrict $ runPut $ do
          putFloatbe f1
          putFloatbe f2
          putFloatbe f3
    QBArrayOfString strs -> do
      w32 0x00010300
      writeLen strs
      ptrStringPairs <- mapM (\s -> (\p -> (p, s)) <$> reservePointer) strs
      forM_ ptrStringPairs $ \(p, str) -> do
        fillPointer p
        append $ str <> B.singleton 0
      padOutputTo4

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
      QBStructItemFloatsX2 x f1 f2 -> do
        w32 0x00010500
        w32 x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        w32 0x00010000
        append $ BL.toStrict $ runPut $ do
          putFloatbe f1
          putFloatbe f2
        return p
      QBStructItemFloatsX3 x f1 f2 f3 -> do
        w32 0x00010600
        w32 x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        w32 0x00010000
        append $ BL.toStrict $ runPut $ do
          putFloatbe f1
          putFloatbe f2
          putFloatbe f3
        return p
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
  QBSectionQbKey itemQbKeyCrc fileId k -> do
    w32 0x00200D00
    w32 itemQbKeyCrc
    w32 fileId
    w32 k
    w32 0
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
  QBSectionString itemQbKeyCrc fileId str -> do
    w32 0x00200300
    w32 itemQbKeyCrc
    w32 fileId
    p <- reservePointer
    w32 0
    fillPointer p
    append $ padTo4 $ str <> "\0"
  QBSectionStringW itemQbKeyCrc fileId str -> do
    w32 0x00200400
    w32 itemQbKeyCrc
    w32 fileId
    p <- reservePointer
    w32 0
    fillPointer p
    append $ padTo4 $ TE.encodeUtf16BE $ str <> "\0"
  QBSectionQbKeyStringQs itemQbKeyCrc fileId qs -> do
    w32 0x00201C00
    w32 itemQbKeyCrc
    w32 fileId
    w32 qs
    w32 0
  QBSectionQbKeyString itemQbKeyCrc fileId k -> do
    w32 0x00201A00
    w32 itemQbKeyCrc
    w32 fileId
    w32 k
    w32 0
  QBSectionFloat itemQbKeyCrc fileId f1 -> do
    w32 0x00200200
    w32 itemQbKeyCrc
    w32 fileId
    append $ BL.toStrict $ runPut $ putFloatbe f1
    w32 0
  QBSectionFloatsX2 itemQbKeyCrc fileId f1 f2 -> do
    w32 0x200500
    w32 itemQbKeyCrc
    w32 fileId
    p <- reservePointer
    w32 0
    fillPointer p
    w32 0x00010000
    append $ BL.toStrict $ runPut $ do
      putFloatbe f1
      putFloatbe f2
  QBSectionFloatsX3 itemQbKeyCrc fileId f1 f2 f3 -> do
    w32 0x200600
    w32 itemQbKeyCrc
    w32 fileId
    p <- reservePointer
    w32 0
    fillPointer p
    w32 0x00010000
    append $ BL.toStrict $ runPut $ do
      putFloatbe f1
      putFloatbe f2
      putFloatbe f3

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

stripBackL :: T.Text -> T.Text
stripBackL s = fromMaybe s $ T.stripPrefix "\\L" s
