{- |
Written with much assistance from Queen Bee (https://github.com/Nanook/Queen-Bee)
and Honeycomb (https://github.com/AddyMills/GH-Toolkit-NET)
-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
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
import           Data.Bits                    (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import qualified Data.HashMap.Strict          as HM
import           Data.Int                     (Int32)
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
import           Onyx.Neversoft.CRC           (QBKey (..), qbKeyCRC)
import           System.IO.Unsafe             (unsafePerformIO)

{-
Every element in a .qb starts with a 4-byte type word, not endian-dependent:

* section: @00 ff tt 00@, where @ff@ is 0x20 on big endian and 0x04 on PS2
  (same flag bit; bitfield order flips with endianness)
* struct item, GH3/GH:A style: @00 (0x80 .|. tt) 00 00@ on big endian,
  @00 (tt << 1 .|. 1) 00 00@ on PS2
* struct item, GHWT and later: @00 01 tt 00@ on all platforms.
  GH3 can read these too (later GH3 DLC uses them)
* array: @00 01 tt 00@, where type 0 means an empty array
* struct header: @00 00 01 00@
* header before the floats of a pair/vector: @00 01 00 00@

All other 32-bit values (keys, numbers, pointers, sizes) use the platform's
endianness; wide strings are UTF-16BE on all platforms
-}

-- | Settings needed to write a .qb file.
-- Reading only needs the endianness, everything else is detected from the bytes
data QBFormat = QBFormat
  { endian    :: ByteOrder
  , itemStyle :: QBItemStyle
  , header    :: QBHeaderStyle
  } deriving (Eq, Show)

data QBItemStyle
  = QBItemsGH3  -- ^ struct item type stored in byte 1 with a flag bit (GH3, GH:A)
  | QBItemsGHWT -- ^ struct item header @00 01 tt 00@ (GHWT and later)
  deriving (Eq, Show)

data QBHeaderStyle
  = QBHeaderGH3 -- ^ fixed 20 bytes after the file size (GH3, GH:A, GHWT)
  | QBHeaderGH5 -- ^ header size and content size (GH5, BH, WoR)
  deriving (Eq, Show)

qbFormatGH3, qbFormatGHWT, qbFormatGH5 :: ByteOrder -> QBFormat
qbFormatGH3  e = QBFormat { endian = e, itemStyle = QBItemsGH3 , header = QBHeaderGH3 }
qbFormatGHWT e = QBFormat { endian = e, itemStyle = QBItemsGHWT, header = QBHeaderGH3 }
qbFormatGH5  e = QBFormat { endian = e, itemStyle = QBItemsGHWT, header = QBHeaderGH5 }

-- Type bytes (the @tt@ above)
typeInteger, typeFloat, typeString, typeStringW, typeFloatsX2, typeFloatsX3,
  typeScript, typeStruct, typeArray, typeQbKey, typeQbKeyString,
  typeStringPointer, typeQbKeyStringQs :: Word8
typeInteger       = 0x01
typeFloat         = 0x02
typeString        = 0x03
typeStringW       = 0x04
typeFloatsX2      = 0x05
typeFloatsX3      = 0x06
typeScript        = 0x07
typeStruct        = 0x0A
typeArray         = 0x0C
typeQbKey         = 0x0D
typeQbKeyString   = 0x1A -- a reference to a global, written as $name in .q
typeStringPointer = 0x1B -- listed by QueenBee but not seen in any GH files
typeQbKeyStringQs = 0x1C -- localized string key, written as qs(...) in .q

data QBSection qs k
  = QBSectionInteger k k Int32
  | QBSectionArray k k (QBArray qs k)
  | QBSectionQbKey k k k
  | QBSectionStruct k k [QBStructItem qs k]
  | QBSectionScript k k Word32 Word32 B.ByteString
    -- ^ checksum, decompressed size, then compressed bytestring.
    -- checksum is qbKeyCRCNoLowercase of the decompressed script
    -- with newline (0x01) and endscript (0x24) opcodes removed.
    -- WoR computes it differently somehow for scripts with ifs or structs.
  | QBSectionString k k B.ByteString -- seen in gh3 ps2
  | QBSectionStringW k k T.Text -- seen in gh3
  | QBSectionQbKeyStringQs k k qs
  | QBSectionQbKeyString k k k
  | QBSectionFloat k k Float
  | QBSectionFloatsX2 k k Float Float
  | QBSectionFloatsX3 k k Float Float Float
  | QBSectionStringPointer k k qs -- not sure if this is a qs or qb key. it points to a SectionScript
  deriving (Eq, Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBSection qs k) where
  toJSON = \case
    QBSectionInteger x y z -> OneKey "SectionInteger" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionArray x y z -> OneKey "SectionArray" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionQbKey x y z -> OneKey "SectionQbKey" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionStruct x y z -> OneKey "SectionStruct" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionScript w x c y z -> OneKey "SectionScript" $ toJSON [toJSON w, toJSON x, toJSON c, toJSON y, toJSON $ B.unpack z]
    QBSectionString x y z -> OneKey "SectionString" $ toJSON [toJSON x, toJSON y, toJSON $ B8.unpack z]
    QBSectionStringW x y z -> OneKey "SectionStringW" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionQbKeyStringQs x y z -> OneKey "SectionQbKeyStringQs" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionQbKeyString x y z -> OneKey "SectionQbKeyString" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBSectionFloat x y f1 -> OneKey "SectionFloat" $ toJSON [toJSON x, toJSON y, toJSON f1]
    QBSectionFloatsX2 x y f1 f2 -> OneKey "SectionFloat" $ toJSON [toJSON x, toJSON y, toJSON f1, toJSON f2]
    QBSectionFloatsX3 x y f1 f2 f3 -> OneKey "SectionFloat" $ toJSON [toJSON x, toJSON y, toJSON f1, toJSON f2, toJSON f3]
    QBSectionStringPointer x y z -> OneKey "SectionStringPointer" $ toJSON [toJSON x, toJSON y, toJSON z]

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
      [w, x, c, y, z] -> QBSectionScript <$> parseJSON w <*> parseJSON x <*> parseJSON c <*> parseJSON y <*> fmap B.pack (parseJSON z)
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
    OneKey "SectionStringPointer" xs -> parseJSON xs >>= \case
      [x, y, z] -> QBSectionStringPointer <$> parseJSON x <*> parseJSON y <*> parseJSON z
      _ -> fail "QB json error"
    _ -> fail "QB json error"

data QBArray qs k
  = QBArrayEmpty -- array type 0, all empty arrays in official files use this
  | QBArrayOfQbKey [k]
  | QBArrayOfInteger [Int32]
  | QBArrayOfStruct [[QBStructItem qs k]]
  | QBArrayOfFloat [Float]
  | QBArrayOfQbKeyStringQs [qs]
  | QBArrayOfArray [QBArray qs k]
  | QBArrayOfQbKeyString [k]
  | QBArrayOfFloatsX2 [(Float, Float)]
  | QBArrayOfFloatsX3 [(Float, Float, Float)]
  | QBArrayOfString [B.ByteString]
  | QBArrayOfStringW [T.Text]
  deriving (Eq, Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBArray qs k) where
  toJSON = \case
    QBArrayEmpty             -> "ArrayEmpty"
    QBArrayOfQbKey x         -> OneKey "ArrayOfQbKey" $ toJSON x
    QBArrayOfInteger x       -> OneKey "ArrayOfInteger" $ toJSON x
    QBArrayOfStruct x        -> OneKey "ArrayOfStruct" $ toJSON x
    QBArrayOfFloat x         -> OneKey "ArrayOfFloat" $ toJSON x
    QBArrayOfQbKeyStringQs x -> OneKey "ArrayOfQbKeyStringQs" $ toJSON x
    QBArrayOfArray x         -> OneKey "ArrayOfArray" $ toJSON x
    QBArrayOfQbKeyString x   -> OneKey "ArrayOfQbKeyString" $ toJSON x
    QBArrayOfFloatsX2 x      -> OneKey "ArrayOfFloatsX2" $ toJSON x
    QBArrayOfFloatsX3 x      -> OneKey "ArrayOfFloatsX3" $ toJSON x
    QBArrayOfString x        -> OneKey "ArrayOfString" $ toJSON $ map B8.unpack x
    QBArrayOfStringW x       -> OneKey "ArrayOfStringW" $ toJSON x

instance (FromJSON qs, FromJSON k) => FromJSON (QBArray qs k) where
  parseJSON = \case
    "ArrayEmpty"                    -> return QBArrayEmpty
    OneKey "ArrayOfQbKey" x         -> QBArrayOfQbKey <$> parseJSON x
    OneKey "ArrayOfInteger" x       -> QBArrayOfInteger <$> parseJSON x
    OneKey "ArrayOfStruct" x        -> QBArrayOfStruct <$> parseJSON x
    OneKey "ArrayOfFloat" x         -> QBArrayOfFloat <$> parseJSON x
    OneKey "ArrayOfQbKeyStringQs" x -> QBArrayOfQbKeyStringQs <$> parseJSON x
    OneKey "ArrayOfArray" x         -> QBArrayOfArray <$> parseJSON x
    OneKey "ArrayOfQbKeyString" x   -> QBArrayOfQbKeyString <$> parseJSON x
    OneKey "ArrayOfFloatsX2" x      -> QBArrayOfFloatsX2 <$> parseJSON x
    OneKey "ArrayOfFloatsX3" x      -> QBArrayOfFloatsX3 <$> parseJSON x
    OneKey "ArrayOfString" x        -> QBArrayOfString . map B8.pack <$> parseJSON x
    OneKey "ArrayOfStringW" x       -> QBArrayOfStringW <$> parseJSON x
    _                               -> fail "QB json error"

-- struct header is unrepresented as it is always written;
-- whether items are written GH3-style or GHWT-style is up to the 'QBFormat'
data QBStructItem qs k
  = QBStructItemStruct k [QBStructItem qs k]
  | QBStructItemQbKey k k
  | QBStructItemString k B.ByteString
  | QBStructItemStringW k T.Text
  | QBStructItemQbKeyString k k
  | QBStructItemQbKeyStringQs k qs
  | QBStructItemInteger k Int32
  | QBStructItemFloat k Float
  | QBStructItemArray k (QBArray qs k)
  | QBStructItemFloatsX2 k Float Float
  | QBStructItemFloatsX3 k Float Float Float
  deriving (Eq, Show, Functor)

instance (ToJSON qs, ToJSON k) => ToJSON (QBStructItem qs k) where
  toJSON = \case
    QBStructItemStruct x y -> OneKey "StructItemStruct" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKey x y -> OneKey "StructItemQbKey" $ toJSON [toJSON x, toJSON y]
    QBStructItemString x y -> OneKey "StructItemString" $ toJSON [toJSON x, toJSON $ B8.unpack y]
    QBStructItemStringW x y -> OneKey "StructItemStringW" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKeyString x y -> OneKey "StructItemQbKeyString" $ toJSON [toJSON x, toJSON y]
    QBStructItemQbKeyStringQs x y -> OneKey "StructItemQbKeyStringQs" $ toJSON [toJSON x, toJSON y]
    QBStructItemInteger x y -> OneKey "StructItemInteger" $ toJSON [toJSON x, toJSON y]
    QBStructItemFloat x y -> OneKey "StructItemFloat" $ toJSON [toJSON x, toJSON y]
    QBStructItemArray x y -> OneKey "StructItemArray" $ toJSON [toJSON x, toJSON y]
    QBStructItemFloatsX2 x y z -> OneKey "StructItemFloatsX2" $ toJSON [toJSON x, toJSON y, toJSON z]
    QBStructItemFloatsX3 w x y z -> OneKey "StructItemFloatsX3" $ toJSON [toJSON w, toJSON x, toJSON y, toJSON z]

instance (FromJSON qs, FromJSON k) => FromJSON (QBStructItem qs k) where
  parseJSON = \case
    OneKey "StructItemStruct" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemStruct <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemQbKey" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemQbKey <$> parseJSON x <*> parseJSON y
      _      -> fail "QB json error"
    OneKey "StructItemString" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemString <$> parseJSON x <*> fmap B8.pack (parseJSON y)
      _ -> fail "QB json error"
    OneKey "StructItemStringW" xs -> parseJSON xs >>= \case
      [x, y] -> QBStructItemStringW <$> parseJSON x <*> parseJSON y
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
    _ -> fail "QB json error"

instance Bifunctor QBSection where
  first f = \case
    QBSectionInteger x y n         -> QBSectionInteger x y n
    QBSectionArray x y arr         -> QBSectionArray x y $ first f arr
    QBSectionQbKey x y z           -> QBSectionQbKey x y z
    QBSectionStruct x y items      -> QBSectionStruct x y $ map (first f) items
    QBSectionScript w x c y z      -> QBSectionScript w x c y z
    QBSectionString x y s          -> QBSectionString x y s
    QBSectionStringW x y s         -> QBSectionStringW x y s
    QBSectionQbKeyStringQs x y z   -> QBSectionQbKeyStringQs x y (f z)
    QBSectionQbKeyString x y z     -> QBSectionQbKeyString x y z
    QBSectionFloat    x y f1       -> QBSectionFloat x y f1
    QBSectionFloatsX2 x y f1 f2    -> QBSectionFloatsX2 x y f1 f2
    QBSectionFloatsX3 x y f1 f2 f3 -> QBSectionFloatsX3 x y f1 f2 f3
    QBSectionStringPointer x y z   -> QBSectionStringPointer x y (f z)
  second = fmap

instance Bifunctor QBArray where
  first f = \case
    QBArrayEmpty              -> QBArrayEmpty
    QBArrayOfQbKey ks         -> QBArrayOfQbKey ks
    QBArrayOfInteger ns       -> QBArrayOfInteger ns
    QBArrayOfStruct structs   -> QBArrayOfStruct $ map (map $ first f) structs
    QBArrayOfFloat ns         -> QBArrayOfFloat ns
    QBArrayOfQbKeyStringQs qs -> QBArrayOfQbKeyStringQs $ map f qs
    QBArrayOfArray arrs       -> QBArrayOfArray $ map (first f) arrs
    QBArrayOfQbKeyString ks   -> QBArrayOfQbKeyString ks
    QBArrayOfFloatsX2 vs      -> QBArrayOfFloatsX2 vs
    QBArrayOfFloatsX3 vs      -> QBArrayOfFloatsX3 vs
    QBArrayOfString bs        -> QBArrayOfString bs
    QBArrayOfStringW bs       -> QBArrayOfStringW bs
  second = fmap

instance Bifunctor QBStructItem where
  first f = \case
    QBStructItemStruct x items -> QBStructItemStruct x $ map (first f) items
    QBStructItemQbKey x y -> QBStructItemQbKey x y
    QBStructItemString x y -> QBStructItemString x y
    QBStructItemStringW x y -> QBStructItemStringW x y
    QBStructItemQbKeyString x y -> QBStructItemQbKeyString x y
    QBStructItemQbKeyStringQs x qs -> QBStructItemQbKeyStringQs x $ f qs
    QBStructItemInteger x y -> QBStructItemInteger x y
    QBStructItemFloat x y -> QBStructItemFloat x y
    QBStructItemArray x arr -> QBStructItemArray x $ first f arr
    QBStructItemFloatsX2 x y z -> QBStructItemFloatsX2 x y z
    QBStructItemFloatsX3 w x y z -> QBStructItemFloatsX3 w x y z
  second = fmap

instance Bifoldable QBSection where
  bifoldMap f g = \case
    QBSectionInteger x y _       -> g x <> g y
    QBSectionArray   x y arr     -> g x <> g y <> bifoldMap f g arr
    QBSectionQbKey   x y z       -> g x <> g y <> g z
    QBSectionStruct  x y items   -> g x <> g y <> mconcat (map (bifoldMap f g) items)
    QBSectionScript  x y _ _ _   -> g x <> g y
    QBSectionString  x y _       -> g x <> g y
    QBSectionStringW x y _       -> g x <> g y
    QBSectionQbKeyStringQs x y z -> g x <> g y <> f z
    QBSectionQbKeyString x y z   -> g x <> g y <> g z
    QBSectionFloat    x y _      -> g x <> g y
    QBSectionFloatsX2 x y _ _    -> g x <> g y
    QBSectionFloatsX3 x y _ _ _  -> g x <> g y
    QBSectionStringPointer x y z -> g x <> g y <> f z

instance Bifoldable QBArray where
  bifoldMap f g = \case
    QBArrayEmpty                   -> mempty
    QBArrayOfQbKey         ks      -> mconcat (map g ks)
    QBArrayOfInteger       _       -> mempty
    QBArrayOfStruct        structs -> mconcat $ mconcat $ map (map $ bifoldMap f g) structs
    QBArrayOfFloat         _       -> mempty
    QBArrayOfQbKeyStringQs qs      -> mconcat $ map f qs
    QBArrayOfArray         arrs    -> mconcat $ map (bifoldMap f g) arrs
    QBArrayOfQbKeyString   ks      -> mconcat (map g ks)
    QBArrayOfFloatsX2      _       -> mempty
    QBArrayOfFloatsX3      _       -> mempty
    QBArrayOfString        _       -> mempty
    QBArrayOfStringW       _       -> mempty

instance Bifoldable QBStructItem where
  bifoldMap f g = \case
    QBStructItemStruct        x items -> g x <> mconcat (map (bifoldMap f g) items)
    QBStructItemQbKey         x y     -> g x <> g y
    QBStructItemString        x _     -> g x
    QBStructItemStringW       x _     -> g x
    QBStructItemQbKeyString   x y     -> g x <> g y
    QBStructItemQbKeyStringQs x qs    -> g x <> f qs
    QBStructItemInteger       x _     -> g x
    QBStructItemFloat         x _     -> g x
    QBStructItemArray         x arr   -> g x <> bifoldMap f g arr
    QBStructItemFloatsX2      x _ _   -> g x
    QBStructItemFloatsX3      x _ _ _ -> g x

allQS :: (Bifoldable obj) => obj qs k -> [qs]
allQS = bifoldMap (: []) (const [])

-----------------------------
-- Reading

type TypeWord = (Word8, Word8, Word8, Word8)

getTypeWord :: Get TypeWord
getTypeWord = (,,,) <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8

showTypeWord :: TypeWord -> String
showTypeWord (a, b, c, d) = concatMap hex2 [a, b, c, d] where
  hex2 w = case showHex w "" of
    [x] -> ['0', x]
    s   -> s

-- | Gets the type byte from a struct item header, in either the GH3 or GHWT style.
decodeItemType :: (?endian :: ByteOrder) => TypeWord -> Maybe Word8
decodeItemType = \case
  (0, 1, t, 0) | t /= 0 -> Just t
  (0, b, 0, 0) -> case ?endian of
    BigEndian    | b .&. 0x80 /= 0 -> Just $ b .&. 0x7F
    LittleEndian | b .&. 0x01 /= 0 -> Just $ b `shiftR` 1
    _                              -> Nothing
  _ -> Nothing

shouldBeAt :: Word32 -> Get ()
shouldBeAt w = do
  p <- fromIntegral <$> bytesRead
  unless (p == w) $ fail $ unwords
    [ "QB parser position expected to be"
    , "0x" <> showHex w ""
    , "but we're at"
    , "0x" <> showHex p ""
    ]

parseQBSubArray :: (?endian :: ByteOrder) => Get (QBArray Word32 QBKey)
parseQBSubArray = do
  tw@(a, b, arrayType, d) <- getTypeWord
  unless (a == 0 && b == 1 && d == 0) $ fail $ "Unrecognized array header: " <> showTypeWord tw
  len <- fromIntegral <$> getW32
  case len of
    0 -> skip 4
    1 -> return ()
    _ -> do
      p3 <- getW32
      shouldBeAt p3
  let arrayOfArray = do
        arrayStarts <- replicateM len getW32
        fmap QBArrayOfArray $ forM arrayStarts \p4 -> do
          shouldBeAt p4
          parseQBSubArray
      arrayOfString = do
        stringStarts <- replicateM len getW32
        res <- fmap QBArrayOfString $ forM stringStarts \p4 -> do
          shouldBeAt p4
          qbNullTerm
        jumpTo4
        return res
      arrayOfStringW = do
        stringStarts <- replicateM len getW32
        res <- fmap QBArrayOfStringW $ forM stringStarts \p4 -> do
          shouldBeAt p4
          getUtf16BE
        jumpTo4
        return res
      arrayOfStruct = do
        structStarts <- replicateM len getW32
        fmap QBArrayOfStruct $ forM structStarts \p4 -> do
          shouldBeAt p4
          parseQBStruct
      arrayOfFloatsX2 = do
        vectorStarts <- replicateM len getW32
        fmap QBArrayOfFloatsX2 $ forM vectorStarts \p4 -> do
          shouldBeAt p4
          parseQBFloatsX2
      arrayOfFloatsX3 = do
        vectorStarts <- replicateM len getW32
        fmap QBArrayOfFloatsX3 $ forM vectorStarts \p4 -> do
          shouldBeAt p4
          parseQBFloatsX3
  case arrayType of
    0x00 | len == 0           -> return QBArrayEmpty
    t | t == typeQbKey        -> QBArrayOfQbKey <$> replicateM len getQBKey
      | t == typeInteger      -> QBArrayOfInteger <$> replicateM len getI32
      | t == typeStruct       -> arrayOfStruct
      | t == typeFloat        -> QBArrayOfFloat <$> replicateM len getFloat
      | t == typeQbKeyStringQs -> QBArrayOfQbKeyStringQs <$> replicateM len getW32
      | t == typeArray        -> arrayOfArray
      | t == typeQbKeyString  -> QBArrayOfQbKeyString <$> replicateM len getQBKey
      | t == typeFloatsX2     -> arrayOfFloatsX2
      | t == typeFloatsX3     -> arrayOfFloatsX3
      | t == typeString       -> arrayOfString
      | t == typeStringW      -> arrayOfStringW
    _ -> fail $ "Unrecognized array header: " <> showTypeWord tw

parseQBArray :: (?endian :: ByteOrder) => Get (QBArray Word32 QBKey, Word32)
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

floatsHeader :: TypeWord
floatsHeader = (0, 1, 0, 0)

structHeader :: TypeWord
structHeader = (0, 0, 1, 0)

parseQBFloatsX2 :: (?endian :: ByteOrder) => Get (Float, Float)
parseQBFloatsX2 = do
  tw <- getTypeWord
  unless (tw == floatsHeader) $ fail $ "FloatsX2 does not have Floats subtype: " <> showTypeWord tw
  f1 <- getFloat
  f2 <- getFloat
  return (f1, f2)

parseQBFloatsX3 :: (?endian :: ByteOrder) => Get (Float, Float, Float)
parseQBFloatsX3 = do
  tw <- getTypeWord
  unless (tw == floatsHeader) $ fail $ "FloatsX3 does not have Floats subtype: " <> showTypeWord tw
  f1 <- getFloat
  f2 <- getFloat
  f3 <- getFloat
  return (f1, f2, f3)

-- | Parses a struct, starting from its header.
parseQBStruct :: (?endian :: ByteOrder) => Get [QBStructItem Word32 QBKey]
parseQBStruct = do
  tw <- getTypeWord
  unless (tw == structHeader) $ fail $ "Unrecognized struct header: " <> showTypeWord tw
  firstItem <- getW32
  case firstItem of
    0 -> return []
    _ -> do
      shouldBeAt firstItem
      parseQBStructItems

parseQBStructItems :: (?endian :: ByteOrder) => Get [QBStructItem Word32 QBKey]
parseQBStructItems = do
  tw <- getTypeWord
  itemType <- maybe (fail $ "Unrecognized struct item type: " <> showTypeWord tw) return $ decodeItemType tw
  let simple f getValue = do
        x <- getQBKey
        y <- getValue
        p <- getW32
        return (f x y, p)
      structItemString = do
        x <- getQBKey
        start <- getW32
        p <- getW32
        shouldBeAt start
        b <- qbNullTerm
        jumpTo4
        return (QBStructItemString x b, p)
      structItemStringW = do
        x <- getQBKey
        start <- getW32
        p <- getW32
        shouldBeAt start
        str <- getUtf16BE
        jumpTo4
        return (QBStructItemStringW x str, p)
      structItemArray = do
        x <- getQBKey
        (array, p) <- parseQBArray
        return (QBStructItemArray x array, p)
      structItemFloatsX2 = do
        x <- getQBKey
        p1 <- getW32
        p2 <- getW32
        shouldBeAt p1
        (f1, f2) <- parseQBFloatsX2
        return (QBStructItemFloatsX2 x f1 f2, p2)
      structItemFloatsX3 = do
        x <- getQBKey
        p1 <- getW32
        p2 <- getW32
        shouldBeAt p1
        (f1, f2, f3) <- parseQBFloatsX3
        return (QBStructItemFloatsX3 x f1 f2 f3, p2)
      structItemStruct = do
        x <- getQBKey
        p1 <- getW32
        p2 <- getW32
        shouldBeAt p1
        items <- parseQBStruct
        return (QBStructItemStruct x items, p2)
  (item, nextPosition) <- case itemType of
    t | t == typeQbKey         -> simple QBStructItemQbKey getQBKey
      | t == typeQbKeyStringQs -> simple QBStructItemQbKeyStringQs getW32
      | t == typeQbKeyString   -> simple QBStructItemQbKeyString getQBKey
      | t == typeInteger       -> simple QBStructItemInteger getI32
      | t == typeFloat         -> simple QBStructItemFloat getFloat
      | t == typeString        -> structItemString
      | t == typeStringW       -> structItemStringW
      | t == typeArray         -> structItemArray
      | t == typeFloatsX2      -> structItemFloatsX2
      | t == typeFloatsX3      -> structItemFloatsX3
      | t == typeStruct        -> structItemStruct
    _ -> fail $ "Unrecognized struct item type: " <> showTypeWord tw
  case nextPosition of
    0 -> return [item] -- this is the last item
    _ -> do
      shouldBeAt nextPosition
      (item :) <$> parseQBStructItems

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

parseQBSection :: (?endian :: ByteOrder) => Get (QBSection Word32 QBKey)
parseQBSection = do
  -- flags byte is 0x20 on big endian, 0x04 on PS2
  tw@(a, _flags, sectionType, d) <- getTypeWord
  unless (a == 0 && d == 0) $ fail $ "Unrecognized section header: " <> showTypeWord tw
  itemQbKeyCrc <- getQBKey
  fileId <- getQBKey
  let sectionInteger = do
        n1 <- getI32
        n2 <- getW32
        when (n2 /= 0) $ fail "SectionInteger: expected 0 for second number"
        return $ QBSectionInteger itemQbKeyCrc fileId n1
      sectionArray = do
        (array, _) <- parseQBArray
        -- the snd above should be 0, I think
        return $ QBSectionArray itemQbKeyCrc fileId array
      sectionQbKey = do
        n1 <- getQBKey
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
        checksum <- getW32
        decompressedSize <- getW32
        compressedSize <- getW32
        bs <- getByteString $ fromIntegral compressedSize
        jumpTo4
        return $ QBSectionScript itemQbKeyCrc fileId checksum decompressedSize bs
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
        n1 <- getQBKey
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
      sectionStringPointer = do
        n1 <- getW32
        n2 <- getW32
        when (n2 /= 0) $ fail "SectionStringPointer: expected 0 for second number"
        return $ QBSectionStringPointer itemQbKeyCrc fileId n1
  case sectionType of
    t | t == typeInteger       -> sectionInteger
      | t == typeArray         -> sectionArray
      | t == typeQbKey         -> sectionQbKey
      | t == typeStruct        -> sectionStruct
      | t == typeScript        -> sectionScript
      | t == typeString        -> sectionString
      | t == typeStringW       -> sectionStringW
      | t == typeQbKeyStringQs -> sectionQbKeyStringQs
      | t == typeQbKeyString   -> sectionQbKeyString
      | t == typeFloat         -> sectionFloat
      | t == typeFloatsX2      -> sectionFloatsX2
      | t == typeFloatsX3      -> sectionFloatsX3
      | t == typeStringPointer -> sectionStringPointer
    _ -> fail $ "Unrecognized section header: " <> showTypeWord tw

parseQB :: (?endian :: ByteOrder) => Get [QBSection Word32 QBKey]
parseQB = do
  _zero1 <- getW32
  fileSize <- getW32
  -- either a fixed 20 bytes (GH3/GHWT), or header size + content size (GH5/WoR)
  skip 20
  let parseSections = do
        pos <- fromIntegral <$> bytesRead
        if pos >= fileSize
          then return []
          else (:) <$> parseQBSection <*> parseSections
  parseSections

-- weird format that looks like a QB but is just the contents of a struct
parseSGHStruct :: Get [QBStructItem Word32 QBKey]
parseSGHStruct = do
  let ?endian = BigEndian
  _ <- getByteString 28 -- sort of normal qb header but don't need it
  parseQBStruct

getW32 :: (?endian :: ByteOrder) => Get Word32
getW32 = codecIn binEndian

getQBKey :: (?endian :: ByteOrder) => Get QBKey
getQBKey = QBKey <$> getW32

getI32 :: (?endian :: ByteOrder) => Get Int32
getI32 = fromIntegral <$> getW32

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
  = UnknownQB QBKey
  | KnownQB B.ByteString
  deriving (Show)

instance ToJSON QBResult where
  toJSON = \case
    UnknownQB qb -> toJSON $ fromQBKey qb
    KnownQB b    -> toJSON $ B8.unpack b

instance FromJSON QBResult where
  parseJSON = \case
    Number n -> return $ UnknownQB $ QBKey $ round n
    String s -> return $ KnownQB $ B8.pack $ T.unpack s
    _        -> fail "QB key json error"

lookupQS :: (Bifunctor obj) => HM.HashMap Word32 T.Text -> obj Word32 k -> obj QSResult k
lookupQS mapping = first \qs -> case HM.lookup qs mapping of
  Nothing -> UnknownQS qs
  Just t  -> KnownQS qs t

lookupQB :: (Bifunctor obj) => HM.HashMap QBKey B.ByteString -> obj qs QBKey -> obj qs QBResult
lookupQB mapping = second \qb -> case HM.lookup qb mapping of
  Nothing -> UnknownQB qb
  Just b  -> KnownQB b

-----------------------------
-- Writing

type PutSeek s = StateT (MV.STVector s Word8, Int) (ST s)
type Pointer = Int

-- | Implicit parameters used while writing
type QBWriter = (?endian :: ByteOrder, ?itemStyle :: QBItemStyle)

grow :: Int -> PutSeek s ()
grow need = do
  (v, used) <- get
  let available = MV.length v
  when (available < need) do
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
  forM_ (zip [used ..] $ B.unpack b) \(i, c) -> MV.write v i c
  put (v, used + B.length b)

putW32 :: (?endian :: ByteOrder) => Word32 -> Put
putW32 = case ?endian of
  BigEndian    -> putWord32be
  LittleEndian -> putWord32le

putF32 :: (?endian :: ByteOrder) => Float -> Put
putF32 = case ?endian of
  BigEndian    -> putFloatbe
  LittleEndian -> putFloatle

w32 :: (?endian :: ByteOrder) => Word32 -> PutSeek s ()
w32 = append . BL.toStrict . runPut . putW32

i32 :: (?endian :: ByteOrder) => Int32 -> PutSeek s ()
i32 = w32 . fromIntegral

f32 :: (?endian :: ByteOrder) => Float -> PutSeek s ()
f32 = append . BL.toStrict . runPut . putF32

typeWord :: TypeWord -> PutSeek s ()
typeWord (a, b, c, d) = append $ B.pack [a, b, c, d]

sectionTypeWord :: (?endian :: ByteOrder) => Word8 -> PutSeek s ()
sectionTypeWord t = typeWord (0, flags, t, 0) where
  flags = case ?endian of
    BigEndian    -> 0x20
    LittleEndian -> 0x04

itemTypeWord :: QBWriter => Word8 -> PutSeek s ()
itemTypeWord t = typeWord $ case ?itemStyle of
  QBItemsGHWT -> (0, 1, t, 0)
  QBItemsGH3  -> case ?endian of
    BigEndian    -> (0, 0x80 .|. t, 0, 0)
    LittleEndian -> (0, (t `shiftL` 1) .|. 1, 0, 0)

putQBKey :: (?endian :: ByteOrder) => QBKey -> PutSeek s ()
putQBKey (QBKey k) = w32 k

reservePointer :: (?endian :: ByteOrder) => PutSeek s Pointer
reservePointer = do
  p <- gets snd
  w32 0xDEADBEEF
  return p

setPointer :: (?endian :: ByteOrder) => Pointer -> Word32 -> PutSeek s ()
setPointer p w = do
  let bs = BL.toStrict $ runPut $ putW32 w
  (v, _) <- get
  forM_ (zip [p ..] $ B.unpack bs) \(i, c) -> MV.write v i c

fillPointer :: (?endian :: ByteOrder) => Pointer -> PutSeek s ()
fillPointer p = do
  posn <- gets snd
  setPointer p $ fromIntegral posn

putQBArray :: QBWriter => QBArray Word32 QBKey -> PutSeek s Pointer
putQBArray ary = do
  p1 <- reservePointer
  p2 <- reservePointer
  fillPointer p1
  putQBSubArray ary
  return p2

putFloats :: (?endian :: ByteOrder) => [Float] -> PutSeek s ()
putFloats fs = do
  typeWord floatsHeader
  mapM_ f32 fs

putQBSubArray :: QBWriter => QBArray Word32 QBKey -> PutSeek s ()
putQBSubArray ary = let
  -- official files always use type 0 for empty arrays
  arrayOf t xs body = if null xs
    then do
      typeWord (0, 1, 0, 0)
      w32 0
      w32 0
    else do
      typeWord (0, 1, t, 0)
      let len = length xs
      w32 $ fromIntegral len
      case len of
        1 -> return ()
        _ -> reservePointer >>= fillPointer
      body
  pointersTo xs putOne = do
    ptrPairs <- mapM (\x -> (\p -> (p, x)) <$> reservePointer) xs
    forM_ ptrPairs \(p, x) -> do
      fillPointer p
      putOne x
  in case ary of
    QBArrayEmpty -> arrayOf 0 ([] :: [()]) $ return ()
    QBArrayOfQbKey ks -> arrayOf typeQbKey ks $ mapM_ putQBKey ks
    QBArrayOfInteger ns -> arrayOf typeInteger ns $ mapM_ i32 ns
    QBArrayOfStruct structs -> arrayOf typeStruct structs $ pointersTo structs putQBStruct
    QBArrayOfFloat floats -> arrayOf typeFloat floats $ mapM_ f32 floats
    QBArrayOfQbKeyStringQs qs -> arrayOf typeQbKeyStringQs qs $ mapM_ w32 qs
    QBArrayOfArray arrs -> arrayOf typeArray arrs $ pointersTo arrs putQBSubArray
    QBArrayOfQbKeyString ks -> arrayOf typeQbKeyString ks $ mapM_ putQBKey ks
    QBArrayOfFloatsX2 vs -> arrayOf typeFloatsX2 vs $ pointersTo vs \(f1, f2) -> putFloats [f1, f2]
    QBArrayOfFloatsX3 vs -> arrayOf typeFloatsX3 vs $ pointersTo vs \(f1, f2, f3) -> putFloats [f1, f2, f3]
    QBArrayOfString strs -> arrayOf typeString strs $ do
      pointersTo strs \str -> append $ str <> B.singleton 0
      padOutputTo4
    QBArrayOfStringW strs -> arrayOf typeStringW strs $ do
      pointersTo strs \str -> append $ TE.encodeUtf16BE $ str <> "\0"
      padOutputTo4

padTo4 :: B.ByteString -> B.ByteString
padTo4 bs = case rem (B.length bs) 4 of
  0 -> bs
  r -> bs <> B.replicate (4 - r) 0

-- | Writes a struct, starting with its header.
putQBStruct :: QBWriter => [QBStructItem Word32 QBKey] -> PutSeek s ()
putQBStruct struct = let
  go prevPointer [] = setPointer prevPointer 0
  go prevPointer (item : items) = do
    fillPointer prevPointer
    p <- case item of
      QBStructItemQbKey x y -> do
        itemTypeWord typeQbKey
        putQBKey x
        putQBKey y
        reservePointer
      QBStructItemQbKeyStringQs x y -> do
        itemTypeWord typeQbKeyStringQs
        putQBKey x
        w32 y
        reservePointer
      QBStructItemQbKeyString x y -> do
        itemTypeWord typeQbKeyString
        putQBKey x
        putQBKey y
        reservePointer
      QBStructItemInteger x y -> do
        itemTypeWord typeInteger
        putQBKey x
        i32 y
        reservePointer
      QBStructItemString x b -> do
        itemTypeWord typeString
        putQBKey x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        append $ padTo4 $ b <> B.singleton 0
        return p
      QBStructItemStringW x str -> do
        itemTypeWord typeStringW
        putQBKey x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        append $ padTo4 $ TE.encodeUtf16BE $ str <> "\0"
        return p
      QBStructItemFloat x f -> do
        itemTypeWord typeFloat
        putQBKey x
        f32 f
        reservePointer
      QBStructItemFloatsX2 x f1 f2 -> do
        itemTypeWord typeFloatsX2
        putQBKey x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        putFloats [f1, f2]
        return p
      QBStructItemFloatsX3 x f1 f2 f3 -> do
        itemTypeWord typeFloatsX3
        putQBKey x
        start <- reservePointer
        p <- reservePointer
        fillPointer start
        putFloats [f1, f2, f3]
        return p
      QBStructItemArray x array -> do
        itemTypeWord typeArray
        putQBKey x
        putQBArray array
      QBStructItemStruct x sub -> do
        itemTypeWord typeStruct
        putQBKey x
        p1 <- reservePointer
        p2 <- reservePointer
        fillPointer p1
        putQBStruct sub
        return p2
    go p items
  in do
    typeWord structHeader
    firstItem <- reservePointer
    go firstItem struct

putQBSection :: QBWriter => QBSection Word32 QBKey -> PutSeek s ()
putQBSection = \case
  QBSectionInteger itemQbKeyCrc fileId n1 -> do
    sectionTypeWord typeInteger
    putQBKey itemQbKeyCrc
    putQBKey fileId
    i32 n1
    w32 0
  QBSectionArray itemQbKeyCrc fileId array -> do
    sectionTypeWord typeArray
    putQBKey itemQbKeyCrc
    putQBKey fileId
    p <- putQBArray array
    setPointer p 0
  QBSectionQbKey itemQbKeyCrc fileId k -> do
    sectionTypeWord typeQbKey
    putQBKey itemQbKeyCrc
    putQBKey fileId
    putQBKey k
    w32 0
  QBSectionStruct itemQbKeyCrc fileId struct -> do
    sectionTypeWord typeStruct
    putQBKey itemQbKeyCrc
    putQBKey fileId
    p <- reservePointer
    w32 0
    fillPointer p
    putQBStruct struct
  QBSectionScript itemQbKeyCrc fileId checksum decompressedSize bs -> do
    sectionTypeWord typeScript
    putQBKey itemQbKeyCrc
    putQBKey fileId
    p <- reservePointer
    w32 0
    fillPointer p
    w32 checksum
    w32 decompressedSize
    w32 $ fromIntegral $ B.length bs
    append $ padTo4 bs
  QBSectionString itemQbKeyCrc fileId str -> do
    sectionTypeWord typeString
    putQBKey itemQbKeyCrc
    putQBKey fileId
    p <- reservePointer
    w32 0
    fillPointer p
    append $ padTo4 $ str <> "\0"
  QBSectionStringW itemQbKeyCrc fileId str -> do
    sectionTypeWord typeStringW
    putQBKey itemQbKeyCrc
    putQBKey fileId
    p <- reservePointer
    w32 0
    fillPointer p
    append $ padTo4 $ TE.encodeUtf16BE $ str <> "\0"
  QBSectionQbKeyStringQs itemQbKeyCrc fileId qs -> do
    sectionTypeWord typeQbKeyStringQs
    putQBKey itemQbKeyCrc
    putQBKey fileId
    w32 qs
    w32 0
  QBSectionQbKeyString itemQbKeyCrc fileId k -> do
    sectionTypeWord typeQbKeyString
    putQBKey itemQbKeyCrc
    putQBKey fileId
    putQBKey k
    w32 0
  QBSectionFloat itemQbKeyCrc fileId f1 -> do
    sectionTypeWord typeFloat
    putQBKey itemQbKeyCrc
    putQBKey fileId
    f32 f1
    w32 0
  QBSectionFloatsX2 itemQbKeyCrc fileId f1 f2 -> do
    sectionTypeWord typeFloatsX2
    putQBKey itemQbKeyCrc
    putQBKey fileId
    p <- reservePointer
    w32 0
    fillPointer p
    putFloats [f1, f2]
  QBSectionFloatsX3 itemQbKeyCrc fileId f1 f2 f3 -> do
    sectionTypeWord typeFloatsX3
    putQBKey itemQbKeyCrc
    putQBKey fileId
    p <- reservePointer
    w32 0
    fillPointer p
    putFloats [f1, f2, f3]
  QBSectionStringPointer itemQbKeyCrc fileId qs -> do
    sectionTypeWord typeStringPointer
    putQBKey itemQbKeyCrc
    putQBKey fileId
    w32 qs
    w32 0

-- | 20 header bytes after the file size, in GH3 through GHWT
qbHeaderGH3 :: B.ByteString
qbHeaderGH3 = B.pack
  [ 0x1C, 0x08, 0x02, 0x04, 0x10, 0x04, 0x08, 0x0C, 0x0C, 0x08
  , 0x02, 0x04, 0x14, 0x02, 0x04, 0x0C, 0x10, 0x10, 0x0C, 0x00
  ]

putQB :: QBFormat -> [QBSection Word32 QBKey] -> BL.ByteString
putQB fmt sects = let
  go :: PutSeek s ()
  go = do
    let ?endian = fmt.endian
    let ?itemStyle = fmt.itemStyle
    w32 0
    pFileSize <- reservePointer
    pContentSize <- case fmt.header of
      QBHeaderGH3 -> do
        append qbHeaderGH3
        return Nothing
      QBHeaderGH5 -> do
        append $ B.pack [0x1C, 0, 0, 0, 0, 0, 0, 0]
        p <- reservePointer
        append $ B.replicate 8 0
        return $ Just p
    headerSize <- gets snd
    mapM_ putQBSection sects
    fileSize <- gets snd
    setPointer pFileSize $ fromIntegral fileSize
    forM_ pContentSize \p -> setPointer p $ fromIntegral $ fileSize - headerSize
  in BL.pack $ V.toList $ V.create do
    v <- MV.new 0
    (v', used) <- execStateT go (v, 0)
    return $ MV.take used v'

discardQS :: [QBSection QSResult k] -> [QBSection Word32 k]
discardQS = map $ first \case UnknownQS qs -> qs; KnownQS qs _ -> qs

discardQB :: [QBSection qs QBResult] -> [QBSection qs QBKey]
discardQB = map $ second \case UnknownQB qb -> qb; KnownQB b -> qbKeyCRC b

discardStrings :: [QBSection QSResult QBResult] -> [QBSection Word32 QBKey]
discardStrings = discardQS . discardQB

stripBackL :: T.Text -> T.Text
stripBackL s = fromMaybe s $ T.stripPrefix "\\L" s
