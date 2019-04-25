{- |
WIP port of (parts of) X360, a GPL C# library by DJ Shepherd
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
module STFS.Create where

import           Control.Applicative            (liftA2)
import           Control.Monad.Extra            (forM_, guard, ifM, unlessM,
                                                 when, whenM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Cont       hiding (shift)
import           Control.Monad.Trans.StackTrace (QueueLog, StackTraceT,
                                                 catchError, fatal)
import qualified Data.Binary.Put                as Put
import           Data.Bits                      (Bits (..))
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Function                  ((&))
import           Data.Int                       (Int16, Int32, Int64)
import           Data.IORef                     (newIORef, readIORef,
                                                 writeIORef)
import           Data.List.Split                (splitOn)
import qualified Data.Time                      as Time
import           Data.Word                      (Word16, Word32, Word64, Word8)
import           RockBand.Common                (each, reverseLookup)

(%) :: (a -> b) -> a -> b
(%) = id
infixl 0 %

data X v m a where
  V :: m a -> (a -> m ()) -> X v m a
  E :: m a -> X RValue m a

data LValue
data RValue
type V = X LValue
type E = X RValue

newtype Var m a = Var (forall v. X v m a)

instance (Functor m) => Functor (E m) where
  fmap f expr = E $ fmap f $ get expr

instance (Applicative m) => Applicative (E m) where
  pure = E . pure
  ef <*> ex = E $ get ef <*> get ex

instance (Monad m) => Monad (E m) where
  ex >>= f = E $ get ex >>= get . f

instance (Applicative m, Num a) => Num (E m a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum

liftX :: (forall b. m b -> n b) -> X v m a -> X v n a
liftX f = \case
  V gtr str -> V (f gtr) (f . str)
  E gtr     -> E (f gtr)

liftVar :: (forall b. m b -> n b) -> Var m a -> Var n a
liftVar f (Var g) = Var (liftX f g)

type F = Var C

get :: E m a -> m a
get (E t  ) = t
get (V t _) = t

auto :: (MonadIO m) => E m a -> m (Var m a)
auto x = do
  x' <- get x
  r  <- liftIO $ newIORef x'
  return $ Var $ V (liftIO $ readIORef r) (liftIO . writeIORef r)

($=) :: (Monad m) => V m a -> E m a -> m ()
V _ setter $= y = do
  y' <- get y
  setter y'
infixr 0 $=
-- same as ($)

modify :: (Monad m) => (a -> a) -> Var m a -> m ()
modify f = modifyM $ return . f

modifyM :: (Monad m) => (a -> m a) -> Var m a -> m ()
modifyM f (Var v) = do
  x <- get v
  v $= E (f x)

type C = StackTraceT (QueueLog IO)

type Meth m obj args a = args -> E m obj -> E m a

type Meth0 obj                a = Meth C obj () a
type Meth1 obj a1             a = Meth C obj (E C a1) a
type Meth2 obj a1 a2          a = Meth C obj (E C a1, E C a2) a
type Meth3 obj a1 a2 a3       a = Meth C obj (E C a1, E C a2, E C a3) a
type Meth4 obj a1 a2 a3 a4    a = Meth C obj (E C a1, E C a2, E C a3, E C a4) a
type Meth5 obj a1 a2 a3 a4 a5 a = Meth C obj (E C a1, E C a2, E C a3, E C a4, E C a5) a

liftHs :: (Monad m) => Meth m a (a -> m b) b
liftHs(f) this = E $ get this >>= f

some :: Meth0 (Maybe a) a
some() = liftHs $ maybe (fatal "Null reference") return

type Str = Maybe String

type D_List a = Var C [a]
type List a = Maybe (D_List a)

listAdd :: Meth1 (List a) a ()
listAdd(x) this = (this & some() &) $ liftHs $ \lst -> do
  elt <- get x
  modify (++ [elt]) lst

listAddRange :: Meth1 (List a) (List a) ()
listAddRange(rng) this = do
  xs <- rng & hsList()
  forM_ xs $ \x -> this & listAdd(pure x)

hsList :: Meth0 (List a) [a]
hsList() this = this & some() & liftHs (\(Var v) -> get v)

listCount :: Meth0 (List a) Int32
listCount() this = this & hsList() & liftHs (pure . fromIntegral . length)

fun :: (Monad m) => ((a -> ContT a (StackTraceT m) b) -> ContT a (StackTraceT m) a) -> StackTraceT m a
fun = evalContT . callCC

lastIndexOf :: (Eq a) => Meth1 (List a) a (Maybe Int)
lastIndexOf(x) this = (this & hsList() &) $ liftHs $ \lst -> do
  elt <- get x
  pure $ lookup elt $ reverse $ zip lst [0..]

newList :: [a] -> E C (List a)
newList xs = E $ Just <$> auto (pure xs)

(&.) :: E C (Maybe a) -> (a -> F b) -> X v C b
eobj &. field = let
  getVar = fmap field $ get (eobj & some())
  gtr = getVar >>= \(Var v) -> get v
  str x = getVar >>= \(Var v) -> v $= pure x
  in V gtr str
infixl 1 &. -- same as (&)

makeField :: (E C (Maybe a) -> (forall v. X v C b)) -> a -> F b
makeField f x = Var $ f $ return $ Just x

class Make obj where
  make :: E C obj

class New obj args where
  new :: args -> E C obj

ix :: E C Int -> D_List a -> F a
ix ei lst = let
  gtr = get (liftA2 drop ei (pure (Just lst) & hsList())) >>= \case
    []    -> fatal "Array index out of bounds"
    x : _ -> return x
  str x = get $ (pure (Just lst) & some() &) $ liftHs $ modifyM $ \xs -> do
    i <- get ei
    case splitAt i xs of
      (_, [])    -> fatal "Array index out of bounds"
      (a, _ : b) -> return $ a ++ [x] ++ b
  in Var $ V gtr str

(==.), (/=.), (<.), (>.), (<=.), (>=.) :: (Ord a) => E C a -> E C a -> E C Bool
(==.) = liftA2 (==)
(/=.) = liftA2 (/=)
(<.) = liftA2 (<)
(>.) = liftA2 (>)
(<=.) = liftA2 (<=)
(>=.) = liftA2 (>=)
infix 4 ==., /=., <., >., <=., >=.

(<<.), (>>.) :: (Bits a) => E C a -> E C Int -> E C a
(<<.) = liftA2 shiftL
(>>.) = liftA2 shiftR
infixl 8 <<., >>.

(.&), (.|) :: (Bits a) => E C a -> E C a -> E C a
(.&) = liftA2 (.&.)
(.|) = liftA2 (.|.)
infixl 7 .&
infixl 5 .|

new0 :: (Make o) => (E C o -> C ()) -> () -> E C o
new0 f () = E $ do
  Var this <- auto make
  f this
  get this

new1 :: (Make o) => (Var C a -> E C o -> C ()) -> E C a -> E C o
new1 f x1 = E $ do
  Var this <- auto make
  x1' <- auto x1
  f x1' this
  get this

new2 :: (Make o) => ((Var C a, Var C b) -> E C o -> C ()) -> (E C a, E C b) -> E C o
new2 f (x1, x2) = E $ do
  Var this <- auto make
  x1' <- auto x1
  x2' <- auto x2
  f (x1', x2') this
  get this

new3 :: (Make o) => ((Var C a, Var C b, Var C c) -> E C o -> C ()) -> (E C a, E C b, E C c) -> E C o
new3 f (x1, x2, x3) = E $ do
  Var this <- auto make
  x1' <- auto x1
  x2' <- auto x2
  x3' <- auto x3
  f (x1', x2', x3') this
  get this

new4 :: (Make o) => ((Var C a, Var C b, Var C c, Var C d) -> E C o -> C ()) -> (E C a, E C b, E C c, E C d) -> E C o
new4 f (x1, x2, x3, x4) = E $ do
  Var this <- auto make
  x1' <- auto x1
  x2' <- auto x2
  x3' <- auto x3
  x4' <- auto x4
  f (x1', x2', x3', x4') this
  get this

args0 :: C r -> () -> E C r
args0 f () = E f

args1 :: (Var C a -> C r) -> E C a -> E C r
args1 f x1 = E $ auto x1 >>= f

args2 :: ((Var C a, Var C b) -> C r) -> (E C a, E C b) -> E C r
args2 f (x1, x2) = E $ ((,) <$> auto x1 <*> auto x2) >>= f

args3 :: ((Var C a, Var C b, Var C c) -> C r) -> (E C a, E C b, E C c) -> E C r
args3 f (x1, x2, x3) = E $ ((,,) <$> auto x1 <*> auto x2 <*> auto x3) >>= f

args4 :: ((Var C a, Var C b, Var C c, Var C d) -> C r) -> (E C a, E C b, E C c, E C d) -> E C r
args4 f (x1, x2, x3, x4) = E $ ((,,,) <$> auto x1 <*> auto x2 <*> auto x3 <*> auto x4) >>= f

args5 :: ((Var C a, Var C b, Var C c, Var C d, Var C e) -> C r) -> (E C a, E C b, E C c, E C d, E C e) -> E C r
args5 f (x1, x2, x3, x4, x5) = E $ ((,,,,) <$> auto x1 <*> auto x2 <*> auto x3 <*> auto x4 <*> auto x5) >>= f

meth0 :: (E C o -> C r) -> () -> E C o -> E C r
meth0 f () obj = E $ get obj >>= f . pure

meth1 :: (Var C a -> E C o -> C r) -> E C a -> E C o -> E C r
meth1 f x1 obj = E $ do
  x1' <- auto x1
  get obj >>= f x1' . pure

meth2 :: ((Var C a, Var C b) -> E C o -> C r) -> (E C a, E C b) -> E C o -> E C r
meth2 f (x1, x2) obj = E $ do
  x1' <- auto x1
  x2' <- auto x2
  get obj >>= f (x1', x2') . pure

meth3 :: ((Var C a, Var C b, Var C c) -> E C o -> C r) -> (E C a, E C b, E C c) -> E C o -> E C r
meth3 f (x1, x2, x3) obj = E $ do
  x1' <- auto x1
  x2' <- auto x2
  x3' <- auto x3
  get obj >>= f (x1', x2', x3') . pure

meth4 :: ((Var C a, Var C b, Var C c, Var C d) -> E C o -> C r) -> (E C a, E C b, E C c, E C d) -> E C o -> E C r
meth4 f (x1, x2, x3, x4) obj = E $ do
  x1' <- auto x1
  x2' <- auto x2
  x3' <- auto x3
  x4' <- auto x4
  get obj >>= f (x1', x2', x3', x4') . pure

meth5 :: ((Var C a, Var C b, Var C c, Var C d, Var C e) -> E C o -> C r) -> (E C a, E C b, E C c, E C d, E C e) -> E C o -> E C r
meth5 f (x1, x2, x3, x4, x5) obj = E $ do
  x1' <- auto x1
  x2' <- auto x2
  x3' <- auto x3
  x4' <- auto x4
  x5' <- auto x5
  get obj >>= f (x1', x2', x3', x4', x5') . pure

fi :: (Functor f, Integral a, Num b) => f a -> f b
fi = fmap fromIntegral

glue4Bytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
glue4Bytes a b c d
  =   (fromIntegral a `shiftL` 24)
  .|. (fromIntegral b `shiftL` 16)
  .|. (fromIntegral c `shiftL` 8)
  .|.  fromIntegral d

glue3Bytes :: Word8 -> Word8 -> Word8 -> Word32
glue3Bytes = glue4Bytes 0

glue2Bytes :: Word8 -> Word8 -> Word16
glue2Bytes a b = (fromIntegral a `shiftL` 8) .|. fromIntegral b

-- TODO I'm hoping we don't have to handle non-mapped values in enum types

-- 1. derive Enum and Bounded
class (Show i, Integral i, Enum e, Bounded e) => CEnum i e | e -> i where
  -- 2. optionally override this
  enumMapping :: e -> i
  enumMapping = fromIntegral . fromEnum

enum :: (CEnum i e) => Meth0 i e
enum() = liftHs $ \this -> case reverseLookup each enumMapping this of
  Nothing -> fatal $ "enum() failure: no mapping for " ++ show this
  -- TODO include the attempted enum type
  Just x  -> pure x

unenum :: (CEnum i e) => Meth0 e i
unenum() = liftHs $ pure . enumMapping

castEnumField :: (Integral i, Enum e) => F i -> F e
castEnumField (Var v) = Var $ V
  (fmap (toEnum . fromIntegral) $ get v)
  ((v $=) . fromIntegral . fromEnum)

whileLoop
  :: (MonadIO m)
  => StackTraceT m Bool
  -> (ContT () (StackTraceT m) () -> ContT () (StackTraceT m) ())
  -> StackTraceT m ()
whileLoop cond fn = whenM cond $ do
  Var broke <- auto $ pure False
  fun $ \ret -> fn $ do
    lift $ broke $= pure True
    ret ()
  unlessM (get broke) $ whileLoop cond fn

---------
-- STFSDescriptor.cs
---------

data D_BlockRecord = BlockRecord
  { br_xFlags     :: F (List Word8)
  , br_xThisBlock :: F (List Word8)
  , br_xLevel     :: F Word8
  }
type BlockRecord = Maybe D_BlockRecord

instance Make BlockRecord where
  make = E $ do
    br_xFlags     <- auto $ pure Nothing
    br_xThisBlock <- auto $ newList $ replicate 3 0
    br_xLevel     <- auto 0
    return $ Just BlockRecord{..}

br_ThisBlock :: D_BlockRecord -> F Word32
br_ThisBlock = makeField $ \this -> V
  % do
    get $ glue3Bytes
      <$> (this &. br_xThisBlock &. ix 0)
      <*> (this &. br_xThisBlock &. ix 1)
      <*> (this &. br_xThisBlock &. ix 2)
  % \value -> do
  this &. br_xThisBlock &. ix 0 $= fi ((pure value >>. 16) .& 0xFF)
  this &. br_xThisBlock &. ix 1 $= fi ((pure value >>. 8) .& 0xFF)
  this &. br_xThisBlock &. ix 2 $= fi (pure value .& 0xFF)

br_ThisLevel :: D_BlockRecord -> F TreeLevel
br_ThisLevel = castEnumField . br_xLevel

br_Indicator :: D_BlockRecord -> F Word8
br_Indicator = makeField $ \this -> V
  % do get $ (this &. br_xFlags &. ix 0) >>. 6
  % \value -> this &. br_xFlags &. ix 0 $= (pure value .& 3) <<. 6

br_Flags :: D_BlockRecord -> F Word32
br_Flags = makeField $ \this -> V
  % do
    get $ glue4Bytes
      <$> (this &. br_xFlags &. ix 0)
      <*> (this &. br_xFlags &. ix 1)
      <*> (this &. br_xFlags &. ix 2)
      <*> (this &. br_xFlags &. ix 3)
  % \value -> this &. br_xFlags $= bitConv_GetBytes (pure value, pure True)

-- public BlockRecord() { xFlags = new byte[] { 0, 0, 0, 0 }; }
instance New BlockRecord () where
  new = new0 $ \this -> do
    this &. br_xFlags $= newList [0, 0, 0, 0]

-- public BlockRecord(uint xFlagIn) { Flags = xFlagIn; }
instance New BlockRecord (E C Word32) where
  new = new1 $ \(Var xFlagIn) this -> do
    this &. br_Flags $= xFlagIn

-- public BlockRecord(HashStatus xStatus, uint xNext) { Flags = (uint)((uint)xStatus << 30 | (xNext & 0xFFFFFF)); }
instance New BlockRecord (E C HashStatus, E C Word32) where
  new = new2 $ \(Var xStatus, Var xNext) this -> do
    this &. br_Flags $= ((xStatus & unenum() & fi) <<. 30) .| (xNext .& 0xFFFFFF)

-- public HashStatus Status { get { return (HashStatus)(xFlags[0] >> 6); } set { xFlags[0] = (byte)((int)value << 6);} }
br_Status :: D_BlockRecord -> F HashStatus
br_Status = makeField $ \this -> V
  % do get $ ((this &. br_xFlags &. ix 0) >>. 6) & enum()
  % \value -> do this &. br_xFlags &. ix 0 $= (pure value & unenum()) <<. 6

-- public uint NextBlock { get { return (uint)(xFlags[1] << 16 | xFlags[2] << 8 | xFlags[3]); } set { Flags = (uint)((xFlags[0] << 24) | (int)(value & 0xFFFFFF)); }}
br_NextBlock :: D_BlockRecord -> F Word32
br_NextBlock = makeField $ \this -> V
  % do
    get $ glue3Bytes
      <$> (this &. br_xFlags &. ix 1)
      <*> (this &. br_xFlags &. ix 2)
      <*> (this &. br_xFlags &. ix 3)
  % \value -> this &. br_Flags $= ((fi (this &. br_xFlags &. ix 0) <<. 24) .| (pure value .& 0xFFFFFF))

br_MarkOld :: Meth0 BlockRecord ()
br_MarkOld = meth0 $ \this -> do
  this &. br_Status $= pure HashStatus_Old
  this &. br_NextBlock $= pure constants_STFSEnd

br_Index :: Meth0 BlockRecord Word8
br_Index = meth0 $ \this -> get $ (this &. br_Indicator) .& 1

br_AllocationFlag :: D_BlockRecord -> F HashFlag
br_AllocationFlag = castEnumField . br_Indicator

br_BlocksFree :: D_BlockRecord -> F Int32
br_BlocksFree = makeField $ \this -> V
  % do get $ fi $ ((this &. br_Flags) >>. 15) .& 0x7FFF
  % \value -> do
    -- Sets unused/free blocks in each table, checks for errors
    -- if blocksfree is 0xAA for Level 1 or 0x70E4 for L2, whole table can be full o shit, cause theres no used blocks :P
    let value' = max value 0
    this &. br_Flags $= (fi (this &. br_Indicator) <<. 30) .| (fi (pure value') <<. 15)

br_Switch :: Meth0 BlockRecord Bool
br_Switch = meth0 $ \this -> catchError
  % do
    get (this &. br_AllocationFlag) >>= \case
      Unallocated -> do
        this &. br_Flags $= (1 <<. 30) .| (fi (this &. br_BlocksFree) <<. 15)
        return True
      AllocatedFree -> do
        this &. br_Flags $= (2 <<. 30) .| (fi (this &. br_BlocksFree) <<. 15)
        return True
      AllocatedInUseOld -> do
        this &. br_Flags $= (3 <<. 30) .| (fi (this &. br_BlocksFree) <<. 15)
        return True
      AllocatedInUseCurrent -> do
        -- is this really correct? or should it be, like, 4 << 30?
        this &. br_Flags $= (2 <<. 30) .| (fi (this &. br_BlocksFree) <<. 15)
        return True
      -- would return false if none of the above
  % \_ -> return False

data D_STFSDescriptor = STFSDescriptor
  { sd_xStruct       :: F (List Word8)
  , sd_xSpaceBetween :: F (List Word32) -- also SpaceBetween
  , sd_xBaseByte     :: F Word8
  , sd_xBlockCount   :: F Word32 -- also BlockCount
  , sd_TopRecord     :: F BlockRecord
  , sd_Shift         :: F Word8
  }
type STFSDescriptor = Maybe D_STFSDescriptor

sd_DirectoryBlockCount :: D_STFSDescriptor -> F Word16
sd_DirectoryBlockCount = makeField $ \this -> V
  % do get $ glue2Bytes <$> (this &. sd_xStruct &. ix 1) <*> (this &. sd_xStruct &. ix 0)
  % \value -> do
    -- Max is 0x3FF blocks (0xFFFF entries / 0x40 per block)
    this &. sd_xStruct &. ix 0 $= fi (pure value) .& 0xFF
    this &. sd_xStruct &. ix 1 $= fi (pure value >>. 8) .& 3

sd_DirectoryBlock :: D_STFSDescriptor -> F Word32
sd_DirectoryBlock = makeField $ \this -> V
  % do get $ glue3Bytes <$> (this &. sd_xStruct &. ix 4) <*> (this &. sd_xStruct &. ix 3) <*> (this &. sd_xStruct &. ix 2)
  % \value -> do
    Var x <- auto $ newList []
    get $ x & listAddRange(bitConv_GetBytes (pure value, pure False))
    forM_ [0, 1, 2] $ \i -> do
      this &. sd_xStruct &. ix (2 + i) $= x &. ix i

sd_BaseBlock :: Meth0 STFSDescriptor Word16
sd_BaseBlock = meth0 $ \this -> get $ fi (this &. sd_xBaseByte) <<. 0xC

sd_ThisType :: Meth0 STFSDescriptor STFSType
sd_ThisType = meth0 $ \this -> get $ this &. sd_Shift & fi & enum()

sd_OldBlockCount :: Meth0 STFSDescriptor Word32
sd_OldBlockCount = meth0 $ \this -> get $ fi $ this &. sd_TopRecord &. br_BlocksFree

instance Make STFSDescriptor where
  make = E $ do
    sd_xStruct       <- auto $ pure Nothing
    sd_xSpaceBetween <- auto $ newList $ replicate 3 0
    sd_xBaseByte     <- auto 0
    sd_xBlockCount   <- auto 0
    sd_TopRecord     <- auto $ new ()
    sd_Shift         <- auto 0
    return $ Just STFSDescriptor{..}

-- internal STFSDescriptor(STFSType xType, uint xTotalBlocks)
instance New STFSDescriptor (E C STFSType, E C Word32) where
  new = new2 $ \(Var xType, Var xTotalBlocks) this -> do
    get $ this & sd_XSetStructure(xType)
    (this &. sd_xStruct) $= newList [0, 0, 0, 0, 0]
    whenM (get $ xTotalBlocks >. (this &. sd_xSpaceBetween &. ix 2)) $ do
      this &. sd_xStruct $= pure Nothing
      fatal "STFSExcepts.MaxOver"
    this &. sd_xBlockCount $= xTotalBlocks
    this &. sd_xBaseByte $= ifM ((this & sd_ThisType()) ==. pure STFSType_Type0) 0xB 0xA

-- void XSetStructure(STFSType xType)
sd_XSetStructure :: Meth1 STFSDescriptor STFSType ()
sd_XSetStructure = meth1 $ \(Var xType) this -> get xType >>= \case
  STFSType_Type0 -> do
    this &. sd_xSpaceBetween &. ix 0 $= 0xA0
    this &. sd_xSpaceBetween &. ix 1 $= 0x718F
    this &. sd_xSpaceBetween &. ix 2 $= 0xFE7DA -- Max Block
    this &. sd_Shift $= 0
  STFSType_Type1 -> do
    this &. sd_xSpaceBetween &. ix 0 $= 0xAC
    this &. sd_xSpaceBetween &. ix 1 $= 0x723A
    this &. sd_xSpaceBetween &. ix 2 $= 0xFD00B -- Max Block before size of package over does FATX limit
    this &. sd_Shift $= 1

-- internal STFSDescriptor(byte[] xDescriptor, uint xTotalBlocks, uint xOldBlocks, byte xType)
instance New STFSDescriptor (E C (List Word8), E C Word32, E C Word32, E C Word8) where
  new = new4 $ \(Var xDescriptor, Var xTotalBlocks, Var xOldBlocks, Var xType) this -> do
    this &. sd_xStruct $= xDescriptor
    get $ this & sd_XSetStructure((xType .& 1) & fi & enum())
    this &. sd_TopRecord $= new ((fi (xType >>. 1) <<. 30) .| (fi xOldBlocks <<. 15) :: E C Word32)
    ifM (get $ xTotalBlocks >. (this &. sd_xSpaceBetween &. ix 2))
      % do
        this &. sd_xStruct $= pure Nothing
      % do
        this &. sd_xBlockCount $= xTotalBlocks
        this &. sd_xBaseByte $= ifM ((this & sd_ThisType()) ==. pure STFSType_Type0) 0xB 0xA
        xOldBlocks $= 0 -- is this useless???

-- internal STFSDescriptor(STFSPackage xPackage)
instance New STFSDescriptor (E C STFSPackage) where
  new = new1 $ \(Var xPackage) this -> do
    xPackage &. sp_xIO &. dj_Position $= 0x340
    xPackage &. sp_xIO &. dj_IsBigEndian $= pure True
    xBlockInfo <- get $ xPackage &. sp_xIO & dj_ReadInt32()
    let xBaseByte = fromIntegral $ ((xBlockInfo + 0xFFF) .&. 0xF000) `shiftR` 0xC :: Word8
    xPackage &. sp_xIO &. dj_Position $= 0x379
    whenM (get $ (xPackage &. sp_xIO & dj_ReadByte()) /=. 0x24) -- Struct Size
      $ fatal "STFSExcepts.Type"
    whenM (get $ (xPackage &. sp_xIO & dj_ReadByte()) /=. 0) -- Reversed
      $ fatal "STFSExcepts.Type"
    {-  STRUCT OF THE NEXT 6 BYTES:
        byte for block separation
        Little Endian File Table block count short (2 bytes)
        3 bytes in Little Endian for the starting block of the File Table
    -}
    idx <- get $ (xPackage &. sp_xIO & dj_ReadByte()) .& 3
    this &. sd_xStruct $= xPackage &. sp_xIO & dj_ReadBytes(5)
    xPackage &. sp_xIO &. dj_Position $= 0x395
    this &. sd_xBlockCount $= xPackage &. sp_xIO & dj_ReadUInt32()
    xOldBlocks <- get $ xPackage &. sp_xIO & dj_ReadUInt32()
    -- Checks the type of Structure
    case xBaseByte of
      0xB -> if idx == 1
        then get $ this & sd_XSetStructure(pure STFSType_Type0)
        else fatal "STFSExcepts.Type"
      0xA -> if idx == 0 || idx == 2
        then get $ this & sd_XSetStructure(pure STFSType_Type1)
        else fatal "STFSExcepts.Type"
      _ -> fatal "STFSExcepts.Type"
    whenM (get $ (this &. sd_xBlockCount) >. (this &. sd_xSpaceBetween &. ix 2))
      $ fatal "STFSExcepts.MaxOver"
    this &. sd_TopRecord $= new ((fi ((pure idx >>. 1) .& 1) <<. 30) .| (pure xOldBlocks <<. 15) :: E C Word32)
    -- Grab Real Block Count
    Var i <- auto $ (this &. sd_xBlockCount) - 1
    whileLoop (get $ i >=. 0) $ \brk -> do
      lift $ this &. sd_xBlockCount $= i + 1
      whenM (lift $ get $ (this & sd_GenerateDataOffset(i)) <. (xPackage &. sp_xIO &. dj_Length))
        brk
      lift $ i $= i - 1

-- internal uint GenerateDataBlock(uint xBlock)
sd_GenerateDataBlock :: Meth1 STFSDescriptor Word32 Word32
sd_GenerateDataBlock = meth1 $ \(Var xBlock') this -> do
  xBlock <- get xBlock'
  if xBlock >= 0x4AF768
    then return $ fromIntegral constants_STFSEnd
    else catchError
      % do
        shft <- fromIntegral <$> get (this &. sd_Shift)
        return $ sum $ concat
          -- Gets 0xAA section, shifts it for 1 or 2 tables per section, and adds original block
          [ return $ (((xBlock `quot` constants_BlockLevel0) + 1) `shiftL` shft) + xBlock
          -- Gets current 0x70e4 section, adjusts to table count
          , do
            guard $ xBlock >= constants_BlockLevel0
            return $ ((xBlock `quot` constants_BlockLevel1) + 1) `shiftL` shft
          -- There is only going to be 1 0x4AF768 section, add to base
          , do
            guard $ xBlock >= constants_BlockLevel1
            return $ 1 `shiftL` shft
          ]
      % \_ -> fatal "STFSExcepts.General"

-- internal uint GenerateHashBlock(uint xBlock, TreeLevel xTree)
sd_GenerateHashBlock :: Meth2 STFSDescriptor Word32 TreeLevel Word32
sd_GenerateHashBlock = meth2 $ \(Var xBlock', Var xTree') this -> do
  xBlock <- get xBlock'
  xTree <- get xTree'
  if xBlock >= 0x4AF768
    then return $ fromIntegral constants_STFSEnd
    else catchError
      % case xTree of
        L0 -> do
          sp0 <- get $ this &. sd_xSpaceBetween &. ix 0
          shft <- fromIntegral <$> get (this &. sd_Shift)
          return $ sum $ concat
            -- Get Base Level 0 Table
            [ return $ (xBlock `quot` constants_BlockLevel0) * sp0
            -- Adjusts the result for Level 1 table count
            , do
              guard $ xBlock >= constants_BlockLevel0
              return $ ((xBlock `quot` constants_BlockLevel1) + 1) `shiftL` shft
            -- Adjusts for the Level 2 table
            , do
              guard $ xBlock >= constants_BlockLevel1
              return $ 1 `shiftL` shft
            ]
        L1 -> do
          -- Grab the number of Table 1 blocks
          if xBlock < constants_BlockLevel1
            then get $ this &. sd_xSpaceBetween &. ix 0
            else do
              sb1 <- get $ this &. sd_xSpaceBetween &. ix 1
              shft <- fromIntegral <$> get (this &. sd_Shift)
              return $ sb1 * (xBlock `quot` constants_BlockLevel1) + (1 `shiftL` shft)
        L2 -> do
          -- Only one Level 2 table
          get $ this &. sd_xSpaceBetween &. ix 1
        _ -> return $ fromIntegral constants_STFSEnd
      % \_ -> fatal "STFSExcepts.General"

-- internal long GenerateHashOffset(uint xBlock, TreeLevel xTree)
sd_GenerateHashOffset :: Meth2 STFSDescriptor Word32 TreeLevel Int64
sd_GenerateHashOffset = meth2 $ \(Var xBlock', Var xTree') this -> do
  xBlock <- get xBlock'
  xTree <- get xTree'
  if xBlock >= 0x4AF768
    then return $ fromIntegral constants_STFSEnd
    else catchError
      % do
        xReturn <- get $ this & sd_BlockToOffset (this & sd_GenerateHashBlock(xBlock', xTree'))
        return $ xReturn + case xTree of
          L0 -> 0x18 * fromIntegral (xBlock `rem` constants_BlockLevel0)
          L1 -> 0x18 * fromIntegral ((xBlock `quot` constants_BlockLevel0) `rem` constants_BlockLevel0)
          L2 -> 0x18 * fromIntegral ((xBlock `quot` constants_BlockLevel1) `rem` constants_BlockLevel0)
          _  -> 0
      % \_ -> fatal "STFSExcepts.General"

-- internal long GenerateDataOffset(uint xBlock)
sd_GenerateDataOffset :: Meth1 STFSDescriptor Word32 Int64
sd_GenerateDataOffset = meth1 $ \(Var xBlock) this -> catchError
  % do
    ifM (get $ xBlock >=. 0x4AF768)
      % do return $ fromIntegral constants_STFSEnd
      % do get $ this & sd_BlockToOffset(this & sd_GenerateDataBlock(xBlock))
  % \_ -> fatal "STFSExcepts.General"

-- internal long BlockToOffset(uint xBlock)
sd_BlockToOffset :: Meth1 STFSDescriptor Word32 Int64
sd_BlockToOffset = meth1 $ \(Var xBlock) this -> catchError
  % do
    get $ ((fi xBlock * 0x1000) + fi (this & sd_BaseBlock()))
  % \_ -> fatal "STFSExcepts.General"

-- internal long GenerateBaseOffset(uint xBlock, TreeLevel xTree)
sd_GenerateBaseOffset :: Meth2 STFSDescriptor Word32 TreeLevel Int64
sd_GenerateBaseOffset = meth2 $ \(Var xBlock, Var xTree) this -> catchError
  % do
    get $ this & sd_BlockToOffset(this & sd_GenerateHashBlock(xBlock, xTree))
  % \_ -> fatal "STFSExcepts.General"

-- internal byte[] GetData()
sd_GetData :: Meth0 STFSDescriptor (List Word8)
sd_GetData = meth0 $ \this -> do
  Var idx <- auto 1
  whenM (get $ (this & sd_ThisType()) ==. pure STFSType_Type1) $ do
    idx $= (this &. sd_TopRecord & br_Index()) <<. 1
  -- Returns the Descriptor in a data fashion
  Var xReturn <- auto $ newList []
  get $ xReturn & listAddRange(newList [0x24, 0])
  get $ xReturn & listAdd(idx)
  get $ xReturn & listAddRange(this &. sd_xStruct)
  get $ xReturn & listAddRange(newList $ replicate 20 0)
  get $ xReturn & listAddRange(bitConv_GetBytes (this &. sd_xBlockCount, pure True))
  get $ xReturn & listAddRange(bitConv_GetBytes (this &. sd_TopRecord &. br_BlocksFree, pure True))
  get xReturn

---------
-- Create.cs
---------

data D_CItemEntry = CItemEntry
  { ci_create    :: F CreateSTFS
  , ci_xthispath :: F Str
  }
type CItemEntry = Maybe D_CItemEntry

{-

newCItemEntry :: (MonadIO m) => Maybe String -> Maybe CreateSTFS -> m CItemEntry
newCItemEntry path xCreate = do
  ci_xthispath <- var path
  ci_create <- var xCreate
  return CItemEntry{..}

-- internal string getparentpath()
ci_getparentpath :: (MonadIO m) => Meth m CItemEntry String
ci_getparentpath this = do
  str <- get (ci_xthispath this) >>= noNull
  return $ case lastIndexOf '/' str of
    Nothing -> ""
    Just i  -> map toLower $ take i str

ci_Name_get :: (MonadIO m) => Meth m CItemEntry (Maybe String)
ci_Name_get this = fmap (Just . xExtractName) $ get (ci_xthispath this) >>= noNull

ci_Name_set :: (MonadIO m) => Maybe String -> Meth m CItemEntry ()
ci_Name_set value this = do
  v <- noNull value
  _ <- isValidXboxName v
  let v' = take 0x28 v
  xtp <- get (ci_xthispath this) >>= noNull
  ci_xthispath this $= case lastIndexOf '/' xtp of
    Nothing -> Just v'
    Just i  -> Just $ take i xtp ++ "/" ++ v'

-- public static uint BlockCount(string file)
createTools_BlockCount :: (MonadIO m) => Maybe FilePath -> StackTraceT m Word32
createTools_BlockCount mfile = do
  file <- noNull mfile
  stackIO $ Dir.doesFileExist file >>= \case
    False -> return constants_STFSEnd
    True -> do
      len <- (fromIntegral :: Integer -> Int64) <$> withBinaryFile file ReadMode hFileSize
      return $ fromIntegral $ quot (len - 1) 0x1000 + 1

-}

data D_CFileEntry = CFileEntry
  { cfi_base       :: CItemEntry
  , cfi_filelocale :: F Str
  }
type CFileEntry = Maybe D_CFileEntry

{-

cfi_BlockCount :: (MonadIO m) => CFileEntry -> StackTraceT m Word32
cfi_BlockCount this = get (cfi_filelocale this) >>= createTools_BlockCount

-- public int GetLength() { return (int)new FileInfo(filelocale).Length; }
cfi_GetLength :: (MonadIO m) => Meth m CFileEntry Int32
cfi_GetLength = undefined

newCFileEntry :: (MonadIO m) => Maybe String -> Maybe String -> Maybe CreateSTFS -> m CFileEntry
newCFileEntry xFile path xCreate = do
  cfi_base <- newCItemEntry path xCreate
  cfi_filelocale <- var xFile
  return CFileEntry{..}

-}

data D_CFolderEntry = CFolderEntry
  { cfo_base :: CItemEntry
  }
type CFolderEntry = Maybe D_CFolderEntry

{-

newCFolderEntry :: (MonadIO m) => Maybe String -> Maybe CreateSTFS -> m CFolderEntry
newCFolderEntry path xCreate = CFolderEntry <$> newCItemEntry path xCreate

-- public CFileEntry[] GetFiles()
cfo_GetFiles :: Meth m CFolderEntry (List CFileEntry)
cfo_GetFiles = undefined
-- List<CFileEntry> xReturn = new List<CFileEntry>();
-- foreach (CFileEntry x in create.xFileDirectory)
-- {
--     if (x.getparentpath() == xthispath.ToLower())
--         xReturn.Add(x);
-- }
-- return xReturn.ToArray();

-}

data SphereColor

data DashStyle

data ThemeParams

data D_CreateSTFS = CreateSTFS
  { cs_xFileDirectory   :: F (List CFileEntry)
  , cs_xFolderDirectory :: F (List CFolderEntry)
  , cs_STFSType         :: F STFSType -- also xStruct
  , cs_HeaderData       :: F HeaderData
  , cs_xtheme           :: F ThemeParams -- also ThemeSettings
  , cs_root             :: F CFolderEntry -- also RootPath
  }
type CreateSTFS = Maybe D_CreateSTFS

-- internal uint[] BlockStep
cs_BlockStep :: Meth0 CreateSTFS [Word32]
cs_BlockStep = meth0 $ \this -> do
  xStruct <- get $ this &. cs_STFSType
  return
    [ 0xAA, 0x70E4
    , case xStruct of
      STFSType_Type0 -> 0xFE7DA
      STFSType_Type1 -> 0xFD00B
    ]

{-

-- internal byte GetDirectoryCount { get { return (byte)(((xFileDirectory.Count + xFolderDirectory.Count - 1) / 0x40) + 1); } }
cs_GetDirectoryCount :: (MonadIO m) => Meth m CreateSTFS Word8
-- Subtract 1 for prevention of Modular error
cs_GetDirectoryCount this = do
  files <- get (cs_xFileDirectory this) >>= listCount
  folders <- get (cs_xFolderDirectory this) >>= listCount
  return $ fromIntegral $ quot (files + folders - 1) 0x40 + 1

-- short UppedDirectCount { get { return (byte)(((xFileDirectory.Count + xFolderDirectory.Count - 1) / 0x40) + 1); } }
cs_UppedDirectCount :: (MonadIO m) => Meth m CreateSTFS Int16
cs_UppedDirectCount this = do
  files <- get (cs_xFileDirectory this) >>= listCount
  folders <- get (cs_xFolderDirectory this) >>= listCount
  let n = quot (files + folders - 1) 0x40 + 1
  return $ fromIntegral (fromIntegral n :: Word8)

-- internal uint TotalBlocks
cs_TotalBlocks :: (MonadIO m) => Meth m CreateSTFS Word32
cs_TotalBlocks this = do
  xReturn <- fromIntegral <$> cs_GetDirectoryCount this
  files <- get (cs_xFileDirectory this) >>= hsList
  sum . (xReturn :) <$> mapM cfi_BlockCount files

-- uint UppedTotalBlocks(uint xFileAdd) { return (uint)(UppedDirectCount + xFileAdd); }
cs_UppedTotalBlocks :: (MonadIO m) => Word32 -> Meth m CreateSTFS Word32
cs_UppedTotalBlocks xFileAdd this = (xFileAdd +) . fromIntegral <$> cs_UppedDirectCount this

initCreateSTFS :: (MonadIO m) => StackTraceT m CreateSTFS
initCreateSTFS = do
  cs_xFileDirectory   <- var [] >>= var . Just
  cs_xFolderDirectory <- var [] >>= var . Just
  cs_STFSType         <- var STFSType_Type0
  cs_HeaderData       <- undefined -- public HeaderData HeaderData = new HeaderData();
  cs_xtheme           <- undefined -- ThemeParams xtheme = new ThemeParams();
  cs_root             <- var Nothing
  return CreateSTFS{..}

-- public CreateSTFS() { root = new CFolderEntry("", this); }
newCreateSTFS :: (MonadIO m) => StackTraceT m CreateSTFS
newCreateSTFS = do
  this <- initCreateSTFS
  (cs_root this $=) . Just =<< newCFolderEntry (Just "") (Just this)
  return this

-- public bool AddFile(string FileLocation, string FilePath)
cs_AddFile :: (MonadIO m) => Maybe FilePath -> Maybe FilePath -> Meth m CreateSTFS Bool
cs_AddFile fileLocation filePath this = let
  c1 = (>= 0x3FF) <$> cs_UppedDirectCount this
  utb = createTools_BlockCount fileLocation >>= \bc -> this & cs_UppedTotalBlocks bc
  bs2 = (!! 2) <$> cs_BlockStep this
  c2 = liftA2 (>) utb bs2
  c3 = return $ null filePath
  in orM [c1, c2, c3] >>= \case
    True -> return False
    False -> do
      filePath' <- xExtractLegitPath filePath
      (this & cs_containsfile filePath') >>= \case
        True -> return False
        False -> do
          cfe <- newCFileEntry fileLocation filePath' (Just this)
          get (cs_xFileDirectory this) >>= listAdd cfe
          return True

-- public bool AddFolder(string FolderPath)
cs_AddFolder :: (MonadIO m) => Maybe FilePath -> Meth m CreateSTFS Bool
cs_AddFolder folderPath this = fun $ \ret -> do
  folderPath' <- lift $ xExtractLegitPath folderPath
  when (folderPath' == Just "") $ ret False
  idx <- lift $ lastIndexOf '/' <$> noNull folderPath'
  name <- var $ Just ""
  case idx of
    Nothing -> name $= folderPath'
    Just i -> do
      fp' <- lift $ noNull folderPath'
      name $= Just (drop (i + 1) fp')
      let parentpath = take i fp'
      b <- lift $ this & cs_containspath (Just parentpath)
      unless b $ ret False
  b <- lift $ this & cs_containspath folderPath'
  when b $ ret False
  _ <- lift $ get name >>= noNull >>= isValidXboxName
  cfe <- lift $ newCFolderEntry folderPath' (Just this)
  lift $ get (cs_xFolderDirectory this) >>= listAdd cfe
  return True

-- bool containspath(string path)
cs_containspath :: (MonadIO m) => Maybe FilePath -> Meth m CreateSTFS Bool
cs_containspath path this = do
  cfes <- get (cs_xFolderDirectory this) >>= hsList
  flip anyM cfes $ \x -> do
    xtp <- get $ ci_xthispath $ cfo_base x
    (==) <$> fmap (map toLower) (noNull xtp) <*> fmap (map toLower) (noNull path)

cs_containsfile :: (MonadIO m) => Maybe FilePath -> Meth m CreateSTFS Bool
cs_containsfile = cs_containspath

-- public bool DeleteFolder(string FolderPath)
cs_DeleteFolder :: (MonadIO m) => Maybe String -> Meth m CreateSTFS Bool
cs_DeleteFolder = undefined
{-
FolderPath = FolderPath.xExtractLegitPath();
if (!containspath(FolderPath))
    return false;
for (int i = 0; i < xFolderDirectory.Count; i++)
{
    if (xFolderDirectory[i].getparentpath() == FolderPath.ToLower())
        DeleteFolder(xFolderDirectory[i].xthispath);
}
for (int i = 0; i < xFileDirectory.Count; i++)
{
    if (xFileDirectory[i].getparentpath() == FolderPath.ToLower())
        xFileDirectory.RemoveAt(i--);
}
return true;
-}

-- public bool DeleteFile(string FilePath)
cs_DeleteFile :: (MonadIO m) => Maybe String -> Meth m CreateSTFS Bool
cs_DeleteFile = undefined
{-
FilePath = FilePath.xExtractLegitPath();
for (int i = 0; i < xFileDirectory.Count; i++)
{
    if (xFileDirectory[i].xthispath == FilePath.ToLower())
        xFileDirectory.RemoveAt(i--);
}
return true;
-}

-- public CFileEntry GetFile(string FilePath)
cs_GetFile :: (MonadIO m) => Maybe String -> Meth m CreateSTFS (Maybe CFileEntry)
cs_GetFile filePath this = fun $ \ret -> do
  fp <- lift $ xExtractLegitPath filePath >>= noNull
  files <- lift $ get (cs_xFileDirectory this) >>= hsList
  forM_ files $ \x -> do
    xptl <- lift $ get (ci_xthispath $ cfi_base x) >>= noNull
    when (map toLower xptl == map toLower fp) $ ret $ Just x
  return Nothing

-- public CFolderEntry GetFolder(string FolderPath)
cs_GetFolder :: (MonadIO m) => Maybe String -> Meth m CreateSTFS (Maybe CFolderEntry)
cs_GetFolder folderPath this = fun $ \ret -> do
  fp <- lift $ xExtractLegitPath folderPath >>= noNull
  when (fp == "") $ get (cs_root this) >>= ret
  folders <- lift $ get (cs_xFolderDirectory this) >>= hsList
  forM_ folders $ \x -> do
    xptl <- lift $ get (ci_xthispath $ cfo_base x) >>= noNull
    when (map toLower xptl == map toLower fp) $ ret $ Just x
  return Nothing

-}

---------
-- STFSPackage.cs
---------

data D_ItemEntry = ItemEntry
  { ie_xPackage         :: F STFSPackage
  , ie_xCreated         :: F Int32
  , ie_xAccessed        :: F Int32
  , ie_xSize            :: F Int32
  , ie_xBlockCount      :: F Word32
  , ie_xStartBlock      :: F Word32
  , ie_xName            :: F Str
  , ie_xEntryID         :: F Word16
  , ie_xFolderPointer   :: F Word16
  , ie_xFlag            :: F Word8
  , ie_xDirectoryOffset :: F Int64
  }
type ItemEntry = Maybe D_ItemEntry

-- TODO rest of ItemEntry

data D_FileEntry = FileEntry
  { fi_base       :: ItemEntry
  , fi_xBlocks    :: F (List BlockRecord)
  , fi_RealStream :: F DJsIO
  }
type FileEntry = Maybe D_FileEntry

-- TODO rest of FileEntry

data D_FolderEntry = FolderEntry
  { fo_base :: ItemEntry
  }
type FolderEntry = Maybe D_FolderEntry

{-

newFolderEntry_1 :: (MonadIO m) => Maybe ItemEntry -> StackTraceT m FolderEntry
newFolderEntry_1 xEntry = FolderEntry <$> undefined xEntry
-- internal FolderEntry(ItemEntry xEntry) : base(xEntry) { }

newFolderEntry_5 :: (MonadIO m) =>
  Maybe String -> Int32 -> Word16 -> Word16 -> Maybe STFSPackage -> StackTraceT m FolderEntry
newFolderEntry_5 nameIn sizeIn xID xFolder xPackageIn =
  FolderEntry <$> undefined nameIn sizeIn True xID xFolder xPackageIn
-- internal FolderEntry(string NameIn, int SizeIn, ushort xID, ushort xFolder, STFSPackage xPackageIn)
--     : base(NameIn, SizeIn, true, xID, xFolder, xPackageIn) { }

-}

-- TODO rest of FolderEntry

data D_STFSLicense = STFSLicense
  { sl_xID    :: F Int64
  , sl_xInt1  :: F Int32
  , sl_xInt2  :: F Int32
  , sl_xfirst :: F Bool
  }
type STFSLicense = Maybe D_STFSLicense

-- TODO rest of STFSLicense

data D_HeaderData = HeaderData
  -- TODO rest of the fields
  { hd_TitleID            :: F Word32
  , hd_Publisher          :: F Str
  , hd_Title_Package      :: F Str
  , hd_ThisType           :: F PackageType
  , hd_Title_Display      :: F Str
  , hd_Description        :: F Str
  , hd_PackageImageBinary :: F (Maybe B.ByteString)
  , hd_ContentImageBinary :: F (Maybe B.ByteString)
  }
type HeaderData = Maybe D_HeaderData

-- TODO rest of HeaderData

data D_STFSPackage = STFSPackage
  { sp_xHeader          :: F HeaderData -- also Header
  , sp_xFileDirectory   :: F (List FileEntry)
  , sp_xFolderDirectory :: F (List FolderEntry)
  , sp_xIO              :: F DJsIO
  , sp_xSTFSStruct      :: F STFSDescriptor -- also STFSStruct
  , sp_xFileBlocks      :: F (List BlockRecord)
  , sp_xActive          :: F Bool
  , sp_xroot            :: F FolderEntry -- also RootDirectory
  }
type STFSPackage = Maybe D_STFSPackage

{-

-- public bool ParseSuccess { get { return xIO != null; } }
sp_ParseSuccess :: (MonadIO m) => Meth m STFSPackage Bool
sp_ParseSuccess = fmap isJust . get . sp_xIO

-- uint xNewEntBlckCnt(uint xCount)
sp_xNewEntBlckCnt :: (MonadIO m) => Word32 -> Meth m STFSPackage Word32
sp_xNewEntBlckCnt = undefined
-- uint x = (uint)(xFileDirectory.Count + xFolderDirectory.Count + xCount);
-- if (x != 0)
--     return (uint)(((x - 1) / 0x40) + 1);
-- return 0;

-- internal uint xCurEntBlckCnt
sp_xCurEntBlckCnt :: (MonadIO m) => Meth m STFSPackage Word32
sp_xCurEntBlckCnt = undefined
-- int x = (xFileDirectory.Count + xFolderDirectory.Count);
-- if (x != 0)
--     return (uint)(((x - 1) / 0x40) + 1);
-- return 0;

-- bool xExtractPayload(string xOutLocale, bool xIncludeSubItems, bool xIncludeHeader)

-- protected internal void AddToLog(string xInput)

-- protected internal bool ParseCheck()
sp_ParseCheck :: (MonadIO m) => Meth m STFSPackage Bool
sp_ParseCheck = undefined
-- if (xIO == null || !xIO.Accessed || !ParseSuccess)
--     throw STFSExcepts.Unsuccessful;
-- return true;

-- internal bool ActiveCheck()
sp_ActiveCheck :: (MonadIO m) => Meth m STFSPackage Bool
sp_ActiveCheck this = sp_ParseCheck this >>= \case
  False -> return False
  True -> get (sp_xActive this) >>= \case
    True -> return False
    False -> do
      sp_xActive this $= True
      return True

-- internal bool GetBlocks(uint xCount, uint xStartBlock, out BlockRecord[] xOutBlocks)

-- internal bool XTakeHash(long xRead, long xWrite, int xSize)
sp_XTakeHash_3 :: (MonadIO m) => Int64 -> Int64 -> Int32 -> Meth m STFSPackage Bool
sp_XTakeHash_3 xRead xWrite xSize this = do
  xIO <- get $ sp_xIO this
  this & sp_XTakeHash_5 xIO xRead xWrite xSize xIO

-- bool XTakeHash(long xRead, long xWrite, int xSize, ref DJsIO io)
sp_XTakeHash_4 :: (MonadIO m) => Int64 -> Int64 -> Int32 -> Maybe DJsIO -> Meth m STFSPackage Bool
sp_XTakeHash_4 xRead xWrite xSize io = sp_XTakeHash_5 io xRead xWrite xSize io

-- bool XTakeHash(ref DJsIO ioin, long xRead, long xWrite, int xSize, ref DJsIO ioout)
sp_XTakeHash_5 :: (MonadIO m) =>
  Maybe DJsIO -> Int64 -> Int64 -> Int32 -> Maybe DJsIO -> Meth m STFSPackage Bool
sp_XTakeHash_5 _ioin _xRead _xWrite _xSize _ioout _this = catchError
  % do
    -- ioin.Position = xRead;
    -- byte[] xData = ioin.ReadBytes(xSize);
    -- ioout.Position = xWrite;
    -- ioout.Write(SHA1Quick.ComputeHash(xData));
    -- return true;
    undefined
  % \_ -> return False

-- bool XVerifyHash(long xRead, int xSize, ref byte[] xHash)

-- bool xEntriesToFile(out DJsIO xFile)

-- internal bool xWriteTo(ref DJsIO xIOIn, BlockRecord[] xBlocks)

-- internal bool xDoAdd(ref DJsIO xIOIn, ref BlockRecord[] xEntAlloc, ref BlockRecord[] xFileAlloc)

-- bool xWriteDescriptor(ref DJsIO io)
sp_xWriteDescriptor :: (SendMessage m, MonadIO m) => Maybe DJsIO -> Meth m STFSPackage Bool
sp_xWriteDescriptor _io _this = do
  lg "Writing new Descriptor"
  -- io.Position = 0x379;
  -- xSTFSStruct.xDirectoryBlockCount = (ushort)xCurEntBlckCnt;
  -- io.Write(xSTFSStruct.GetData());
  -- io.Flush();
  _ <- undefined
  return True

-- internal long GenerateDataOffset(uint xBlock)
sp_GenerateDataOffset :: (MonadIO m) => Word32 -> Meth m STFSPackage Int64
sp_GenerateDataOffset xBlock this =
  get (sp_xSTFSStruct this) >>= noNull >>= sd_GenerateDataOffset xBlock

-- internal long GenerateHashOffset(uint xBlock, TreeLevel xTree)
sp_GenerateHashOffset :: (MonadIO m) => Word32 -> TreeLevel -> Meth m STFSPackage Int64
sp_GenerateHashOffset = undefined
-- long xReturn = xSTFSStruct.GenerateHashOffset(xBlock, xTree);
-- if (xSTFSStruct.ThisType == STFSType.Type1) // Grabs the one up level block record for shifting
--     xReturn += (GetRecord(xBlock, (TreeLevel)((byte)xTree + 1)).Index << 0xC);
-- return xReturn;

-- internal long GenerateBaseOffset(uint xBlock, TreeLevel xTree)
sp_GenerateBaseOffset :: (MonadIO m) => Word32 -> TreeLevel -> Meth m STFSPackage Int64
sp_GenerateBaseOffset = undefined
-- long xReturn = xSTFSStruct.GenerateBaseOffset(xBlock, xTree);
-- if (xSTFSStruct.ThisType == STFSType.Type1) // Grabs the one up level block record for shifting
--     xReturn += (GetRecord(xBlock, (TreeLevel)((byte)xTree + 1)).Index << 0xC);
-- return xReturn;

-- Verified VerifySignature(bool xDev)

-- void SetSamePackage(ref STFSPackage xIn)

-- bool xWriteTables()
sp_xWriteTables :: (SendMessage m, MonadIO m) => Meth m STFSPackage Bool
sp_xWriteTables this = do
  lg "Fixing Level 0"
  ssbc <- get (sp_xSTFSStruct this) >>= noNull >>= get . sd_xBlockCount
  forM_ [0 .. ssbc - 1] $ \i -> do
    join $ sp_XTakeHash_4
      <$> sp_GenerateDataOffset i this
      <*> sp_GenerateHashOffset i L0 this
      <*> return 0x1000
      <*> get (sp_xIO this)
      <*> return this
  {-
  if (STFSStruct.BlockCount > Constants.BlockLevel[0])
  {
      AddToLog("Fixing Level 1");
      // Get level 1 count
      uint ct = (((xSTFSStruct.BlockCount - 1) / Constants.BlockLevel[0]) + 1);
      for (uint i = 0; i < ct; i++)
      {
          XTakeHash(GenerateBaseOffset(i * Constants.BlockLevel[0], TreeLevel.L0),
          GenerateHashOffset(i * Constants.BlockLevel[0], TreeLevel.L1),
              0x1000, ref xIO);
      }
      if (STFSStruct.BlockCount > Constants.BlockLevel[1])
      {
          AddToLog("Fixing Level 2");
          ct = (((xSTFSStruct.BlockCount - 1) / Constants.BlockLevel[1]) + 1);
          for (uint i = 0; i < ct; i++)
          {
              XTakeHash(GenerateHashOffset((i * Constants.BlockLevel[1]), TreeLevel.L1),
                  GenerateHashOffset((i * Constants.BlockLevel[1]), TreeLevel.L2),
                  0x1000, ref xIO);
          }
      }
  }
  xIO.Flush();
  -}
  return True

sp_xWriteHeader :: (SendMessage m, MonadIO m) => Maybe RSAParams -> Meth m STFSPackage Bool
sp_xWriteHeader = undefined
{-
internal bool xWriteHeader(RSAParams xParams)
{
    if (!xParams.Valid)
        throw CryptoExcepts.ParamError;
    // Writes, hashes, and signs data to a temp file
    AddToLog("Writing Header values");
    DJsIO x = new DJsIO(true);
    if (!x.Accessed)
        return false;
    if (!xHeader.Write(ref x))
    {
        x.Close();
        return false;
    }
    xHeader.SetSize(xIO.Length - xSTFSStruct.BaseBlock);
    x.Position = 0x340;
    if (xSTFSStruct.ThisType == STFSType.Type0)
        x.Write((int)0xAD0E);
    else x.Write((int)0x971A);
    // Fills to bottom of header
    x.Position = x.Length;
    x.Write(new byte[(0x8E6 + (xSTFSStruct.BaseBlock - 0xA000))]);
    x.Position = 0x379;
    xWriteDescriptor(ref x);
    AddToLog("Writing Master hash");
    long xLocale = 0;
    if (xSTFSStruct.xBlockCount <= Constants.BlockLevel[0])
        xLocale = GenerateBaseOffset(0, TreeLevel.L0);
    else if (xSTFSStruct.xBlockCount <= Constants.BlockLevel[1])
        xLocale = GenerateBaseOffset(0, TreeLevel.L1);
    else xLocale = GenerateBaseOffset(0, TreeLevel.L2);
    XTakeHash(ref xIO, xLocale, 0x381, 0x1000, ref x);
    AddToLog("Writing Header hash");
    int xSize = 0;
    if (xSTFSStruct.BaseBlock == 0xA000)
        xSize = 0x9CBC;
    else xSize = 0xACBC; // b000
    XTakeHash(0x344, 0x32C, xSize, ref x);
    AddToLog("Signing Header");
    x.Position = 0x22C;
    byte[] xHash = SHA1Quick.ComputeHash(x.ReadBytes(0x118));
    x.Position = 4;
    if (xParams.Type == PackageMagic.CON)
    {
        x.Write(xParams.Certificate);
        x.Write(ScrambleMethods.StockScramble(RSAQuick.SignatureGenerate(xParams.RSAKeys, xHash), true));
    }
    else
    {
        x.Write(ScrambleMethods.DevScramble(RSAQuick.SignatureGenerate(xParams.RSAKeys, xHash)));
        x.Write(new byte[0x128]);
    }
    x.IsBigEndian = true;
    x.Position = 0;
    x.Write(((uint)xParams.Type));
    x.Flush();
    xHeader.xMagic = xParams.Type;
    // Writes header to Package just incase of a emergency close, the Package still attains original strucure
    AddToLog("Writing Header to Package");
    xIO.Position = 0;
    xIO.Write(x.ReadStream());
    xIO.Flush();
    // Flush all the unused blocks to say they are written and now perm wif the new STFS Descriptor
    /*switched0.Clear();
    switched1.Clear();
    switched2 = false;*/
    x.Close();
    VariousFunctions.DeleteFile(x.FileNameLong);
    return true;
}
-}

-- internal string GetFolderNameByID(ushort ID)

-- internal FolderEntry xGetFolder(ushort ID)

-- internal FolderEntry xGetParentFolder(string Path)

-- internal FileEntry xGetFile(string Name, ushort FolderPointer)

-- bool xAddFile(DJsIO xIOIn, string xFileName, ushort Folder)

-- internal int xDeleteEntry(ItemEntry x)

-- int sortpathct(CFolderEntry x1, CFolderEntry x2)
sortpathct :: (MonadIO m) => CFolderEntry -> CFolderEntry -> StackTraceT m Ordering
sortpathct x1 x2 = do
  pc1 <- xPathCount <$> (get (ci_xthispath $ cfo_base x1) >>= noNull)
  pc2 <- xPathCount <$> (get (ci_xthispath $ cfo_base x2) >>= noNull)
  return $ compare pc1 pc2

-- string dlcname()
sp_dlcname :: (MonadIO m) => Meth m STFSPackage (Maybe String)
sp_dlcname _this = catchError
  % do
    -- xIO.Position = 0x32C;
    -- return xIO.ReadBytes(0x14).HexString() + ((byte)(xHeader.TitleID >> 16)).ToString("X2");
    undefined
  % \_ -> return $ Just "00000000000000000000000000000000000000000000"

-}

data SwitchType
  = SwitchType_None
  | SwitchType_Allocate
  | SwitchType_Delete
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Int32 SwitchType

{-

-- void SwitchNWrite(BlockRecord RecIn, SwitchType Change)
sp_SwitchNWrite :: (MonadIO m) => Maybe BlockRecord -> SwitchType -> Meth m STFSPackage ()
sp_SwitchNWrite = undefined

-- BlockRecord GetRecord(uint xBlock, TreeLevel xLevel)

sp_xWriteChain :: (MonadIO m) => Maybe (List BlockRecord) -> Meth m STFSPackage Bool
sp_xWriteChain xRecs this = do
  recs <- noNull xRecs >>= get
  forM_ (zip [0..] recs) $ \(i, rec) -> do
    if (i + 1 < length recs)
      then (br_NextBlock rec $=) =<< get (br_ThisBlock $ recs !! (i + 1))
      else br_NextBlock rec $= constants_STFSEnd
    this & sp_SwitchNWrite (Just rec) SwitchType_Allocate
  return True

-- internal bool xDeleteChain(BlockRecord[] xBlocks)

-- internal BlockRecord[] xAllocateBlocks(uint count, uint xStart)

initSTFSPackage :: (MonadIO m) => StackTraceT m STFSPackage
initSTFSPackage = do
  sp_xHeader          <- var Nothing
  sp_xFileDirectory   <- var [] >>= var . Just
  sp_xFolderDirectory <- var [] >>= var . Just
  sp_xIO              <- var Nothing
  sp_xSTFSStruct      <- var Nothing
  sp_xFileBlocks      <- var Nothing
  sp_xActive          <- var False
  sp_xroot            <- var Nothing
  return STFSPackage{..}

newSTFSPackage_load :: (SendMessage m, MonadIO m) =>
  FilePath -> StackTraceT m STFSPackage
newSTFSPackage_load = undefined

-- public STFSPackage(CreateSTFS xSession, RSAParams xSigning, string xOutPath, LogRecord LogIn)
newSTFSPackage_create :: (SendMessage m, MonadIO m) =>
  CreateSTFS -> RSAParams -> FilePath -> StackTraceT m STFSPackage
newSTFSPackage_create xSession xSigning _xOutPath = do
  this <- initSTFSPackage
  sp_xActive this $= True
  unlessM (get $ rp_xV xSigning) $
    fatal "CryptoExcepts.ParamError"
  whenM ((== 0) <$> (get (cs_xFileDirectory xSession) >>= listCount)) $
    fatal "new Exception()"
  catchError
    % do
      lg "Setting Package variables"
      -- ignoring threading stuff
      (sp_xroot this $=) . Just =<< newFolderEntry_5 (Just "") 0 0xFFFF 0xFFFF (Just this)
      -- ignoring ThematicSkin/Game stuff
      (sp_xHeader this $=) =<< get (cs_HeaderData xSession)
      get (cs_STFSType xSession) >>= \stype -> newSTFSDescriptor_type_uint stype 0 >>= (sp_xSTFSStruct this $=) . Just
      (sp_xIO this $=) . Just =<< newDJsIO_temp True
      (sp_xFileBlocks this $=) . Just =<< var =<< do
        gdc <- cs_GetDirectoryCount xSession
        forM [0 .. fromIntegral gdc - 1] $ \i -> do
          br <- newBlockRecord_empty
          br_ThisBlock br $= i
          return br
      void $ get (sp_xFileBlocks this) >>= \xfb -> this & sp_xWriteChain xfb
      get (sp_xSTFSStruct this) >>= noNull >>= \desc -> do
        (sd_DirectoryBlockCount desc $=) . fromIntegral . length =<< hsList =<< get (sp_xFileBlocks this)
      xCurID <- var (0 :: Word16)
      get (cs_xFolderDirectory xSession) >>= listSort sortpathct
      {-
      foreach (CFolderEntry x in xSession.xFolderDirectory)
      {
          ushort pointer = 0xFFFF;
          if (x.xthispath.xPathCount() > 1)
              pointer = xGetParentFolder(x.Path).EntryID;
          xFolderDirectory.Add(new FolderEntry(x.Name, 0, xCurID++, pointer, this));
          xFolderDirectory[xFolderDirectory.Count - 1].xFixOffset();
      }
      foreach (CFileEntry x in xSession.xFileDirectory)
      {
          ushort pointer = 0xFFFF;
          if (x.xthispath.xPathCount() > 1)
              pointer = xGetParentFolder(x.Path).EntryID;
          xFileDirectory.Add(new FileEntry(x.Name, (int)x.GetLength(), false,xCurID++, pointer, this));
          List<BlockRecord> xAlloc = new List<BlockRecord>();
          for (uint i = 0; i < x.BlockCount(); i++)
          {
              xAlloc.Add(new BlockRecord());
              xAlloc[xAlloc.Count - 1].ThisBlock = xcurblock++;
              /*if (!switched0.Contains((int)(xcurblock / Constants.BlockLevel[0])))
                  switched0.Add((int)(xcurblock / Constants.BlockLevel[0]));
              if (!switched1.Contains((int)(xcurblock / Constants.BlockLevel[1])))
                  switched1.Add((int)(xcurblock / Constants.BlockLevel[1]));*/
          }
          xFileDirectory[xFileDirectory.Count - 1].xBlockCount = (uint)xAlloc.Count;
          xFileDirectory[xFileDirectory.Count - 1].xStartBlock = xAlloc[0].ThisBlock;
          xFileDirectory[xFileDirectory.Count - 1].xPackage = this;
          xFileDirectory[xFileDirectory.Count - 1].xFixOffset();
          xWriteChain(xAlloc.ToArray());
      }
      AddToLog("Writing Entry Table");
      DJsIO xent;
      if (!xEntriesToFile(out xent))
          throw new Exception();
      xWriteTo(ref xent, xFileBlocks);
      xent.Close();
      VariousFunctions.DeleteFile(xent.FileNameLong);
      AddToLog("Writing Files");
      uint curblck = xSession.GetDirectoryCount;
      foreach (CFileEntry z in xSession.xFileDirectory)
      {
          List<BlockRecord> w = new List<BlockRecord>();
          uint ct = z.BlockCount();
          for (uint y = 0; y < ct; y++)
          {
              w.Add(new BlockRecord());
              w[w.Count - 1].ThisBlock = curblck++;
          }
          DJsIO x = null;
          try
          {
              x = new DJsIO(z.FileLocale, DJFileMode.Open, true);
              xWriteTo(ref x, w.ToArray());
          }
          catch { }
          if (x != null)
              x.Dispose();
      }
      xWriteTables();
      xWriteHeader(xSigning);
      xIO.Close();
      VariousFunctions.MoveFile(xIO.FileNameLong, xOutPath);
      xIO = new DJsIO(xOutPath, DJFileMode.Open, true);
      xActive = false;
      -}
      void undefined
    % \err -> do
      sp_xFileDirectory this $= Nothing
      sp_xFolderDirectory this $= Nothing
      _ <- get (sp_xIO this) >>= noNull >>= dj_Dispose False
      throwError err
  return this

-- public bool UpdateHeader(RSAParams xParams)

-- public bool FlushPackage(RSAParams xParams)
sp_FlushPackage :: (SendMessage m, MonadIO m) => Maybe RSAParams -> Meth m STFSPackage Bool
sp_FlushPackage xParams this = sp_ActiveCheck this >>= \case
  False -> return False
  True -> catchError
    % do
      xsucceeded <- andM [this & sp_xWriteTables, this & sp_xWriteHeader xParams]
      sp_xActive this $= False
      return xsucceeded
    % \err -> do
      sp_xActive this $= False
      throwError err

-- public Verified[] VerifyHashTables()

-- public Verified[] VerifyHeader()

-- public bool AddFolder(string FolderPath)

-- public bool ExtractPayload(string xOutLocale, bool xIncludeSubItems, bool xIncludeHeader)

-- public bool ExtractPayload(bool xIncludeSubItems, string xDescription, bool xIncludeHeader)

-- public FileEntry GetFile(string Path)
-- public FileEntry GetFile(string Name, ushort FolderPointer)

-- public FolderEntry GetFolder(ushort FolderID)
-- public FolderEntry GetFolder(string Path)

-- public FileEntry[] GetFiles(ushort FolderPointer)
-- public FileEntry[] GetFiles(string FolderPath)

-- public bool MakeFile(string Name, DJsIO xIOIn, ushort FolderID, AddType xType)
-- public bool MakeFile(string Path, DJsIO xIOIn, AddType xType)

-- public bool MakeBackup(string xOutLocation)

-- public bool RebuildPackage(RSAParams xParams)

-- public string GetCurrentDLCFileName()

-- public string FileNameLong { get { return xIO.FileNameLong; }}
-- public string FileNameShort { get { return xIO.FileNameShort; }}
-- public string FilePath { get { return xIO.FilePath; }}
-- public string FileExtension { get { return xIO.FileExtension; }}

-- public bool CloseIO()
sp_closeIO :: (MonadIO m) => Meth m STFSPackage Bool
sp_closeIO this = get (sp_xActive this) >>= \case
  True -> return False
  False -> do
    sp_xActive this $= True
    get (sp_xIO this) >>= \case
      Nothing -> return ()
      Just xIO -> void $ dj_Close xIO
    return True

-}

---------
-- STFSStuff.cs
---------

data AddType
  = AddType_NoOverWrite
  | AddType_Inject
  | AddType_Replace
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Word8 AddType

data Languages
  = English
  | Japanese
  | German
  | French
  | Spanish
  | Italian
  | Korean
  | Chinese
  | Portuguese
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Word8 Languages

data HashStatus
  = HashStatus_Unused
  | HashStatus_Old
  | HashStatus_New
  | HashStatus_Reused
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Word8 HashStatus

data HashFlag
  = Unallocated
  | AllocatedFree
  | AllocatedInUseOld
  | AllocatedInUseCurrent
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Word8 HashFlag

data STFSType = STFSType_Type0 | STFSType_Type1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Int32 STFSType

data PackageType
  = PT_None -- ^ No package type
  | PT_SavedGame -- ^ Game save
  | PT_MarketPlace -- ^ Market place item
  | PT_Publisher -- ^ Unknown
  | PT_IPTV_DVR -- ^ IPTV DVR
  | PT_Xbox360Title -- ^ Unknown
  | PT_IPTV_PauseBuffer -- ^ IPTV Buffer
  | PT_XNACommunity -- ^ XNA Game?
  | PT_HDDInstalledGame -- ^ Hard drive installed game
  | PT_OriginalXboxGame -- ^ Original game
  | PT_SocialTitle -- ^ Unknown
  | PT_GamesOnDemand -- ^ Games on demand
  | PT_SystemPacks -- ^ Unknown
  | PT_AvatarItem -- ^ Avatar item
  | PT_Profile -- ^ Xbox 360 title
  | PT_GamerPicture -- ^ Xbox profile gamerpictures
  | PT_ThematicSkin -- ^ Xbox theme skin
  | PT_Cache -- ^ System cache?
  | PT_StorageDownload -- ^ Unknown
  | PT_XboxSavedGame -- ^ Unknown
  | PT_XboxDownload -- ^ Unknown
  | PT_GameDemo -- ^ Game Demo
  | PT_Video -- ^ Video
  | PT_GameTitle -- ^ Unknown
  | PT_Installer -- ^ Unknown
  | PT_GameTrailer -- ^ Game trailer
  | PT_Arcade
  | PT_XNA -- ^ XNA Launcher?
  | PT_LicenseStore -- ^ Xbox Licenses
  | PT_Movie -- ^ Marketplace movie
  | PT_TV -- ^ Marketplace TV show
  | PT_MusicVideo -- ^ Marketplace Music Video
  | PT_GameVideo -- ^ Unknown
  | PT_PodcastVideo -- ^ Podcast video
  | PT_ViralVideo -- ^ Unknown
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance CEnum Word32 PackageType where
  enumMapping = \case
    PT_None -> 0
    PT_SavedGame -> 1
    PT_MarketPlace -> 2
    PT_Publisher -> 3
    PT_IPTV_DVR -> 0xFFD
    PT_Xbox360Title -> 0x1000
    PT_IPTV_PauseBuffer -> 0x2000
    PT_XNACommunity -> 0x3000
    PT_HDDInstalledGame -> 0x4000
    PT_OriginalXboxGame -> 0x5000
    PT_SocialTitle -> 0x6000
    PT_GamesOnDemand -> 0x7000
    PT_SystemPacks -> 0x8000
    PT_AvatarItem -> 0x9000
    PT_Profile -> 0x10000
    PT_GamerPicture -> 0x20000
    PT_ThematicSkin -> 0x30000
    PT_Cache -> 0x40000
    PT_StorageDownload -> 0x50000
    PT_XboxSavedGame -> 0x60000
    PT_XboxDownload -> 0x70000
    PT_GameDemo -> 0x80000
    PT_Video -> 0x90000
    PT_GameTitle -> 0xA0000
    PT_Installer -> 0xB0000
    PT_GameTrailer -> 0xC0000
    PT_Arcade -> 0xD0000
    PT_XNA -> 0xE0000
    PT_LicenseStore -> 0xF0000
    PT_Movie -> 0x100000
    PT_TV -> 0x200000
    PT_MusicVideo -> 0x300000
    PT_GameVideo -> 0x400000
    PT_PodcastVideo -> 0x500000
    PT_ViralVideo -> 0x600000

data PackageMagic
  = PackageMagic_CON
  | PackageMagic_LIVE
  | PackageMagic_PIRS
  | PackageMagic_Unknown
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance CEnum Word32 PackageMagic where
  enumMapping = \case
    PackageMagic_CON -> 0x434F4E20
    PackageMagic_LIVE -> 0x4C495645
    PackageMagic_PIRS -> 0x50495253
    PackageMagic_Unknown -> 0xFFFFFFF

data TreeLevel = L0 | L1 | L2 | LT
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Word8 TreeLevel

-- not translating STFSExcepts (just using strings)

data StrongSigned
  = StrongSigned_LIVE
  | StrongSigned_PIRS
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Int32 StrongSigned

data D_RSAParams = RSAParams
  { rp_xC :: F (List Word8)
  , rp_xK :: F RSAParameters
  , rp_xM :: F PackageMagic
  , rp_xV :: F Bool -- also Valid
  }
type RSAParams = Maybe D_RSAParams

-- TODO rest of RSAParams

-- TODO Verified

data ItemType
  = ItemType_Data -- ^ Data block hash
  | ItemType_TableTree0 -- ^ Hash table level 0 hash
  | ItemType_TableTree1 -- ^ Hash table level 1 hash
  | ItemType_Master -- ^ Master hash
  | ItemType_Header -- ^ Header hash
  | ItemType_Signature -- ^ Data Digest RSA Signature
  | ItemType_Certificate -- ^ Certificate Digest RSA Signature
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Word8 ItemType

data TransferLock
  = TransferLock_NoTransfer
  | TransferLock_ProfileAllowOnly
  | TransferLock_DeviceAllowOnly
  | TransferLock_AllowTransfer
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance CEnum Word8 TransferLock

---------
-- Other.cs (only needed parts)
---------

constants_STFSEnd :: Word32
constants_STFSEnd = 0xFFFFFF

constants_BlockLevel0, constants_BlockLevel1 :: Word32
constants_BlockLevel0 = 0xAA
constants_BlockLevel1 = 0x70E4

-- internal static string xExtractLegitPath(this string xin)
xExtractLegitPath_pure :: String -> String
xExtractLegitPath_pure "" = ""
xExtractLegitPath_pure xin = let
  xin1 = map (\case '\\' -> '/'; c -> c) xin
  xin2 = case xin1 of
    '/' : t -> t
    _       -> xin1
  xin3 = case reverse xin2 of
    '/' : t -> reverse t
    _       -> xin2
  in xin3

xExtractLegitPath :: Meth0 Str Str
xExtractLegitPath() this = this & some() & liftHs (pure . Just . xExtractLegitPath_pure)

-- internal static string xExtractName(this string xin)
xExtractName_pure :: String -> String
xExtractName_pure = reverse . takeWhile (/= '/') . reverse

xExtractName :: Meth0 Str Str
xExtractName() this = this & some() & liftHs (pure . Just . xExtractName_pure)

-- internal static int xPathCount(this string xin) { return xin.Split(new char[] { '/' }).Length; }
xPathCount_pure :: String -> Int
xPathCount_pure = length . splitOn "/"

xPathCount :: Meth0 Str Int
xPathCount() this = this & some() & liftHs (pure . xPathCount_pure)

-- public static bool IsValidXboxName(this string x)
isValidXboxName :: (Monad m) => String -> StackTraceT m Bool
isValidXboxName x = do
  let no = map toEnum $ concat
        [ 0 ... 0x20
        -- char 0x20 - 0x2D usable symbols except 0x22 and 0x2A
        , [0x22] -- '"'
        , [0x2A] -- '*'
        , [0x2F] -- '/'
        -- char 0x30 - 0x39 are '0' - '9'
        , [0x3A] -- ':'
        -- char 0x3B and 0x3D are usable
        , [0x3C] -- '<'
        , 0x3E ... 0x40 -- unusable
        -- 0x41 - 0x5A are A - Z, usable symbols up thru 0x60 except 0x5C
        , [0x5C] -- '\'
        -- 0x61 - 0x7A are a - z, 0x7B, 0x7D, and 0x7E are usable
        , [0x7C] -- '|'
        , 0x7F ... 0xFF
        , [0xFF]
        ]
      a ... b = takeWhile (< b) [a..]
  when (x == "" || any (`elem` no) x) $ fatal "STFSExcepts.InvalChars"
  return True

-- public static DateTime FatTimeDT(int xDateTime)
fatTimeDT :: (MonadIO m) => Int32 -> m Time.LocalTime
fatTimeDT xDateTime = let
  -- MT: these are Int16 (short) in C# but that doesn't make sense
  xDate = fromIntegral $ xDateTime `shiftR` 0x10 :: Word16
  xTime = fromIntegral $ xDateTime .&. 0xFFFF :: Word16
  in if xDate == 0 && xTime == 0
    then liftIO $ Time.zonedTimeToLocalTime <$> Time.getZonedTime
    else return $ let
      year   = fromIntegral $ ((xDate .&. 0xFE00) `shiftR` 9) + 0x7BC
      month  = fromIntegral $ (xDate .&. 0x1E0) `shiftR` 5
      day    = fromIntegral $ xDate .&. 0x1F
      hour   = fromIntegral $ (xTime .&. 0xF800) `shiftR` 0xB
      minute = fromIntegral $ (xTime .&. 0x7E0) `shiftR` 5
      sec    = fromIntegral $ (xTime .&. 0x1F) * 2
      in Time.LocalTime (Time.fromGregorian year month day) (Time.TimeOfDay hour minute sec)

class BitConv_GetBytes a where
  bitConv_GetBytes :: (E C a, E C Bool) -> E C (List Word8)

instance BitConv_GetBytes Int16 where
  bitConv_GetBytes = args2 $ \(Var value, Var bigEndian) -> do
    v <- get value
    be <- get bigEndian
    get $ newList $ BL.unpack $ Put.runPut $ (if be then Put.putInt16be else Put.putInt16le) v
instance BitConv_GetBytes Word16 where
  bitConv_GetBytes = args2 $ \(Var value, Var bigEndian) -> do
    v <- get value
    be <- get bigEndian
    get $ newList $ BL.unpack $ Put.runPut $ (if be then Put.putWord16be else Put.putWord16le) v
instance BitConv_GetBytes Int32 where
  bitConv_GetBytes = args2 $ \(Var value, Var bigEndian) -> do
    v <- get value
    be <- get bigEndian
    get $ newList $ BL.unpack $ Put.runPut $ (if be then Put.putInt32be else Put.putInt32le) v
instance BitConv_GetBytes Word32 where
  bitConv_GetBytes = args2 $ \(Var value, Var bigEndian) -> do
    v <- get value
    be <- get bigEndian
    get $ newList $ BL.unpack $ Put.runPut $ (if be then Put.putWord32be else Put.putWord32le) v
instance BitConv_GetBytes Int64 where
  bitConv_GetBytes = args2 $ \(Var value, Var bigEndian) -> do
    v <- get value
    be <- get bigEndian
    get $ newList $ BL.unpack $ Put.runPut $ (if be then Put.putInt64be else Put.putInt64le) v
instance BitConv_GetBytes Word64 where
  bitConv_GetBytes = args2 $ \(Var value, Var bigEndian) -> do
    v <- get value
    be <- get bigEndian
    get $ newList $ BL.unpack $ Put.runPut $ (if be then Put.putWord64be else Put.putWord64le) v
instance BitConv_GetBytes Float where
  bitConv_GetBytes = args2 $ \(Var value, Var bigEndian) -> do
    v <- get value
    be <- get bigEndian
    get $ newList $ BL.unpack $ Put.runPut $ (if be then Put.putFloatbe else Put.putFloatle) v
instance BitConv_GetBytes Double where
  bitConv_GetBytes = args2 $ \(Var value, Var bigEndian) -> do
    v <- get value
    be <- get bigEndian
    get $ newList $ BL.unpack $ Put.runPut $ (if be then Put.putDoublebe else Put.putDoublele) v

---------
-- DJsIO.cs
---------

-- skipping X360.IO.FATXExtensions

dj_BlockCountSTFS :: Meth0 DJsIO Word32
dj_BlockCountSTFS = undefined

dj_BlockRemainderSTFS :: Meth0 DJsIO Int32
dj_BlockRemainderSTFS = undefined

-- skipping X360.IO.SearchExtensions until needed
-- skipping X360.IO.ExtraExtensions until needed

data StringForm
  = StringForm_ASCII
  | StringForm_Unicode
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance CEnum Word8 StringForm where
  enumMapping = \case
    StringForm_ASCII   -> 1
    StringForm_Unicode -> 2

data DJFileMode
  = DJFileMode_Create
  | DJFileMode_Open
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PadLocale
  = PadLocale_Left
  | PadLocale_Right
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- skipping PadType

data StringRead
  = StringRead_Defined
  | StringRead_ToNull
  | StringRead_PrecedingLength

data DataType
  = DataType_None -- ^ No specific type
  | DataType_Memory -- ^ Memory IO
  | DataType_File -- ^ File IO
  | DataType_Drive -- ^ Device IO
  | DataType_Real
  | DataType_MultiFile -- ^ Contains multiple IO's
  | DataType_Other -- ^ Some other IO
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data RealType
  = RealType_None
  | RealType_STFS
  | RealType_FATX
  | RealType_SVOD
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- skipping IOExcepts

data D_DJsIO = DJsIO
  { dj_IsBigEndian :: F Bool
  , dj_xThisData   :: F DataType
  , dj_xFile       :: F Str
  , dj_xStream     :: F Stream
  , dj_txtidx      :: F (List Int32)
  }
type DJsIO = Maybe D_DJsIO

-- public bool Accessed
-- public string FileNameShort
-- public string FilePath
-- public string FileNameLong
-- public string FileExtension
-- public DataType IOType

-- protected void XSetStrings()
-- void XSetStream(DJFileMode xftype)

-- public DJsIO(string xFileIn, DJFileMode xType, bool BigEndian)

-- skipping
--   public DJsIO(DJFileMode xType, string xTitle, string xFilter, bool BigEndian)
--   xImp1, xImp2
--   public DJsIO(ref SaveFileDialog xSFDin, bool BigEndian)
--   public DJsIO(ref OpenFileDialog xOFD, bool BigEndian)

-- public DJsIO(Stream ImportGeneric, bool BigEndian)
-- public DJsIO(Stream ImportGeneric, bool BigEndian)
-- public DJsIO(long ArraySize, bool BigEndian)
-- public DJsIO(bool BigEndian)
-- protected DJsIO() { }

-- public virtual byte[] ReadBytes(int xSize)
dj_ReadBytes :: Meth1 DJsIO Int32 (List Word8)
dj_ReadBytes = undefined

-- internal byte[] unbufferedread(int xSize)
-- public short ReadInt16()
-- public short ReadInt16(bool BigEndian)
-- public uint ReadUInt24()
-- public uint ReadUInt24(bool BigEndian)
-- public ulong ReadUInt40()
-- public ulong ReadUInt40(bool BigEndian)
-- public ulong ReadUInt48()
-- public ulong ReadUInt48(bool BigEndian)
-- public ulong ReadUInt56()
-- public ulong ReadUInt56(bool BigEndian)

-- public int ReadInt32()
dj_ReadInt32 :: Meth0 DJsIO Int32
dj_ReadInt32 = undefined

-- public int ReadInt32(bool BigEndian)

-- public long ReadInt64()
-- public long ReadInt64(bool BigEndian)

-- public byte ReadByte()
dj_ReadByte :: Meth0 DJsIO Word8
dj_ReadByte = undefined

-- public sbyte ReadSByte()
-- public float ReadSingle()
-- public float ReadSingle(bool BigEndian)
-- public double ReadDouble()
-- public double ReadDouble(bool BigEndian)
-- public ushort ReadUInt16()
-- public ushort ReadUInt16(bool BigEndian)

-- public uint ReadUInt32()
dj_ReadUInt32 :: Meth0 DJsIO Word32
dj_ReadUInt32 = undefined

-- public uint ReadUInt32(bool BigEndian)
-- public ulong ReadUInt64()
-- public ulong ReadUInt64(bool BigEndian)
-- public bool ReadBool()
-- public string ReadLine()
-- public string ReadLine(byte BreakType)
-- public string ReadLine(StringForm xType)
-- public string ReadLine(StringForm xType, bool BigEndian)
-- public string ReadLine(StringForm xType, short BreakIndicator, bool BigEndian)
-- public string ReadString(StringForm xStringType)
-- public string ReadString(StringForm xStringType, bool BigEndian)
-- public string ReadString(StringForm xStringType, int xStringSize)
-- public string ReadString(StringForm xStringType, int xStringSize, bool BigEndian)
-- public string ReadString(StringForm xStringType, int xStringSize, StringRead xRead)
-- public string ReadString(StringForm xStringType, int xStringSize, StringRead xRead, bool BigEndian)
-- public string ReadHexString(int xLength)
-- public DateTime ReadFileTime()
-- public virtual byte[] ReadStream()

-- public virtual void Write(byte[] xIn)
-- internal void unbufferedwrite(byte[] xIn)
-- public void Write(short xIn)
-- public void Write(short xIn, bool BigEndian)
-- public void Write(int xIn)
-- public void Write(int xIn, bool BigEndian)
-- public void Write(long xIn)
-- public void Write(long xIn, bool BigEndian)
-- public void Write(ushort xIn)
-- public void Write(ushort xIn, bool BigEndian)
-- public void Write(uint xIn)
-- public void Write(uint xIn, bool BigEndian)
-- public void Write(ulong xIn)
-- public void Write(ulong xIn, bool BigEndian)
-- public void Write(float xIn)
-- public void Write(float xIn, bool BigEndian)
-- public void Write(double xIn)
-- public void Write(double xIn, bool BigEndian)
-- public void Write(sbyte xIn) { Write((byte)xIn); }
-- public void Write(bool xIn)
-- public void WriteUInt24(uint xIn)
-- public void WriteUInt24(uint xIn, bool BigEndian)
-- public void WriteUInt40(ulong xIn)
-- public void WriteUInt40(ulong xIn, bool BigEndian)
-- public void WriteUInt48(ulong xIn)
-- public void WriteUInt48(ulong xIn, bool BigEndian)
-- public void WriteUInt56(ulong xIn)
-- public void WriteUInt56(ulong xIn, bool BigEndian)
-- public void Write(string xIn)
-- public void Write(string xIn, StringForm xType)
-- public void Write(string xIn, StringForm xType, int xDesiredSize, PadLocale xPadLocale, char PadChar)
-- public void WriteHexString(string xIn)
-- public void Write(byte xIn) { Write(new byte[]{ xIn }); }
-- public void WriteFileTime(DateTime xIn)
-- public virtual void Flush()

-- public virtual long Length
dj_Length :: D_DJsIO -> F Int64
dj_Length = undefined

-- public string LengthFriendly
-- public virtual long Position
dj_Position :: D_DJsIO -> F Int64
dj_Position = undefined
-- public virtual bool Close()
-- public virtual bool Dispose()
-- internal bool Dispose(bool DeleteFile)
-- public virtual bool OpenAgain()
-- public virtual Stream GrabStream()
-- public virtual bool SetLength(long xLen)
-- protected virtual bool GetAccessed()

-- skipping DriveIO

-- do we need STFSStreamIO ?

-- skipping FATXStreamIO

-- don't think we need MultiFileIO

-- probably don't need DiskGeometry, DeviceType, Drive

---------
-- UNSORTED/POLYFILL
---------

data D_Stream
type Stream = Maybe D_Stream

data D_RSAParameters
type RSAParameters = Maybe D_RSAParameters
