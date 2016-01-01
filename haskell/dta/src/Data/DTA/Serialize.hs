{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable, OverloadedStrings, FlexibleInstances #-}
module Data.DTA.Serialize where

import Data.DTA.Base
import Control.Applicative (liftA2)
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Traversable as T

unserialize :: (FromChunks a) => DTA String -> Either String a
unserialize (DTA _ (Tree _ cs)) = fromChunks cs

serialize :: (ToChunks a) => a -> DTA String
serialize = DTA 0 . Tree 0 . toChunks

-- | Values which are stored as one or many chunks. Scalar types become one
-- chunk, while lists and record types can make more.
class ToChunks a where
  toChunks :: a -> [Chunk String]
  listToChunks :: [a] -> [Chunk String]
  listToChunks = concatMap toChunks

-- | Values which can be read from one or many chunks.
class FromChunks a where
  fromChunks :: [Chunk String] -> Either String a
  listFromChunks :: [Chunk String] -> Either String [a]
  listFromChunks = mapM $ \x -> fromChunks [x]

instance ToChunks (DTA String) where
  toChunks = treeChunks . topTree

instance FromChunks (DTA String) where
  fromChunks = Right . DTA 0 . Tree 0

instance ToChunks (Chunk String) where
  toChunks x = [x]

instance FromChunks (Chunk String) where
  fromChunks [x] = Right x
  fromChunks cs = Left $ "Expected 1 chunk, got: " ++ show cs

-- | A key-value structure which is stored as a sequence of @(tag rest...)@
-- chunks.
newtype Dict a = Dict { fromDict :: Map.Map String a }
  deriving (Eq, Ord, Show, Read, Functor, F.Foldable, T.Traversable)

instance (ToChunks a) => ToChunks (Dict a) where
  toChunks = makeDict . fmap toChunks

instance (FromChunks a) => FromChunks (Dict a) where
  fromChunks cs = getDict cs >>= T.mapM fromChunks

getDict :: [Chunk String] -> Either String (Dict [Chunk String])
getDict cs = let
  toPair c = case c of
    Parens (Tree _ (Key k : rest)) -> Right (k, rest)
    _ -> Left $ "Expected (tag rest...), got: " ++ show c
  in fmap (Dict . Map.fromList) $ mapM toPair cs

makeDict :: Dict [Chunk String] -> [Chunk String]
makeDict (Dict m) =
  [ Parens $ Tree 0 $ Key k : v | (k, v) <- Map.toList m ]

dictLookup :: String -> Dict v -> Either String v
dictLookup k (Dict m) = case Map.lookup k m of
  Nothing -> Left $ "Couldn't find key " ++ show k
  Just v  -> Right v

-- | A value which is DTA-stored as a parenthesized subtree around the normal
-- representation.
newtype InParens a = InParens { fromInParens :: a }
  deriving (Eq, Ord, Show, Read)

instance (ToChunks a) => ToChunks (InParens a) where
  toChunks (InParens xs) = [Parens $ Tree 0 $ toChunks xs]

instance (FromChunks a) => FromChunks (InParens a) where
  fromChunks [Parens (Tree _ cs)] = fmap InParens $ fromChunks cs
  fromChunks cs = Left $ "Couldn't read as InParens: " ++ show cs

-- | An integer 0 or 1.
instance ToChunks Bool where
  toChunks True = [Int 1]
  toChunks False = [Int 0]

-- | An integer 0 or 1, or keyword TRUE or FALSE.
instance FromChunks Bool where
  fromChunks [Int 1      ] = Right True
  fromChunks [Int 0      ] = Right False
  fromChunks [Key "TRUE" ] = Right True
  fromChunks [Key "FALSE"] = Right False
  fromChunks cs = Left $ "Couldn't read as Bool: " ++ show cs

instance ToChunks Integer where
  toChunks i = [Int $ fromIntegral i]

instance FromChunks Integer where
  fromChunks [Int i] = Right $ fromIntegral i
  fromChunks cs = Left $ "Couldn't read as Integer: " ++ show cs

instance ToChunks Float where
  toChunks f = [Float f]

instance FromChunks Float where
  fromChunks [Int   i] = Right $ fromIntegral i
  fromChunks [Float f] = Right f
  fromChunks cs = Left $ "Couldn't read as Float: " ++ show cs

-- | A String, not a 'Key'.
instance ToChunks Char where
  toChunks c = [String [c]]
  listToChunks s = [String s]

-- | A String, not a 'Key'.
instance FromChunks Char where
  fromChunks [String [c]] = Right c
  fromChunks cs = Left $ "Couldn't read as Char: " ++ show cs
  listFromChunks [String s] = Right s
  listFromChunks cs = Left $ "Couldn't read as ByteString: " ++ show cs

-- | Stored as two chunks. Each subtype should be a single chunk.
instance (ToChunks a, ToChunks b) => ToChunks (a, b) where
  toChunks (x, y) = toChunks x ++ toChunks y

-- | Stored as two chunks. Each subtype should be a single chunk.
instance (FromChunks a, FromChunks b) => FromChunks (a, b) where
  fromChunks [x, y] = liftA2 (,) (fromChunks [x]) (fromChunks [y])
  fromChunks cs = Left $ "Couldn't read as pair: " ++ show cs

-- | Represents 'Nothing' with an empty chunk list.
instance (ToChunks a) => ToChunks (Maybe a) where
  toChunks Nothing = []
  toChunks (Just x) = toChunks x

-- | Represents 'Nothing' with an empty chunk list.
instance (FromChunks a) => FromChunks (Maybe a) where
  fromChunks [] = Right Nothing
  fromChunks cs = fmap Just $ fromChunks cs

-- | Each value is stored as one chunk in the list.
instance (ToChunks a) => ToChunks [a] where
  toChunks = listToChunks

-- | Each value is stored as one chunk in the list.
instance (FromChunks a) => FromChunks [a] where
  fromChunks = listFromChunks

-- | Stored as a 'Key', unlike the 'String' instance which is a String.
newtype Keyword = Keyword { fromKeyword :: String }
  deriving (Eq, Ord, Show, Read)

instance ToChunks Keyword where
  toChunks (Keyword k) = [Key k]

instance FromChunks Keyword where
  fromChunks [Key k] = Right $ Keyword k
  fromChunks cs = Left $ "Couldn't read as Keyword: " ++ show cs

-- | Uses whichever 'toChunks' is applicable. Does not tag which type is used.
instance (ToChunks a, ToChunks b) => ToChunks (Either a b) where
  toChunks = either toChunks toChunks

-- | First tries to read the 'Left' type, then 'Right' if that fails.
instance (FromChunks a, FromChunks b) => FromChunks (Either a b) where
  fromChunks cs = case (fromChunks cs, fromChunks cs) of
    (Right l, _      ) -> Right $ Left l
    (_      , Right r) -> Right $ Right r
    (Left  _, Left  _) -> Left $ "Couldn't read as Either: " ++ show cs
