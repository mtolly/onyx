{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
module Audio.Types where

import           Control.Monad (ap)

data Audio t a
  = Silence Int t
  | Input a
  | Mix            [Audio t a]
  | Merge          [Audio t a]
  | Concatenate    [Audio t a]
  | Gain Double    (Audio t a)
  | Take Edge t    (Audio t a)
  | Drop Edge t    (Audio t a)
  | Fade Edge t    (Audio t a)
  | Pad  Edge t    (Audio t a)
  | Resample       (Audio t a)
  | Channels [Int] (Audio t a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Applicative (Audio t) where
  pure = Input
  (<*>) = ap

instance Monad (Audio t) where
  return = Input
  x >>= f = let
    join_ = \case
      Silence c t      -> Silence c t
      Input       sub  -> sub
      Mix         auds -> Mix $ map join_ auds
      Merge       auds -> Merge $ map join_ auds
      Concatenate auds -> Concatenate $ map join_ auds
      Gain      d aud  -> Gain d $ join_ aud
      Take    e t aud  -> Take e t $ join_ aud
      Drop    e t aud  -> Drop e t $ join_ aud
      Fade    e t aud  -> Fade e t $ join_ aud
      Pad     e t aud  -> Pad e t $ join_ aud
      Resample    aud  -> Resample $ join_ aud
      Channels cs aud  -> Channels cs $ join_ aud
    in join_ $ fmap f x

data Edge = Start | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

mapTime :: (t -> u) -> Audio t a -> Audio u a
mapTime f aud = case aud of
  Silence c t     -> Silence c $ f t
  Input   x       -> Input x
  Mix         xs  -> Mix         $ map (mapTime f) xs
  Merge       xs  -> Merge       $ map (mapTime f) xs
  Concatenate xs  -> Concatenate $ map (mapTime f) xs
  Gain g x        -> Gain g $ mapTime f x
  Take e t x      -> Take e (f t) $ mapTime f x
  Drop e t x      -> Drop e (f t) $ mapTime f x
  Fade e t x      -> Fade e (f t) $ mapTime f x
  Pad  e t x      -> Pad  e (f t) $ mapTime f x
  Resample x      -> Resample     $ mapTime f x
  Channels cs x   -> Channels cs  $ mapTime f x
