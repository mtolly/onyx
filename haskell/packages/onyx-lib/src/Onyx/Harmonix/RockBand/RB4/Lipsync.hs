{-
Ported from LibForge
https://github.com/maxton/LibForge
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Harmonix.RockBand.RB4.Lipsync
( LipsyncPS4(..), getLipsyncPS4, fromLipsyncPS4
) where

import           Control.Monad                       (forM)
import           Data.Binary.Get
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as BL
import           Data.Int
import           Data.List.Split                     (splitOn)
import           Data.Maybe                          (catMaybes)
import           Data.Word
import           Onyx.Harmonix.RockBand.Milo.Lipsync
import           Onyx.Harmonix.RockBand.RB4.RBSong   (getArray, getString)
import           Onyx.MIDI.Track.Lipsync             (VisemeEvent (..))
import           Onyx.Util.Binary                    (runGetM)

data LipsyncPS4 = LipsyncPS4
  { version      :: Int32
  , subtype      :: Int32
  , frameRate    :: Float
  , visemes      :: [B.ByteString]
  , players      :: [B.ByteString]
  , frameIndices :: [Word32]
  , frameData    :: B.ByteString
  } deriving (Show)

getLipsyncPS4 :: Get LipsyncPS4
getLipsyncPS4 = do
  version <- getInt32le
  subtype <- getInt32le
  frameRate <- getFloatle
  visemes <- getArray getString
  players <- getArray getString
  frameIndices <- getArray getWord32le
  frameData <- let
    len = case reverse frameIndices of
      []    -> 0
      i : _ -> fromIntegral i
    in getByteString len
  return LipsyncPS4{..}

splitFrameGroups :: LipsyncPS4 -> [B.ByteString]
splitFrameGroups lip = do
  (this, next) <- zip (0 : lip.frameIndices) $ lip.frameIndices
  return $ B.take (fromIntegral $ next - this) $ B.drop (fromIntegral this) lip.frameData

splitFrameGroup :: Get [Keyframe]
splitFrameGroup = let
  go prev = isEmpty >>= \case
    True -> return $ reverse prev
    False -> getWord8 >>= \case
      0xFF -> go $ Nothing : prev
      k    -> do
        let visemeKey = fromIntegral k
            visemeGraph = ()
        visemeWeight <- getWord8
        go $ Just VisemeEvent{..} : prev
  in map (Keyframe . catMaybes) . splitOn [Nothing] <$> go []

fromLipsyncPS4 :: (MonadFail m) => LipsyncPS4 -> m [(B.ByteString, Lipsync)]
fromLipsyncPS4 lip = do
  let groups = splitFrameGroups lip
  results <- forM groups $ \frame -> do
    runGetM splitFrameGroup $ BL.fromStrict frame
  return $ do
    (i, player) <- zip [0..] lip.players
    return (player, emptyLipsync
      { lipsyncVisemes = lip.visemes
      , lipsyncKeyframes = map
        (\group -> case drop i group of
          []     -> Keyframe []
          kf : _ -> kf
        )
        results
      })
