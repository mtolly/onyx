{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Amplitude.File where

import           Control.Monad                    (forM, forM_)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (get)
import           Data.Char                        (isDigit)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
import qualified Data.Text                        as T
import           Onyx.Amplitude.Track
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.File             (ParseFile (..), fileTrack)
import qualified Sound.MIDI.Util                  as U
import qualified Text.ParserCombinators.ReadP     as P

data AmplitudeFile t = AmplitudeFile
  { ampTracks :: Map.Map Int (AmplitudeTrack t)
  -- todo: BG_CLICK, WORLD
  } deriving (Eq, Show)

data AmplitudeTrack t
  = Catch Instrument T.Text (CatchTrack t)
  | Freestyle (FreestyleTrack t)
  deriving (Eq, Show)

-- | Always empty it seems
data FreestyleTrack t = FreestyleTrack
  deriving (Eq, Show)

instance TraverseTrack AmplitudeTrack where
  traverseTrack fn (Freestyle       trk) = Freestyle <$> traverseTrack fn trk
  traverseTrack fn (Catch inst name trk) = Catch inst name <$> traverseTrack fn trk

instance TraverseTrack FreestyleTrack where
  traverseTrack _ FreestyleTrack = pure FreestyleTrack

instance ParseTrack FreestyleTrack where
  parseTrack = return FreestyleTrack

data Instrument
  = Drums
  | Bass
  | Synth
  | Vocal
  | Guitar
  deriving (Eq, Ord, Show, Enum, Bounded)

instance TraverseTrack AmplitudeFile where
  traverseTrack fn (AmplitudeFile trks) = AmplitudeFile <$> traverse (traverseTrack fn) trks

instance ParseFile AmplitudeFile where
  parseFile = do
    ampTracks <- ampTracks =. Codec
      { codecIn = fmap Map.fromList $ do
        trks <- lift get
        fmap catMaybes $ forM trks $ \trk -> let
          trackName = fromMaybe "" $ U.trackName trk
          parseName = do
            _ <- P.char 'T'
            n <- fmap (read :: String -> Int) $ P.munch1 isDigit
            P.choice
              [ do
                _ <- P.string " FREESTYLE"
                P.eof
                return $ Left n
              , do
                _ <- P.string " CATCH:"
                inst <- P.choice
                  [ P.char 'D' >> return Drums
                  , P.char 'B' >> return Bass
                  , P.char 'S' >> return Synth
                  , P.char 'V' >> return Vocal
                  , P.char 'G' >> return Guitar
                  ]
                _ <- P.char ':'
                name <- T.pack <$> P.munch (const True)
                let name' = "T" <> T.pack (show n) <> "-" <> T.strip name
                return $ Right (n, inst, name')
              ]
          in case P.readP_to_S parseName trackName of
            [(fn, "")] -> Just <$> case fn of
              Left  n               -> (n,) . Freestyle       <$> codecIn (fileTrack $ pure $ T.pack trackName)
              Right (n, inst, name) -> (n,) . Catch inst name <$> codecIn (fileTrack $ pure $ T.pack trackName)
            _ -> return Nothing
      , codecOut = fmapArg $ \parts -> forM_ (Map.toAscList parts) $ \(ix, trk) -> undefined ix trk
      }
    return AmplitudeFile{..}
