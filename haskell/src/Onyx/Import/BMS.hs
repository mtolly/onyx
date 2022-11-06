{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Import.BMS where

import           Control.Monad                    (guard)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Audio
import           Onyx.Beatmania.BMS
import           Onyx.DTXMania.DTX
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Edge (..), Key (..),
                                                   blipEdgesRB_,
                                                   joinEdgesSimple)
import           Onyx.MIDI.Track.File             (FlexPartName (..))
import qualified Onyx.MIDI.Track.File             as RBFile
import           Onyx.MIDI.Track.ProKeys
import           Onyx.Project
import           Onyx.StackTrace

joinLongNotes :: (NNC.C t) =>
  RTB.T t (BMKey, Chip, Bool) -> RTB.T t (BMKey, Chip, t)
joinLongNotes
  = fmap (\(chip, key, t) -> (key, chip, t))
  . joinEdgesSimple
  . fmap (\(key, chip, b) -> if b then EdgeOn chip key else EdgeOff key)

importBMS :: (SendMessage m, MonadIO m) => FilePath -> Import m
importBMS bmsPath level = do
  bms <- stackIO $ readBMSLines <$> loadDTXLines bmsPath
  let simpleAudio f chips = if RTB.null chips
        then return Nothing
        else do
          src <- getBMSAudio chips bmsPath bms
          return $ Just (f, src, CA.channels src)
  audioSong <- simpleAudio "song.wav" $ bms_BGM bms
  audioPlayer1 <- simpleAudio "player1.wav" $ RTB.merge
    (snd <$> bms_Player1 bms)
    (RTB.mapMaybe (\(_key, chip, b) -> guard b >> Just chip) $ bms_Player1Long bms)
  audioPlayer2 <- simpleAudio "player2.wav" $ RTB.merge
    (snd <$> bms_Player2 bms)
    (RTB.mapMaybe (\(_key, chip, b) -> guard b >> Just chip) $ bms_Player2Long bms)
  let audioExpr (aud, _, _) = PlanAudio
        { _planExpr = Input $ Named $ T.pack aud
        , _planPans = []
        , _planVols = []
        }
      midi = case level of
        ImportQuick -> emptyChart
        ImportFull -> RBFile.Song (bms_TempoMap bms) (bms_MeasureMap bms) mempty
          { RBFile.onyxParts = Map.fromList $ do
            (fpart, chips, chipsLong) <-
              [ (FlexExtra "player1", bms_Player1 bms, bms_Player1Long bms)
              , (FlexExtra "player2", bms_Player2 bms, bms_Player2Long bms)
              ]
            guard $ not $ RTB.null chips && RTB.null chipsLong
            let opart = mempty
                  { RBFile.onyxPartRealKeysX = mempty
                    { pkLanes = RTB.singleton 0 RangeC
                    , pkNotes = let
                      lookupKey = \case
                        BMScratch -> RedYellow E
                        BMKey1    -> RedYellow F
                        BMKey2    -> RedYellow Fs
                        BMKey3    -> RedYellow G
                        BMKey4    -> RedYellow Gs
                        BMKey5    -> RedYellow A
                        BMKey6    -> RedYellow As
                        BMKey7    -> RedYellow B
                      short = flip fmap chips
                        $ \(key, _) -> (lookupKey key, Nothing)
                      long = flip fmap (joinLongNotes chipsLong)
                        $ \(key, _, len) -> (lookupKey key, Just len)
                      in blipEdgesRB_ $ RTB.merge short long
                    }
                  }
            return (fpart, opart)
          }
  return SongYaml
    { _metadata = def'
      { _title        = bms_TITLE bms
      , _artist       = bms_ARTIST bms
      , _genre        = bms_GENRE bms
      , _fileAlbumArt = Nothing
      }
    , _global = def'
      { _backgroundVideo = Nothing
      , _fileBackgroundImage = Nothing
      , _fileMidi = SoftFile "notes.mid" $ SoftChart midi
      , _fileSongAnim = Nothing
      }
    , _audio = HM.fromList $ do
      (f, src, chans) <- catMaybes [audioSong, audioPlayer1, audioPlayer2]
      return $ (T.pack f ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just $ SoftFile f $ SoftAudio src
        , _rate = Nothing
        , _channels = chans
        }
    , _jammit = HM.empty
    , _plans = HM.singleton "bms" Plan
      { _song         = fmap audioExpr audioSong
      , _countin      = Countin []
      , _planParts    = Parts $ HM.fromList $ catMaybes
        [ (FlexExtra "player1" ,) . PartSingle . audioExpr <$> audioPlayer1
        , (FlexExtra "player2" ,) . PartSingle . audioExpr <$> audioPlayer2
        ]
      , _crowd = Nothing
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      }
    , _targets = HM.empty
    , _parts = Parts $ HM.fromList $ do
      (fpart, chips) <-
        [ (FlexExtra "player1", bms_Player1 bms)
        , (FlexExtra "player2", bms_Player2 bms)
        ]
      guard $ not $ RTB.null chips
      return $ (fpart,) def
        { partProKeys = Just PartProKeys
          { pkDifficulty  = Tier 1
          , pkFixFreeform = True
          }
        }
    }
