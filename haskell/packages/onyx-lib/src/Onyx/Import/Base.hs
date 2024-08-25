{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
module Onyx.Import.Base where

import           Codec.Picture
import           Control.Applicative              ((<|>))
import           Control.Concurrent.Async         (async, wait)
import           Control.Monad                    (forM)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State        (evalStateT, gets, modify)
import           Data.Char                        (toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (Default, def)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Data.Tuple.Extra                 (fst3, snd3, thd3)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Audio                       (audioIO)
import           Onyx.Codec.JSON                  (toJSON, yamlEncodeFile)
import           Onyx.MIDI.Track.Drums
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.ProGuitar        as PG
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (Readable, saveReadable)
import qualified Sound.MIDI.Util                  as U
import           System.Directory                 (createDirectoryIfMissing)
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   (</>))

data SoftContents
  = SoftReadable Readable
  | SoftAudio (CA.AudioSource (ResourceT IO) Float)
  | SoftImage (Image PixelRGB8)
  | SoftChart (F.Song (F.OnyxFile U.Beats))

instance Show SoftContents where
  show (SoftReadable r) = "SoftReadable{" <> show r <> "}"
  show (SoftAudio _)    = "SoftAudio{}" -- TODO rate/channels/duration
  show (SoftImage _)    = "SoftImage{}" -- TODO dimensions
  show (SoftChart _)    = "SoftChart{}" -- probably leave empty

data SoftFile = SoftFile FilePath SoftContents
  deriving (Show)

data ImportLevel
  = ImportFull  -- ^ Actually load the whole chart, parse MIDIs, etc.
  | ImportQuick -- ^ Only load basic metadata, as fast as possible
  deriving (Eq)

type Import m = ImportLevel -> StackTraceT m (SongYaml SoftFile)

saveImport :: FilePath -> SongYaml SoftFile -> IO (SongYaml FilePath)
saveImport dout yaml = do
  yaml2 <- flip evalStateT [] $ forM yaml $ \(SoftFile newName contents) -> do
    gets (lookup newName) >>= \case
      Just prev -> return prev
      Nothing -> do
        newAsync <- liftIO $ async $ do
          let newNameFull = dout </> newName
          createDirectoryIfMissing True $ takeDirectory newNameFull
          case contents of
            SoftReadable r -> saveReadable r newNameFull
            SoftImage img -> case map toLower $ takeExtension newName of
              ".png" -> writePng newNameFull img
              ext    -> error $ "saveImport: unhandled image extension " <> show ext
            SoftChart song -> F.saveMIDIUtf8 newNameFull song
            SoftAudio aud -> audioIO Nothing aud newNameFull
          return newName
        modify ((newName, newAsync) :)
        return newAsync
  yaml3 <- traverse wait yaml2
  yamlEncodeFile (dout </> "song.yml") $ toJSON yaml3
  return yaml3

determine2xBass :: T.Text -> (T.Text, Bool)
determine2xBass s = let
  stripInfix inf str = case T.breakOn inf str of
    (x, y) -> (x <>) <$> T.stripPrefix inf y
  in case stripInfix " (2x Bass Pedal)" s <|> stripInfix " (2X Bass Pedal)" s of
    Nothing -> (s , False)
    Just s' -> (s', True )

def' :: (Default (f FilePath)) => f FilePath
def' = def

bothFirstSecond :: (NNC.C t, Ord a) => RTB.T t a -> RTB.T t a -> (RTB.T t a, RTB.T t a, RTB.T t a)
bothFirstSecond t1 t2 = let
  result = fmap eachInstant $ RTB.collectCoincident $ RTB.merge (fmap Left t1) (fmap Right t2)
  eachInstant es = let
    xs = Set.fromList $ lefts es
    ys = Set.fromList $ rights es
    in  ( Set.toList $ Set.intersection xs ys
        , Set.toList $ Set.difference xs ys
        , Set.toList $ Set.difference ys xs
        )
  in
    ( RTB.flatten $ fmap fst3 result
    , RTB.flatten $ fmap snd3 result
    , RTB.flatten $ fmap thd3 result
    )

detectExtProBass :: F.FixedFile t -> PG.GtrBase
detectExtProBass trks = let
  strs = do
    trk <- [F.fixedPartRealBass trks, F.fixedPartRealBass22 trks]
    diff <- toList trk.pgDifficulties
    (str, _) <- toList diff.pgNotes >>= toList
    return str
  in if elem PG.S1 strs
    then PG.GtrCustom [28, 33, 38, 43, 47, 52] -- bass with 2 high gtr strings
    else if elem PG.S2 strs
      then PG.GtrCustom [28, 33, 38, 43, 47] -- bass with 1 high gtr string
      else PG.Bass4

emptyChart :: (Monoid a) => F.Song a
emptyChart = F.Song
  { F.s_tempos = U.makeTempoMap RTB.empty
  , F.s_signatures = U.makeMeasureMap U.Ignore RTB.empty
  , F.s_tracks = mempty
  }

-- | Discards drum accents/ghosts unless [ENABLE_CHART_DYNAMICS] is present.
checkEnableDynamics
  :: F.Song (F.FixedFile U.Beats)
  -> F.Song (F.FixedFile U.Beats)
checkEnableDynamics (F.Song tmap mmap ps) = let
  checkTrack trk = if RTB.null trk.drumEnableDynamics
    then trk { drumDifficulties = noDynamics <$> trk.drumDifficulties }
    else trk
  noDynamics dd = dd
    { drumGems = (\(gem, _) -> (gem, VelocityNormal)) <$> dd.drumGems
    }
  in F.Song tmap mmap ps
    { F.fixedPartDrums       = checkTrack $ F.fixedPartDrums       ps
    , F.fixedPartDrums2x     = checkTrack $ F.fixedPartDrums2x     ps
    , F.fixedPartRealDrumsPS = checkTrack $ F.fixedPartRealDrumsPS ps
    }
