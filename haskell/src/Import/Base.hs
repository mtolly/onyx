{-# LANGUAGE OverloadedStrings #-}
module Import.Base where

import           Audio                            (audioIO)
import           Codec.Picture
import           Config
import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM)
import           Control.Monad.Codec.Onyx.JSON    (toJSON, yamlEncodeFile)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Char                        (toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (Default, def)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Set                         as Set
import           Data.SimpleHandle                (Readable, saveReadable)
import qualified Data.Text                        as T
import           Data.Tuple.Extra                 (fst3, snd3, thd3)
import qualified Numeric.NonNegative.Class        as NNC
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.ProGuitar         as PG
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension, (</>))

data SoftContents
  = SoftReadable Readable
  | SoftAudio (CA.AudioSource (ResourceT IO) Float)
  | SoftImage (Image PixelRGB8)
  | SoftChart (RBFile.Song (RBFile.OnyxFile U.Beats))

data SoftFile = SoftFile FilePath SoftContents

type Import  m = StackTraceT m (SongYaml SoftFile)
type Imports m = StackTraceT m [SongYaml SoftFile]

saveImport :: FilePath -> SongYaml SoftFile -> IO (SongYaml FilePath)
saveImport dout yaml = do
  yaml' <- forM yaml $ \(SoftFile newName contents) -> do
    let newNameFull = dout </> newName
    case contents of
      SoftReadable r -> saveReadable r newNameFull
      SoftImage img -> case map toLower $ takeExtension newName of
        ".png" -> writePng newNameFull img
        ext    -> error $ "saveImport: unhandled image extension " <> show ext
      SoftChart song -> Save.toFile newNameFull $ RBFile.showMIDIFile' song
      SoftAudio aud -> audioIO Nothing aud newNameFull
    return newName
  yamlEncodeFile (dout </> "song.yml") $ toJSON yaml'
  return yaml'

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

detectExtProBass :: RBFile.FixedFile t -> PG.GtrBase
detectExtProBass trks = let
  strs = do
    trk <- [RBFile.fixedPartRealBass trks, RBFile.fixedPartRealBass22 trks]
    diff <- toList $ PG.pgDifficulties trk
    (str, _) <- toList (PG.pgNotes diff) >>= toList
    return str
  in if elem PG.S1 strs
    then PG.GtrCustom [28, 33, 38, 43, 47, 52] -- bass with 2 high gtr strings
    else if elem PG.S2 strs
      then PG.GtrCustom [28, 33, 38, 43, 47] -- bass with 1 high gtr string
      else PG.Bass4
