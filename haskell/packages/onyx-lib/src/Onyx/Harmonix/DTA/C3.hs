{- |
Add the C3 comments into a @songs.dta@ file.
-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Onyx.Harmonix.DTA.C3 where

import           Control.Monad                        (forM)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Writer.Strict
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import           Data.Char                            (isSpace)
import           Data.Default.Class                   (Default (..))
import           Data.Foldable                        (forM_)
import qualified Data.HashMap.Strict                  as Map
import           Data.List.HT                         (partitionMaybe)
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       listToMaybe, mapMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import           Data.Version                         (showVersion)
import           Onyx.Codec.Common                    (makeValue)
import qualified Onyx.Harmonix.DTA                    as D
import qualified Onyx.Harmonix.DTA.Crypt              as D
import           Onyx.Harmonix.DTA.Serialize
import qualified Onyx.Harmonix.DTA.Serialize.RockBand as D
import           Onyx.Project
import           Onyx.Resources                       (missingSongData)
import           Onyx.StackTrace
import           Onyx.Util.Text.Decode                (decodeGeneral)
import           Paths_onyx_lib                       (version)

writeUtf8CRLF :: (MonadIO m) => FilePath -> T.Text -> m ()
writeUtf8CRLF fp = liftIO . B.writeFile fp . TE.encodeUtf8
  . T.concatMap (\case '\n' -> "\r\n"; c -> T.singleton c)

writeLatin1CRLF :: (MonadIO m) => FilePath -> T.Text -> m ()
writeLatin1CRLF fp = liftIO . B.writeFile fp . B8.pack . T.unpack
  . T.concatMap (\case '\n' -> "\r\n"; c -> T.singleton c)

stringLit :: T.Text -> T.Text
stringLit s = "\"" <> T.concatMap (\case '"' -> "\\q"; c -> T.singleton c) s <> "\""

data C3DTAComments = C3DTAComments
  { c3dtaCreatedUsing :: Maybe T.Text
  , c3dtaAuthoredBy   :: Maybe T.Text
  , c3dtaSong         :: Maybe T.Text
  , c3dtaLanguages    :: Maybe [T.Text]
  , c3dtaKaraoke      :: Maybe Bool
  , c3dtaMultitrack   :: Maybe Bool
  , c3dtaConvert      :: Maybe Bool
  , c3dta2xBass       :: Maybe Bool
  , c3dtaRhythmKeys   :: Maybe Bool
  , c3dtaRhythmBass   :: Maybe Bool
  , c3dtaCATemh       :: Maybe Bool
  , c3dtaExpertOnly   :: Maybe Bool
  } deriving (Eq, Ord, Show)

instance Default C3DTAComments where
  def = C3DTAComments Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

makeC3DTAComments :: Metadata f -> Plan f -> TargetRB3 -> C3DTAComments
makeC3DTAComments meta plan rb3 = C3DTAComments
  { c3dtaCreatedUsing = Just $ T.pack $ "Onyx Music Game Toolkit version " <> showVersion version
  , c3dtaAuthoredBy   = Just $ getAuthor meta
  , c3dtaSong         = Just $ fromMaybe (getTitle meta) rb3.common.title
  , c3dtaLanguages    = Just meta.languages
  , c3dtaKaraoke      = Just $ getKaraoke plan
  , c3dtaMultitrack   = Just $ getMultitrack plan
  , c3dtaConvert      = Just meta.convert
  , c3dta2xBass       = Just rb3.is2xBassPedal
  , c3dtaRhythmKeys   = Just meta.rhythmKeys
  , c3dtaRhythmBass   = Just meta.rhythmBass
  , c3dtaCATemh       = Just meta.catEMH
  , c3dtaExpertOnly   = Just meta.expertOnly
  }

data DTASingle = DTASingle
  { dtaTopKey      :: T.Text
  , dtaSongPackage :: D.SongPackage
  , dtaC3Comments  :: C3DTAComments
  } deriving (Eq, Show)

-- | CONs put out by C3 Magma sometimes bizarrely have the @tracks_count@ key
-- completely removed from @songs.dta@, but the list of track counts is still
-- there. So, we have to put it back before parsing @song@ as a key-value map.
fixTracksCount :: [D.Chunk T.Text] -> [D.Chunk T.Text]
fixTracksCount = map findSong where
  findSong = \case
    D.Parens (D.Tree w (D.Sym "song" : rest)) ->
      D.Parens (D.Tree w (D.Sym "song" : map findTracksCount rest))
    x -> x
  findTracksCount = \case
    D.Parens (D.Tree w [D.Parens (D.Tree w2 nums)]) ->
      D.Parens $ D.Tree w [D.Sym "tracks_count", D.Parens $ D.Tree w2 nums]
    x -> x

-- | Remove keys which were moved or renamed in RB3.
-- The correct versions are added by missing_song_data.dta
removeOldDTAKeys :: [D.Chunk T.Text] -> [D.Chunk T.Text]
removeOldDTAKeys = filter $ \case
  D.Parens (D.Tree _ (D.Sym "hopo_threshold" : _)) -> False -- Aesthetics of Hate. it should be under 'song'
  D.Parens (D.Tree _ (D.Sym "tuning_offset"  : _)) -> False -- in a few songs. changed to 'tuning_offset_cents'
  _                                                -> True

-- | Ignore DTA scripts used in songs such as TBRB's All You Need Is Love,
-- as well as songs produced by the TBRB Custom DLC Project.
skipScripting :: [D.Chunk T.Text] -> [D.Chunk T.Text]
skipScripting = map $ \chunk -> case chunk of
  D.Braces (D.Tree _ (D.Sym "do" : xs)) -> case reverse xs of
    val : _ -> val
    []      -> chunk
  D.Parens   (D.Tree w xs) -> D.Parens   $ D.Tree w $ skipScripting xs
  D.Braces   (D.Tree w xs) -> D.Braces   $ D.Tree w $ skipScripting xs
  D.Brackets (D.Tree w xs) -> D.Brackets $ D.Tree w $ skipScripting xs
  _ -> chunk

missingMapping :: Map.HashMap T.Text [D.Chunk T.Text]
missingMapping = case missingSongData of
  D.DTA _ (D.Tree _ chunks) -> let
    getPair = \case
      D.Parens (D.Tree _ (D.Sym k : rest)) -> (k, rest)
      _ -> error "panic! missing_song_data not in expected format"
    in Map.fromList $ map getPair chunks

-- | Applies an update from missing_song_data.dta.
applyUpdate :: [D.Chunk T.Text] -> [D.Chunk T.Text] -> [D.Chunk T.Text]
applyUpdate original update = let
  getSong = partitionMaybe $ \case
    D.Parens (D.Tree _ (D.Sym "song" : s)) -> Just s
    _                                      -> Nothing
  (originalSong, untouched) = getSong original
  (songUpdate, otherUpdate) = getSong update
  newSong = D.Parens $ D.Tree 0 $ D.Sym "song" : concat (originalSong ++ songUpdate)
  in newSong : (untouched ++ otherUpdate)

readC3Comments :: T.Text -> C3DTAComments
readC3Comments t = let
  dtaLines = T.lines $ T.filter (/= '\r') t
  findBool s
    | elem (";" <> s <> "=0") dtaLines = Just False
    | elem (";" <> s <> "=1") dtaLines = Just True
    | otherwise                        = Nothing
  in C3DTAComments
    { c3dtaCreatedUsing = listToMaybe $ mapMaybe (T.stripPrefix ";Created using ") dtaLines
    , c3dtaAuthoredBy = listToMaybe $ mapMaybe (T.stripPrefix ";Song authored by ") dtaLines
    , c3dtaSong = listToMaybe $ mapMaybe (T.stripPrefix ";Song=") dtaLines
    , c3dtaLanguages
      = fmap (filter (not . T.null) . T.splitOn ",")
      $ listToMaybe $ mapMaybe (T.stripPrefix ";Language(s)=") dtaLines
    , c3dtaKaraoke = findBool "Karaoke"
    , c3dtaMultitrack = findBool "Multitrack"
    , c3dtaConvert = findBool "Convert"
    , c3dta2xBass = findBool "2xBass"
    , c3dtaRhythmKeys = findBool "RhythmKeys"
    , c3dtaRhythmBass = findBool "RhythmBass"
    , c3dtaCATemh = findBool "CATemh"
    , c3dtaExpertOnly = findBool "ExpertOnly"
    }

readDTASingle :: (SendMessage m) => (B.ByteString, D.Chunk B.ByteString) -> StackTraceT m (DTASingle, Bool)
readDTASingle (bytes, chunk) = do
  let readTextChunk = \case
        D.Parens (D.Tree _ (D.Sym k : chunks)) -> do
          let missingChunks = fromMaybe [] $ Map.lookup k missingMapping
          pkg <- unserialize stackChunks $ D.DTA 0 $ D.Tree 0
            $ removeOldDTAKeys $ fixTracksCount $ skipScripting
            $ applyUpdate chunks missingChunks
          return (k, pkg)
        _ -> fatal "Not a valid song chunk in the format (topkey ...)"
  (latinKey, latinPkg) <- readTextChunk $ fmap TE.decodeLatin1 chunk
  (k, pkg, isUTF8) <- case D.encoding latinPkg of
    Nothing -> return (latinKey, latinPkg, False)
    Just "latin1" -> return (latinKey, latinPkg, False)
    Just "utf8" -> do
      (k, pkg) <- readTextChunk $ fmap decodeGeneral chunk
      return (k, pkg, True)
    Just enc -> fatal $ "Unrecognized DTA character encoding: " ++ T.unpack enc
  let comments = readC3Comments $ (if isUTF8 then decodeGeneral else TE.decodeLatin1) bytes
  return (DTASingle k pkg comments, isUTF8)

readDTASingles :: (SendMessage m) => B.ByteString -> StackTraceT m [(DTASingle, Bool)]
readDTASingles bs = do
  songs <- D.readDTASections bs
  fmap catMaybes $ forM (zip [1..] songs) $ \(i, pair) -> do
    inside ("songs.dta entry #" ++ show (i :: Int) ++ " (starting from 1)") $ do
      errorToWarning $ readDTASingle pair

readDTBSingles :: (SendMessage m) => B.ByteString -> StackTraceT m [(DTASingle, Bool)]
readDTBSingles bs = do
  -- TODO use proper error handling instead of pure D.decodeDTB
  -- TODO better determination of encrypted .dtb
  let songs = D.treeChunks $ D.topTree $ D.decodeDTB $ if B.take 1 bs == "\x01"
        then BL.fromStrict bs
        else D.decrypt D.newCrypt $ BL.fromStrict bs
  fmap catMaybes $ forM (zip [1..] songs) $ \(i, chunk) -> do
    inside ("songs.dta entry #" ++ show (i :: Int) ++ " (starting from 1)") $ do
      errorToWarning $ readDTASingle (B.empty, chunk)

readFileSongsDTA :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [(DTASingle, Bool)]
readFileSongsDTA file = inside ("loading songs.dta from: " ++ show file) $ do
  stackIO (B.readFile file) >>= readDTASingles

--- | Returns @(short song name, DTA file contents, is UTF8)@
readRB3DTA :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (T.Text, D.SongPackage, Bool)
readRB3DTA f = readFileSongsDTA f >>= \case
  [(DTASingle k pkg _, b)] -> return (k, pkg, b)
  _                        -> fatal $ "Not exactly 1 song in songs.dta: " ++ show f

writeDTASingle :: DTASingle -> T.Text
writeDTASingle (DTASingle x y z) = prettyDTA x y z

writeC3Comments :: C3DTAComments -> [T.Text]
writeC3Comments C3DTAComments{..} = execWriter $ do
  let ln s = tell [s]
  ln ";DO NOT EDIT THE FOLLOWING LINES MANUALLY"
  forM_ c3dtaCreatedUsing $ \t -> ln $ ";Created using " <> t
  forM_ c3dtaAuthoredBy $ \t -> ln $ ";Song authored by " <> t
  forM_ c3dtaSong $ \t -> ln $ ";Song=" <> t
  forM_ c3dtaLanguages $ \ts -> ln $ ";Languages=" <> T.concat (map (<> ",") ts)
  forM_ c3dtaKaraoke $ \b -> ln $ ";Karaoke=" <> if b then "1" else "0"
  forM_ c3dtaMultitrack $ \b -> ln $ ";Multitrack=" <> if b then "1" else "0"
  forM_ c3dtaConvert $ \b -> ln $ ";Convert=" <> if b then "1" else "0"
  forM_ c3dta2xBass $ \b -> ln $ ";2xBass=" <> if b then "1" else "0"
  forM_ c3dtaRhythmKeys $ \b -> ln $ ";RhythmKeys=" <> if b then "1" else "0"
  forM_ c3dtaRhythmBass $ \b -> ln $ ";RhythmBass=" <> if b then "1" else "0"
  forM_ c3dtaCATemh $ \b -> ln $ ";CATemh=" <> if b then "1" else "0"
  forM_ c3dtaExpertOnly $ \b -> ln $ ";ExpertOnly=" <> if b then "1" else "0"

prettyDTA :: T.Text -> D.SongPackage -> C3DTAComments -> T.Text
prettyDTA name pkg c3 = let
  dta = D.DTA 0 $ D.Tree 0 [D.Parens $ D.Tree 0 $ D.Sym name : makeValue stackChunks pkg]
  dtaLines = filter (T.any $ not . isSpace) $ T.lines $ D.showDTA dta
  c3Lines = writeC3Comments c3
  in T.unlines $ case reverse dtaLines of
    ")" : dtaLines' -> reverse dtaLines' ++ c3Lines ++ [")"]
    _               -> dtaLines ++ c3Lines -- I don't think this works but it shouldn't happen
