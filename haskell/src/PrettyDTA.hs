{- |
Add the C3 comments into a @songs.dta@ file.
-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module PrettyDTA where

import           Config

import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import           Data.Char                      (isSpace)
import qualified Data.DTA                       as D
import           Data.DTA.Serialize
import qualified Data.DTA.Serialize.RB3         as D
import           Data.Foldable                  (forM_)
import qualified Data.HashMap.Strict            as Map
import           Data.List                      (stripPrefix)
import           Data.List.HT                   (partitionMaybe)
import           Data.List.Split                (splitOn)
import           Data.Maybe                     (fromMaybe, listToMaybe,
                                                 mapMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           JSONData                       (stackShow)
import           Resources                      (missingSongData)
import           System.IO.Extra                (latin1, readFileEncoding',
                                                 utf8)

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
  } deriving (Eq, Ord, Show, Read)

makeC3DTAComments :: Metadata -> Plan -> Bool -> C3DTAComments
makeC3DTAComments meta plan is2x = C3DTAComments
  { c3dtaCreatedUsing = Just "Onyx Music Game Toolkit"
  , c3dtaAuthoredBy   = Just $ getAuthor meta
  , c3dtaSong         = Just $ getTitle meta
  , c3dtaLanguages    = Just $ _languages meta
  , c3dtaKaraoke      = Just $ getKaraoke plan
  , c3dtaMultitrack   = Just $ getMultitrack plan
  , c3dtaConvert      = Just $ _convert meta
  , c3dta2xBass       = Just is2x
  , c3dtaRhythmKeys   = Just $ _rhythmKeys meta
  , c3dtaRhythmBass   = Just $ _rhythmBass meta
  , c3dtaCATemh       = Just $ _catEMH meta
  , c3dtaExpertOnly   = Just $ _expertOnly meta
  }

data DTASingle = DTASingle
  { dtaTopKey      :: T.Text
  , dtaSongPackage :: D.SongPackage
  , dtaC3Comments  :: C3DTAComments
  } deriving (Eq, Show, Read)

-- | CONs put out by C3 Magma sometimes bizarrely have the @tracks_count@ key
-- completely removed from @songs.dta@, but the list of track counts is still
-- there. So, we have to put it back before parsing @song@ as a key-value map.
fixTracksCount :: [D.Chunk T.Text] -> [D.Chunk T.Text]
fixTracksCount = map findSong where
  findSong = \case
    D.Parens (D.Tree w (D.Key "song" : rest)) ->
      D.Parens (D.Tree w (D.Key "song" : map findTracksCount rest))
    x -> x
  findTracksCount = \case
    D.Parens (D.Tree w [D.Parens (D.Tree w2 nums)]) ->
      D.Parens $ D.Tree w [D.Key "tracks_count", D.Parens $ D.Tree w2 nums]
    x -> x

-- | Remove keys which were moved or renamed in RB3.
-- The correct versions are added by missing_song_data.dta
removeOldDTAKeys :: [D.Chunk T.Text] -> [D.Chunk T.Text]
removeOldDTAKeys = filter $ \case
  D.Parens (D.Tree _ (D.Key "hopo_threshold" : _)) -> False -- Aesthetics of Hate. it should be under 'song'
  D.Parens (D.Tree _ (D.Key "tuning_offset"  : _)) -> False -- in a few songs. changed to 'tuning_offset_cents'
  _                                                -> True

missingMapping :: Map.HashMap T.Text [D.Chunk T.Text]
missingMapping = case missingSongData of
  D.DTA _ (D.Tree _ chunks) -> let
    getPair = \case
      D.Parens (D.Tree _ (D.Key k : rest)) -> (k, rest)
      _ -> error "panic! missing_song_data not in expected format"
    in Map.fromList $ map getPair chunks

-- | Applies an update from missing_song_data.dta.
applyUpdate :: [D.Chunk T.Text] -> [D.Chunk T.Text] -> [D.Chunk T.Text]
applyUpdate original update = let
  getSong = partitionMaybe $ \case
    D.Parens (D.Tree _ (D.Key "song" : s)) -> Just s
    _ -> Nothing
  (originalSong, untouched) = getSong original
  (songUpdate, otherUpdate) = getSong update
  newSong = D.Parens $ D.Tree 0 $ D.Key "song" : concat (originalSong ++ songUpdate)
  in newSong : (untouched ++ otherUpdate)

readRB3DTABytes :: (SendMessage m) => D.DTA B.ByteString -> StackTraceT m (T.Text, D.SongPackage, Bool)
readRB3DTABytes dtabs = do
  -- First we read as Latin-1. Then redo as UTF-8 if encoding says so
  let readSongWith rdr = do
        dta <- mapM rdr dtabs
        (k, chunks) <- case D.treeChunks $ D.topTree dta of
          [D.Parens (D.Tree _ (D.Key k : chunks))] -> return (k, chunks)
          _ -> fatal $ "Not a valid songs.dta with exactly one song"
        let missingChunks = fromMaybe [] $ Map.lookup k missingMapping
        pkg <- unserialize stackChunks $ D.DTA 0 $ D.Tree 0
          $ removeOldDTAKeys $ fixTracksCount $ applyUpdate chunks missingChunks
        return (k, pkg)
      decodeUtf8Stack bs = inside "decoding utf-8 string" $
        either (fatal . show) return $ TE.decodeUtf8' bs
  (k_l1, l1) <- readSongWith $ return . TE.decodeLatin1
  case D.encoding l1 of
    Just "utf8" -> (\(k, pkg) -> (k, pkg, True)) <$> readSongWith decodeUtf8Stack
    Just "latin1" -> return (k_l1, l1, False)
    Nothing -> return (k_l1, l1, False)
    Just enc -> fatal $ "Unrecognized DTA character encoding: " ++ T.unpack enc

-- | Returns @(short song name, DTA file contents, is UTF8)@
readRB3DTA :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (T.Text, D.SongPackage, Bool)
readRB3DTA dtaPath = inside ("loading DTA file " ++ show dtaPath) $ do
  stackIO (B.readFile dtaPath) >>= D.readDTABytes >>= readRB3DTABytes

readDTASingle :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m DTASingle
readDTASingle file = do
  (topKey, pkg, isUTF8) <- readRB3DTA file
  -- C3 puts extra info in DTA comments
  dtaLines <- liftIO $ fmap (lines . filter (/= '\r')) $ readFileEncoding' (if isUTF8 then utf8 else latin1) file
  let findBool s
        | elem (";" ++ s ++ "=0") dtaLines = Just False
        | elem (";" ++ s ++ "=1") dtaLines = Just True
        | otherwise                        = Nothing
      comments = C3DTAComments
        { c3dtaCreatedUsing = Just "Onyx Music Game Toolkit"
        , c3dtaAuthoredBy = fmap T.pack $ listToMaybe $ mapMaybe (stripPrefix ";Song authored by ") dtaLines
        , c3dtaSong = fmap T.pack $ listToMaybe $ mapMaybe (stripPrefix ";Song=") dtaLines
        , c3dtaLanguages
          = fmap (map T.pack .  filter (not . null) . splitOn ",")
          $ listToMaybe $ mapMaybe (stripPrefix ";Language(s)=") dtaLines
        , c3dtaKaraoke = findBool "Karaoke"
        , c3dtaMultitrack = findBool "Multitrack"
        , c3dtaConvert = findBool "Convert"
        , c3dta2xBass = findBool "2xBass"
        , c3dtaRhythmKeys = findBool "RhythmKeys"
        , c3dtaRhythmBass = findBool "RhythmBass"
        , c3dtaCATemh = findBool "CATemh"
        , c3dtaExpertOnly = findBool "ExpertOnly"
        }
  return $ DTASingle topKey pkg comments

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
  dta = D.DTA 0 $ D.Tree 0 [D.Parens $ D.Tree 0 $ D.Key name : stackShow stackChunks pkg]
  dtaLines = filter (T.any $ not . isSpace) $ T.lines $ D.showDTA dta
  c3Lines = writeC3Comments c3
  in T.unlines $ case reverse dtaLines of
    ")" : dtaLines' -> reverse dtaLines' ++ c3Lines ++ [")"]
    _               -> dtaLines ++ c3Lines -- I don't think this works but it shouldn't happen
