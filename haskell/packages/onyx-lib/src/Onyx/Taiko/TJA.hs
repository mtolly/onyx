{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Onyx.Taiko.TJA where

import           Control.Monad           (forM)
import qualified Data.ByteString         as B
import           Data.Char               (isSpace)
import           Data.Maybe              (catMaybes)
import           Data.Scientific         (Scientific)
import qualified Data.Text               as T
import           Onyx.StackTrace         (SendMessage, StackTraceT, inside,
                                          warn)
import           Onyx.Util.Text.Decode   (decodeWithDefault)
import           Onyx.Util.Text.ShiftJIS (decodeShiftJIS)
import           Text.Read               (readMaybe)

data TJALine
  = Metadata T.Text T.Text
  | Notes T.Text Bool -- bool is true for line ending in comma
  | Start (Maybe T.Text) -- "P1" or "P2" for a multiplayer-specific chart
  | End
  | Measure Int Int
  | BPMChange Scientific
  | Delay Scientific
  | Scroll Scientific
  | GoGo Bool
  | BarLine Bool
  | BranchStart T.Text Scientific Scientific
  | Path PathType
  | BranchEnd
  | Section
  | Lyric T.Text
  | LevelHold
  | BMScroll
  | HBScroll
  | SENoteChange Int
  | NextSong NextSongInfo
  | Direction Int
  | Sudden Scientific Scientific
  | JPosScroll Scientific Int Int
  deriving (Show)

data PathType = PathN | PathE | PathM
  deriving (Show)

data NextSongInfo = NextSongInfo
  { title         :: T.Text
  , subtitle      :: T.Text
  , genre         :: T.Text
  , audioFilename :: T.Text
  , scoreInit     :: Maybe Int -- these can be blank
  , scoreDiff     :: Maybe Int
  } deriving (Show)

readTJALines :: (SendMessage m) => B.ByteString -> StackTraceT m [(Int, TJALine)]
readTJALines bs = do
  -- encoding is either Shift-JIS by default, or UTF-8 with BOM
  -- (maybe we should check for valid Shift-JIS and fallback to UTF-8?)
  let lns = T.lines $ decodeWithDefault decodeShiftJIS bs
  catMaybes <$> forM (zip [1..] lns) \(i, orig) -> inside ("Line " <> show i) do
    let line = T.strip orig
        found x = return $ Just (i, x)
        -- strips a trailing "// comment", as seen after commands and notes
        uncommented = T.strip . fst . T.breakOn "//"
        uncommentedLine = uncommented line
    if T.null uncommentedLine
      then return Nothing
      else case T.stripPrefix "#" line of
        Just command -> let
          (name, rawArg) = T.break isSpace command
          -- lyrics and song names can contain arbitrary text, so no comment stripping
          arg = T.strip rawArg
          args = uncommented rawArg
          argWords = T.words args
          number :: (Read a) => T.Text -> Maybe a
          number = readMaybe . T.unpack . T.strip
          parseWith f = case f args of
            Just x  -> found x
            Nothing -> do
              warn $ "Couldn't parse arguments to #" <> T.unpack name <> ": " <> show args
              return Nothing
          noArgs x = do
            case args of
              "" -> return ()
              _  -> warn $ "Ignoring unexpected arguments to #" <> T.unpack name <> ": " <> show args
            found x
          in case name of
            "START" -> found $ Start $ case args of
              "" -> Nothing
              _  -> Just args
            "END" -> noArgs End
            "MEASURE" -> parseWith \s -> case T.splitOn "/" s of
              [n, d] -> Measure <$> number n <*> number d
              _      -> Nothing
            "BPMCHANGE" -> parseWith $ fmap BPMChange . number
            "DELAY" -> parseWith $ fmap Delay . number
            "SCROLL" -> parseWith $ fmap Scroll . number
            "GOGOSTART" -> noArgs $ GoGo True
            "GOGOEND" -> noArgs $ GoGo False
            "BARLINEON" -> noArgs $ BarLine True
            "BARLINEOFF" -> noArgs $ BarLine False
            "BRANCHSTART" -> parseWith \s -> case map T.strip $ T.splitOn "," s of
              [typ, adv, mas] -> BranchStart typ <$> number adv <*> number mas
              _               -> Nothing
            "N" -> noArgs $ Path PathN
            "E" -> noArgs $ Path PathE
            "M" -> noArgs $ Path PathM
            "BRANCHEND" -> noArgs BranchEnd
            "SECTION" -> noArgs Section
            "LYRIC" -> found $ Lyric arg
            "LEVELHOLD" -> noArgs LevelHold
            "BMSCROLL" -> noArgs BMScroll
            "HBSCROLL" -> noArgs HBScroll
            "SENOTECHANGE" -> parseWith $ fmap SENoteChange . number
            "NEXTSONG" -> case splitEscapedCommas arg of
              title : subtitle : genre : audioFilename : scoreInit : scoreDiff : extra -> do
                case extra of
                  [] -> return ()
                  _  -> warn $ "Ignoring extra fields in #NEXTSONG: " <> show extra
                found $ NextSong NextSongInfo
                  { title         = title
                  , subtitle      = subtitle
                  , genre         = genre
                  , audioFilename = audioFilename
                  , scoreInit     = number scoreInit
                  , scoreDiff     = number scoreDiff
                  }
              _ -> do
                warn $ "Not enough fields in #NEXTSONG: " <> show arg
                return Nothing
            "DIRECTION" -> parseWith $ fmap Direction . number
            "SUDDEN" -> parseWith \_ -> case argWords of
              [a, b] -> Sudden <$> number a <*> number b
              _      -> Nothing
            "JPOSSCROLL" -> parseWith \_ -> case argWords of
              [a, b, c] -> JPosScroll <$> number a <*> number b <*> number c
              _         -> Nothing
            _ -> do
              warn $ "Unrecognized command: " <> T.unpack command
              return Nothing
        Nothing -> case T.break (== ':') line of
          (meta, rest) | not (T.null rest) && not ("//" `T.isInfixOf` meta)
            -> found $ Metadata meta $ T.drop 1 rest
          _ -> if T.null uncommentedLine
            then return Nothing
            else found case T.unsnoc uncommentedLine of
              Just (notes', ',') -> Notes notes'           True
              _                  -> Notes uncommentedLine  False

-- split on commas, unless escaped with backslash (or yen sign from Shift-JIS)
splitEscapedCommas :: T.Text -> [T.Text]
splitEscapedCommas = go "" where
  go cur t = case T.uncons t of
    Nothing              -> [cur]
    Just (esc, t') | elem esc ['\\', '¥'], Just (',', t'') <- T.uncons t'
                         -> go (T.snoc cur ',') t''
    Just (',', t')       -> cur : go "" t'
    Just (c, t')         -> go (T.snoc cur c) t'
