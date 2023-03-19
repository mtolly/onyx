{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.DDR.SM where

import           Control.Monad         (forM, guard)
import qualified Data.ByteString       as B
import           Data.Char             (isSpace)
import           Data.Scientific
import qualified Data.Text             as T
import           Onyx.Util.Text.Decode (decodeGeneral)
import           Text.Read             (readMaybe)

data Token
  = Hash
  | Atom String
  | Colon
  | Semicolon
  deriving (Eq, Show)

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe _ [] = ([], [])
spanMaybe f xs@(x : xt) = case f x of
  Nothing -> ([], xs)
  Just y -> case spanMaybe f xt of
    (yes, no) -> (y : yes, no)

scan :: String -> [Token]
scan = let
  go ""              = []
  go ('\\' : c : cs) = Atom [c]  : go cs
  go ('#' : cs)      = Hash      : go cs
  go (':' : cs)      = Colon     : go cs
  go (';' : cs)      = Semicolon : go cs
  go (c : cs)        = Atom [c]  : go cs
  condense []              = []
  condense (Atom s : toks) = let
    (strs, rest) = spanMaybe (\case Atom a -> Just a; _ -> Nothing) toks
    in Atom (concat $ s : strs) : condense rest
  condense (tok : toks)    = tok : condense toks
  in condense . go

parse :: [Token] -> [(T.Text, [T.Text])]
parse toks = case break (== Hash) toks of
  (_, Hash : toks') -> let
    (line, after) = break (== Semicolon) toks'
    in case map T.pack [ s | Atom s <- line ] of
      []           -> parse after
      atom : atoms -> (T.strip atom, map T.strip atoms) : parse after
  _ -> []

-- | Scan and parse .sm or .ssc files.
loadSMLines :: FilePath -> IO [(T.Text, [T.Text])]
loadSMLines f = do
  txt <- decodeGeneral <$> B.readFile f
  -- first remove comments
  let noComments = case T.splitOn "//" txt of
        []     -> ""
        t : ts -> T.concat $ t : map (T.dropWhile (/= '\n')) ts
  return $ parse $ scan $ T.unpack noComments

data SM = SM
  { sm_TITLE            :: Maybe T.Text
  , sm_SUBTITLE         :: Maybe T.Text
  , sm_ARTIST           :: Maybe T.Text
  , sm_TITLETRANSLIT    :: Maybe T.Text
  , sm_SUBTITLETRANSLIT :: Maybe T.Text
  , sm_ARTISTTRANSLIT   :: Maybe T.Text
  , sm_GENRE            :: Maybe T.Text
  , sm_CREDIT           :: Maybe T.Text
  , sm_BANNER           :: Maybe T.Text
  , sm_JACKET           :: Maybe T.Text
  , sm_BACKGROUND       :: Maybe T.Text
  -- TODO #BGCHANGES
  , sm_LYRICSPATH       :: Maybe T.Text
  , sm_CDTITLE          :: Maybe T.Text
  , sm_MUSIC            :: Maybe T.Text
  , sm_OFFSET           :: Maybe Scientific
  , sm_SAMPLESTART      :: Maybe Scientific
  , sm_SAMPLELENGTH     :: Maybe Scientific
  , sm_SELECTABLE       :: Maybe Bool
  , sm_DISPLAYBPM       :: [Scientific]
  , sm_BPMS             :: [(Scientific, Scientific)]
  , sm_STOPS            :: [(Scientific, Scientific)]
  , sm_NOTES            :: [SMNotes]
  } deriving (Show)

getSMString :: (MonadFail m) => [(T.Text, [T.Text])] -> T.Text -> m (Maybe T.Text)
getSMString lns k = case [ v | (k', v) <- lns, k == k' ] of
  []  -> return Nothing
  [x] -> case x of
    []  -> return Nothing
    [v] -> return $ guard (T.any (not . isSpace) v) >> Just v
    vs  -> fail $ show (length vs) <> " elements for data #" <> T.unpack k
  xs  -> fail $ show (length xs) <> " lines matching key #" <> T.unpack k

parseSMNumber :: (MonadFail m) => T.Text -> m Scientific
parseSMNumber s = case readMaybe $ T.unpack s of
  Nothing -> fail $ "Couldn't parse as number: " <> show s
  Just d  -> return d

getSMNumber :: (MonadFail m) => [(T.Text, [T.Text])] -> T.Text -> m (Maybe Scientific)
getSMNumber lns k = getSMString lns k >>= \case
  Nothing -> return Nothing
  Just s  -> Just <$> parseSMNumber s

getSMNumberPairs :: (MonadFail m) => [(T.Text, [T.Text])] -> T.Text -> m [(Scientific, Scientific)]
getSMNumberPairs lns k = getSMString lns k >>= \case
  Nothing -> return []
  Just s -> forM (splitNoEmpty "," s) $ \pair -> case T.splitOn "=" pair of
    [x, y] -> case (readMaybe $ T.unpack x, readMaybe $ T.unpack y) of
      (Just x', Just y') -> return (x', y')
      _                  -> fail $ "Couldn't parse number=number: " <> show pair
    _ -> fail $ "Couldn't parse number=number: " <> show pair

readSM :: (MonadFail m) => [(T.Text, [T.Text])] -> m SM
readSM lns = do
  sm_TITLE            <- getSMString lns "TITLE"
  sm_SUBTITLE         <- getSMString lns "SUBTITLE"
  sm_ARTIST           <- getSMString lns "ARTIST"
  sm_TITLETRANSLIT    <- getSMString lns "TITLETRANSLIT"
  sm_SUBTITLETRANSLIT <- getSMString lns "SUBTITLETRANSLIT"
  sm_ARTISTTRANSLIT   <- getSMString lns "ARTISTTRANSLIT"
  sm_GENRE            <- getSMString lns "GENRE"
  sm_CREDIT           <- getSMString lns "CREDIT"
  sm_BANNER           <- getSMString lns "BANNER"
  sm_JACKET           <- getSMString lns "JACKET"
  sm_BACKGROUND       <- getSMString lns "BACKGROUND"
  sm_LYRICSPATH       <- getSMString lns "LYRICSPATH"
  sm_CDTITLE          <- getSMString lns "CDTITLE"
  sm_MUSIC            <- getSMString lns "MUSIC"
  sm_OFFSET           <- getSMNumber lns "OFFSET"
  sm_SAMPLESTART      <- getSMNumber lns "SAMPLESTART"
  sm_SAMPLELENGTH     <- getSMNumber lns "SAMPLELENGTH"
  sm_SELECTABLE       <- getSMString lns "SELECTABLE" >>= \case
    Nothing    -> return Nothing
    Just "YES" -> return $ Just True
    Just "NO"  -> return $ Just False
    Just s     -> fail $ "Couldn't recognize boolean (YES/NO): " <> show s
  sm_DISPLAYBPM       <- mapM parseSMNumber $ concat [ v | ("DISPLAYBPM", v) <- lns ]
  sm_BPMS             <- getSMNumberPairs lns "BPMS"
  sm_STOPS            <- getSMNumberPairs lns "STOPS"
  sm_NOTES            <- mapM readSMNotes [ v | ("NOTES", v) <- lns ]
  return SM{..}

data SMNotes = SMNotes
  { smn_ChartType      :: T.Text
  , smn_Author         :: T.Text
  , smn_Difficulty     :: T.Text
  , smn_NumericalMeter :: Int
  , smn_GrooveRadar    :: [Scientific]
  , smn_Notes          :: [[T.Text]]
  } deriving (Show)

readSMNotes :: (MonadFail m) => [T.Text] -> m SMNotes
readSMNotes = \case
  [smn_ChartType, smn_Author, smn_Difficulty, nm, gr, notes] -> do
    smn_NumericalMeter <- case readMaybe $ T.unpack nm of
      Nothing -> fail $ "Couldn't read numerical meter value: " <> show nm
      Just v  -> return v
    smn_GrooveRadar <- forM (splitNoEmpty "," gr) $ \x -> case readMaybe $ T.unpack x of
      Nothing -> fail $ "Couldn't read groove radar value: " <> show x
      Just v  -> return v
    smn_Notes <- forM (splitNoEmpty "," notes) $ \bar -> do
      forM (filter (T.any $ not . isSpace) $ T.lines bar) $ \line -> do
        return $ T.strip line
    return SMNotes{..}
  xs -> fail $ "Expected 6 parts to #NOTES but found " <> show (length xs)

splitNoEmpty :: T.Text -> T.Text -> [T.Text]
splitNoEmpty x y = case T.splitOn x y of
  [v] -> if T.all isSpace v then [] else [v]
  vs  -> vs
