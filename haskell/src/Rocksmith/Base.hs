{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.Base where

import           Control.Arrow                  (second)
import           Control.Monad                  (forM_, guard, unless, void)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char                      (isSpace)
import           Data.Fixed                     (Milli)
import           Data.Foldable                  (toList)
import           Data.Maybe                     (isJust, isNothing, mapMaybe)
import           Data.Profunctor                (dimap)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           JSONData
import qualified Sound.MIDI.Util                as U
import           Text.Read                      (readMaybe)
import           Text.XML.Light

data Inside = Inside
  { insideAttrs   :: [Attr]
  , insideContent :: [Content]
  }

instance Semigroup Inside where
  Inside a b <> Inside c d = Inside (a ++ c) (b ++ d)

instance Monoid Inside where
  mempty = Inside [] []

type InsideParser m = StackTraceT (StateT Inside m)
type InsideBuilder = Writer Inside
type InsideCodec m a = Codec (InsideParser m) InsideBuilder a

class IsInside a where
  insideCodec :: (SendMessage m) => InsideCodec m a

matchTag :: T.Text -> Element -> Maybe (StackTraceT m a -> StackTraceT m a)
matchTag t elt = do
  guard $ elName elt == QName (T.unpack t) Nothing Nothing
  return $ inside $ unwords $ concat
    [ toList (elLine elt) >>= \ln -> ["line", show ln]
    , ["tag <" <> T.unpack t <> ">"]
    ]

makeTag :: T.Text -> Inside -> Element
makeTag t ins = Element
  { elName = QName (T.unpack t) Nothing Nothing
  , elAttribs = insideAttrs ins
  , elContent = insideContent ins
  , elLine = Nothing
  }

isTag :: (Monad m) => T.Text -> ValueCodec m Inside a -> ValueCodec m Element a
isTag t cdc = Codec
  { codecIn = do
    elt <- lift ask
    case matchTag t elt of
      Nothing -> fatal $ "Expected a tag <" <> T.unpack t <> "> but got: " <> show (elName elt)
      Just layer -> layer $ parseFrom (Inside (elAttribs elt) (elContent elt)) $ codecIn cdc
  , codecOut = mapWriter (second $ makeTag t) . codecOut cdc
  }

childTag :: (Monad m) => T.Text -> ValueCodec m Inside a -> InsideCodec m a
childTag t cdc = Codec
  { codecIn = do
    ins <- lift get
    let isElement = \case Elem e -> Just e; _ -> Nothing
    case break (isJust . snd) [ (x, isElement x >>= matchTag t) | x <- insideContent ins ] of
      (before, (Elem elt, Just layer) : after) -> do
        let ins' = Inside (elAttribs elt) (elContent elt)
        x <- layer $ mapStackTraceT (lift . (`runReaderT` ins')) $ codecIn cdc
        lift $ put $ ins { insideContent = map fst $ before ++ after }
        return x
      _ -> fatal $ "Missing required child tag <" <> T.unpack t <> ">"
  , codecOut = let
    makeChild ins = Inside
      { insideAttrs = []
      , insideContent = [Elem $ makeTag t ins]
      }
    in mapWriter (second makeChild) . codecOut cdc
  }

parseInside :: (Monad m) => InsideCodec m a -> ValueCodec m Inside a
parseInside cdc = Codec
  { codecIn = do
    ins <- lift ask
    mapStackTraceT (lift . (`evalStateT` ins)) $ codecIn cdc
  , codecOut = codecOut cdc
  }

warnUnused :: (SendMessage m) => InsideCodec m a -> InsideCodec m a
warnUnused cdc = cdc
  { codecIn = codecIn cdc <* do
    ins <- lift get
    forM_ (insideAttrs ins) $ \attr -> warn $ "Unused attribute " <> show (qName $ attrKey attr)
    forM_ (insideContent ins) $ \case
      Elem elt -> warn $ unwords $ concat
        [ ["Unused element <" <> qName (elName elt) <> ">"]
        , toList (elLine elt) >>= \ln -> ["at line", show ln]
        ]
      Text cd -> maybe id (inside . ("line " <>) . show) (cdLine cd) $ do
        unless (all isSpace $ cdData cd)
          $ warn $ "Found text: " <> show (cdData cd)
      CRef _ -> warn "Found CRef"
  }

parseInside' :: (SendMessage m) => InsideCodec m a -> ValueCodec m Inside a
parseInside' = parseInside . warnUnused

reqAttr :: (Monad m) => T.Text -> InsideCodec m T.Text
reqAttr a = Codec
  { codecIn = do
    ins <- lift get
    case break (\attr -> attrKey attr == QName (T.unpack a) Nothing Nothing) $ insideAttrs ins of
      (before, attr : after) -> do
        lift $ put $ ins { insideAttrs = before ++ after }
        return $ T.pack $ attrVal attr
      _ -> fatal $ "Missing required attribute " <> show a
  , codecOut = makeOut $ \x -> Inside
    { insideAttrs = [Attr (QName (T.unpack a) Nothing Nothing) (T.unpack x)]
    , insideContent = []
    }
  }

countList :: (SendMessage m) => ValueCodec m Element a -> InsideCodec m [a]
countList cdc = Codec
  { codecIn = undefined
  , codecOut = let
    makeList elts = Inside
      { insideAttrs = [Attr (QName "count" Nothing Nothing) (show $ length elts)]
      , insideContent = map Elem elts
      }
    in makeOut $ makeList . map (makeValue' cdc)
  }

childText :: (Monad m) => InsideCodec m T.Text
childText = Codec
  { codecIn = do
    ins <- lift get
    let getText = \case Text cd -> Just cd; _ -> Nothing
    lift $ put $ ins { insideContent = filter (isNothing . getText) $ insideContent ins }
    return
      $ T.unwords
      $ filter (not . T.null)
      $ map (T.strip . T.pack . cdData)
      $ mapMaybe getText
      $ insideContent ins
  , codecOut = makeOut $ \t -> Inside
    { insideAttrs = []
    , insideContent = [Text $ CData CDataText (T.unpack t) Nothing | not $ T.null t]
    }
  }

intText :: (Monad m, Integral a) => InsideCodec m T.Text -> InsideCodec m a
intText cdc = Codec
  { codecIn = codecIn cdc >>= \str -> case readMaybe $ T.unpack str of
    Nothing -> fatal $ "Expected an integer, got: " <> show str
    Just i  -> return $ fromInteger i
  , codecOut = \i -> do
    void $ codecOut cdc $ T.pack $ show $ toInteger i
    return i
  }

milliText :: (Monad m, RealFrac a) => InsideCodec m T.Text -> InsideCodec m a
milliText cdc = Codec
  { codecIn = codecIn cdc >>= \str -> case readMaybe $ T.unpack str of
    Nothing -> fatal $ "Expected an integer, got: " <> show str
    Just m  -> return $ realToFrac (m :: Milli)
  , codecOut = \n -> do
    void $ codecOut cdc $ T.pack $ show (realToFrac n :: Milli)
    return n
  }

seconds :: (Monad m) => InsideCodec m T.Text -> InsideCodec m U.Seconds
seconds = dimap U.fromSeconds U.Seconds . milliText

bpm :: (Monad m) => InsideCodec m T.Text -> InsideCodec m U.BPS
bpm = dimap ((* 60) . U.fromBPS) (U.BPS . (/ 60)) . milliText

data Arrangement = Arrangement
  { arr_version                :: Int
  , arr_title                  :: T.Text
  , arr_arrangement            :: T.Text
  , arr_part                   :: Int -- what is this?
  , arr_offset                 :: U.Seconds
  , arr_centOffset             :: Int
  , arr_songLength             :: U.Seconds
  , arr_lastConversionDateTime :: T.Text -- whatever
  , arr_startBeat              :: U.Seconds
  , arr_averageTempo           :: U.BPS
  , arr_tuning                 :: Tuning
  , arr_capo                   :: Int
  , arr_artistName             :: T.Text
  , arr_artistNameSort         :: T.Text
  } deriving (Eq, Show)

instance IsInside Arrangement where
  insideCodec = warnUnused $ do
    arr_version                <- arr_version                =. intText (reqAttr "version")
    arr_title                  <- arr_title                  =. childTag "title"                  (parseInside' childText)
    arr_arrangement            <- arr_arrangement            =. childTag "arrangement"            (parseInside' childText)
    arr_part                   <- arr_part                   =. childTag "part"                   (parseInside' $ intText childText)
    arr_offset                 <- arr_offset                 =. childTag "offset"                 (parseInside' $ seconds childText)
    arr_centOffset             <- arr_centOffset             =. childTag "centOffset"             (parseInside' $ intText childText)
    arr_songLength             <- arr_songLength             =. childTag "songLength"             (parseInside' $ seconds childText)
    arr_lastConversionDateTime <- arr_lastConversionDateTime =. childTag "lastConversionDateTime" (parseInside' childText)
    arr_startBeat              <- arr_startBeat              =. childTag "startBeat"              (parseInside' $ seconds childText)
    arr_averageTempo           <- arr_averageTempo           =. childTag "averageTempo"           (parseInside' $ bpm childText)
    arr_tuning                 <- arr_tuning                 =. childTag "tuning"                 (parseInside' insideCodec)
    arr_capo                   <- arr_capo                   =. childTag "capo"                   (parseInside' $ intText childText)
    arr_artistName             <- arr_artistName             =. childTag "artistName"             (parseInside' childText)
    arr_artistNameSort         <- arr_artistNameSort         =. childTag "artistNameSort"         (parseInside' childText)
    return Arrangement{..}

data Tuning = Tuning
  { tuning_string0 :: Int
  , tuning_string1 :: Int
  , tuning_string2 :: Int
  , tuning_string3 :: Int
  , tuning_string4 :: Int
  , tuning_string5 :: Int
  } deriving (Eq, Show)

instance IsInside Tuning where
  insideCodec = warnUnused $ do
    tuning_string0 <- tuning_string0 =. intText (reqAttr "string0")
    tuning_string1 <- tuning_string1 =. intText (reqAttr "string1")
    tuning_string2 <- tuning_string2 =. intText (reqAttr "string2")
    tuning_string3 <- tuning_string3 =. intText (reqAttr "string3")
    tuning_string4 <- tuning_string4 =. intText (reqAttr "string4")
    tuning_string5 <- tuning_string5 =. intText (reqAttr "string5")
    return Tuning{..}

testParse :: FilePath -> IO Arrangement
testParse f = do
  let cdc = isTag "song" $ parseInside' insideCodec
  T.readFile f >>= \t -> case parseXMLDoc t of
    Nothing -> error "couldn't parse xml"
    Just elt -> logStdout (mapStackTraceT (`runReaderT` elt) $ codecIn cdc) >>= \case
      Left msgs -> error $ show msgs
      Right arr -> return arr
