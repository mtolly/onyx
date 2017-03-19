-- | All functions that take a 'Handle' or 'FilePath' do their reading/writing
-- strictly.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.DTA
( DTA(..), Tree(..), Chunk(..)
, decodeDTB, encodeDTB
, readFileDTB, writeFileDTB
, readDTA, showDTA
, readFileDTA, readFileDTA_latin1, readFileDTA_utf8
, readFileDTA', readFileDTA_latin1', readFileDTA_utf8'
, writeFileDTA_latin1, writeFileDTA_utf8
, renumberFrom
) where

import           Control.Applicative            ((<|>))
import           Control.Exception.Extra
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.StackTrace (StackTraceT, catchError, fatal,
                                                 inside)
import           Data.Binary                    (decode, encode)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.DTA.Base
import           Data.DTA.Lex
import           Data.DTA.Parse
import           Data.DTA.PrettyPrint
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeLatin1, decodeUtf8,
                                                 decodeUtf8', encodeUtf8)
import           System.IO.Error                (tryIOError)

decodeDTB :: BL.ByteString -> DTA B.ByteString
decodeDTB = decode

encodeDTB :: DTA B.ByteString -> BL.ByteString
encodeDTB = encode

readFileDTB :: FilePath -> IO (DTA B.ByteString)
readFileDTB = fmap (decodeDTB . BL.fromStrict) . B.readFile

writeFileDTB :: FilePath -> DTA B.ByteString -> IO ()
writeFileDTB fp dta = BL.writeFile fp $ encodeDTB dta

readDTA :: T.Text -> DTA T.Text
readDTA = parse . scan . removeBOM

removeBOM :: T.Text -> T.Text
removeBOM s = fromMaybe s
  $   T.stripPrefix "\xFEFF"       s -- normal unicode-decoded BOM
  <|> T.stripPrefix "\xEF\xBB\xBF" s -- latin1 decoding of a utf8 BOM

readFileDTA :: FilePath -> IO (DTA T.Text)
readFileDTA f = readFileDTA_utf8 f `catch_` \_ -> readFileDTA_latin1 f

readFileDTA_latin1 :: FilePath -> IO (DTA T.Text)
readFileDTA_latin1 = fmap (readDTA . decodeLatin1) . B.readFile

readFileDTA_utf8 :: FilePath -> IO (DTA T.Text)
readFileDTA_utf8 = fmap (readDTA . decodeUtf8) . B.readFile

readFileDTA' :: (MonadIO m) => FilePath -> StackTraceT m (DTA T.Text)
readFileDTA' f = readFileDTA_utf8' f `catchError` \_ -> readFileDTA_latin1' f

readFileDTA_latin1' :: (MonadIO m) => FilePath -> StackTraceT m (DTA T.Text)
readFileDTA_latin1' f = inside ("DTA file: " ++ show f) $ do
  liftIO (tryIOError $ B.readFile f) >>= \case
    Left err -> fatal $ show err
    Right bs -> scanStack (removeBOM $ decodeLatin1 bs) >>= parseStack

readFileDTA_utf8' :: (MonadIO m) => FilePath -> StackTraceT m (DTA T.Text)
readFileDTA_utf8' f = inside ("DTA file: " ++ show f) $ do
  liftIO (tryIOError $ B.readFile f) >>= \case
    Left err -> fatal $ show err
    Right bs -> case decodeUtf8' bs of
      Left err  -> inside "decoding as utf-8" $ fatal $ show err
      Right txt -> scanStack (removeBOM txt) >>= parseStack

writeFileDTA_latin1 :: FilePath -> DTA T.Text -> IO ()
writeFileDTA_latin1 fp dta = B.writeFile fp $ encodeLatin1 $ showDTA dta
  where encodeLatin1 = B8.pack . T.unpack

writeFileDTA_utf8 :: FilePath -> DTA T.Text -> IO ()
writeFileDTA_utf8 fp dta = B.writeFile fp $ encodeUtf8 $ showDTA dta
