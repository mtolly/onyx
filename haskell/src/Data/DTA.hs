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
, readFileDTA_latin1', readFileDTA_utf8'
, readDTABytes, readDTASections
, writeFileDTA_latin1, writeFileDTA_utf8
, renumberFrom
, removeBOM
) where

import           Control.Exception.Extra
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.StackTrace (StackTraceT, fatal, inside)
import           Data.Binary                    (decode, encode)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.DTA.Base
import           Data.DTA.Lex
import           Data.DTA.Parse
import           Data.DTA.PrettyPrint
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeLatin1, decodeUtf8,
                                                 decodeUtf8', encodeUtf8)
import           System.IO.Error                (tryIOError)
import           Text.Decode                    (removeBOM)

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

readFileDTA :: FilePath -> IO (DTA T.Text)
readFileDTA f = readFileDTA_utf8 f `catch_` \_ -> readFileDTA_latin1 f

readFileDTA_latin1 :: FilePath -> IO (DTA T.Text)
readFileDTA_latin1 = fmap (readDTA . decodeLatin1) . B.readFile

readFileDTA_utf8 :: FilePath -> IO (DTA T.Text)
readFileDTA_utf8 = fmap (readDTA . decodeUtf8) . B.readFile

readFileDTA_latin1' :: (MonadIO m) => FilePath -> StackTraceT m (DTA T.Text)
readFileDTA_latin1' f = inside ("DTA file: " ++ show f) $ do
  liftIO (tryIOError $ B.readFile f) >>= \case
    Left err -> fatal $ show err
    Right bs -> scanStack (removeBOM $ decodeLatin1 bs) >>= parseStack

readDTABytes :: (Monad m) => B.ByteString -> StackTraceT m (DTA B.ByteString)
readDTABytes bs = do
  dta <- scanStack (removeBOM $ decodeLatin1 bs) >>= parseStack
  return $ B8.pack . T.unpack <$> dta

readDTASections :: (Monad m) => B.ByteString -> StackTraceT m [(B.ByteString, Chunk B.ByteString)]
readDTASections bs = do
  sects <- scanStack (removeBOM $ decodeLatin1 bs) >>= parseStackPositions
  return $ do
    (sect, next) <- zip sects $ map Just (drop 1 sects) ++ [Nothing]
    let AlexPn startBytes _ _ = fst sect
        taker = case next of
          Nothing                       -> id
          Just (AlexPn endBytes _ _, _) -> B.take $ endBytes - startBytes
        sectBS = taker $ B.drop startBytes bs
    return (sectBS, B8.pack . T.unpack <$> snd sect)

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
