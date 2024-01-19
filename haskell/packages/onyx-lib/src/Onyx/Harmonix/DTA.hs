-- | All functions that take a 'Handle' or 'FilePath' do their reading/writing
-- strictly.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Harmonix.DTA
( DTA(..), Tree(..), Chunk(..)
, decodeDTB, encodeDTB
, decodeDecryptDTB
, readFileDTB, writeFileDTB
, readDTA, showDTA
, readFileDTA, readFileDTA_latin1, readFileDTA_utf8
, readFileDTA_latin1', readFileDTA_utf8'
, readDTABytes, readDTASections, readDTA_latin1
, writeFileDTA_latin1, writeFileDTA_utf8
, renumberFrom
, removeBOM
) where

import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.Binary             (decodeOrFail, encode)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as B8
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text               as T
import           Data.Text.Encoding      (decodeLatin1, decodeUtf8, decodeUtf8',
                                          encodeUtf8)
import           Onyx.Harmonix.DTA.Base
import           Onyx.Harmonix.DTA.Crypt (decrypt, newCrypt, oldCrypt)
import           Onyx.Harmonix.DTA.Parse
import           Onyx.Harmonix.DTA.Print
import           Onyx.Harmonix.DTA.Scan
import           Onyx.StackTrace         (StackTraceT, fatal, inside)
import           Onyx.Util.Text.Decode   (decodeGeneral, encodeLatin1,
                                          removeBOM)
import           System.IO.Error         (tryIOError)

decodeDTBEither :: BL.ByteString -> Either String (DTA B.ByteString)
decodeDTBEither bs = case decodeOrFail bs of
  Right (_, _, x)       -> Right x
  Left (_, offset, err) -> Left $ "Decode error at byte offset " <> show offset <> ": " <> err

decodeDTB :: (MonadFail m) => BL.ByteString -> m (DTA B.ByteString)
decodeDTB = either fail return . decodeDTBEither

decodeDecryptDTB :: (MonadFail m) => BL.ByteString -> m (DTA B.ByteString)
decodeDecryptDTB bs = case decodeDTBEither $ decrypt oldCrypt bs of
  Left errOld -> case decodeDTBEither $ decrypt newCrypt bs of
    Left errNew -> fail $ concat
      [ "Couldn't decode encrypted .dtb"
      , "- Old encryption error: " <> errOld
      , "- New encryption error: " <> errNew
      ]
    Right x -> return x
  Right x -> return x

encodeDTB :: DTA B.ByteString -> BL.ByteString
encodeDTB = encode

readFileDTB :: FilePath -> IO (DTA B.ByteString)
readFileDTB f = B.readFile f >>= decodeDTB . BL.fromStrict

writeFileDTB :: FilePath -> DTA B.ByteString -> IO ()
writeFileDTB fp dta = BL.writeFile fp $ encodeDTB dta

readDTA :: T.Text -> DTA T.Text
readDTA = parse . scan . removeBOM

readFileDTA :: FilePath -> IO (DTA T.Text)
readFileDTA = fmap (readDTA . decodeGeneral) . B.readFile

readFileDTA_latin1 :: FilePath -> IO (DTA T.Text)
readFileDTA_latin1 = fmap (readDTA . decodeLatin1) . B.readFile

readFileDTA_utf8 :: FilePath -> IO (DTA T.Text)
readFileDTA_utf8 = fmap (readDTA . decodeUtf8) . B.readFile

readFileDTA_latin1' :: (MonadIO m) => FilePath -> StackTraceT m (DTA T.Text)
readFileDTA_latin1' f = inside ("DTA file: " ++ show f) $ do
  liftIO (tryIOError $ B.readFile f) >>= \case
    Left err -> fatal $ show err
    Right bs -> scanStack (removeBOM $ decodeLatin1 bs) >>= parseStack

readDTA_latin1 :: (Monad m) => B.ByteString -> StackTraceT m (DTA T.Text)
readDTA_latin1 bs = scanStack (removeBOM $ decodeLatin1 bs) >>= parseStack

readDTABytes :: (Monad m) => B.ByteString -> StackTraceT m (DTA B.ByteString)
readDTABytes bs = fmap (B8.pack . T.unpack) <$> readDTA_latin1 bs

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

writeFileDTA_utf8 :: FilePath -> DTA T.Text -> IO ()
writeFileDTA_utf8 fp dta = B.writeFile fp $ encodeUtf8 $ showDTA dta
