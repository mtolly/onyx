-- | All functions that take a 'Handle' or 'FilePath' do their reading/writing
-- strictly.
{-# LANGUAGE CPP #-}
module Data.DTA
( DTA(..), Tree(..), Chunk(..)
, decodeDTB, encodeDTB
, readFileDTB, writeFileDTB
, readDTA, showDTA
, readFileDTA, readFileDTA_latin1, readFileDTA_utf8
, writeFileDTA_latin1, writeFileDTA_utf8
, renumberFrom
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import Data.Binary (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO.Extra
import Control.Exception.Extra

import Data.DTA.Base
import Data.DTA.Lex
import Data.DTA.Parse
import Data.DTA.PrettyPrint

decodeDTB :: BL.ByteString -> DTA B.ByteString
decodeDTB = decode

encodeDTB :: DTA B.ByteString -> BL.ByteString
encodeDTB = encode

readFileDTB :: FilePath -> IO (DTA B.ByteString)
readFileDTB = fmap (decodeDTB . BL.fromStrict) . B.readFile

writeFileDTB :: FilePath -> DTA B.ByteString -> IO ()
writeFileDTB fp dta = BL.writeFile fp $ encodeDTB dta

readDTA :: String -> DTA String
readDTA = parse . scan . removeBOM

removeBOM :: String -> String
removeBOM ('\xFEFF' : s)                 = s -- normal unicode-decoded BOM
removeBOM ('\xEF' : '\xBB' : '\xBF' : s) = s -- latin1 decoding of a utf8 BOM
removeBOM s                              = s

readFileDTA :: FilePath -> IO (DTA String)
readFileDTA f = readFileDTA_utf8 f `catch_` \_ -> readFileDTA_latin1 f

readFileDTA_latin1 :: FilePath -> IO (DTA String)
readFileDTA_latin1 = fmap readDTA . readFileEncoding' latin1

readFileDTA_utf8 :: FilePath -> IO (DTA String)
readFileDTA_utf8 = fmap readDTA . readFileEncoding' utf8

writeFileDTA_latin1 :: FilePath -> DTA String -> IO ()
writeFileDTA_latin1 fp dta = writeFileEncoding latin1 fp $ showDTA dta

writeFileDTA_utf8 :: FilePath -> DTA String -> IO ()
writeFileDTA_utf8 fp dta = writeFileEncoding utf8 fp $ showDTA dta
