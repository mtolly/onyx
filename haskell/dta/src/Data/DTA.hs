-- | All functions that take a 'Handle' or 'FilePath' do their reading/writing
-- strictly.
{-# LANGUAGE CPP #-}
module Data.DTA
( DTA(..), Tree(..), Chunk(..)
, lFromDTB, hFromDTB, fromDTB
, lToDTB, hToDTB, toDTB
, sFromDTA, hFromDTA, fromDTA
, sToDTA, hToDTA, toDTA
, renumberFrom
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import System.IO (withFile, Handle, IOMode(ReadMode, WriteMode))

import Data.Binary (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Data.DTA.Base
import Data.DTA.Lex
import Data.DTA.Parse
import Data.DTA.PrettyPrint

lFromDTB :: BL.ByteString -> DTA
lFromDTB = decode

lToDTB :: DTA -> BL.ByteString
lToDTB = encode

fromDTB :: FilePath -> IO DTA
fromDTB fp = withFile fp ReadMode hFromDTB

hFromDTB :: Handle -> IO DTA
hFromDTB h = decode . strictToLazy <$> B.hGetContents h
  where strictToLazy b = BL.fromChunks [b]

toDTB :: FilePath -> DTA -> IO ()
toDTB fp dta = withFile fp WriteMode $ \h -> hToDTB h dta

hToDTB :: Handle -> DTA -> IO ()
hToDTB h dta = B.hPutStr h $ lazyToStrict $ encode dta
  where lazyToStrict = B.concat . BL.toChunks

sFromDTA :: String -> DTA
sFromDTA = parse . scan

bFromDTA :: B8.ByteString -> DTA
bFromDTA = sFromDTA . B8.unpack

hFromDTA :: Handle -> IO DTA
hFromDTA h = bFromDTA <$> B8.hGetContents h

fromDTA :: FilePath -> IO DTA
fromDTA fp = withFile fp ReadMode hFromDTA

bToDTA :: DTA -> B8.ByteString
bToDTA = B8.pack . sToDTA

hToDTA :: Handle -> DTA -> IO ()
hToDTA h = B8.hPutStr h . bToDTA

toDTA :: FilePath -> DTA -> IO ()
toDTA fp dta = withFile fp WriteMode $ \h -> hToDTA h dta
