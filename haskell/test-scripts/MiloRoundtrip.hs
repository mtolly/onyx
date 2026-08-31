{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
-- Round-trip test: for each .milo_* file, decompress, parse the dir
-- structure, re-serialize, and compare against the decompressed bytes.
module Main (main) where

import           Control.Monad               (forM, forM_)
import           Data.Binary.Get             (runGetOrFail)
import qualified Data.ByteString.Lazy        as BL
import           Data.Int                    (Int64)
import           Data.List                   (isInfixOf)
import qualified Data.Map                    as Map
import           Onyx.Harmonix.RockBand.Milo
import           System.Directory            (doesDirectoryExist,
                                              listDirectory)
import           System.Environment          (getArgs)
import           System.FilePath             (takeExtension, (</>))

data Result
  = DecompressFail String
  | ParseFail String
  | Mismatch Int64 -- first differing byte offset
  | LengthDiff Int Int
  | OKRemainder Int -- parsed fine but some dir had leftover bytes before ADDE
  | OK
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  dirs <- getArgs
  files <- concat <$> mapM findMilos dirs
  results <- forM files \f -> do
    bs <- BL.readFile f
    let res = case runGetOrFail decompressMilo bs of
          Left (_, _, err) -> DecompressFail err
          Right (_, _, dec) -> case runGetOrFail parseMiloFile dec of
            Left (_, pos, err) -> ParseFail (err <> " @ " <> show pos)
            Right (_, _, dir) -> let
              out = makeMiloFile dir
              in if out == dec
                then if remainderBytes dir == 0
                  then OK
                  else OKRemainder (remainderBytes dir)
                else case firstDiff dec out of
                  Nothing -> LengthDiff (fromIntegral $ BL.length dec) (fromIntegral $ BL.length out)
                  Just i  -> Mismatch i
    res `seq` return (f, res)
  let tally = Map.fromListWith (+) [ (bucket r, 1 :: Int) | (_, r) <- results ]
      bucket = \case
        DecompressFail _ -> "decompress fail"
        ParseFail e      -> "parse fail: " <> takeWhile (/= '@') e
        Mismatch _       -> "roundtrip mismatch"
        LengthDiff _ _   -> "roundtrip length diff"
        OKRemainder _    -> "ok (leftover bytes)"
        OK               -> "ok"
  putStrLn $ "total files: " <> show (length files)
  forM_ (Map.toList tally) \(k, v) -> putStrLn $ show v <> "\t" <> k
  putStrLn "\n-- sample non-ok --"
  let bad = [ (f, r) | (f, r) <- results, r /= OK ]
  forM_ bad \(f, r) -> putStrLn $ f <> "\n  " <> show r

remainderBytes :: MiloDir -> Int
remainderBytes dir = fromIntegral (BL.length dir.miloObjectDir.odRemainder)
  + sum (map remainderBytes $ miloSubdirs dir)

firstDiff :: BL.ByteString -> BL.ByteString -> Maybe Int64
firstDiff a b = go 0 (BL.unpack a) (BL.unpack b) where
  go !i (x:xs) (y:ys) = if x == y then go (i+1) xs ys else Just i
  go _ _ _ = Nothing -- equal, or length difference

findMilos :: FilePath -> IO [FilePath]
findMilos d = do
  entries <- listDirectory d
  fmap concat $ forM entries \e -> do
    let full = d </> e
    isDir <- doesDirectoryExist full
    if isDir
      then findMilos full
      else return [ full | any (`isInfixOf` takeExtension full)
        ["milo_xbox", "milo_ps3", "milo_wii", "milo_ps2"] ]
