{-# LANGUAGE TemplateHaskell #-}
module Magma where

import Data.FileEmbed (embedDir)
import qualified Data.ByteString as B
import qualified System.Directory as Dir
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Process (callProcess, readProcess)
import Control.Monad (forM_)
import Control.Exception (bracket_)
import Data.Word (Word32)
import qualified System.IO as IO
import Data.Bits (shiftL)
import System.FilePath ((</>))

import qualified Sound.File.Sndfile as Snd
import Data.Conduit.Audio (Duration(..), silent, AudioSource)
import Data.Conduit.Audio.Sndfile (sinkSnd)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Int (Int16)

magmaFiles :: [(FilePath, B.ByteString)]
magmaFiles = $(embedDir "magma/")

withExe :: (FilePath -> [String] -> IO a) -> FilePath -> [String] -> IO a
withExe f exe args = if os == "mingw32"
  then f exe args
  else f "wine" $ exe : args

runMagmaMIDI :: FilePath -> FilePath -> IO ()
runMagmaMIDI proj mid = withSystemTempDirectory "magma" $ \tmp -> do
  wd <- Dir.getCurrentDirectory
  let proj' = wd </> proj
      mid'  = wd </> mid
  bracket_ (Dir.setCurrentDirectory tmp) (Dir.setCurrentDirectory wd) $ do
    Dir.createDirectory "gen"
    forM_ magmaFiles $ uncurry B.writeFile
    withExe callProcess "MagmaCompilerC3.exe" ["-export_midi", proj', mid']

runMagma :: FilePath -> FilePath -> IO ()
runMagma proj rba = withSystemTempDirectory "magma" $ \tmp -> do
  wd <- Dir.getCurrentDirectory
  let proj' = wd </> proj
      rba'  = wd </> rba
  bracket_ (Dir.setCurrentDirectory tmp) (Dir.setCurrentDirectory wd) $ do
    Dir.createDirectory "gen"
    forM_ magmaFiles $ uncurry B.writeFile
    withExe callProcess "MagmaCompilerC3.exe" [proj', rba']

oggToMogg :: FilePath -> FilePath -> IO ()
oggToMogg ogg mogg = withSystemTempDirectory "ogg2mogg" $ \tmp -> do
  wd <- Dir.getCurrentDirectory
  let ogg'  = wd </> ogg
      mogg' = wd </> mogg
  bracket_ (Dir.setCurrentDirectory tmp) (Dir.setCurrentDirectory wd) $ do
    Dir.createDirectory "gen"
    forM_ magmaFiles $ uncurry B.writeFile
    Dir.renameFile "oggenc-redirect.exe" "oggenc.exe"
    Dir.copyFile ogg' "audio.ogg"
    let proj = "hellskitchen.rbproj"
        rba = "out.rba"
    runResourceT
      $ sinkSnd "silence.wav"
      (Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile)
      (silent (Seconds 31) 44100 2 :: AudioSource (ResourceT IO) Int16)
    _ <- withExe
      (\exe args -> readProcess exe args "")
     "MagmaCompilerC3.exe"
     [proj, rba]
    IO.withBinaryFile rba IO.ReadMode $ \hrba -> do
      IO.hSeek hrba IO.AbsoluteSeek $ 4 + (4 * 3)
      moggOffset <- hReadWord32le hrba
      IO.hSeek hrba IO.AbsoluteSeek $ 4 + (4 * 10)
      moggLength <- hReadWord32le hrba
      IO.hSeek hrba IO.AbsoluteSeek $ fromIntegral moggOffset
      moggData <- B.hGet hrba $ fromIntegral moggLength
      B.writeFile mogg' moggData

hReadWord32le :: IO.Handle -> IO Word32
hReadWord32le h = do
  [a, b, c, d] <- fmap (map fromIntegral . B.unpack) $ B.hGet h 4
  return $ a + (b `shiftL` 8) + (c `shiftL` 16) + (d `shiftL` 24)
