{-# LANGUAGE CPP #-}
module MoggDecrypt (moggToOgg) where

#ifdef MOGGDECRYPT
import qualified Data.ByteString as B
import Resources (mogg2ogg)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import System.Info (os)
#else
import Data.Binary.Get (runGet, getWord32le)
import qualified Data.ByteString.Lazy as BL
import Control.Applicative (liftA2)
#endif

-- | Only supports unencrypted MOGGs, unless you provide an external decryptor.
moggToOgg :: FilePath -> FilePath -> IO ()
#ifdef MOGGDECRYPT
moggToOgg mogg ogg = withSystemTempDirectory "mogg2ogg" $ \tmp -> do
  let exe = tmp </> "mogg2ogg.exe"
  B.writeFile exe mogg2ogg
  withExe callProcess exe [mogg, ogg]
#else
moggToOgg mogg ogg = do
  bs <- BL.readFile mogg
  let (moggType, oggStart) = runGet (liftA2 (,) getWord32le getWord32le) bs
  if moggType == 0xA
    then BL.writeFile ogg $ BL.drop (fromIntegral oggStart) bs
    else error "moggToOgg: not a supported MOGG file type"
#endif

#ifdef MOGGDECRYPT
withExe :: (FilePath -> [String] -> a) -> FilePath -> [String] -> a
withExe f exe args = if os == "mingw32"
  then f exe args
  else f "mono" $ exe : args
#endif
