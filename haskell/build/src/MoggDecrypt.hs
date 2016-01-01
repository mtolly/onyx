module MoggDecrypt (moggToOgg) where

import Data.Binary.Get (runGet, getWord32le)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Control.Applicative (liftA2)
import Resources (mogg2ogg)
import System.Info (os)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

-- | Only supports unencrypted MOGGs, unless you provide an external decryptor.
moggToOgg :: FilePath -> FilePath -> IO ()
moggToOgg mogg ogg = if False
  then withSystemTempDirectory "mogg2ogg" $ \tmp -> do
    let exe = tmp </> "mogg2ogg.exe"
    B.writeFile exe mogg2ogg
    withExe callProcess exe [mogg, ogg]
  else do
    bs <- BL.readFile mogg
    let (moggType, oggStart) = runGet (liftA2 (,) getWord32le getWord32le) bs
    if moggType == 0xA
      then BL.writeFile ogg $ BL.drop (fromIntegral oggStart) bs
      else error "moggToOgg: not a supported MOGG file type"

withExe :: (FilePath -> [String] -> a) -> FilePath -> [String] -> a
withExe f exe args = if os == "mingw32"
  then f exe args
  else f "mono" $ exe : args
