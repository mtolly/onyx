{-# LANGUAGE LambdaCase #-}
module Resources where

import qualified Codec.Picture      as P
import qualified Data.ByteString    as B
import qualified Data.DTA           as D
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8)
import           System.Directory   (getHomeDirectory)
import           System.Environment (getExecutablePath)
import           System.FilePath    (takeDirectory, takeFileName, (</>))
import           System.IO.Unsafe   (unsafePerformIO)

getResourcesPath :: FilePath -> IO FilePath
getResourcesPath f = do
  exe <- getExecutablePath
  resDir <- if takeFileName exe == "ghc"
    then (</> ".local/bin/onyx-resources") <$> getHomeDirectory -- we're in ghci, use installed resources
    else return $ takeDirectory exe </> "onyx-resources"
  return $ resDir </> f

magmaV1Dir, magmaV2Dir, magmaCommonDir, rb3Updates, kanwadict, itaijidict :: IO FilePath
magmaV1Dir       = getResourcesPath "magma-v1"
magmaV2Dir       = getResourcesPath "magma-v2"
magmaCommonDir   = getResourcesPath "magma-common"
rb3Updates       = getResourcesPath "rb3-updates"
kanwadict        = getResourcesPath "kanwadict"
itaijidict       = getResourcesPath "itaijidict"

xboxKV :: IO FilePath
xboxKV = getResourcesPath "KV.bin"

rb3Thumbnail :: IO FilePath
rb3Thumbnail = getResourcesPath "rb3.png"

rb2Thumbnail :: IO FilePath
rb2Thumbnail = getResourcesPath "rb2.png"

emptyMilo :: IO FilePath
emptyMilo = getResourcesPath "empty.milo_xbox"

emptyMiloRB2 :: IO FilePath
emptyMiloRB2 = getResourcesPath "empty-rb2.milo_xbox"

emptyWeightsRB2 :: IO FilePath
emptyWeightsRB2 = getResourcesPath "empty-rb2_weights.bin"

webDisplay :: IO FilePath
webDisplay = getResourcesPath "player"

onyxAlbum :: IO (P.Image P.PixelRGB8)
onyxAlbum = getResourcesPath "album.png" >>= P.readImage >>= \case
  Left  err -> error $ "panic! couldn't decode default album art into image: " ++ err
  Right dyn -> return $ P.convertRGB8 dyn

{-# NOINLINE missingSongData #-}
missingSongData :: D.DTA T.Text
missingSongData = unsafePerformIO $ do
  getResourcesPath "missing_song_data.dta" >>= fmap (D.readDTA . decodeUtf8) . B.readFile

colorMapDrums, colorMapGRYBO, colorMapGHL :: IO FilePath
colorMapDrums = getResourcesPath "rockband_drums.png"
colorMapGRYBO = getResourcesPath "rockband_guitarbass.png"
colorMapGHL   = getResourcesPath "rockband_ghl.png"
