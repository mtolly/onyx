module Onyx.Util.Binary where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import           Numeric              (showHex)

runGetM :: (MonadFail m) => Get a -> BL.ByteString -> m a
runGetM = runGetMOffset 0

runGetMOffset :: (MonadFail m) => Int64 -> Get a -> BL.ByteString -> m a
runGetMOffset offset g bs = case runGetOrFail g bs of
  Left (_, pos, err) -> fail $ "Binary parse error at position 0x" <> showHex (pos + offset) ": " <> err
  Right (_, _, x) -> return x
