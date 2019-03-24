module MoggDecrypt (moggToOgg) where

import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import           Control.Applicative            (liftA2)
import           Data.Binary.Get                (getWord32le, runGet)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toUpper)
import           Numeric                        (showHex)

-- | Just strips the header off an unencrypted MOGG for now.
moggToOgg :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
moggToOgg mogg ogg = do
  bs <- liftIO $ BL.readFile mogg
  let (moggType, oggStart) = runGet (liftA2 (,) getWord32le getWord32le) bs
      hex = "0x" <> map toUpper (showHex moggType "")
  if moggType == 0xA
    then liftIO $ BL.writeFile ogg $ BL.drop (fromIntegral oggStart) bs
    else fatal $ "moggToOgg: encrypted MOGG (type " <> hex <> ") not supported"
