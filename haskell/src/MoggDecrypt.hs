{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module MoggDecrypt (moggToOgg) where

import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
#ifdef MOGGDECRYPT
import qualified Sound.MOGG
#else
import           Control.Applicative            (liftA2)
import           Data.Binary.Get                (getWord32le, runGet)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toUpper)
import           Numeric                        (showHex)
#endif

-- | Only supports unencrypted MOGGs, unless you provide an external decryptor.
moggToOgg :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
#ifdef MOGGDECRYPT
moggToOgg mogg ogg = liftIO (Sound.MOGG.moggToOgg mogg ogg) >>= \case
  True  -> return ()
  False -> fatal $ "Couldn't decrypt MOGG: " ++ mogg
#else
moggToOgg mogg ogg = do
  bs <- liftIO $ BL.readFile mogg
  let (moggType, oggStart) = runGet (liftA2 (,) getWord32le getWord32le) bs
      hex = "0x" <> map toUpper (showHex moggType "")
  if moggType == 0xA
    then liftIO $ BL.writeFile ogg $ BL.drop (fromIntegral oggStart) bs
    else fatal $ "moggToOgg: encrypted MOGG (type " <> hex <> ") not supported"
#endif
