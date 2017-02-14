{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module MoggDecrypt (moggToOgg) where

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.StackTrace
#ifdef MOGGDECRYPT
import qualified Sound.MOGG
#else
import           Control.Applicative            (liftA2)
import           Data.Binary.Get                (getWord32le, runGet)
import qualified Data.ByteString.Lazy           as BL
#endif

-- | Only supports unencrypted MOGGs, unless you provide an external decryptor.
moggToOgg :: FilePath -> FilePath -> StackTraceT IO ()
#ifdef MOGGDECRYPT
moggToOgg mogg ogg = liftIO (Sound.MOGG.moggToOgg mogg ogg) >>= \case
  True  -> return ()
  False -> fatal $ "Couldn't decrypt MOGG: " ++ mogg
#else
moggToOgg mogg ogg = do
  bs <- liftIO $ BL.readFile mogg
  let (moggType, oggStart) = runGet (liftA2 (,) getWord32le getWord32le) bs
  if moggType == 0xA
    then liftIO $ BL.writeFile ogg $ BL.drop (fromIntegral oggStart) bs
    else fatal $ "moggToOgg: not a supported MOGG file type (" ++ show moggType ++ ")"
#endif
