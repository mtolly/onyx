{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module MoggDecrypt (moggToOgg) where

#ifdef MOGGDECRYPT
import qualified Sound.MOGG
#else
import           Control.Applicative  (liftA2)
import           Data.Binary.Get      (getWord32le, runGet)
import qualified Data.ByteString.Lazy as BL
#endif

-- | Only supports unencrypted MOGGs, unless you provide an external decryptor.
moggToOgg :: FilePath -> FilePath -> IO ()
#ifdef MOGGDECRYPT
moggToOgg mogg ogg = Sound.MOGG.moggToOgg mogg ogg >>= \case
  True  -> return ()
  False -> error $ "Couldn't decrypt MOGG: " ++ mogg
#else
moggToOgg mogg ogg = do
  bs <- BL.readFile mogg
  let (moggType, oggStart) = runGet (liftA2 (,) getWord32le getWord32le) bs
  if moggType == 0xA
    then BL.writeFile ogg $ BL.drop (fromIntegral oggStart) bs
    else error "moggToOgg: not a supported MOGG file type"
#endif
