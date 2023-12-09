{-# LANGUAGE CPP #-}
module Onyx.Util.ShiftJIS (decodeShiftJIS, kakasi) where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TB
import           Data.Tuple              (swap)
import           Onyx.Kakasi
import           Onyx.Resources          (itaijidict, kanwadict, shiftJISTable)

#ifdef WINDOWS
import           Control.Monad           (when)
import           Foreign.C               (CInt (..), CString, withCString)
#else
import           System.Environment      (setEnv)
#endif

kakasi :: [String] -> T.Text -> IO T.Text
kakasi args str = do
  kanwadict >>= setEnvKakasi "KANWADICT"
  itaijidict >>= setEnvKakasi "ITAIJIDICT"
  n <- kakasiArgs $ "onyx-kakasi" : "-i" : "sjis" : args
  case n of
    0 -> decodeShiftJIS <$> kakasiDo (encodeShiftJIS str)
    _ -> error $ "DTXMania.ShiftJIS.kakasi: got non-zero error code " <> show n

{-
hack to fix crash on windows.
haskell setEnv on windows tries to be good and call SetEnvironmentVariableW.
but then on the C side, kakasi calls getenv, and it does not see the variable change
(the variable remains whatever it was on process launch).
according to https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/getenv-wgetenv
these two tables (char* and wchar_t*) don't necessarily stay in sync...
so instead, we use putenv, which seems to update both methods.
-}

setEnvKakasi :: String -> String -> IO ()
#ifdef WINDOWS
setEnvKakasi k v = withCString (k <> "=" <> v) $ \cstr -> do
  res <- c_putenv cstr
  when (res /= 0) $ fail $ "setEnvKakasi: couldn't set variable, code " <> show res

foreign import ccall unsafe "putenv"
  c_putenv :: CString -> IO CInt
#else
setEnvKakasi = setEnv
#endif

encodeShiftJIS :: T.Text -> B.ByteString
encodeShiftJIS = BL.toStrict . BB.toLazyByteString . T.foldl'
  (\build c -> build <> case HM.lookup c shiftJISReverse of
    Just jis -> BB.byteString jis
    Nothing  -> blackSquare
  ) mempty
  where blackSquare = BB.byteString $ B.pack [0x81, 0xA1] -- "■"

decodeShiftJIS :: B.ByteString -> T.Text
decodeShiftJIS = TL.toStrict . TB.toLazyText . go where
  go b = if B.null b
    then mempty
    else case B.splitAt 1 b of
      (h1, t1) -> case HM.lookup h1 shiftJISTable of
        Just c -> TB.singleton c <> go t1
        Nothing -> if B.length b >= 2
          then case B.splitAt 2 b of
            (h2, t2) -> case HM.lookup h2 shiftJISTable of
              Just c  -> TB.singleton c <> go t2
              Nothing -> TB.singleton '�' <> go t1
          else TB.singleton '�'

shiftJISReverse :: HM.HashMap Char B.ByteString
shiftJISReverse = HM.fromList $ map swap $ HM.toList shiftJISTable
