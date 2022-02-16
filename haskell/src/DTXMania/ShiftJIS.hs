{-# LANGUAGE CPP #-}
module DTXMania.ShiftJIS (decodeShiftJIS, kakasi) where

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import           Data.Tuple          (swap)
import           Kakasi
import           Resources           (itaijidict, kanwadict, shiftJISTable)

#ifdef WINDOWS
import           Control.Monad       (when)
import           Foreign.C           (CInt (..), CString, withCString)
#else
import           System.Environment  (setEnv)
#endif

kakasi :: [String] -> String -> IO String
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

encodeShiftJIS :: String -> B.ByteString
encodeShiftJIS = B.concat . map
  (\c -> fromMaybe (B.pack [0x81, 0xA1]) -- black square (■)
  $ HM.lookup c shiftJISReverse
  )

decodeShiftJIS :: B.ByteString -> String
decodeShiftJIS b = if B.null b
  then ""
  else case B.splitAt 1 b of
    (h1, t1) -> case HM.lookup h1 shiftJISTable of
      Just c -> c : decodeShiftJIS t1
      Nothing -> if B.length b >= 2
        then case B.splitAt 2 b of
          (h2, t2) -> case HM.lookup h2 shiftJISTable of
            Just c  -> c : decodeShiftJIS t2
            Nothing -> '�' : decodeShiftJIS t1
        else "�"

shiftJISReverse :: HM.HashMap Char B.ByteString
shiftJISReverse = HM.fromList $ map swap $ HM.toList shiftJISTable
