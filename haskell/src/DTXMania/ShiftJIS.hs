module DTXMania.ShiftJIS (decodeShiftJIS, kakasi) where

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import           Data.Tuple          (swap)
import           Kakasi
import           Resources           (itaijidict, kanwadict, shiftJISTable)
import           System.Environment  (setEnv)

kakasi :: [String] -> String -> IO String
kakasi args str = do
  kanwadict >>= setEnv "KANWADICT"
  itaijidict >>= setEnv "ITAIJIDICT"
  n <- kakasiArgs $ "onyx-kakasi" : "-i" : "sjis" : args
  case n of
    0 -> decodeShiftJIS <$> kakasiDo (encodeShiftJIS str)
    _ -> error $ "DTXMania.ShiftJIS.kakasi: got non-zero error code " <> show n

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
