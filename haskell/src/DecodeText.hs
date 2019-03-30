{-# LANGUAGE OverloadedStrings #-}
module DecodeText (decodeGeneral, removeBOM) where

import           Control.Applicative      ((<|>))
import qualified Data.ByteString          as B
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error (lenientDecode)

decodeGeneral :: B.ByteString -> T.Text
decodeGeneral bs
  | Just bs' <- B.stripPrefix (B.pack [0xFE, 0xFF]) bs = TE.decodeUtf16BEWith lenientDecode bs'
  | Just bs' <- B.stripPrefix (B.pack [0xFF, 0xFE]) bs = TE.decodeUtf16LEWith lenientDecode bs'
  | Just bs' <- B.stripPrefix (B.pack [0xEF, 0xBB, 0xBF]) bs = TE.decodeUtf8With lenientDecode bs'
  | otherwise = removeBOM $ case TE.decodeUtf8' bs of
    Right t -> t
    Left  _ -> TE.decodeLatin1 bs

removeBOM :: T.Text -> T.Text
removeBOM s = fromMaybe s
  $   T.stripPrefix "\xFEFF"       s -- normal unicode-decoded BOM
  <|> T.stripPrefix "\xEF\xBB\xBF" s -- latin1 decoding of a utf8 BOM
