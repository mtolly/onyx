{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Util.Text.Transform where

import           Control.Monad.IO.Class
import           Data.Char              (isLatin1, toUpper)
import           Data.Fixed             (Milli)
import qualified Data.Text              as T
import           Onyx.Util.ShiftJIS     (kakasi)
import qualified Sound.MIDI.Util        as U

-- | Transform a string to Latin-1.
-- Allows Ÿ (not Latin-1) and ÿ (is Latin-1) for RB, but not in Magma .rbproj.
replaceCharsRB :: (MonadIO m) => Bool -> T.Text -> m T.Text
replaceCharsRB rbproj txt = liftIO $ let
  jpnRanges =
    [ (0x3000, 0x30ff) -- Japanese-style punctuation, Hiragana, Katakana
    , (0xff00, 0xffef) -- Full-width roman characters and half-width katakana
    , (0x4e00, 0x9faf) -- CJK unifed ideographs - Common and uncommon kanji
    ]
  isJapanese c = let
    point = fromEnum c
    in any (\(pmin, pmax) -> pmin <= point && point <= pmax) jpnRanges
  nonBreakingSpace = '\8203'
  latinifier s = flip T.map (T.filter (/= nonBreakingSpace) s) $ \case
    -- TODO should replace this with either text-icu or utf8proc, using normalization
    'ÿ' | rbproj -> 'y'
    'Ÿ' | rbproj -> 'Y'
    '–'          -> '-' -- en dash
    '—'          -> '-' -- em dash
    -- various japanese chars
    '\x301C'     -> '~' -- "wave dash"
    '\xFF5E'     -> '~' -- "full width tilde"
    '、'          -> ','
    '。'          -> '.'
    '｛'          -> '{'
    '｝'          -> '}'
    '（'          -> '('
    '）'          -> ')'
    '［'          -> '['
    '］'          -> ']'
    '【'          -> '['
    '】'          -> ']'
    '「'          -> '['
    '」'          -> ']'
    '：'          -> ':'
    -- random chars from a Eximperituserqethhzebibšiptugakkathšulweliarzaxułum song
    'ł'          -> 'l'
    'ź'          -> 'z'
    'š'          -> 's'
    'č'          -> 'c'
    'ę'          -> 'e'
    'ń'          -> 'n'
    'Ŭ'          -> 'U'
    'ś'          -> 's'
    'ć'          -> 'c'
    'ĺ'          -> 'l'
    'Ž'          -> 'Z'
    'Š'          -> 'S'
    'ī'          -> 'i'
    'ŭ'          -> 'u'
    c            -> if isLatin1 c || c == 'Ÿ' || c == 'ÿ' then c else '?'
  in if T.any isJapanese txt
    then do
      -- only '\x301C' (wave dash) is in the shift jis mapping
      let txt' = T.replace "\xFF5E" "\x301C" $ T.replace "~" "\x301C" txt
      -- this used to include `-Ea` for unrecognized characters
      rom <- kakasi (words "-Ha -Ka -Ja -ka -s") $ T.unpack txt'
      let capital s = case T.uncons s of
            Nothing      -> s
            Just (c, cs) -> T.cons (toUpper c) cs
      return
        -- T.replace "(kigou)" "?" -- only needed with -Ea
        $ T.replace "^" "-" -- kakasi writes long vowels as ^
        -- get rid of extra space around punctuation
        $ T.replace "( " "("
        $ T.replace " )" ")"
        $ T.replace "[ " "["
        $ T.replace " ]" "]"
        $ T.replace "{ " "{"
        $ T.replace " }" "}"
        $ T.replace " ," ","
        $ T.replace " ." "."
        $ T.replace " !" "!"
        $ T.replace " ?" "?"
        $ T.unwords $ map capital $ T.words
        $ latinifier
        $ T.pack rom
    else return $ latinifier txt

showTimestamp :: U.Seconds -> T.Text
showTimestamp secs = let
  minutes = floor $ secs / 60 :: Int
  seconds = secs - realToFrac minutes * 60
  milli = realToFrac seconds :: Milli
  pad = if milli < 10 then "0" else ""
  in T.pack $ show minutes ++ ":" ++ pad ++ show milli
