{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Util.Text.Transform where

import           Control.Monad          (guard)
import           Control.Monad.IO.Class
import           Data.Char              (isLatin1, toUpper)
import           Data.Fixed             (Milli)
import           Data.Maybe             (mapMaybe)
import qualified Data.Text              as T
import           Data.Text.UTF8Proc
import           Onyx.Util.ShiftJIS     (kakasi)
import qualified Sound.MIDI.Util        as U

-- | Transform a string to (mostly) only Latin-1 characters.
-- Allows Ÿ (not Latin-1) and ÿ (is Latin-1) for RB, but not in Magma .rbproj.
replaceCharsRB :: (MonadIO m) => Bool -> T.Text -> m T.Text
replaceCharsRB _      txt | T.isAscii txt = return txt -- usual case
replaceCharsRB rbproj txt                 = liftIO $ let
  jpnRanges =
    [ (0x3000, 0x30ff) -- Japanese-style punctuation, Hiragana, Katakana
    , (0xff00, 0xffef) -- Full-width roman characters and half-width katakana
    , (0x4e00, 0x9faf) -- CJK unifed ideographs - Common and uncommon kanji
    ]
  isJapanese c = let
    point = fromEnum c
    in any (\(pmin, pmax) -> pmin <= point && point <= pmax) jpnRanges
  latinifier s = do
    -- NFD but also strip default ignorable chars such as non-breaking space
    nfd <- case utf8proc_map (utf8proc_STABLE <> utf8proc_DECOMPOSE <> utf8proc_IGNORE) s of
      Left  err -> fail $ "utf8proc error: " <> show err
      Right nfd -> return nfd
    return $ T.concat $ map latinCluster $ breakGraphemes nfd
  latinCluster cluster = case T.unpack $ T.filter isLatin1 cluster of
    core : _ -> T.singleton $ case mapMaybe (tryMark core cluster) marks of
      []           -> core
      combined : _ -> combined
    []       -> case mapMaybe (tryExtraMapping cluster) extraMapping of
      []        -> "?"
      extra : _ -> extra
  tryExtraMapping cluster (extraIn, extraOut) = do
    guard $ T.elem extraIn cluster
    Just extraOut
  extraMapping =
    [ ('–'     , "-") -- en dash
    , ('—'     , "-") -- em dash
    , ('Ĳ'     , "IJ")
    , ('ĳ'     , "ij")
    , ('Ł'     , "L")
    , ('ł'     , "l")
    -- japanese/fullwidth stuff
    , ('\x301C', "~") -- "wave dash"
    , ('\xFF5E', "~") -- "full width tilde"
    , ('、'     , ",")
    , ('。'     , ".")
    , ('｛'     , "{")
    , ('｝'     , "}")
    , ('（'     , "(")
    , ('）'     , ")")
    , ('［'     , "[")
    , ('］'     , "]")
    , ('【'     , "[")
    , ('】'     , "]")
    , ('「'     , "[")
    , ('」'     , "]")
    , ('：'     , ":")
    ]
  -- check for combining accent marks that result in a supported character
  tryMark core cluster mark = do
    guard $ T.elem mark cluster
    [composed] <- return $ either (const []) T.unpack $ utf8proc_NFC $ T.pack [core, mark]
    guard $ isLatin1 composed || (not rbproj && composed == 'Ÿ')
    guard $ not $ rbproj && composed == 'ÿ'
    Just composed
  marks =
    [ '\768' -- grave
    , '\769' -- acute
    , '\770' -- circumflex
    , '\771' -- tilde
    , '\776' -- umlaut
    , '\778' -- overring
    , '\807' -- cedilla
    ]
  in if T.any isJapanese txt
    then do
      -- only '\x301C' (wave dash) is in the shift jis mapping
      let txt' = T.replace "\xFF5E" "\x301C" $ T.replace "~" "\x301C" txt
      -- this used to include `-Ea` for unrecognized characters
      rom <- kakasi (words "-Ha -Ka -Ja -ka -s") txt'
      let capital s = case T.uncons s of
            Nothing      -> s
            Just (c, cs) -> T.cons (toUpper c) cs
      latin <- latinifier rom
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
        $ T.unwords $ map capital $ T.words latin
    else latinifier txt

showTimestamp :: U.Seconds -> T.Text
showTimestamp secs = let
  minutes = floor $ secs / 60 :: Int
  seconds = secs - realToFrac minutes * 60
  milli = realToFrac seconds :: Milli
  pad = if milli < 10 then "0" else ""
  in T.pack $ show minutes ++ ":" ++ pad ++ show milli
