{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module DDR.SM where

import qualified Data.ByteString as B
import qualified Data.Text       as T
import           Text.Decode     (decodeGeneral)

data Token
  = Hash
  | Atom String
  | Colon
  | Semicolon
  deriving (Eq, Show)

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe _ [] = ([], [])
spanMaybe f xs@(x : xt) = case f x of
  Nothing -> ([], xs)
  Just y -> case spanMaybe f xt of
    (yes, no) -> (y : yes, no)

scan :: String -> [Token]
scan = let
  go ""              = []
  go ('\\' : c : cs) = Atom [c]  : go cs
  go ('#' : cs)      = Hash      : go cs
  go (':' : cs)      = Colon     : go cs
  go (';' : cs)      = Semicolon : go cs
  go (c : cs)        = Atom [c]  : go cs
  condense []              = []
  condense (Atom s : toks) = let
    (strs, rest) = spanMaybe (\case Atom a -> Just a; _ -> Nothing) toks
    in Atom (concat $ s : strs) : condense rest
  condense (tok : toks)    = tok : condense toks
  in condense . go

parse :: [Token] -> [(T.Text, [T.Text])]
parse toks = case break (== Hash) toks of
  (_, Hash : toks') -> let
    (line, after) = break (== Semicolon) toks'
    in case map T.pack [ s | Atom s <- line ] of
      []           -> parse after
      atom : atoms -> (atom, atoms) : parse after
  _ -> []

-- | Scan and parse .sm or .ssc files.
loadSMLines :: FilePath -> IO [(T.Text, [T.Text])]
loadSMLines f = do
  txt <- decodeGeneral <$> B.readFile f
  -- first remove comments
  let noComments = case T.splitOn "//" txt of
        []     -> ""
        t : ts -> T.concat $ t : map (T.dropWhile (/= '\n')) ts
  return $ parse $ scan $ T.unpack noComments
