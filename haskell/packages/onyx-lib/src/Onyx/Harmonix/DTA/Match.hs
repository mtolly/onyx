{-
(claude) Emit an edited .dta trying to match formatting + comments from an original one
-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Harmonix.DTA.Match (matchFormatDTA) where

import           Data.Array.ST          (newArray, readArray, runSTUArray,
                                         writeArray)
import           Data.Array.Unboxed     (UArray, (!))
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (isSpace)
import           Data.Foldable          (foldl', forM_)
import qualified Data.Text.Encoding     as TE
import qualified Data.Vector            as V
import qualified Onyx.Harmonix.DTA      as D
import qualified Onyx.Harmonix.DTA.Scan as S
import           Onyx.StackTrace        (StackTraceT, fatal)
import           Onyx.Util.Text.Decode  (encodeLatin1)

toTokens :: D.Chunk s -> [S.Token s]
toTokens = \case
  D.Int i                  -> [S.Int i]
  D.Float f                -> [S.Float f]
  D.Var s                  -> [S.Var s]
  D.Sym s                  -> [S.Sym s]
  D.Unhandled              -> [S.Unhandled]
  D.IfDef s                -> [S.IfDef, S.Sym s]
  D.Else                   -> [S.Else]
  D.EndIf                  -> [S.EndIf]
  D.Parens (D.Tree _ xs)   -> [S.LParen] <> concatMap toTokens xs <> [S.RParen]
  D.Braces (D.Tree _ xs)   -> [S.LBrace] <> concatMap toTokens xs <> [S.RBrace]
  D.String s               -> [S.String s]
  D.Brackets (D.Tree _ xs) -> [S.LBracket] <> concatMap toTokens xs <> [S.RBracket]
  D.Define s               -> [S.Define, S.Sym s]
  D.Include s              -> [S.Include, S.Sym s]
  D.Merge s                -> [S.Merge, S.Sym s]
  D.IfNDef s               -> [S.IfNDef, S.Sym s]
  D.Autorun                -> [S.Autorun]
  D.Undef s                -> [S.Undef, S.Sym s]

-- | Text for a single token, when it has to be newly written out.
renderToken :: S.Token B.ByteString -> B.ByteString
renderToken = \case
  S.Int i     -> B8.pack $ show i
  S.Float f   -> B8.pack $ show f
  S.Var s     -> "$" <> s
  S.Sym s     -> renderSym s
  S.Unhandled -> "kDataUnhandled"
  S.IfDef     -> "#ifdef"
  S.Else      -> "#else"
  S.EndIf     -> "#endif"
  S.LParen    -> "("
  S.RParen    -> ")"
  S.LBrace    -> "{"
  S.RBrace    -> "}"
  S.String s  -> "\"" <> B8.concatMap (\case '"' -> "\\q"; c -> B8.singleton c) s <> "\""
  S.LBracket  -> "["
  S.RBracket  -> "]"
  S.Define    -> "#define"
  S.Include   -> "#include"
  S.Merge     -> "#merge"
  S.IfNDef    -> "#ifndef"
  S.Autorun   -> "#autorun"
  S.Undef     -> "#undef"

-- | Writes a symbol as a raw identifier if it scans back to the same symbol,
-- otherwise in single quotes.
renderSym :: B.ByteString -> B.ByteString
renderSym s = case S.scanEither $ TE.decodeLatin1 s of
  Right [(_, S.Sym s')] | encodeLatin1 s' == s -> s
  _ -> "'" <> B8.concatMap (\case '\'' -> "\\'"; c -> B8.singleton c) s <> "'"

data DiffOp
  = DiffBoth Int Int -- ^ index in first list, index in second list
  | DiffDel Int -- ^ index in first list
  | DiffIns Int -- ^ index in second list
  deriving (Eq, Show)

-- | Weighted longest-common-subsequence diff, returning an edit script in order.
-- The weight function returns 0 for elements that can't be matched.
-- Common prefix/suffix (by '==') are stripped first, then the remaining middle sections
-- use a full O(n*m) table, so this is meant for small edits to a song entry
-- rather than unrelated large inputs.
diffWeighted :: (Eq a) => (a -> a -> Int) -> V.Vector a -> V.Vector a -> [DiffOp]
diffWeighted weight xs ys = let
  n = V.length xs
  m = V.length ys
  pre = length $ takeWhile id $ V.toList $ V.zipWith (==) xs ys
  suf = length $ takeWhile id $ V.toList
    $ V.zipWith (==) (V.reverse $ V.drop pre xs) (V.reverse $ V.drop pre ys)
  midX = V.slice pre (n - pre - suf) xs
  midY = V.slice pre (m - pre - suf) ys
  mx = V.length midX
  my = V.length midY
  -- score ! (i, j) = best total weight of matches between (drop i midX) and (drop j midY)
  score :: UArray (Int, Int) Int
  score = runSTUArray do
    arr <- newArray ((0, 0), (mx, my)) 0
    forM_ [mx - 1, mx - 2 .. 0] \i -> forM_ [my - 1, my - 2 .. 0] \j -> do
      skipX <- readArray arr (i + 1, j)
      skipY <- readArray arr (i, j + 1)
      matched <- case weight (midX V.! i) (midY V.! j) of
        0 -> return 0
        w -> (+ w) <$> readArray arr (i + 1, j + 1)
      writeArray arr (i, j) $ maximum [skipX, skipY, matched]
    return arr
  walk i j
    | i == mx = [DiffIns $ pre + j' | j' <- [j .. my - 1]]
    | j == my = [DiffDel $ pre + i' | i' <- [i .. mx - 1]]
    | w <- weight (midX V.! i) (midY V.! j), w /= 0, score ! (i, j) == score ! (i + 1, j + 1) + w
      = DiffBoth (pre + i) (pre + j) : walk (i + 1) (j + 1)
    | score ! (i + 1, j) >= score ! (i, j + 1) = DiffDel (pre + i) : walk (i + 1) j
    | otherwise                                = DiffIns (pre + j) : walk i (j + 1)
  in [DiffBoth k k | k <- [0 .. pre - 1]]
    <> walk 0 0
    <> [DiffBoth (n - suf + k) (m - suf + k) | k <- [0 .. suf - 1]]

-- | Pairs each token with its bracket depth (matching open/close brackets get the same depth),
-- and for brackets, the first token inside them (usually a key symbol).
annotateTokens :: [S.Token s] -> [(Int, S.Token s, Maybe (S.Token s))]
annotateTokens = go [] where
  go _ [] = []
  go stack (t : ts)
    | isOpenToken t = let
      key = case ts of
        next : _ | not $ isCloseToken next -> Just next
        _                                  -> Nothing
      in (d, t, key) : go (key : stack) ts
    | isCloseToken t = case stack of
      key : stack' -> (d - 1, t, key) : go stack' ts
      []           -> (d, t, Nothing) : go stack ts
    | otherwise = (d, t, Nothing) : go stack ts
    where d = length stack
  isOpenToken = \case
    S.LParen   -> True
    S.LBrace   -> True
    S.LBracket -> True
    _          -> False
  isCloseToken = \case
    S.RParen   -> True
    S.RBrace   -> True
    S.RBracket -> True
    _          -> False

-- | How much to value keeping a token from the original text. Symbols are
-- used as waypoints, and brackets are preferred to match up with ones that
-- start with the same key.
tokenMatchWeight :: (Eq s) => (Int, S.Token s, Maybe (S.Token s)) -> (Int, S.Token s, Maybe (S.Token s)) -> Int
tokenMatchWeight (d1, t1, k1) (d2, t2, k2)
  | d1 /= d2 || t1 /= t2 = 0
  | otherwise = case t1 of
    S.Sym _ -> 3
    _       -> if k1 /= Nothing && k1 == k2 then 2 else 1

-- | Emits a new tree trying to match formatting of an existing dta, preserving whitespace/comments.
-- The token streams of the original text and the new tree are diffed
-- (only matching tokens at the same bracket depth, and weighting symbols higher
-- so that keys act as waypoints). Matched tokens are copied from the original
-- along with the whitespace/comments before them; inserted tokens are written
-- on one line, indented like their neighbors.
matchFormatDTA :: (Monad m) => B.ByteString -> [D.Chunk B.ByteString] -> StackTraceT m BL.ByteString
matchFormatDTA origWithBOM new = do
  let utf8BOM = "\xEF\xBB\xBF"
      (bom, orig) = if utf8BOM `B.isPrefixOf` origWithBOM
        then B.splitAt 3 origWithBOM
        else ("", origWithBOM)
  -- positions are character offsets, which equal byte offsets since we decode as latin-1
  spans <- S.scanSpansStack $ TE.decodeLatin1 orig
  let origSpans = V.fromList [ (s, e) | (S.AlexPn s _ _, S.AlexPn e _ _, _) <- spans ]
      origTokens = [ encodeLatin1 <$> tok | (_, _, tok) <- spans ]
      newTokens = V.fromList $ new >>= toTokens
      n = V.length origSpans

      rawOrig i = case origSpans V.! i of
        (s, e) -> B.take (e - s) $ B.drop s orig
      -- the bytes before original token i (gap n is after the last token)
      gap i = let
        gs = if i == 0 then 0 else snd $ origSpans V.! (i - 1)
        ge = if i == n then B.length orig else fst $ origSpans V.! i
        in B.take (ge - gs) $ B.drop gs orig
      -- for a close bracket, the index of its open bracket; otherwise the same index
      elementStart :: V.Vector Int
      elementStart = V.fromList $ reverse $ snd $ foldl' step ([], []) $ zip [0 ..] $ map rawOrig [0 .. n - 1] where
        step (stack, acc) (i, raw)
          | isOpen raw = (i : stack, i : acc)
          | isClose raw = case stack of
            o : stack' -> (stack', o : acc)
            []         -> (stack, i : acc)
          | otherwise = (stack, i : acc)

      isOpen raw = elem raw ["(", "{", "["]
      isClose raw = elem raw [")", "}", "]"]
      -- raw identifiers would run into a following token without whitespace between
      isWordy raw = case B8.uncons raw of
        Just (c, _) -> notElem c ("'\"()[]{}" :: String)
        Nothing     -> False
      hasComment = B8.any $ not . isSpace
      -- whitespace to use between two sibling elements, based on an original gap
      sepFromGap g = case B8.elemIndexEnd '\n' g of
        Nothing -> if not (B.null g) && not (hasComment g) then g else " "
        Just ix -> let
          newline = if ix > 0 && B8.index g (ix - 1) == '\r' then "\r\n" else "\n"
          indent = B8.takeWhile (`elem` [' ', '\t']) $ B.drop (ix + 1) g
          in newline <> indent
      -- adds a space if needed so a token doesn't merge with a preceding raw identifier
      tokenSep needSep raw = if needSep && not (isOpen raw || isClose raw) then " " else ""
      emitGaps needSep = \case
        [] -> ([], needSep)
        g : gs
          | B.null g  -> emitGaps needSep gs
          | otherwise -> let
            g' = if needSep && not (isSpace $ B8.head g) then " " <> g else g
            in case emitGaps False gs of
              (rest, needSep') -> (g' : rest, needSep')
      -- comments from in between deleted tokens, kept as long as they're not in the gap we always emit
      lostGaps prev i = filter hasComment [gap k | k <- [maybe 1 (+ 1) prev .. i - 1]]

      -- prev: index of last original token emitted
      -- needSep: whether the last thing emitted was a token that needs whitespace before another
      go :: Maybe Int -> Bool -> [DiffOp] -> [B.ByteString]
      go prev needSep = \case
        [] -> if n == 0 then [] else fst $ emitGaps needSep $ lostGaps prev n <> [gap n]
        DiffDel _ : rest -> go prev needSep rest
        DiffBoth i _ : rest -> let
          (gapBytes, needSep') = emitGaps needSep $ lostGaps prev i <> [gap i | i /= 0]
          raw = rawOrig i
          in gapBytes <> [tokenSep needSep' raw, raw] <> go (Just i) (isWordy raw) rest
        insOps@(DiffIns _ : _) -> let
          (run, rest) = span (\case DiffBoth{} -> False; _ -> True) insOps
          raws = [renderToken $ newTokens V.! j | DiffIns j <- run]
          firstSep = case raws of
            r : _ | isClose r -> ""
            _ -> case prev of
              Nothing -> ""
              Just p
                | isOpen $ rawOrig p -> let
                  -- first child of a list: copy the original line break if there is one
                  g = gap $ p + 1
                  in if B8.elem '\n' g then sepFromGap g else ""
                | otherwise -> sepFromGap $ gap $ elementStart V.! p
          innerSep a b = if isOpen a || isClose b then "" else " "
          runBytes = case raws of
            [] -> []
            r : rs -> (if B.null firstSep then tokenSep needSep r else firstSep)
              : r : concat (zipWith (\a b -> [innerSep a b, b]) raws rs)
          in runBytes <> go prev (isWordy $ last raws) rest

      ops = diffWeighted tokenMatchWeight
        (V.fromList $ annotateTokens origTokens)
        (V.fromList $ annotateTokens $ V.toList newTokens)
      output = B.concat $ gap 0 : go Nothing False ops

  -- sanity check that the output reads back as the new tree
  case S.scanEither $ TE.decodeLatin1 output of
    Right toks | map (fmap encodeLatin1 . snd) toks == V.toList newTokens
      -> return $ BL.fromStrict $ bom <> output
    _ -> fatal "Internal error: DTA output with original formatting doesn't match the edited DTA"
