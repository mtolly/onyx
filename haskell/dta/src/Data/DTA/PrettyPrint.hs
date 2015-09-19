-- | Pretty-print text (DTA) files with the HughesPJ library.
module Data.DTA.PrettyPrint (showDTA) where

import qualified Text.PrettyPrint.HughesPJ as PP

import Data.DTA.Base

ppChunk :: Chunk String -> PP.Doc
ppChunk c = case c of
  Int i -> PP.text $ show i
  Float f -> PP.text $ show f
  Var t -> PP.hcat [PP.char '$', PP.text t]
  Key t -> ppKey t
  Unhandled -> PP.text "kDataUnhandled"
  IfDef t -> PP.hsep [PP.text "#ifdef", PP.text t]
  Else -> PP.text "#else"
  EndIf -> PP.text "#endif"
  Parens tr -> PP.parens $ ppTree tr
  Braces tr -> PP.braces $ ppTree tr
  String t -> PP.text $ "\"" ++ concatMap f t ++ "\"" where
    f '"' = "\\q"
    f ch  = [ch]
  Brackets tr -> PP.brackets $ ppTree tr
  Define t -> PP.hsep [PP.text "#define", PP.text t]
  Include t -> PP.hsep [PP.text "#include", PP.text t]
  Merge t -> PP.hsep [PP.text "#merge", PP.text t]
  IfNDef t -> PP.hsep [PP.text "#ifndef", PP.text t]

-- | Automatically chooses between horizontal and vertical arrangements,
-- depending on what kind of chunks are in the tree.
ppTree :: Tree String -> PP.Doc
ppTree (Tree _ chks)
  | all simpleChunk chks = PP.hsep $ map ppChunk chks
  | otherwise            = PP.vcat $ map ppChunk chks
  where simpleChunk c = case c of
          Int _ -> True
          Float _ -> True
          Var _ -> True
          Key _ -> True
          Unhandled -> True
          _ -> False

-- | Produces a single-quoted string literal.
ppKey :: String -> PP.Doc
ppKey = PP.text . f . show where
  -- simply convert a double-quoted string to single-quoted string
  f "" = ""
  f ('"':xs) = '\'' : f xs
  f ('\'':xs) = '\\' : '\'' : f xs
  f ('\\':x:xs) = '\\' : x : f xs
  f (x:xs) = x : f xs

ppDTA :: DTA String -> PP.Doc
ppDTA = PP.vcat . map ppChunk . treeChunks . topTree

showDTA :: DTA String -> String
showDTA = PP.render . ppDTA
