-- | Pretty-print text (DTA) files with the HughesPJ library.
module Data.DTA.PrettyPrint (showDTA) where

import           Data.DTA.Base
import qualified Data.Text                 as T
import           Text.PrettyPrint.HughesPJ (($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

-- These functions are designed to emulate the format Magma uses
-- when creating songs.dta files, so that C3 CON Tools' parser can read them.

ppChunk :: Chunk String -> PP.Doc
ppChunk c = case c of

  -- c3 hacks
  Parens (Tree _ [Key "downloaded", Int 1]) -> PP.text "(downloaded TRUE)"
  Parens (Tree _ [Key "midi_file", String _]) -> rawOneLine c
  Parens (Tree _ [Key "drum_bank", Key _]) -> rawOneLine c
  Parens (Tree _ [Key "solo", Parens{}]) -> rawOneLine c
  Parens (Tree _ [Key "real_guitar_tuning", Parens{}]) -> rawOneLine c
  Parens (Tree _ [Key "real_bass_tuning", Parens{}]) -> rawOneLine c

  -- normal cases
  Int i -> PP.text $ show i
  Float f -> PP.text $ show f
  Var t -> PP.hcat [PP.char '$', PP.text t]
  Key t -> ppKey t
  Unhandled -> PP.text "kDataUnhandled"
  IfDef t -> PP.hsep [PP.text "#ifdef", PP.text t]
  Else -> PP.text "#else"
  EndIf -> PP.text "#endif"
  Parens tr -> ppTree "(" ")" tr
  Braces tr -> ppTree "{" "}" tr
  String t -> PP.text $ "\"" ++ concatMap f t ++ "\"" where
    f '"' = "\\q"
    f ch  = [ch]
  Brackets tr -> ppTree "[" "]" tr
  Define t -> PP.hsep [PP.text "#define", PP.text t]
  Include t -> PP.hsep [PP.text "#include", PP.text t]
  Merge t -> PP.hsep [PP.text "#merge", PP.text t]
  IfNDef t -> PP.hsep [PP.text "#ifndef", PP.text t]

-- | Used for certain attributes that C3 can only parse on one line,
-- with no single quotes around keywords.
rawOneLine :: Chunk String -> PP.Doc
rawOneLine c = case c of
  Key t                  -> PP.text t
  Parens (Tree _ chks)   -> PP.parens $ PP.hsep $ map rawOneLine chks
  Braces (Tree _ chks)   -> PP.braces $ PP.hsep $ map rawOneLine chks
  Brackets (Tree _ chks) -> PP.brackets $ PP.hsep $ map rawOneLine chks
  _                      -> ppChunk c
  -- TODO might want to add more sanity checks,
  -- e.g. verify that the keywords can be emitted without quotes

-- | Automatically chooses between horizontal and vertical arrangements,
-- depending on what kind of chunks are in the tree.
ppTree :: String -> String -> Tree String -> PP.Doc
ppTree sl sr (Tree _ chks)
  | all simpleChunk chks = PP.hcat [PP.text sl, PP.hsep $ map ppChunk chks, PP.text sr]
  | otherwise            = PP.text sl $+$ PP.nest 3 (PP.vcat $ map ppChunk chks) $+$ PP.text sr
  where simpleChunk c = case c of
          Int _     -> True
          Float _   -> True
          Var _     -> True
          Key _     -> True
          Unhandled -> True
          _         -> False

-- | Produces a single-quoted string literal.
ppKey :: String -> PP.Doc
ppKey = PP.text . f . show where
  -- simply convert a double-quoted string to single-quoted string
  f ""          = ""
  f ('"':xs)    = '\'' : f xs
  f ('\'':xs)   = '\\' : '\'' : f xs
  f ('\\':x:xs) = '\\' : x : f xs
  f (x:xs)      = x : f xs

ppDTA :: DTA String -> PP.Doc
ppDTA = PP.vcat . map ppChunk . treeChunks . topTree

showDTA :: DTA T.Text -> T.Text
showDTA = T.pack . PP.render . ppDTA . fmap T.unpack
