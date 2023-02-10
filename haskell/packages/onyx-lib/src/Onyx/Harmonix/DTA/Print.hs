-- | Pretty-print text (DTA) files.
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Harmonix.DTA.Print (showDTA) where

import qualified Data.Text                 as T
import           Onyx.Harmonix.DTA.Base
import qualified Prettyprinter             as PP
import           Prettyprinter.Render.Text (renderStrict)

-- These functions are designed to emulate the format Magma uses
-- when creating songs.dta files, so that C3 CON Tools' parser can read them.

ppChunk :: Chunk T.Text -> PP.Doc ()
ppChunk c = case c of

  -- c3 hacks
  Parens (Tree _ [Sym "downloaded", Int 1]) -> "(downloaded TRUE)"
  Parens (Tree _ [Sym "midi_file", String _]) -> rawOneLine c
  Parens (Tree _ [Sym "drum_bank", Sym _]) -> rawOneLine c
  Parens (Tree _ [Sym "solo", Parens{}]) -> rawOneLine c
  Parens (Tree _ [Sym "real_guitar_tuning", Parens{}]) -> rawOneLine c
  Parens (Tree _ [Sym "real_bass_tuning", Parens{}]) -> rawOneLine c

  -- normal cases
  Int i -> PP.pretty $ show i
  Float f -> PP.pretty $ show f
  Var t -> PP.hcat ["$", PP.pretty t]
  Sym t -> ppSym t
  Unhandled -> "kDataUnhandled"
  IfDef t -> PP.hsep ["#ifdef", PP.pretty t]
  Else -> "#else"
  EndIf -> "#endif"
  Parens tr -> ppTree "(" ")" tr
  Braces tr -> ppTree "{" "}" tr
  String t -> PP.pretty $ "\"" <> T.concatMap f t <> "\"" where
    f '"' = "\\q"
    -- TODO brought these over from dtab for amp .bin -> .dta to work right
    -- (if we put actual newlines in, pretty printer will add extra spaces)
    -- should maybe double check if this breaks anything though
    f '\n' = "\\n"
    f '\\' = "\\\\"
    f ch  = T.singleton ch
  Brackets tr -> ppTree "[" "]" tr
  Define t -> PP.hsep ["#define", PP.pretty t]
  Include t -> PP.hsep ["#include", PP.pretty t]
  Merge t -> PP.hsep ["#merge", PP.pretty t]
  IfNDef t -> PP.hsep ["#ifndef", PP.pretty t]
  Autorun -> "#autorun"
  Undef t -> PP.hsep ["#undef", PP.pretty t]

-- | Used for certain attributes that C3 can only parse on one line,
-- with no single quotes around symbols.
rawOneLine :: Chunk T.Text -> PP.Doc ()
rawOneLine c = case c of
  Sym t                  -> PP.pretty t
  Parens (Tree _ chks)   -> PP.parens $ PP.hsep $ map rawOneLine chks
  Braces (Tree _ chks)   -> PP.braces $ PP.hsep $ map rawOneLine chks
  Brackets (Tree _ chks) -> PP.brackets $ PP.hsep $ map rawOneLine chks
  _                      -> ppChunk c
  -- TODO might want to add more sanity checks,
  -- e.g. verify that the symbols can be emitted without quotes

-- | Automatically chooses between horizontal and vertical arrangements,
-- depending on what kind of chunks are in the tree.
ppTree :: PP.Doc () -> PP.Doc () -> Tree T.Text -> PP.Doc ()
ppTree sl sr (Tree _ chks)
  | all simpleChunk chks = PP.hcat [sl, PP.hsep $ map ppChunk chks, sr]
  | otherwise            = PP.vcat [sl, PP.indent 3 $ PP.vcat $ map ppChunk chks, sr]
  where simpleChunk c = case c of
          Int _     -> True
          Float _   -> True
          Var _     -> True
          Sym _     -> True
          Unhandled -> True
          _         -> False

-- | Produces a single-quoted string literal.
ppSym :: T.Text -> PP.Doc ()
ppSym = PP.pretty . f . show where
  -- simply convert a double-quoted string to single-quoted string
  f ""          = ""
  f ('"':xs)    = '\'' : f xs
  f ('\'':xs)   = '\\' : '\'' : f xs
  f ('\\':x:xs) = '\\' : x : f xs
  f (x:xs)      = x : f xs

ppDTA :: DTA T.Text -> PP.Doc ()
ppDTA = PP.vcat . map ppChunk . treeChunks . topTree

showDTA :: DTA T.Text -> T.Text
showDTA = let
  opts = PP.defaultLayoutOptions
    { PP.layoutPageWidth = PP.Unbounded
    }
  in renderStrict . PP.layoutPretty opts . ppDTA
