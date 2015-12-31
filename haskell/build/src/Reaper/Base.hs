module Reaper.Base where

import           Data.List        (foldl')
import           Text.PrettyPrint ((<+>), (<>))
import qualified Text.PrettyPrint as PP

data Element = Element String [String] (Maybe [Element])
  deriving (Eq, Ord, Show, Read)

writeRPP :: FilePath -> Element -> IO ()
writeRPP path = writeFile path . PP.render . showElement

showElement :: Element -> PP.Doc
showElement (Element k ks Nothing) = foldl' (<+>) (showAtom k) (map showAtom ks)
showElement (Element k ks (Just sub)) = let
  start = PP.char '<' <> foldl' (<+>) (showAtom k) (map showAtom ks)
  sublines = PP.nest 2 $ PP.vcat $ map showElement sub
  end = PP.char '>'
  in PP.vcat [start, sublines, end]

showAtom :: String -> PP.Doc
showAtom s = PP.text $ case (elem '"' s, elem '\'' s, elem ' ' s) of
  (True, True, _) -> "`" ++ map removeTick s ++ "`"
  (False, False, False) -> s
  (False, _, True) -> wrap '"'
  (False, True, False) -> wrap '"'
  (True, False, _) -> wrap '\''
  where wrap c = [c] ++ s ++ [c]
        removeTick = \case '`' -> '\''; c -> c
