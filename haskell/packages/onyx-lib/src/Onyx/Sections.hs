{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
module Onyx.Sections
( SectionType(..), Section(..)
, simpleSection
, parseSection, emitSection, sectionBody
, makeDisplaySection
, makeRBN2Sections
, makeRB3Section
, makeRB2Section
, makeGH2Section
, fromGH2Section
) where

import           Data.Char         (isAscii, isDigit, isPrint)
import           Data.List         (sort)
import           Data.List.HT      (partitionMaybe)
import           Data.Maybe        (fromMaybe)
import qualified Data.Text         as T
import           Onyx.FretsOnFire  (stripTags)
import           Onyx.Sections.GH2 (gh2Sections)
import           Onyx.Sections.RB3 (findRBN2Section)

data SectionType
  = SectionRB2 -- @[section foo]@
  | SectionRB3 -- @[prc_foo]@
  deriving (Eq, Ord, Show, Enum, Bounded)

data Section = Section
  { type_   :: SectionType
  , segment :: Maybe T.Text -- song name using my format for album projects
  , name    :: T.Text
  } deriving (Eq, Ord, Show)

simpleSection :: T.Text -> Section
simpleSection t = Section
  { type_   = SectionRB2
  , segment = Nothing
  , name    = t
  }

parseSection :: [T.Text] -> Maybe Section
parseSection = \case
  "section" : s -> withType SectionRB2 $ T.unwords s
  x : xs        -> do
    x' <- T.stripPrefix "prc_" x
    withType SectionRB3 $ T.unwords $ x' : xs
  []            -> Nothing
  where withType t s = Just $ case T.stripPrefix "<" s of
          Nothing -> Section t Nothing s
          Just s' -> case T.breakOn ">" s' of
            (x, y) -> Section t (Just x) $ T.strip $ T.drop 1 y

-- does not include brackets
emitSection :: Section -> T.Text
emitSection s = T.concat
  [ case s.type_ of SectionRB2 -> "section "; SectionRB3 -> "prc_"
  , sectionBody s
  ]

sectionBody :: Section -> T.Text
sectionBody s = T.concat
  [ case s.segment of Nothing -> ""; Just seg -> "<" <> seg <> "> "
  , s.name
  ]

-- Used for emitting in CH format, and also to display in preview
makeDisplaySection :: Section -> Section
makeDisplaySection s = s
  { type_ = SectionRB2
  -- segment left unchanged
  , name = T.replace "_" " " $ stripTags $ case findRBN2Section s.name of
    Nothing           -> s.name
    Just (_, display) -> display
  }

-- Used for all Harmonix game output
underscoreForm :: T.Text -> T.Text
underscoreForm
  -- starting with a number crashes rb3 practice mode apparently
  = (\s -> case T.uncons s of
    Just (c, _) | isDigit c -> "x_" <> s
    _                       -> s
    )
  . T.intercalate "_"
  . T.words
  -- tested in rb3: all ascii punctuation/symbols work other than square brackets
  . T.map (\case '[' -> '('; ']' -> ')'; c -> c)
  . T.filter (\c -> isAscii c && isPrint c)
  . T.replace "_" " "
  . stripTags

tryRBN2Form :: Section -> Maybe Section
tryRBN2Form s = case s.segment of
  Just _  -> Nothing
  Nothing -> do
    (prcForm, _) <- findRBN2Section s.name
    return Section
      { type_   = SectionRB3
      , segment = Nothing
      , name    = prcForm
      }

makeRB3Section :: Section -> Section
makeRB3Section s = fromMaybe (rbCustomForm s) $ tryRBN2Form s

makeRB2Section :: Section -> Section
makeRB2Section s = (makeRB3Section s)
  { type_ = SectionRB2
  }

rbCustomForm :: Section -> Section
rbCustomForm s = Section
  { type_   = SectionRB2
  , segment = Nothing
  , name    = underscoreForm s.name
  }

-- | Returns only Magma v2 @prc@ sections, and a list of discarded strings.
makeRBN2Sections :: (Ord a) => [(a, Section)] -> ([(a, Section)], [Section])
makeRBN2Sections sects = let
  (valid, notValid) = partitionMaybe (mapM tryRBN2Form) sects
  used = map (\(_, s) -> s.name) valid
  replacements = map (Section SectionRB3 Nothing) $ filter (`notElem` used) $ do
    x <- ['a'..'k']
    y <- ['0'..'9']
    return $ T.pack $ x : [y | y /= '0']
  in (sort $ valid <> zip (map fst notValid) replacements, map snd notValid)

makeGH2Section :: T.Text -> T.Text
makeGH2Section = underscoreForm -- TODO look up in gh2Sections

fromGH2Section :: T.Text -> T.Text
fromGH2Section s = fromMaybe s $ lookup s gh2Sections
