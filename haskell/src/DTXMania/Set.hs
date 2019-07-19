{-# LANGUAGE OverloadedStrings #-}
module DTXMania.Set where

import           Data.Foldable (toList)
import qualified Data.Text     as T
import           DTXMania.DTX  (loadDTXLines)

data SetDef = SetDef
  { setTitle   :: T.Text
  , setL1Label :: Maybe T.Text
  , setL1File  :: Maybe T.Text
  , setL2Label :: Maybe T.Text
  , setL2File  :: Maybe T.Text
  , setL3Label :: Maybe T.Text
  , setL3File  :: Maybe T.Text
  , setL4Label :: Maybe T.Text
  , setL4File  :: Maybe T.Text
  , setL5Label :: Maybe T.Text
  , setL5File  :: Maybe T.Text
  } deriving (Show)

loadSet :: FilePath -> IO [SetDef]
loadSet f = let
  go cur [] = toList cur
  go cur (("TITLE", t) : rest) = let
    new = SetDef
      { setTitle   = t
      , setL1Label = Nothing
      , setL1File  = Nothing
      , setL2Label = Nothing
      , setL2File  = Nothing
      , setL3Label = Nothing
      , setL3File  = Nothing
      , setL4Label = Nothing
      , setL4File  = Nothing
      , setL5Label = Nothing
      , setL5File  = Nothing
      }
    in toList cur ++ go (Just new) rest
  go Nothing (_ : rest) = go Nothing rest
  go (Just cur) ((k, v) : rest) = let
    cur' = case k of
      "L1LABEL" -> cur { setL1Label = Just v }
      "L1FILE"  -> cur { setL1File  = Just v }
      "L2LABEL" -> cur { setL2Label = Just v }
      "L2FILE"  -> cur { setL2File  = Just v }
      "L3LABEL" -> cur { setL3Label = Just v }
      "L3FILE"  -> cur { setL3File  = Just v }
      "L4LABEL" -> cur { setL4Label = Just v }
      "L4FILE"  -> cur { setL4File  = Just v }
      "L5LABEL" -> cur { setL5Label = Just v }
      "L5FILE"  -> cur { setL5File  = Just v }
      _         -> cur
    in go (Just cur') rest
  in go Nothing <$> loadDTXLines f
