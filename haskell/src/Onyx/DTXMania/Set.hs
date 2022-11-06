{-# LANGUAGE OverloadedStrings #-}
module Onyx.DTXMania.Set where

import           Data.Default.Class (Default (..))
import           Data.Foldable      (toList)
import qualified Data.Text          as T
import           Onyx.DTXMania.DTX  (loadDTXLines)

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

instance Default SetDef where
  def = SetDef
    { setTitle   = ""
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

loadSet :: FilePath -> IO [SetDef]
loadSet f = let
  modify (k, v) = case k of
    "L1LABEL" -> Just $ \sdef -> sdef { setL1Label = Just v }
    "L1FILE"  -> Just $ \sdef -> sdef { setL1File  = Just v }
    "L2LABEL" -> Just $ \sdef -> sdef { setL2Label = Just v }
    "L2FILE"  -> Just $ \sdef -> sdef { setL2File  = Just v }
    "L3LABEL" -> Just $ \sdef -> sdef { setL3Label = Just v }
    "L3FILE"  -> Just $ \sdef -> sdef { setL3File  = Just v }
    "L4LABEL" -> Just $ \sdef -> sdef { setL4Label = Just v }
    "L4FILE"  -> Just $ \sdef -> sdef { setL4File  = Just v }
    "L5LABEL" -> Just $ \sdef -> sdef { setL5Label = Just v }
    "L5FILE"  -> Just $ \sdef -> sdef { setL5File  = Just v }
    _         -> Nothing
  go cur [] = toList cur
  go cur (("TITLE", t) : rest) = let
    new = def { setTitle = t }
    in toList cur ++ go (Just new) rest
  go Nothing lns@(pair : rest) = case modify pair of
    Nothing -> go Nothing rest
    Just _  -> go (Just def) lns
  go (Just cur) (pair : rest) = let
    cur' = maybe cur ($ cur) $ modify pair
    in go (Just cur') rest
  in go Nothing <$> loadDTXLines f
