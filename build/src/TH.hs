module TH where

import Data.Char (isUpper, toLower)

camelToHyphens :: String -> String
camelToHyphens = concatMap $ \c -> if isUpper c
  then ['-', toLower c]
  else [c]
