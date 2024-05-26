{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Text.Fluent.Locale where

import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import           Data.Scientific     (FPFormat (..), Scientific,
                                      formatScientific)
import qualified Data.Text           as T
import           Text.Read           (readMaybe)

data PluralForm
  = PluralZero
  | PluralOne
  | PluralTwo
  | PluralFew
  | PluralMany
  | PluralOther
  deriving (Eq, Show)

type PluralRule = Scientific -> PluralForm

{-
n   the absolute value of N.*
i   the integer digits of N.*
v   the number of visible fraction digits in N, with trailing zeros.*
-- w   the number of visible fraction digits in N, without trailing zeros.*
f   the visible fraction digits in N, with trailing zeros, expressed as an integer.*
t   the visible fraction digits in N, without trailing zeros, expressed as an integer.*
-- c   compact decimal exponent value: exponent of the power of 10 used in compact decimal formatting.
e   a deprecated synonym for ‘c’. Note: it may be redefined in the future.
-}

-- For now we don't support number formatting options, so input is just a
-- Scientific, expected to be formatted without exponents and no trailing
-- decimal zeroes

data PluralInput = PluralInput
  { n :: Scientific
  , i :: Integer
  , v :: Integer
  , f :: Integer
  , t :: Integer
  , e :: Integer
  }

makePluralInput :: Scientific -> PluralInput
makePluralInput sci = PluralInput
  { n = abs sci
  , i = truncate $ abs sci
  , v = case trail of
    "0" -> 0
    _   -> fromIntegral $ length trail
  , f = trailInt
  , t = trailInt
  , e = expt
  } where
    trail = drop 1 $ dropWhile (/= '.') $ formatScientific Fixed Nothing sci
    trailInt = fromMaybe 0 $ readMaybe trail
    expt = fromMaybe 0 $ readMaybe $ drop 1 $ dropWhile (/= 'e') $ formatScientific Exponent Nothing sci

intRange :: Integer -> (Integer, Integer) -> Bool
intRange n (x, y) = x <= n && n <= y

sciIntRange :: Scientific -> (Integer, Integer) -> Bool
sciIntRange n xy = case properFraction n of
  (n', 0) -> intRange n' xy
  _       -> False

modIntRange :: Scientific -> Integer -> (Integer, Integer) -> Bool
modIntRange n m xy = case properFraction n of
  (n', 0) -> intRange (rem n' m) xy
  _       -> False

modElem :: Scientific -> Integer -> [Integer] -> Bool
modElem n m xs = case properFraction n of
  (n', 0) -> elem (rem n' m) xs
  _       -> False

-- rules from cldr-common-45.0/common/supplemental/plurals.xml
pluralRules :: HM.HashMap T.Text PluralRule
pluralRules = HM.fromList $ concatMap (\(langs, fn) -> let rule = fn . makePluralInput in map (, rule) langs)

  -- 1: other
  [ ( ["bm","bo","dz","hnj","id","ig","ii","in","ja","jbo","jv","jw","kde","kea","km","ko","lkt","lo","ms","my","nqo","osa","root","sah","ses","sg","su","th","to","tpi","vi","wo","yo","yue","zh"]
    , const PluralOther
    )

  -- 2: one,other
  , ( ["am","as","bn","doi","fa","gu","hi","kn","pcm","zu"]
    , \PluralInput{..} -> if
      | i == 0 || n == 1 -> PluralOne
      | otherwise        -> PluralOther
    )
  , ( ["ff","hy","kab"]
    , \PluralInput{..} -> if
      | elem i [0, 1] -> PluralOne
      | otherwise     -> PluralOther
    )
  , ( ["ast","de","en","et","fi","fy","gl","ia","io","ji","lij","nl","sc","scn","sv","sw","ur","yi"]
    , \PluralInput{..} -> if
      | i == 1 && v == 0 -> PluralOne
      | otherwise        -> PluralOther
    )
  , ( ["si"]
    , \PluralInput{..} -> if
      | elem n [0, 1] || i == 0 && f == 1 -> PluralOne
      | otherwise                         -> PluralOther
    )
  , ( ["ak","bho","guw","ln","mg","nso","pa","ti","wa"]
    , \PluralInput{..} -> if
      | sciIntRange n (0, 1) -> PluralOne
      | otherwise            -> PluralOther
    )
  , ( ["tzm"]
    , \PluralInput{..} -> if
      | sciIntRange n (0, 1) || sciIntRange n (11, 99) -> PluralOne
      | otherwise                                      -> PluralOther
    )
  , ( ["af","an","asa","az","bal","bem","bez","bg","brx","ce","cgg","chr","ckb","dv","ee","el","eo","eu","fo","fur","gsw","ha","haw","hu","jgo","jmc","ka","kaj","kcg","kk","kkj","kl","ks","ksb","ku","ky","lb","lg","mas","mgo","ml","mn","mr","nah","nb","nd","ne","nn","nnh","no","nr","ny","nyn","om","or","os","pap","ps","rm","rof","rwk","saq","sd","sdh","seh","sn","so","sq","ss","ssy","st","syr","ta","te","teo","tig","tk","tn","tr","ts","ug","uz","ve","vo","vun","wae","xh","xog"]
    , \PluralInput{..} -> if
      | n == 1    -> PluralOne
      | otherwise -> PluralOther
    )
  , ( ["da"]
    , \PluralInput{..} -> if
      | n == 1 || t /= 0 && elem i [0, 1] -> PluralOne
      | otherwise                         -> PluralOther
    )
  , ( ["is"]
    , \PluralInput{..} -> if
      | t == 0 && rem i 10 == 1 && rem i 100 /= 11 || rem t 10 == 1 && rem t 100 /= 11 -> PluralOne
      | otherwise                                                                      -> PluralOther
    )
  , ( ["mk"]
    , \PluralInput{..} -> if
      | v == 0 && rem i 10 == 1 && rem i 100 /= 11 || rem f 10 == 1 && rem f 100 /= 11 -> PluralOne
      | otherwise                                                                      -> PluralOther
    )
  , ( ["ceb","fil","tl"]
    , \PluralInput{..} -> if
      | v == 0 && elem i [1,2,3] || v == 0 && notElem (rem i 10) [4,6,9] || v /= 0 && notElem (rem f 10) [4,6,9] -> PluralOne
      | otherwise                                                                                                -> PluralOther
    )

  -- 3: zero,one,other
  , ( ["lv","prg"]
    , \PluralInput{..} -> if
      | modElem n 10 [0] || modIntRange n 100 (11, 19) || v == 2 && intRange (rem f 100) (11, 19)                             -> PluralZero
      | modElem n 10 [1] && not (modElem n 100 [11]) || v == 2 && rem f 10 == 1 && rem f 100 /= 11 || v /= 2 && rem f 10 == 1 -> PluralOne
      | otherwise                                                                                                             -> PluralOther
    )
  , ( ["lag"]
    , \PluralInput{..} -> if
      | n == 0                  -> PluralZero
      | elem i [0, 1] && n /= 0 -> PluralOne
      | otherwise               -> PluralOther
    )
  , ( ["ksh","blo"] -- separate rules but seemingly identical? different examples though
    , \PluralInput{..} -> if
      | n == 0    -> PluralZero
      | n == 1    -> PluralOne
      | otherwise -> PluralOther
    )

  -- 3: one,two,other
  , ( ["he","iw"]
    , \PluralInput{..} -> if
      | i == 1 && v == 0 || i == 0 && v /= 0 -> PluralOne
      | i == 2 && v == 0                     -> PluralTwo
      | otherwise                            -> PluralOther
    )
  , ( ["iu","naq","sat","se","sma","smi","smj","smn","sms"]
    , \PluralInput{..} -> if
      | n == 1    -> PluralOne
      | n == 2    -> PluralTwo
      | otherwise -> PluralOther
    )

  -- 3: one,few,other
  , ( ["shi"]
    , \PluralInput{..} -> if
      | i == 0 || n == 1      -> PluralOne
      | sciIntRange n (2, 10) -> PluralFew
      | otherwise             -> PluralOther
    )
  , ( ["mo","ro"]
    , \PluralInput{..} -> if
      | i == 1 && v == 0                                        -> PluralOne
      | v /= 0 || n == 0 || n /= 1 && modIntRange n 100 (1, 19) -> PluralFew
      | otherwise                                               -> PluralOther
    )
  , ( ["bs","hr","sh","sr"]
    , \PluralInput{..} -> if
      | v == 0 && rem i 10 == 1 && rem i 100 /= 11 || rem f 10 == 1 && rem f 100 /= 11                                                                   -> PluralOne
      | v == 0 && intRange (rem i 10) (2, 4) && not (intRange (rem i 100) (12, 14)) || intRange (rem f 10) (2, 4) && not (intRange (rem f 100) (12, 14)) -> PluralFew
      | otherwise                                                                                                                                        -> PluralOther
    )

  -- 3: one,many,other
  , ( ["fr"]
    , \PluralInput{..} -> if
      | elem i [0, 1]                                                               -> PluralOne
      | e == 0 && i /= 0 && rem i 1000000 == 0 && v == 0 || not (intRange e (0, 5)) -> PluralMany
      | otherwise                                                                   -> PluralOther
    )
  , ( ["pt"]
    , \PluralInput{..} -> if
      | intRange i (0, 1)                                                           -> PluralOne
      | e == 0 && i /= 0 && rem i 1000000 == 0 && v == 0 || not (intRange e (0, 5)) -> PluralMany
      | otherwise                                                                   -> PluralOther
    )
  , ( ["ca","it","pt_PT","vec"]
    , \PluralInput{..} -> if
      | i == 1 && v == 0                                                            -> PluralOne
      | e == 0 && i /= 0 && rem i 1000000 == 0 && v == 0 || not (intRange e (0, 5)) -> PluralMany
      | otherwise                                                                   -> PluralOther
    )
  , ( ["es"]
    , \PluralInput{..} -> if
      | n == 1                                                                      -> PluralOne
      | e == 0 && i /= 0 && rem i 1000000 == 0 && v == 0 || not (intRange e (0, 5)) -> PluralMany
      | otherwise                                                                   -> PluralOther
    )

  -- 4: one,two,few,other
  , ( ["gd"]
    , \PluralInput{..} -> if
      | elem n [1, 11]                                  -> PluralOne
      | elem n [2, 12]                                  -> PluralTwo
      | sciIntRange n (3, 10) || sciIntRange n (13, 19) -> PluralFew
      | otherwise                                       -> PluralOther
    )
  , ( ["sl"]
    , \PluralInput{..} -> if
      | v == 0 && rem i 100 == 1                        -> PluralOne
      | v == 0 && rem i 100 == 2                        -> PluralTwo
      | v == 0 && intRange (rem i 100) (3, 4) || v /= 0 -> PluralFew
      | otherwise                                       -> PluralOther
    )
  , ( ["dsb","hsb"]
    , \PluralInput{..} -> if
      | v == 0 && rem i 100 == 1 || rem f 100 == 1                           -> PluralOne
      | v == 0 && rem i 100 == 2 || rem f 100 == 2                           -> PluralTwo
      | v == 0 && intRange (rem i 100) (3, 4) || intRange (rem f 100) (3, 4) -> PluralFew
      | otherwise                                                            -> PluralOther
    )

  -- 4: one,few,many,other
  , ( ["cs","sk"]
    , \PluralInput{..} -> if
      | i == 1 && v == 0            -> PluralOne
      | intRange i (2, 4) && v == 0 -> PluralFew
      | v /= 0                      -> PluralMany
      | otherwise                   -> PluralOther
    )
  , ( ["pl"]
    , \PluralInput{..} -> if
      | i == 1 && v == 0                                                                                                                  -> PluralOne
      | v == 0 && intRange (rem i 10) (2, 4) && not (intRange (rem i 100) (12, 14))                                                       -> PluralFew
      | v == 0 && i /= 1 && intRange (rem i 10) (0, 1) || v == 0 && intRange (rem i 10) (5, 9) || v == 0 && intRange (rem i 100) (12, 14) -> PluralMany
      | otherwise                                                                                                                         -> PluralOther
    )
  , ( ["be"]
    , \PluralInput{..} -> if
      | modElem n 10 [1] && not (modElem n 100 [11])                              -> PluralOne
      | modIntRange n 10 (2, 4) && not (modIntRange n 100 (12, 14))               -> PluralFew
      | modElem n 10 [0] || modIntRange n 10 (5, 9) || modIntRange n 100 (11, 14) -> PluralMany
      | otherwise                                                                 -> PluralOther
    )
  , ( ["lt"]
    , \PluralInput{..} -> if
      | modElem n 10 [1] && not (modIntRange n 100 (11, 19))        -> PluralOne
      | modIntRange n 10 (2, 9) && not (modIntRange n 100 (11, 19)) -> PluralFew
      | f /= 0                                                      -> PluralMany
      | otherwise                                                   -> PluralOther
    )
  , ( ["ru","uk"]
    , \PluralInput{..} -> if
      | v == 0 && rem i 10 == 1 && rem i 100 /= 11                                                                 -> PluralOne
      | v == 0 && intRange (rem i 10) (2, 4) && not (intRange (rem i 100) (12, 14))                                -> PluralFew
      | v == 0 && rem i 10 == 0 || v == 0 && intRange (rem i 10) (5, 9) || v == 0 && intRange (rem i 100) (11, 14) -> PluralMany
      | otherwise                                                                                                  -> PluralOther
    )

  -- 5: one,two,few,many,other
  , ( ["br"]
    , \PluralInput{..} -> if
      | modElem n 10 [1] && not (modElem n 100 [11,71,91]) -> PluralOne
      | modElem n 10 [2] && not (modElem n 100 [12,72,92]) -> PluralTwo
      | modElem n 10 [3, 4, 9]
        && not (modIntRange n 100 (10, 19))
        && not (modIntRange n 100 (70, 79))
        && not (modIntRange n 100 (90, 99))                -> PluralFew
      | n /= 0 && not (modElem n 1000000 [0])              -> PluralMany
      | otherwise                                          -> PluralOther
    )
  , ( ["mt"]
    , \PluralInput{..} -> if
      | n == 1                              -> PluralOne
      | n == 2                              -> PluralTwo
      | n == 0 || modIntRange n 100 (3, 10) -> PluralFew
      | modIntRange n 100 (11, 19)          -> PluralMany
      | otherwise                           -> PluralOther
    )
  , ( ["ga"]
    , \PluralInput{..} -> if
      | n == 1                -> PluralOne
      | n == 2                -> PluralTwo
      | sciIntRange n (3, 6)  -> PluralFew
      | sciIntRange n (7, 10) -> PluralMany
      | otherwise             -> PluralOther
    )
  , ( ["gv"]
    , \PluralInput{..} -> if
      | v == 0 && rem i 10 == 1                    -> PluralOne
      | v == 0 && rem i 10 == 2                    -> PluralTwo
      | v == 0 && elem (rem i 100) [0,20,40,60,80] -> PluralFew
      | v /= 0                                     -> PluralMany
      | otherwise                                  -> PluralOther
    )

  -- 6: zero,one,two,few,many,other
  , ( ["kw"]
    , \PluralInput{..} -> if
      | n == 0                                  -> PluralZero
      | n == 1                                  -> PluralOne
      | modElem n 100 [2,22,42,62,82]
        || modElem n 1000 [0]
        && (modIntRange n 100000 (1000, 20000) || modElem n 100000 [40000,60000,80000])
        || n /= 0
        && modElem n 1000000 [100000]           -> PluralTwo
      | modElem n 100 [3,23,43,63,83]           -> PluralFew
      | n /= 1 && modElem n 100 [1,21,41,61,81] -> PluralMany
      | otherwise                               -> PluralOther
    )
  , ( ["ar","ars"]
    , \PluralInput{..} -> if
      | n == 0                     -> PluralZero
      | n == 1                     -> PluralOne
      | n == 2                     -> PluralTwo
      | modIntRange n 100 (3, 10)  -> PluralFew
      | modIntRange n 100 (11, 99) -> PluralMany
      | otherwise                  -> PluralOther
    )
  , ( ["cy"]
    , \PluralInput{..} -> case n of
      0 -> PluralZero
      1 -> PluralOne
      2 -> PluralTwo
      3 -> PluralFew
      6 -> PluralMany
      _ -> PluralOther
    )

  ]
