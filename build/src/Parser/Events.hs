{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.Events where

import Parser.Base
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import Parser.TH
import Language.Haskell.TH

data Event
  = Simple Simple
  | PracticeSection String
  | PracticeKick
  | PracticeSnare
  | PracticeHihat
  deriving (Eq, Ord, Show, Read)

data Simple
  = MusicStart
  | MusicEnd
  | End
  | Coda
  | CrowdRealtime
  | CrowdIntense
  | CrowdNormal
  | CrowdMellow
  | CrowdNoclap
  | CrowdClap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Simple where
  fromCommand = autoFromCommand
  toCommand = reverseLookup each fromCommand

rosetta :: (Q Exp, Q Exp)
rosetta = translation
  [ ( [e| mapParseOne Simple parseCommand |]
    , [e| \case Simple m -> unparseCommand m |]
    )
  , ( [e| firstEventWhich $ \e -> readCommand' e >>= \case
        ['p':'r':'c':'_':s] -> Just $ PracticeSection s
        _                   -> Nothing
      |]
    , [e| \case PracticeSection s -> RTB.singleton NNC.zero $ showCommand' ["prc_" ++ s] |]
    )
  , blip 24 [p| PracticeKick |]
  , blip 25 [p| PracticeSnare |]
  , blip 26 [p| PracticeHihat |]
  ]
