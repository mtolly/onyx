{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.Countin where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.Util as U
import qualified Numeric.NonNegative.Class as NNC
import Parser.TH
import Language.Haskell.TH

data Event = CountinHere
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

rosetta :: (Q Exp, Q Exp)
rosetta = translation
  [ ( [e| U.extractFirst $ \case
        E.MetaEvent (Meta.TextEvent "countin_here") -> Just CountinHere
        _ -> Nothing
      |]
    , [e| \case CountinHere -> RTB.singleton NNC.zero $ E.MetaEvent $ Meta.TextEvent "countin_here" |]
    )
  ]
