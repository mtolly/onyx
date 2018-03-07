module Style (SustainColors, customize) where

type SustainColors =
  { light :: String
  , normal :: String
  , dark :: String
  , hit :: Number -> String
  }

foreign import customize ::

  { background :: String
  , timestampFont :: String
  , timestampColor :: String

  , progressBorder :: String
  , progressEmpty :: String
  , progressFilled :: String

  , highway :: String
  , highwayBlackKey :: String
  , highwayRailing :: String
  , highwayDivider :: String
  , highwaySolo :: String
  , highwaySoloBlackKey :: String

  , proKeysRangeOverlay :: String
  , glissandoBorder :: String

  , freeformLane :: String

  , harm1Pitch :: String
  , harm2Pitch :: String
  , harm3Pitch :: String
  , harm1Talky :: String
  , harm2Talky :: String
  , harm3Talky :: String
  , percussionOuter :: String
  , percussionInner :: String
  , percussionHit :: Number -> String
  , vocalPhraseEnd :: String
  , vocalTargetLine :: String
  , lyricColor :: String
  , lyricColorEnergy :: String
  , lyricFont :: String
  , lyricFontTalky :: String
  , lyricLaneTop :: String
  , vocalNoteArea :: String
  , lyricLaneBottom :: String

  , sustainBorder :: String
  , sustainGreen :: SustainColors
  , sustainRed :: SustainColors
  , sustainYellow :: SustainColors
  , sustainBlue :: SustainColors
  , sustainOrange :: SustainColors
  , sustainPurple :: SustainColors
  , sustainEnergy :: SustainColors
  , sustainWhiteKey :: SustainColors
  , sustainBlackKey :: SustainColors
  , sustainBlackKeyEnergy :: SustainColors
  , sustainWhiteGHL :: SustainColors
  , sustainBothGHL :: SustainColors
  , sustainBlackGHL :: SustainColors
  , sustainOpenGHL :: SustainColors

  , widthStandardFret :: Int
  , widthProtarFret :: Int

  }
