module Style (SustainColors, customize) where

type SustainColors =
  { light :: String
  , normal :: String
  , dark :: String
  , hit :: Number -> String
  }

foreign import customize ::

  { trackSpeed :: Number
  , targetPositionVert :: Int
  , labelPositionVert :: Int
  , targetPositionHoriz :: Int
  , labelPositionHoriz :: Int
  , labelIconWidth :: Int
  , marginWidth :: Int
  , buttonWidth :: Int

  , background :: String
  , timestampFont :: String
  , timestampColor :: String

  , loadingBackground :: String
  , loadingBigSquare :: String
  , loadingSmallSquares :: String
  , loadingSmallSquareSize :: Number
  , loadingLoopTime_ms :: Number

  , progressBorder :: String
  , progressEmpty :: String
  , progressFilled :: String
  , progressSection :: String
  , progressSectionWidth :: Int

  , highway :: String
  , highwayBlackKey :: String
  , highwayRailing :: String
  , highwayDivider :: String
  , highwaySolo :: String
  , highwaySoloEdge :: String
  , highwaySoloBlackKey :: String

  , highwayLine           :: String
  , highwayBarHeight      :: Int
  , highwayBeatHeight     :: Int
  , highwayHalfBeatHeight :: Int

  , proKeysRangeOverlay :: String
  , glissandoBorder :: String

  , freeformLane :: String

  , proChordNameFont :: String
  , proChordNameFontSuperscript :: String
  , proChordNameColor :: String

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
  , lyricColorActive :: String
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
  , sustainProtarMute :: SustainColors

  , widthStandardFret :: Int
  , widthProtarFret :: Int

  }
