{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Onyx.DDR.DWI where

import           Control.Monad                    (forM, guard)
import           Data.Char                        (isSpace)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe, mapMaybe)
import           Data.Scientific                  (Scientific)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import           Onyx.DDR.SM
import           Onyx.MIDI.Common                 (pattern RNil, pattern Wait)
import           Onyx.PhaseShift.Dance            (Arrow (..))
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data DWI = DWI
  { dwi_GENRE       :: Maybe T.Text
  , dwi_CDTITLE     :: Maybe T.Text
  , dwi_FILE        :: Maybe T.Text
  , dwi_TITLE       :: Maybe T.Text
  , dwi_ARTIST      :: Maybe T.Text
  , dwi_BPM         :: Maybe Scientific
  , dwi_DISPLAYBPM  :: Maybe T.Text
  , dwi_CHANGEBPM   :: [(Scientific, Scientific)]
  , dwi_FREEZE      :: [(Scientific, Scientific)]
  , dwi_GAP         :: Maybe Scientific
  , dwi_SAMPLESTART :: Maybe Scientific
  , dwi_Notes       :: [DWINotes]
  } deriving (Show)

data DWINotes = DWINotes
  { dwin_Mode           :: T.Text
  , dwin_Difficulty     :: T.Text
  , dwin_NumericalMeter :: Int
  , dwin_Notes          :: T.Text
  , dwin_Notes2         :: Maybe T.Text
  } deriving (Show)

readDWI :: (MonadFail m) => [(T.Text, [T.Text])] -> m DWI
readDWI lns = do
  dwi_GENRE       <- getSMString lns "GENRE"
  dwi_CDTITLE     <- getSMString lns "CDTITLE"
  dwi_FILE        <- getSMString lns "FILE"
  dwi_TITLE       <- getSMString lns "TITLE"
  dwi_ARTIST      <- getSMString lns "ARTIST"
  dwi_BPM         <- getSMNumber lns "BPM"
  dwi_DISPLAYBPM  <- getSMString lns "DISPLAYBPM"
  dwi_CHANGEBPM   <- getSMNumberPairs lns "CHANGEBPM"
  dwi_FREEZE      <- getSMNumberPairs lns "FREEZE"
  dwi_GAP         <- getSMNumber lns "GAP"
  dwi_SAMPLESTART <- getSMNumber lns "SAMPLESTART"
  dwi_Notes       <- fmap catMaybes $ forM lns $ \(dwin_Mode, contents) -> do
    if elem dwin_Mode ["SINGLE", "DOUBLE", "COUPLE", "SOLO"]
      then case contents of
        dwin_Difficulty : nm : dwin_Notes : rest -> do
          dwin_NumericalMeter <- case readMaybe $ T.unpack nm of
            Nothing -> fail $ "Couldn't read numerical meter value: " <> show nm
            Just v  -> return v
          let dwin_Notes2 = listToMaybe rest
          return $ Just DWINotes{..}
        _ -> fail $ "Expected at least 3 parameters after " <> show dwin_Mode <> " but found " <> show (length contents)
      else return Nothing
  return DWI{..}

convertDWItoSM :: DWI -> SM
convertDWItoSM DWI{..} = SM
  -- note, several file paths like banner/bg/audio are expected to be named matching the .dwi.
  -- normal .sm import will look for those
  { sm_TITLE            = dwi_TITLE
  , sm_SUBTITLE         = Nothing
  , sm_ARTIST           = dwi_ARTIST
  , sm_TITLETRANSLIT    = Nothing
  , sm_SUBTITLETRANSLIT = Nothing
  , sm_ARTISTTRANSLIT   = Nothing
  , sm_GENRE            = dwi_GENRE
  , sm_CREDIT           = Nothing
  , sm_BANNER           = Nothing
  , sm_JACKET           = Nothing
  , sm_BACKGROUND       = Nothing
  , sm_LYRICSPATH       = Nothing
  , sm_CDTITLE          = dwi_CDTITLE
  , sm_MUSIC            = dwi_FILE
  , sm_OFFSET           = (* (-0.001)) <$> dwi_GAP
  , sm_SAMPLESTART      = dwi_SAMPLESTART
  , sm_SAMPLELENGTH     = Nothing
  , sm_SELECTABLE       = Nothing
  , sm_DISPLAYBPM       = [] -- dwi can have e.g. #DISPLAYBPM:145..290;
  , sm_BPMS
    = toList ((\bpm -> (0, bpm)) <$> dwi_BPM) -- initial bpm
    <> map (\(sixteenths, bpm) -> (sixteenths / 4, bpm)) dwi_CHANGEBPM -- changes are given in 16th note positions?
  , sm_STOPS            = map (\(sixteenths, ms) -> (sixteenths / 4, ms / 1000)) dwi_FREEZE
  , sm_NOTES            = flip mapMaybe dwi_Notes $ \DWINotes{..} -> do
    guard $ dwin_Mode == "SINGLE" -- TODO add DOUBLE/COUPLE/SOLO
    diff <- case dwin_Difficulty of
      "BEGINNER" -> Just "Beginner"
      "BASIC"    -> Just "Easy"
      "ANOTHER"  -> Just "Medium"
      "MANIAC"   -> Just "Hard"
      "SMANIAC"  -> Just "Challenge"
      _          -> Nothing
    return SMNotes
      { smn_ChartType      = "dance-single"
      , smn_Author         = ""
      , smn_Difficulty     = diff
      , smn_NumericalMeter = dwin_NumericalMeter
      , smn_GrooveRadar    = []
      , smn_Notes          = notesDWItoSM dwin_Notes
      }
  }

data DWINote = DWINote
  { dwi_Note :: Char
  , dwi_Hold :: Maybe Char
  } deriving (Show)

-- Timing is 192nd notes (192 = 1 bar)
getDWINotes :: T.Text -> RTB.T NN.Int DWINote
getDWINotes top = go 24 (T.filter (not . isSpace) top) "" where
  -- hoping the division brackets can't be nested
  -- TODO "It is also possible to hit more than two arrows at once. Simply surround any combination of step/jump keys with <Angle Brackets>."
  go division s after = case T.uncons s of
    Nothing -> if T.null after
      then RNil
      else go 24 after ""
    Just (c, s') -> case c of
      '(' -> case T.breakOn ")" s' of
        (x, y) -> go 12 x $ T.drop 1 y
      '[' -> case T.breakOn "]" s' of
        (x, y) -> go 8 x $ T.drop 1 y
      '{' -> case T.breakOn "}" s' of
        (x, y) -> go 3 x $ T.drop 1 y
      '`' -> case T.breakOn "'" s' of
        (x, y) -> go 1 x $ T.drop 1 y
      _ -> case T.uncons s' of
        Just ('!', T.uncons -> Just (hold, next)) ->
          Wait 0 (DWINote c $ Just hold) $ RTB.delay division $ go division next after
        _ -> Wait 0 (DWINote c Nothing) $ RTB.delay division $ go division s' after

-- Results are already RTB.collectCoincident-ed
getDWIArrows :: (NNC.C t) => RTB.T t DWINote -> RTB.T t [(Arrow, Char)]
getDWIArrows = RTB.filter (not . null) . go Set.empty where
  go _    RNil                = RNil
  go held (Wait dt note rest) = let
    arrows = charArrows $ dwi_Note note
    holds = maybe [] charArrows $ dwi_Hold note
    this = flip map arrows $ \arrow -> if elem arrow holds
      then (arrow, '2') -- start hold
      else if Set.member arrow held
        then (arrow, '3') -- end hold
        else (arrow, '1') -- non-hold note
    held' = Set.union (Set.difference held $ Set.fromList arrows) $ Set.fromList holds
    in Wait dt this $ go held' rest
  charArrows = \case
    '0' -> []
    '2' -> [ArrowD]
    '4' -> [ArrowL]
    '6' -> [ArrowR]
    '8' -> [ArrowU]
    '1' -> [ArrowD, ArrowL]
    '3' -> [ArrowD, ArrowR]
    '7' -> [ArrowU, ArrowL]
    '9' -> [ArrowU, ArrowR]
    'B' -> [ArrowL, ArrowR]
    'A' -> [ArrowU, ArrowD]
    -- not supporting Solo arrows now
    _   -> []

notesDWItoSM :: T.Text -> [[T.Text]]
notesDWItoSM dwi = let
  arrows = getDWIArrows $ getDWINotes dwi
  makeBars = \case
    RNil  -> []
    track -> let
      (firstBar, restBars) = U.trackSplit 192 track
      in makeBar firstBar : makeBars restBars
  makeBar bar = let
    division = foldr gcd 48 $ RTB.getTimes bar
    barMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 bar
    in do
      t <- [0, division .. 191]
      let instant = fromMaybe [] $ Map.lookup t barMap
      return $ T.pack $ do
        arrow <- [ArrowL, ArrowD, ArrowU, ArrowR]
        return $ fromMaybe '0' $ lookup arrow instant
  in makeBars arrows
