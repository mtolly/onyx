{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}
module Onyx.RockRevolution.MIDI where

import           Control.Monad                    (guard)
import           Control.Monad.Codec
import           Data.Char                        (isAlphaNum, isDigit)
import           Data.Either                      (rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Functor                     (void)
import           Data.List.Extra                  (elemIndex, nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   blipEdgesRBNice, each,
                                                   isNoteEdgeCPV, makeEdge',
                                                   makeEdgeCPV, pattern RNil,
                                                   pattern Wait)
import           Onyx.MIDI.Read                   (ChannelType (..),
                                                   ParseTrack (..),
                                                   channelBlip_, condenseMap,
                                                   eachKey, edges, edgesCV,
                                                   fatBlips, forceTrackSpine,
                                                   translateEdges)
import           Onyx.MIDI.Track.Beat             (BeatEvent (..))
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.Drums.Elite      as ED
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.Sections
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U

-- five-fret

data RRFiveDifficulty t = RRFiveDifficulty
  { rrfStrums :: RTB.T t (Edge () Five.Color)
  , rrfHOPOs  :: RTB.T t (Edge () Five.Color)
  , rrfSolo   :: RTB.T t Bool
  } deriving (Show)

instance ParseTrack RRFiveDifficulty where
  parseTrack = fatBlips (1/8) $ do
    rrfStrums <- (rrfStrums =.) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
      Five.Green  -> 48
      Five.Red    -> 49
      Five.Yellow -> 50
      Five.Blue   -> 51
      Five.Orange -> 52
    rrfHOPOs <- (rrfHOPOs =.) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
      Five.Green  -> 60
      Five.Red    -> 61
      Five.Yellow -> 62
      Five.Blue   -> 63
      Five.Orange -> 64
    rrfSolo <- rrfSolo =. edges 127
    return RRFiveDifficulty{..}

importRRGuitarBass :: RRFiveDifficulty U.Beats -> Five.FiveDifficulty U.Beats
importRRGuitarBass rr = let
  forceEdges
    = RTB.flatten
    . fmap nubOrd
    . RTB.collectCoincident
    . fmap (\case EdgeOn{} -> True; EdgeOff{} -> False)
  in Five.FiveDifficulty
    { Five.fiveForceStrum = forceEdges $ rrfStrums rr
    , Five.fiveForceHOPO = forceEdges $ rrfHOPOs rr
    , Five.fiveTap = RTB.empty
    , Five.fiveOpen = RTB.empty
    , Five.fiveGems = RTB.merge (rrfStrums rr) (rrfHOPOs rr)
    }

-- drums

-- Used for playing midi drum sounds (when enabled).
-- This corresponds to pitches 36 through 59 of the General MIDI Percussion.
data RRDrum
  = RR_Kick
  | RR_SideStick
  | RR_Snare
  | RR_HandClap
  | RR_ElectricSnare
  | RR_Tom6
  | RR_HihatClosed
  | RR_Tom5
  | RR_HihatPedal
  | RR_Tom4
  | RR_HihatOpen
  | RR_Tom3
  | RR_Tom2
  | RR_Crash1
  | RR_Tom1
  | RR_Ride
  | RR_China
  | RR_RideBell
  | RR_Tambourine
  | RR_Splash
  | RR_Cowbell
  | RR_Crash2
  | RR_Vibraslap
  | RR_Ride2
  deriving (Eq, Ord, Show, Enum, Bounded)

data RRChannel
  = RRC_Hidden -- putting first so it's default
  | RRC_LowTom
  | RRC_HighTom
  | RRC_Kick
  | RRC_Snare
  | RRC_CrashL
  | RRC_CrashR
  | RRC_Hihat
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ChannelType RRChannel where
  encodeChannel = \case
    RRC_LowTom  -> 0
    RRC_HighTom -> 1
    RRC_Kick    -> 2
    RRC_Snare   -> 3
    RRC_CrashL  -> 4
    RRC_CrashR  -> 5
    RRC_Hihat   -> 6
    -- channel 7 never used
    RRC_Hidden  -> 8
    -- there are a few notes with channel 9. maybe should have been 8?

rrChannel7Lane :: RRChannel -> Maybe (ED.EliteGem ())
rrChannel7Lane = \case
  RRC_LowTom  -> Just ED.Tom3
  RRC_HighTom -> Just ED.Tom1
  RRC_Kick    -> Just $ ED.Kick ()
  RRC_Snare   -> Just ED.Snare
  RRC_CrashL  -> Just ED.CrashL
  RRC_CrashR  -> Just ED.CrashR
  RRC_Hihat   -> Just ED.Hihat
  RRC_Hidden  -> Nothing

rrChannel4Lane :: RRChannel -> Maybe (D.Gem D.ProType)
rrChannel4Lane = \case
  RRC_LowTom  -> Just $ D.Pro D.Blue D.Tom
  RRC_HighTom -> Just $ D.Pro D.Blue D.Tom
  RRC_Kick    -> Just D.Kick
  RRC_Snare   -> Just D.Red
  RRC_CrashL  -> Just $ D.Pro D.Green D.Cymbal
  RRC_CrashR  -> Just $ D.Pro D.Green D.Cymbal
  RRC_Hihat   -> Just $ D.Pro D.Yellow D.Cymbal
  RRC_Hidden  -> Nothing

-- These are just conventions (channel is what actually determines gem).
-- Comments are channel counts from disc+dlc midis
rrDrumGuessTD :: RRDrum -> (ED.EliteGem D.Hand, ED.EliteGemType, D.DrumVelocity)
rrDrumGuessTD = \case
  RR_Kick          -> (ED.Kick D.RH, ED.GemNormal     , D.VelocityNormal) -- [48, [["Kick", 23132], ["HighTom", 4], ["Snare", 4]]]
  RR_HandClap      -> (ED.Hihat    , ED.GemNormal     , D.VelocityNormal) -- [49, [["Hihat", 734], ["Kick", 69], ["Snare", 27], ["LowTom", 1]]]
  RR_Snare         -> (ED.Snare    , ED.GemNormal     , D.VelocityNormal) -- [50, [["Snare", 16995], ["HighTom", 4], ["CrashR", 2], ["LowTom", 1]]]
  RR_SideStick     -> (ED.Snare    , ED.GemRim        , D.VelocityNormal) -- [51, [["Snare", 548], ["Hihat", 409], ["CrashR", 59]]]
  RR_ElectricSnare -> (ED.Snare    , ED.GemNormal     , D.VelocityNormal) -- [52, [["Snare", 31], ["LowTom", 30], ["Hihat", 27], ["HighTom", 24]]]
  RR_Tom6          -> (ED.Tom3     , ED.GemNormal     , D.VelocityNormal) -- [53, [["Hihat", 304], ["LowTom", 298], ["Snare", 60]]]
  RR_HihatClosed   -> (ED.Hihat    , ED.GemHihatClosed, D.VelocityNormal) -- [54, [["Hihat", 7161], ["LowTom", 10], ["HighTom", 9], ["CrashL", 6], ["Kick", 1]]]
  RR_Tom5          -> (ED.Tom3     , ED.GemNormal     , D.VelocityNormal) -- [55, [["LowTom", 1766], ["Hihat", 188], ["HighTom", 82], ["Snare", 3], ["CrashR", 2]]]
  RR_HihatPedal    -> (ED.HihatFoot, ED.GemNormal     , D.VelocityNormal) -- [56, [["Hihat", 4901], ["CrashL", 37]]]
  RR_Tom4          -> (ED.Tom2     , ED.GemNormal     , D.VelocityNormal) -- [57, [["HighTom", 1078], ["LowTom", 291], ["Kick", 9], ["Snare", 7]]]
  RR_HihatOpen     -> (ED.Hihat    , ED.GemHihatOpen  , D.VelocityNormal) -- [58, [["Hihat", 5923], ["CrashR", 205], ["CrashL", 6]]]
  RR_Tom3          -> (ED.Tom1     , ED.GemNormal     , D.VelocityNormal) -- [59, [["HighTom", 657], ["CrashR", 12], ["LowTom", 11], ["Snare", 2]]]
  RR_Tom2          -> (ED.Tom1     , ED.GemNormal     , D.VelocityNormal) -- [60, [["Hihat", 217], ["LowTom", 59], ["HighTom", 43], ["Snare", 5], ["CrashL", 2]]]
  RR_Crash1        -> (ED.CrashR   , ED.GemNormal     , D.VelocityNormal) -- [61, [["CrashR", 2922], ["CrashL", 228], ["Snare", 33], ["Hihat", 1]]]
  RR_Tom1          -> (ED.Tom1     , ED.GemNormal     , D.VelocityNormal) -- [62, [["CrashR", 234], ["LowTom", 69], ["CrashL", 31], ["Snare", 20], ["HighTom", 6]]]
  RR_Ride          -> (ED.Ride     , ED.GemNormal     , D.VelocityNormal) -- [63, [["CrashR", 1440], ["Hihat", 70], ["Snare", 8], ["CrashL", 6]]]
  RR_China         -> (ED.CrashR   , ED.GemNormal     , D.VelocityNormal) -- [64, [["CrashR", 146], ["CrashL", 126], ["Snare", 79], ["LowTom", 56], ["HighTom", 3], ["Hihat", 3]]]
  RR_RideBell      -> (ED.Ride     , ED.GemNormal     , D.VelocityNormal) -- [65, [["CrashR", 196], ["LowTom", 66], ["CrashL", 8], ["Snare", 4], ["Hihat", 1]]]
  RR_Tambourine    -> (ED.CrashL   , ED.GemNormal     , D.VelocityNormal) -- [66, [["CrashL", 3189], ["CrashR", 87], ["Hihat", 7]]]
  RR_Splash        -> (ED.CrashL   , ED.GemNormal     , D.VelocityNormal) -- [67, [["CrashL", 187], ["HighTom", 2], ["CrashR", 1]]]
  RR_Cowbell       -> (ED.CrashL   , ED.GemNormal     , D.VelocityNormal) -- [68, [["CrashL", 2100], ["CrashR", 60], ["Hihat", 2], ["Snare", 1]]]
  RR_Crash2        -> (ED.CrashL   , ED.GemNormal     , D.VelocityNormal) -- [69, [["CrashL", 162], ["Kick", 10], ["HighTom", 2]]]
  RR_Vibraslap     -> (ED.CrashL   , ED.GemNormal     , D.VelocityNormal) -- [70, [["CrashL", 352], ["CrashR", 72], ["Hihat", 6]]]
  RR_Ride2         -> (ED.CrashL   , ED.GemNormal     , D.VelocityNormal) -- [71, [["CrashL", 36], ["Kick", 4]]]

data RRDrumDifficulty t = RRDrumDifficulty
  { rrdGems      :: RTB.T t (RRDrum, RRChannel) -- starting from 48. none of these should be RRC_Hidden
  , rrdHidden    :: RTB.T t (RRDrum, RRChannel) -- starting from 72. these should all be RRC_Hidden
  , rrdFreestyle :: RTB.T t (Edge () (RRDrum, RRChannel)) -- starting from 0
  , rrdSolo      :: RTB.T t Bool
  -- TODO pitch 126 (highway star)
  -- TODO pitch 105 (highway star)
  } deriving (Show)

instance ParseTrack RRDrumDifficulty where
  parseTrack = do
    rrdGems      <- (rrdGems      =.) $ fatBlips (1/8) $ condenseMap $ eachKey each $ \drum -> channelBlip_ $ fromEnum drum + 48
    rrdHidden    <- (rrdHidden    =.) $ fatBlips (1/8) $ condenseMap $ eachKey each $ \drum -> channelBlip_ $ fromEnum drum + 72
    rrdFreestyle <- rrdFreestyle =. let
      decodeEdge :: (RRDrum, (Int, Maybe Int)) -> Edge () (RRDrum, RRChannel)
      decodeEdge (drum, (chan, mv)) = let
        chan' = fromMaybe RRC_Hidden $ lookup chan channelMap
        in case mv of
          Nothing -> EdgeOff   (drum, chan')
          Just _  -> EdgeOn () (drum, chan')
      encodeEdge :: Edge () (RRDrum, RRChannel) -> (RRDrum, (Int, Maybe Int))
      encodeEdge = \case
        EdgeOff   (drum, chan) -> (drum, (encodeChannel chan, Nothing))
        EdgeOn () (drum, chan) -> (drum, (encodeChannel chan, Just 96))
      in dimap (fmap encodeEdge) (fmap decodeEdge) $
        condenseMap $ eachKey each $ \drum ->
          edgesCV $ fromEnum drum
    rrdSolo      <- rrdSolo =. edges 127
    return RRDrumDifficulty{..}

importRRDrums :: Map.Map Difficulty (RRDrumDifficulty U.Beats) -> D.DrumTrack U.Beats
importRRDrums diffs = mempty
  { D.drumDifficulties = flip fmap diffs $ \rr -> D.DrumDifficulty
    { D.drumMix         = RTB.empty
    , D.drumPSModifiers = RTB.empty
    , D.drumGems
      -- pretty sure both toms or both crashes at the same time just translates to one 4-lane pad
      = RTB.flatten
      $ fmap nubOrd
      $ RTB.collectCoincident
      $ fmap (\pro -> (void pro, D.VelocityNormal))
      $ RTB.mapMaybe (rrChannel4Lane . snd)
      $ rrdGems rr
    }
  , D.drumToms = let
    expert = maybe mempty (RTB.mapMaybe (rrChannel4Lane . snd) . rrdGems) $ Map.lookup Expert diffs
    in U.trackJoin $ flip fmap expert $ \case
      D.Pro color D.Tom -> RTB.cons 0 (color, D.Tom) $ RTB.cons (1/32) (color, D.Cymbal) RTB.empty
      _                 -> RTB.empty
  , D.drumSolo = maybe RTB.empty rrdSolo $ Map.lookup Expert diffs
  }

importRREliteDrums :: RRDrumDifficulty U.Beats -> ED.EliteDrumDifficulty U.Beats
importRREliteDrums
  = ED.makeEliteDifficulty
  . fmap (\gem -> (D.RH <$ gem, ED.GemNormal, D.VelocityNormal))
  . RTB.mapMaybe (rrChannel7Lane . snd)
  . rrdGems

importRREliteLanes :: (NNC.C t) => RRDrumDifficulty t -> RTB.T t (Edge () (ED.EliteGem ()))
importRREliteLanes rr = flip RTB.mapMaybe rr.rrdFreestyle $ mapM $ \(_, chan) -> rrChannel7Lane chan

importRRHiddenDrums :: RRDrumDifficulty U.Beats -> ED.EliteDrumDifficulty U.Beats
importRRHiddenDrums rr
  = ED.makeEliteDifficulty
  $ fmap (rrDrumGuessTD . fst)
  $ RTB.merge (rrdGems rr) (rrdHidden rr)

-- control

-- notes on channel 0xF in control.mid
data SectionMarker
  = SectionIntro       -- 48
  | SectionVerse       -- 49
  | SectionChorus      -- 50
  | SectionBridge      -- 51
  | SectionMiddleEight -- 52
  | SectionGuitarSolo  -- 53
  | SectionCustom Int  -- pitches 54 through 70, I think (54 = SectionCustom 0)
  | SectionOutro       -- 71
  deriving (Eq, Show)

importRRSections :: [T.Text] -> RTB.T t (SectionMarker, Maybe Int) -> RTB.T t T.Text
importRRSections strings = fmap $ \(sect, num) -> let
  name = case sect of
    SectionIntro       -> "Intro"
    SectionVerse       -> "Verse"
    SectionChorus      -> "Chorus"
    SectionBridge      -> "Bridge"
    SectionMiddleEight -> "Middle Eight"
    SectionGuitarSolo  -> "Guitar Solo"
    SectionCustom n    -> case drop (n + 2) strings of
      []    -> "Custom Section " <> T.singleton (['A'..] !! n)
      s : _ -> T.strip s
    SectionOutro       -> "Outro"
  numbered = case num of
    Just n  -> name <> " " <> T.pack (show n)
    Nothing -> name
  in numbered

makeRRSections :: RTB.T t Section -> (RTB.T t (SectionMarker, Maybe Int), [T.Text])
makeRRSections sects = let
  stripNumberLetter t = case reverse $ T.words t of
    x : xs | isNumberLetter x -> T.unwords $ reverse xs
    _                         -> t
  -- True if t is 0 or more digits followed by an optional letter
  isNumberLetter t = case T.unsnoc t of
    Just (digits, lastChar) -> T.all isDigit digits && isAlphaNum lastChar
    Nothing                 -> False -- shouldn't happen
  decideCustom = flip fmap sects $ \s -> let
    normal = stripNumberLetter (makeDisplaySection s).name
    in case T.toLower $ T.filter isAlphaNum normal of
      "intro"       -> Left SectionIntro
      "verse"       -> Left SectionVerse
      "chorus"      -> Left SectionChorus
      "bridge"      -> Left SectionBridge
      "middleeight" -> Left SectionMiddleEight
      "gtrsolo"     -> Left SectionGuitarSolo
      "guitarsolo"  -> Left SectionGuitarSolo
      "outro"       -> Left SectionOutro
      _             -> Right normal
  maxCustomSections = 17
  customList = case splitAt maxCustomSections $ nubOrd $ rights $ RTB.getBodies decideCustom of
    (list, []   ) -> list
    (list, _ : _) -> init list <> ["Section"]
  markers = flip fmap decideCustom $ \case
    Left  s      -> s
    Right custom -> SectionCustom $ fromMaybe (maxCustomSections - 1) $ elemIndex custom customList
  addNumbers _    RNil            = RNil
  addNumbers prev (Wait t x rest) = let
    num = case (length $ filter (== x) prev, any (== x) rest) of
      (0, False) -> Nothing
      (n, _    ) -> Just $ n + 1
    in Wait t (x, num) $ addNumbers (x : prev) rest
  in (addNumbers [] markers, customList)

data RRControl t = RRControl
  { rrcAnimDrummer   :: RTB.T t Int -- DrummerSignalRuleActions.lua, (Male|Female)DrummerAnimations.lua
  , rrcAnimGuitarist :: RTB.T t Int -- GuitaristSignalRuleActions.lua, (Male|Female)GuitaristAnimations.lua
  , rrcAnimBassist   :: RTB.T t Int -- BassistSignalRuleActions.lua, (Male|Female)BassistAnimations.lua
  , rrcAnimVocalist  :: RTB.T t Int -- VocalistSignalRuleActions.lua, (Male|Female)VocalistAnimations.lua
  , rrcGemsDrums     :: RTB.T t RRDrum
  , rrcGemsGuitar    :: RTB.T t (Edge () Five.Color)
  , rrcGemsBass      :: RTB.T t (Edge () Five.Color)
  , rrcVenue         :: RTB.T t Int -- v0006_002.lua
  , rrcEnd           :: RTB.T t ()
  , rrcSections      :: RTB.T t (SectionMarker, Maybe Int) -- a0041.lua says: "-- SongSections set up in Album.cpp"
  , rrcBeat          :: RTB.T t (Maybe BeatEvent)
  } deriving (Show)

readRRControl :: (NNC.C t) => RTB.T t (E.T s) -> (RRControl t, RTB.T t (E.T s))
readRRControl trk = let
  notes = RTB.mapMaybe isNoteEdgeCPV trk
  getChannelBlips chan = flip RTB.mapMaybe notes $ \(c, p, mv) -> do
    guard $ c == chan && isJust mv
    return p
  getChannelBlipsVelocity chan = flip RTB.mapMaybe notes $ \(c, p, mv) -> do
    guard $ c == chan
    v <- mv
    return (p, v)
  chan14 = getChannelBlips 14
  chan15 = getChannelBlipsVelocity 15
  getGBAnim chan = flip RTB.mapMaybe notes $ \(c, p, mv) -> do
    guard $ c == chan
    color <- lookup p [(72, Five.Green), (73, Five.Red), (74, Five.Yellow), (75, Five.Blue), (76, Five.Orange)]
    return $ if isJust mv then EdgeOn () color else EdgeOff color
  force rtb = seq (forceTrackSpine rtb) rtb
  rrc = RRControl
    { rrcAnimDrummer   = force $ getChannelBlips 0
    , rrcAnimGuitarist = force $ getChannelBlips 1
    , rrcAnimBassist   = force $ getChannelBlips 2
    , rrcAnimVocalist  = force $ getChannelBlips 3
    , rrcGemsDrums     = force $ RTB.mapMaybe
      (\n -> guard (72 <= n && n <= 95) >> Just (toEnum $ n - 72))
      (getChannelBlips 11)
    , rrcGemsGuitar    = force $ getGBAnim 12
    , rrcGemsBass      = force $ getGBAnim 13
    , rrcVenue         = force $ RTB.filter (/= 126) chan14
    , rrcEnd           = force $ void $ RTB.filter (== 126) chan14
    , rrcSections      = force $ flip RTB.mapMaybe chan15 $ \(p, v) -> do
      sect <- case p of
        48                     -> Just SectionIntro
        49                     -> Just SectionVerse
        50                     -> Just SectionChorus
        51                     -> Just SectionBridge
        52                     -> Just SectionMiddleEight
        53                     -> Just SectionGuitarSolo
        n | 54 <= n && n <= 70 -> Just $ SectionCustom $ n - 54
        71                     -> Just SectionOutro
        _                      -> Nothing
      return (sect, guard (v /= 100) >> Just v)
    , rrcBeat          = force $ flip RTB.mapMaybe chan15 $ \(p, _) -> case p of
      96  -> Just $ Just Bar
      98  -> Just $ Just Beat
      100 -> Just Nothing
      _   -> Nothing
    }
  unrecognized = force $ flip RTB.filter trk $ \e -> case isNoteEdgeCPV e of
    Nothing -> case e of
      E.MetaEvent (Meta.TrackName _) -> False
      _                              -> True
    Just (c, p, mv) -> isJust mv && case c of
      0  -> False
      1  -> False
      2  -> False
      3  -> False
      4  -> True
      5  -> True
      6  -> True
      7  -> True
      8  -> True
      9  -> True
      10 -> True
      11 -> not $ 72 <= p && p <= 95
      12 -> not $ 72 <= p && p <= 76
      13 -> not $ 72 <= p && p <= 76
      14 -> False
      15 -> not $ (48 <= p && p <= 71) || elem p [96, 98, 100]
      _  -> False -- shouldn't happen
  in (rrc, unrecognized)

showRRControl :: (Ord s) => RRControl U.Beats -> RTB.T U.Beats (E.T s)
showRRControl rrc = foldr RTB.merge RTB.empty
  [ showBlips 0 rrc.rrcAnimDrummer
  , showBlips 1 rrc.rrcAnimGuitarist
  , showBlips 2 rrc.rrcAnimBassist
  , showBlips 3 rrc.rrcAnimVocalist
  , showBlips 11 $ fmap (\rrd -> fromEnum rrd + 72) rrc.rrcGemsDrums
  , flip fmap rrc.rrcGemsGuitar $ \case
    EdgeOn () color -> makeEdgeCPV 12 (fromEnum color + 72) (Just 96)
    EdgeOff   color -> makeEdgeCPV 12 (fromEnum color + 72) Nothing
  , flip fmap rrc.rrcGemsBass $ \case
    EdgeOn () color -> makeEdgeCPV 13 (fromEnum color + 72) (Just 96)
    EdgeOff   color -> makeEdgeCPV 13 (fromEnum color + 72) Nothing
  , showBlips 14 $ RTB.merge rrc.rrcVenue (126 <$ rrc.rrcEnd)
  , RTB.merge
    (showBlipsVelocity 15 $ sectionToPitch <$> rrc.rrcSections)
    (showBlips         15 $ beatToPitch    <$> rrc.rrcBeat    )
  ] where
    showBlips chan
      = fmap makeEdge'
      . blipEdgesRBNice
      . fmap (\p -> (96, (chan, p), Nothing))
    showBlipsVelocity chan
      = fmap makeEdge'
      . blipEdgesRBNice
      . fmap (\(p, v) -> (v, (chan, p), Nothing))
    sectionToPitch (sect, num) = let
      p = case sect of
        SectionIntro       -> 48
        SectionVerse       -> 49
        SectionChorus      -> 50
        SectionBridge      -> 51
        SectionMiddleEight -> 52
        SectionGuitarSolo  -> 53
        SectionCustom n    -> 54 + n
        SectionOutro       -> 71
      v = fromMaybe 100 num
      in (p, v)
    beatToPitch = \case
      Just Bar  -> 96
      Just Beat -> 98
      Nothing   -> 100

--------------------------------------------------------------------------------

-- These are a hack to account for MP3 audio being delayed when used on 360.
-- When exporting, we delay all midi contents by the appropriate amount
-- by changing initial tempos. When importing, we do that change in reverse.

applyRR360MP3Hack, unapplyRR360MP3Hack :: U.TempoMap -> U.TempoMap
applyRR360MP3Hack tmap = let
  anchorSecondsOriginal = U.applyTempoMap tmap anchorBeats
  anchorSecondsNew = anchorSecondsOriginal + rr360MP3HackAmount
  newStartTempo = U.makeTempo anchorBeats anchorSecondsNew
  in U.tempoMapFromBPS
    $ Wait 0 newStartTempo
    $ Wait anchorBeats (U.getTempoAtTime tmap anchorBeats)
    $ U.trackDropZero
    $ U.trackDrop anchorBeats
    $ U.tempoMapToBPS tmap
unapplyRR360MP3Hack tmap = let
  anchorSecondsOriginal = U.applyTempoMap tmap anchorBeats
  anchorSecondsNew = anchorSecondsOriginal NNC.-| rr360MP3HackAmount
  in if anchorSecondsNew <= 0
    then tmap -- TODO warn or something. too high tempos
    else let
      newStartTempo = U.makeTempo anchorBeats anchorSecondsNew
      in U.tempoMapFromBPS
        $ Wait 0 newStartTempo
        $ Wait anchorBeats (U.getTempoAtTime tmap anchorBeats)
        $ U.trackDropZero
        $ U.trackDrop anchorBeats
        $ U.tempoMapToBPS tmap

anchorBeats :: U.Beats
anchorBeats = 2

rr360MP3HackAmount :: U.Seconds
rr360MP3HackAmount = 0.060
