{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
module Neversoft.Export where

import           Config
import           Control.Monad.Trans.StackTrace   (SendMessage, StackTraceT)
import           Data.Bits
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Word
import           Guitars                          (applyForces, fixSloppyNotes,
                                                   getForces5, openNotes',
                                                   strumHOPOTap')
import           Neversoft.Note
import           RockBand.Codec.Beat
import qualified RockBand.Codec.Drums             as D
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import           RockBand.Common                  (Difficulty (..),
                                                   pattern RNil,
                                                   StrumHOPOTap (..),
                                                   pattern Wait,
                                                   noRedundantStatus)
import           RockBand3                        (BasicTiming (..),
                                                   basicTiming)
import qualified Sound.MIDI.Util                  as U

makeGHWoRNote
  :: (SendMessage m)
  => SongYaml f
  -> TargetGH5
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds -- ^ get longest audio length
  -> StackTraceT m GHNoteFile
makeGHWoRNote songYaml target song@(RBFile.Song tmap _mmap ofile) getAudioLength = let
  beatsToMS :: U.Beats -> Word32
  beatsToMS = floor . (* 1000) . U.applyTempoMap tmap
  makeGB fpart diff = case getPart fpart songYaml >>= partGRYBO of
    Just grybo -> let
      opart = fromMaybe mempty $ Map.lookup fpart $ RBFile.onyxParts ofile
      (trk, algo) = RBFile.selectGuitarTrack RBFile.FiveTypeGuitarExt opart
      fd = fromMaybe mempty $ Map.lookup diff $ F.fiveDifficulties trk
      notes
        = applyForces (getForces5 fd)
        . strumHOPOTap' algo (fromIntegral (gryboHopoThreshold grybo) / 480)
        . fixSloppyNotes (10 / 480)
        . openNotes'
        $ fd
        :: RTB.T U.Beats ((Maybe F.Color, StrumHOPOTap), Maybe U.Beats)
      in GuitarBass
        { gb_instrument = do
          (t, evts) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident $ notes
          let start = beatsToMS t
              maybeLen = NE.nonEmpty (map snd evts) >>= minimum
              end = maybe (start + 1) (\bts -> beatsToMS $ t + bts) maybeLen
          return Note
            { noteTimeOffset = beatsToMS t
            , noteDuration = fromIntegral $ end - start
            , noteBits = foldr (.|.) 0 $ do
              ((mcolor, sht), _len) <- evts
              let shtBit = if sht /= Strum then bit 6 else 0
                  colorBit = bit $ case mcolor of
                    Just F.Green  -> 0
                    Just F.Red    -> 1
                    Just F.Yellow -> 2
                    Just F.Blue   -> 3
                    Just F.Orange -> 4
                    Nothing       -> 5
              return $ shtBit .|. colorBit
            , noteAccent = 31 -- probably doesn't do anything
            }
        , gb_tapping = [] -- TODO
        , gb_starpower = [] -- TODO
        }
    Nothing -> GuitarBass [] [] []
  makeDrums fpart diff = case getPart fpart songYaml >>= partDrums of
    Just pd -> let
      opart = fromMaybe mempty $ Map.lookup fpart $ RBFile.onyxParts ofile
      trk = RBFile.onyxPartDrums opart -- TODO use other tracks potentially
      dd = fromMaybe mempty $ Map.lookup diff $ D.drumDifficulties trk
      add2x xs = case diff of
        Expert -> RTB.merge xs $ Nothing <$ D.drumKick2x trk
        _      -> xs
      fiveLane = case drumsMode pd of
        Drums5 -> add2x $ fmap Just $ D.drumGems dd
        DrumsPro -> let
          pro = add2x $ fmap Just $ D.computePro (Just diff) trk
          eachGroup evts = concat
            [ [Just (D.Kick, vel) | Just (D.Kick, vel) <- evts]
            , [Nothing | Nothing <- evts]
            , [Just (D.Red, vel) | Just (D.Red, vel) <- evts]
            -- Y and G notes in input go to the left and right cymbals/toms
            -- B notes go to right cymbal/tom, or left if there's already a right one
            , let
              ycym = [Just (D.Pro D.Yellow (), vel) | Just (D.Pro D.Yellow D.Cymbal, vel) <- evts]
              gcym = [Just (D.Orange, vel) | Just (D.Pro D.Green D.Cymbal, vel) <- evts]
              bcym = do
                Just (D.Pro D.Blue D.Cymbal, vel) <- evts
                return $ Just (if null gcym then D.Orange else D.Pro D.Yellow (), vel)
              in concat [ycym, gcym, bcym]
            , let
              ytom = [Just (D.Pro D.Blue (), vel) | Just (D.Pro D.Yellow D.Tom, vel) <- evts]
              gtom = [Just (D.Pro D.Green (), vel) | Just (D.Pro D.Green D.Tom, vel) <- evts]
              btom = do
                Just (D.Pro D.Blue D.Tom, vel) <- evts
                return $ Just (D.Pro (if null gtom then D.Green else D.Blue) (), vel)
              in concat [ytom, gtom, btom]
            ]
          in RTB.flatten $ fmap eachGroup $ RTB.collectCoincident pro
        _ -> RTB.empty -- TODO handle basic, real, full
      drumBit = bit . \case
        Just (D.Pro D.Green () , _) -> 0
        Just (D.Red            , _) -> 1
        Just (D.Pro D.Yellow (), _) -> 2
        Just (D.Pro D.Blue ()  , _) -> 3
        Just (D.Orange         , _) -> 4
        Just (D.Kick           , _) -> 5
        Nothing                     -> 6
      accentBit drum = case drum of
        Just (D.Kick, _)           -> 0
        Just (_, D.VelocityAccent) -> drumBit drum
        _                          -> 0
      ghostBit drum = case drum of
        Just (D.Kick, _)          -> 0
        Just (_, D.VelocityGhost) -> drumBit drum
        _                         -> 0
      withGhost = do
        (t, evts) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident fiveLane
        return NoteExpertDrums
          { xdNote = Note
            { noteTimeOffset = beatsToMS t
            , noteDuration = 1 -- should be fine?
            , noteBits = foldr (.|.) 0 $ map drumBit evts
            , noteAccent = foldr (.|.) 0 $ map accentBit evts
            }
          , xdGhost = foldr (.|.) 0 $ map ghostBit evts
          }
      in Drums
        { drums_instrument = case diff of
          Expert -> Right withGhost
          _      -> Left $ map xdNote withGhost
        , drums_starpower = [] -- TODO
        , drums_drumfill = [] -- TODO
        }
    Nothing -> Drums (case diff of Expert -> Right []; _ -> Left []) [] []
  in do
    timing <- basicTiming song getAudioLength
    let fretbars = map beatsToMS $ ATB.getTimes $ RTB.toAbsoluteEventList 0 $ beatLines $ timingBeat timing
        beatToTimesigs = \case
          Wait t _bar rest -> case RTB.span (== Beat) rest of
            (beats, after) -> let
              num = 1 + length beats
              in Wait t num $ beatToTimesigs $ RTB.delay (mconcat $ RTB.getTimes beats) after
          RNil -> RNil
        timesig = do
          (t, num) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ noRedundantStatus $ beatToTimesigs $ beatLines $ timingBeat timing
          return TimeSig
            { tsTimestamp = beatsToMS t
            , tsNumerator = fromIntegral num
            , tsDenominator = 4
            }
    return GHNoteFile
      { gh_guitareasy            = makeGB (gh5_Guitar target) Easy
      , gh_guitarmedium          = makeGB (gh5_Guitar target) Medium
      , gh_guitarhard            = makeGB (gh5_Guitar target) Hard
      , gh_guitarexpert          = makeGB (gh5_Guitar target) Expert

      , gh_basseasy              = makeGB (gh5_Bass target) Easy
      , gh_bassmedium            = makeGB (gh5_Bass target) Medium
      , gh_basshard              = makeGB (gh5_Bass target) Hard
      , gh_bassexpert            = makeGB (gh5_Bass target) Expert

      , gh_drumseasy             = makeDrums (gh5_Drums target) Easy
      , gh_drumsmedium           = makeDrums (gh5_Drums target) Medium
      , gh_drumshard             = makeDrums (gh5_Drums target) Hard
      , gh_drumsexpert           = makeDrums (gh5_Drums target) Expert

      , gh_vocals                = [] -- TODO
      , gh_vocallyrics           = [] -- TODO
      , gh_vocalstarpower        = [] -- TODO
      , gh_vocalphrase           = [] -- TODO
      , gh_vocalfreeform         = [] -- TODO

      , gh_backup_vocalphrase    = Nothing
      , gh_backup_vocals         = Nothing
      , gh_backup_vocalfreeform  = Nothing
      , gh_backup_vocallyrics    = Nothing
      , gh_backup_vocalstarpower = Nothing

      , gh_fretbar               = fretbars
      , gh_timesig               = timesig
      , gh_bandmoment            = [] -- TODO

      , gh_markers               = [] -- TODO
      , gh_vocalmarkers          = [] -- TODO
      , gh_backup_vocalmarkers   = Nothing
      }
