{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Neversoft.Export where

import           Config
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace   (SendMessage, StackTraceT,
                                                   fatal, inside, stackIO)
import           Data.Binary.Put                  (runPut)
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Either                      (rights)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (partition)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.SimpleHandle                (Folder (..), Readable,
                                                   byteStringSimpleHandle,
                                                   handleToByteString,
                                                   makeHandle, useHandle)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Word
import           Guitars                          (applyForces, fixSloppyNotes,
                                                   getForces5, openNotes',
                                                   strumHOPOTap')
import           Neversoft.Checksum               (qbKeyCRC)
import           Neversoft.Metadata               (SongInfo (..), parseSongInfo)
import           Neversoft.Note
import           Neversoft.Pak                    (Node (..), buildPak, makeQS,
                                                   parseQS, splitPakNodes)
import           Neversoft.QB                     (lookupQS, parseQB)
import           RockBand.Codec.Beat
import qualified RockBand.Codec.Drums             as D
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import           RockBand.Common                  (Difficulty (..), Edge (..),
                                                   pattern RNil,
                                                   StrumHOPOTap (..),
                                                   pattern Wait,
                                                   joinEdgesSimple,
                                                   noRedundantStatus)
import           RockBand3                        (BasicTiming (..),
                                                   basicTiming)
import qualified Sound.MIDI.Util                  as U
import           STFS.Package                     (runGetM)

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
  makeStarPower od = do
    (t, ((), (), len)) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ joinEdgesSimple $ flip fmap od $ \case
      True  -> EdgeOn () ()
      False -> EdgeOff ()
    let start = beatsToMS t
        end   = beatsToMS $ t + len
    return $ Single start (fromIntegral $ end - start)
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
              -- TODO open pulloff notes only work on bass for some reason.
              -- Should probably convert to no-opens if any on guitar
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
            , noteAccent = 31
            -- TODO noteAccent is required for extended sustains to work.
            -- Clear bits for colors with notes that overlap this one
            }
        , gb_tapping = makeStarPower $ F.fiveTap fd
        , gb_starpower = makeStarPower $ F.fiveOverdrive trk
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
        , drums_starpower = makeStarPower $ D.drumOverdrive trk
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

data GHWoRInput = GHWoRInput
  { ghworNote    :: GHNoteFile
  , ghworTitle   :: T.Text
  , ghworArtist  :: T.Text
  , ghworAlbum   :: T.Text
  , ghworFSB1    :: BL.ByteString
  , ghworFSB2    :: BL.ByteString
  , ghworFSB3    :: BL.ByteString
  , ghworPreview :: BL.ByteString
  }

replaceGHWoRDLC
  :: (MonadIO m)
  => B.ByteString
  -> GHWoRInput
  -> Folder T.Text Readable
  -> StackTraceT m (Folder T.Text Readable)
replaceGHWoRDLC k input folder = do
  -- find song pak, replace .note file (add key in front)
  -- swap out fsbs
  -- find text pak
  --   load .qb
  --     find metadata for song we are replacing, get QS for title/artist/album
  --   edit .qs.en with same name as lone .qb, replace the QS
  let findOnlyType ftype nodes = case partition (\(node, _) -> nodeFileType node == qbKeyCRC ftype) nodes of
        ([pair], rest) -> return (pair, rest)
        (matches, _) -> fatal $ "Expected 1 " <> B8.unpack ftype <> " file in .pak, but found " <> show (length matches)
  (textPakName, newTextPak) <- case filter (\(name, _) -> "_text.pak.xen" `T.isSuffixOf` name) $ folderFiles folder of
    [(name, r)] -> do
      bs <- stackIO $ useHandle r handleToByteString
      (qbPair@(qbNode, qb), notQb) <- findOnlyType ".qb" $ splitPakNodes bs
      let isMatchingQs node = nodeFileType node == qbKeyCRC ".qs.en" && nodeFilenameCRC node == nodeFilenameCRC qbNode
      ((qsNode, qs), notQs) <- case partition (\(node, _) -> isMatchingQs node) notQb of
        ([pair], rest) -> return (pair, rest)
        (matches, _) -> fatal $ "Expected 1 .qs.en file to go with .qb in .pak, but found " <> show (length matches)
      qbSections <- inside "Parsing .qb" $ runGetM parseQB qb
      qsList <- maybe (fatal "Couldn't parse .qs.en") return $ parseQS qs
      let qsBank = HM.fromList qsList
          qbSections' = map (lookupQS qsBank) qbSections
          songInfos = concat $ rights $ map parseSongInfo qbSections'
      songInfo <- case filter (\si -> songName si == k) songInfos of
        info : _ -> return info
        []       -> fatal "Couldn't find existing song info in _text.pak.xen"
      let replacingQS (w, _) = elem w $ map fst [songTitle songInfo, songArtist songInfo, songAlbumTitle songInfo]
          qsList' = filter (not . replacingQS) qsList <>
            [ (fst $ songTitle songInfo, "\\L" <> ghworTitle input)
            , (fst $ songArtist songInfo, "\\L" <> ghworArtist input)
            , (fst $ songAlbumTitle songInfo, "\\L" <> ghworAlbum input)
            ]
          newNodes = (qsNode, makeQS qsList') : qbPair : notQs
      return (name, buildPak newNodes)
    _ -> fatal "Not exactly 1 _text.pak.xen"
  let songPakName = "b" <> TE.decodeUtf8 k <> "_song.pak.xen"
      fsb1Name = "a" <> TE.decodeUtf8 k <> "_1.fsb.xen"
      fsb2Name = "a" <> TE.decodeUtf8 k <> "_2.fsb.xen"
      fsb3Name = "a" <> TE.decodeUtf8 k <> "_3.fsb.xen"
      previewName = "a" <> TE.decodeUtf8 k <> "_preview.fsb.xen"
  newSongPak <- do
    bs <- case lookup songPakName $ folderFiles folder of
      Nothing -> fatal $ "Couldn't find " <> T.unpack songPakName
      Just r  -> stackIO $ useHandle r handleToByteString
    ((node, _), notNotes) <- findOnlyType ".note" $ splitPakNodes bs
    let newNotePair = (node, runPut $ putNote (qbKeyCRC k) $ makeWoRNoteFile $ ghworNote input)
    return $ buildPak $ newNotePair : notNotes
  let newPair name bs = (name, makeHandle ("New contents for " <> T.unpack name) $ byteStringSimpleHandle bs)
  return folder
    { folderFiles = filter (\(name, _) -> notElem name [textPakName, songPakName, fsb1Name, fsb2Name, fsb3Name, previewName]) (folderFiles folder) <>
      [ newPair textPakName newTextPak
      , newPair songPakName newSongPak
      , newPair fsb1Name $ ghworFSB1 input
      , newPair fsb2Name $ ghworFSB2 input
      , newPair fsb3Name $ ghworFSB3 input
      , newPair previewName $ ghworPreview input
      ]
    }
