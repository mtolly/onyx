{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Build (shakeBuildFiles, shakeBuild, targetTitle, loadYaml, validFileName, validFileNamePiece, NameRule(..), hashRB3) where

import           Audio
import           AudioSearch
import qualified C3
import qualified Codec.Archive.Zip                     as Zip
import           Codec.Picture
import qualified Codec.Picture.STBIR                   as STBIR
import           Codec.Picture.Types                   (dropTransparency)
import           Config                                hiding (Difficulty)
import           Control.Applicative                   (liftA2)
import           Control.Monad.Codec.Onyx              (makeValue, valueId)
import           Control.Monad.Codec.Onyx.JSON         (loadYaml)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                        (first, second)
import           Data.Binary.Put                       (putWord32be, runPut)
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Base64.Lazy           as B64
import qualified Data.ByteString.Char8                 as B8
import qualified Data.ByteString.Lazy                  as BL
import           Data.Char                             (isAlphaNum, isAscii,
                                                        isControl, isDigit,
                                                        isSpace)
import           Data.Conduit                          (runConduit)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.SampleRate
import           Data.Conduit.Audio.Sndfile
import           Data.Default.Class                    (def)
import qualified Data.DTA                              as D
import qualified Data.DTA.Serialize                    as D
import qualified Data.DTA.Serialize.Magma              as Magma
import qualified Data.DTA.Serialize.RB3                as D
import qualified Data.EventList.Absolute.TimeBody      as ATB
import qualified Data.EventList.Relative.TimeBody      as RTB
import           Data.Fixed                            (Centi, Fixed (..),
                                                        Milli)
import           Data.Foldable                         (toList)
import           Data.Functor.Identity                 (Identity (..))
import           Data.Hashable                         (Hashable, hash)
import qualified Data.HashMap.Strict                   as HM
import           Data.Int                              (Int32)
import           Data.List.Extra                       (intercalate, nubOrd,
                                                        sort, sortOn)
import           Data.List.NonEmpty                    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, fromMaybe,
                                                        isJust, isNothing,
                                                        listToMaybe, mapMaybe)
import           Data.SimpleHandle                     (Folder (..),
                                                        crawlFolder,
                                                        fileReadable)
import           Data.String                           (IsString, fromString)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import           Data.Time                             (defaultTimeLocale,
                                                        formatTime,
                                                        getZonedTime)
import qualified Data.UUID                             as UUID
import qualified Data.UUID.V4                          as UUID
import qualified Data.Vector                           as V
import           Data.Version                          (showVersion)
import           DeriveHelpers                         (mergeEmpty)
import           Development.Shake                     hiding (phony, (%>),
                                                        (&%>))
import           Development.Shake.FilePath
import           Difficulty
import           DryVox                                (clipDryVox,
                                                        toDryVoxFormat,
                                                        vocalTubes)
import qualified DTXMania.DTX                          as DTX
import           FFMPEG                                (audioIntegratedVolume)
import qualified FretsOnFire                           as FoF
import           Genre
import           GuitarHeroII.Audio                    (writeVGS,
                                                        writeVGSMultiRate)
import           GuitarHeroII.Convert
import qualified GuitarHeroII.Events                   as GH2
import           GuitarHeroII.File
import           Guitars                               (guitarify', openNotes')
import           Image
import qualified Magma
import qualified MelodysEscape                         as Melody
import           MoggDecrypt
import           Neversoft.Audio                       (gh3Encrypt,
                                                        ghworEncrypt)
import           Neversoft.Checksum                    (qbKeyCRC, qsKey)
import           Neversoft.Export                      (makeGHWoRNote,
                                                        packageNameHash,
                                                        worFileBarePak,
                                                        worFileManifest,
                                                        worFileTextPak)
import           Neversoft.Note                        (makeWoRNoteFile,
                                                        putNote)
import           Neversoft.Pak                         (Node (..), buildPak,
                                                        makeQS, parseQS,
                                                        worMetadataString)
import           Neversoft.QB                          (QBArray (..),
                                                        QBSection (..),
                                                        QBStructItem (..),
                                                        putQB)
import           NPData                                (npdContentID,
                                                        packNPData,
                                                        rb2CustomMidEdatConfig,
                                                        rb3CustomMidEdatConfig)
import qualified Numeric.NonNegative.Class             as NNC
import           OSFiles                               (copyDirRecursive,
                                                        shortWindowsPath)
import           Overdrive                             (calculateUnisons,
                                                        getOverdrive,
                                                        printFlexParts)
import           Path                                  (parseAbsDir, toFilePath)
import           Paths_onyxite_customs_lib             (version)
import           PlayStation.PKG                       (makePKG)
import           Preferences                           (MagmaSetting (..))
import           PrettyDTA
import           ProKeysRanges
import           Reaper.Build                          (TuningInfo (..),
                                                        makeReaperShake)
import           RenderAudio
import           Resources                             (emptyMilo, emptyMiloRB2,
                                                        emptyWeightsRB2,
                                                        getResourcesPath,
                                                        ghWoRSamplePerf,
                                                        ghWoRthumbnail,
                                                        onyxAlbum, webDisplay)
import           RockBand.Codec                        (mapTrack)
import           RockBand.Codec.Beat
import qualified RockBand.Codec.Drums                  as RBDrums
import           RockBand.Codec.Events
import           RockBand.Codec.File                   (saveMIDI, shakeMIDI)
import qualified RockBand.Codec.File                   as RBFile
import           RockBand.Codec.Five
import qualified RockBand.Codec.FullDrums              as FD
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.Venue
import           RockBand.Codec.Vocal                  (nullVox)
import           RockBand.Common
import           RockBand.Milo                         (MagmaLipsync (..),
                                                        autoLipsync,
                                                        defaultTransition,
                                                        englishSyllables,
                                                        gh2Lipsync,
                                                        lipsyncAdjustSpeed,
                                                        lipsyncFromMIDITrack,
                                                        lipsyncPad,
                                                        loadVisemesRB3,
                                                        magmaMilo, parseLipsync,
                                                        putVocFile)
import qualified RockBand.ProGuitar.Play               as PGPlay
import           RockBand.Score                        (gh2Base)
import           RockBand.Sections                     (makeRB2Section,
                                                        makeRB3Section,
                                                        makeRBN2Sections)
import qualified RockBand2                             as RB2
import qualified RockBand3                             as RB3
import qualified Rocksmith.ArrangementXML              as Arr
import qualified Rocksmith.CST                         as CST
import           Rocksmith.MIDI
import qualified Sound.File.Sndfile                    as Snd
import           Sound.FSB                             (emitFSB, ghBandFSB)
import qualified Sound.Jammit.Base                     as J
import qualified Sound.Jammit.Export                   as J
import qualified Sound.MIDI.File.Event                 as E
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.Util                       as U
import           STFS.Package                          (CreateOptions (..),
                                                        LicenseEntry (..),
                                                        gh2pkg, makeCONReadable,
                                                        rb2pkg, rb3pkg, runGetM)
import qualified System.Directory                      as Dir
import           System.Environment.Executable         (getExecutablePath)
import           System.IO                             (IOMode (ReadMode),
                                                        hFileSize,
                                                        withBinaryFile)
import           System.Random                         (randomRIO)
import           Text.Decode                           (decodeGeneral)
import           Text.Read                             (readMaybe)
import           Text.Transform                        (replaceCharsRB)
import           WebPlayer                             (makeDisplay)

applyTargetAudio :: (MonadResource m) => TargetCommon -> RBFile.Song f -> AudioSource m Float -> AudioSource m Float
applyTargetAudio tgt mid = let
  eval = evalPreviewTime False Nothing mid
  bounds seg = liftA2 (,) (eval $ seg_FadeStart seg) (eval $ seg_FadeEnd seg)
  toDuration :: U.Seconds -> Duration
  toDuration = Seconds . realToFrac
  applyEnd = case tgt_End tgt >>= bounds of
    Nothing           -> id
    Just (start, end) -> fadeEnd (toDuration $ end - start) . takeStart (toDuration end)
  applyStart = case tgt_Start tgt >>= bounds of
    Nothing           -> id
    Just (start, end) -> fadeStart (toDuration $ end - start) . dropStart (toDuration start)
  applySpeed = applySpeedAudio tgt
  in applySpeed . applyStart . applyEnd

applySpeedAudio :: (MonadResource m) => TargetCommon -> AudioSource m Float -> AudioSource m Float
applySpeedAudio tgt = case fromMaybe 1 $ tgt_Speed tgt of
  1 -> id
  n -> stretchFull (1 / n) 1

lastEvent :: (NNC.C t) => RTB.T t a -> Maybe (t, a)
lastEvent (Wait !t x RNil) = Just (t, x)
lastEvent (Wait !t _ xs  ) = lastEvent $ RTB.delay t xs
lastEvent RNil             = Nothing

applyTargetMIDI :: TargetCommon -> RBFile.Song (RBFile.OnyxFile U.Beats) -> RBFile.Song (RBFile.OnyxFile U.Beats)
applyTargetMIDI tgt mid = let
  eval = fmap (U.unapplyTempoMap $ RBFile.s_tempos mid) . evalPreviewTime False Nothing mid
  applyEnd = case tgt_End tgt >>= eval . seg_Notes of
    Nothing -> id
    Just notesEnd -> \m -> m
      { RBFile.s_tracks
        = chopTake notesEnd
        $ RBFile.s_tracks m
      -- the RockBand3 module process functions will remove tempos and sigs after [end]
      }
  applyStart = case tgt_Start tgt >>= \seg -> liftA2 (,) (eval $ seg_FadeStart seg) (eval $ seg_Notes seg) of
    Nothing -> id
    Just (audioStart, notesStart) -> \m -> m
      { RBFile.s_tracks
        = mapTrack (RTB.delay $ notesStart - audioStart)
        $ chopDrop notesStart
        $ RBFile.s_tracks m
      , RBFile.s_tempos = case U.trackSplit audioStart $ U.tempoMapToBPS $ RBFile.s_tempos m of
        -- cut time off the front of the tempo map, and copy the last tempo
        -- from before the cut point to the cut point if needed
        (cut, keep) -> U.tempoMapFromBPS $ case U.trackTakeZero keep of
          [] -> U.trackGlueZero (toList $ snd . snd <$> RTB.viewR cut) keep
          _  -> keep
      , RBFile.s_signatures = case U.trackSplit audioStart $ U.measureMapToTimeSigs $ RBFile.s_signatures m of
        (cut, keep) -> U.measureMapFromTimeSigs U.Error $ case U.trackTakeZero keep of
          _ : _ -> keep -- already a time signature at the cut point
          []    -> case lastEvent cut of
            Nothing -> keep
            Just (t, sig) -> let
              len = U.timeSigLength sig
              afterSig = audioStart - t
              (_, barRemainder) = properFraction $ afterSig / len :: (Int, U.Beats)
              in if barRemainder == 0
                then U.trackGlueZero [sig] keep -- cut point is on an existing barline
                else let
                  partial = barRemainder * len
                  afterPartial = U.trackDrop partial keep
                  in U.trackGlueZero [U.measureLengthToTimeSig partial] $
                    case U.trackTakeZero afterPartial of
                      _ : _ -> keep -- after the partial bar there's an existing signature
                      []    -> Wait partial sig afterPartial -- continue with the pre-cut signature
      }
  applySpeed = case fromMaybe 1 $ tgt_Speed tgt of
    1     -> id
    speed -> \m -> m
      { RBFile.s_tempos
        = U.tempoMapFromBPS
        $ fmap (* realToFrac speed)
        $ U.tempoMapToBPS
        $ RBFile.s_tempos m
      }
  in applySpeed . applyStart . applyEnd $ mid

applyTargetLength :: TargetCommon -> RBFile.Song (f U.Beats) -> U.Seconds -> U.Seconds
applyTargetLength tgt mid = let
  applyEnd = case tgt_End tgt >>= evalPreviewTime False Nothing mid . seg_FadeEnd of
    Nothing   -> id
    Just secs -> min secs
  applyStart = case tgt_Start tgt >>= evalPreviewTime False Nothing mid . seg_FadeStart of
    Nothing   -> id
    Just secs -> subtract secs
  applySpeed t = t / realToFrac (fromMaybe 1 $ tgt_Speed tgt)
  in applySpeed . applyStart . applyEnd

addTitleSuffix :: Target f -> T.Text -> T.Text
addTitleSuffix target base = let
  common = targetCommon target
  segments = base : case target of
    RB3 TargetRB3{..} -> makeLabel []                               rb3_2xBassPedal
    RB2 TargetRB2{..} -> makeLabel ["(RB2 version)" | rb2_LabelRB2] rb2_2xBassPedal
    _                 -> makeLabel []                               False
  makeLabel sfxs is2x = case tgt_Label common of
    Just lbl -> [lbl]
    Nothing  -> concat
      [ case tgt_Speed common of
        Nothing  -> []
        Just 1   -> []
        Just spd -> let
          intSpeed :: Int
          intSpeed = round $ spd * 100
          in ["(" <> T.pack (show intSpeed) <> "% Speed)"]
      , ["(2x Bass Pedal)" | is2x && tgt_Label2x common]
      , sfxs
      ]
  in T.intercalate " " segments

targetTitle :: SongYaml f -> Target f -> T.Text
targetTitle songYaml target = let
  base = fromMaybe (getTitle $ _metadata songYaml) $ tgt_Title $ targetCommon target
  in addTitleSuffix target base

targetTitleJP :: SongYaml f -> Target f -> Maybe T.Text
targetTitleJP songYaml target = case tgt_Title $ targetCommon target of
  Just _  -> Nothing -- TODO do we need JP title on targets also
  Nothing -> case _titleJP $ _metadata songYaml of
    Nothing   -> Nothing
    Just base -> Just $ addTitleSuffix target base

data NameRule
  = NameRulePC -- mostly windows but also mac/linux
  | NameRuleXbox -- stfs files on hard drive. includes pc rules too

-- Smarter length trim that keeps 1x, 2x, 125, rb3con, etc. at end of name
makeLength :: Int -> T.Text -> T.Text
makeLength n t = if n >= T.length t
  then t
  else case reverse $ T.splitOn "_" t of
    lastPiece : rest@(_ : _) -> let
      (modifiers, notModifiers) = flip span rest $ \x ->
        x == "1x" || x == "2x" || T.all isDigit x || case T.uncons x of
          Just ('v', v) -> T.all isDigit v
          _             -> False
      base = T.intercalate "_" $ reverse notModifiers
      suffix = T.intercalate "_" $ reverse $ lastPiece : modifiers
      base' = T.dropWhileEnd (== '_') $ T.take (max 1 $ n - (T.length suffix + 1)) base
      in T.take n $ base' <> "_" <> suffix
    _ -> T.take n t

validFileNamePiece :: NameRule -> T.Text -> T.Text
validFileNamePiece rule s = let
  trimLength = case rule of
    NameRulePC   -> id
    NameRuleXbox -> makeLength 42
  invalidChars :: String
  invalidChars = "<>:\"/\\|?*" <> case rule of
    NameRulePC   -> ""
    NameRuleXbox -> "+," -- these are only invalid on hard drives? not usb drives apparently
  eachChar c = if isAscii c && not (isControl c) && notElem c invalidChars
    then c
    else '_'
  fixEnds = T.dropWhile isSpace . T.dropWhileEnd (\c -> isSpace c || c == '.')
  reserved =
    [ ""
    -- rest are invalid names on Windows
    , "CON", "PRN", "AUX", "NUL"
    , "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "COM0"
    , "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9", "LPT0"
    ]
  s' = fixEnds $ trimLength $ T.map eachChar s
  in if elem (T.toUpper s') reserved
    then s' <> "_"
    else s'

validFileName :: NameRule -> FilePath -> FilePath
validFileName rule f = let
  (dir, file) = splitFileName f
  in dir </> T.unpack (validFileNamePiece rule $ T.pack file)

makeShortName :: Int -> SongYaml f -> T.Text
makeShortName num songYaml
  = T.dropWhileEnd (== '_')
  -- Short name doesn't have to be name used in paths but it makes things simple.
  -- Max path name is 40 chars (stfs limit) - 14 chars ("_keep.png_xbox") = 26 chars.
  $ T.take 26
  $ "o" <> T.pack (show num)
    <> "_" <> makePart (getTitle  $ _metadata songYaml)
    <> "_" <> makePart (getArtist $ _metadata songYaml)
  where makePart = T.toLower . T.filter (\c -> isAscii c && isAlphaNum c)

makePS3Name :: Int -> SongYaml f -> B.ByteString
makePS3Name num songYaml
  = TE.encodeUtf8
  $ T.take 0x1B -- 0x1C is probably fine, but leaving a null char so make_npdata doesn't get confused when making edat
  $ T.toUpper
  $ T.filter (\c -> isAscii c && isAlphaNum c)
  $ "O" <> T.pack (show num)
    <> getTitle  (_metadata songYaml)
    <> getArtist (_metadata songYaml)

hashRB3 :: (Hashable f) => SongYaml f -> TargetRB3 f -> Int
hashRB3 songYaml rb3 = let
  hashed =
    ( rb3
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    -- TODO this should use more info, or find a better way to come up with hashes.
    )
  in hash hashed `mod` 1000000000 -- TODO this should be moved to a better range, in case true numbers are used

hashGH2 :: (Hashable f) => SongYaml f -> TargetGH2 -> Int
hashGH2 songYaml gh2 = let
  hashed =
    ( gh2
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

hashGH5 :: (Hashable f) => SongYaml f -> TargetGH5 -> Int
hashGH5 songYaml gh5 = let
  hashed =
    ( gh5
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

hashGH3 :: (Hashable f) => SongYaml f -> TargetGH3 -> Int
hashGH3 songYaml gh3 = let
  hashed =
    ( gh3
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

getPlan :: Maybe T.Text -> SongYaml f -> Maybe (T.Text, Plan f)
getPlan Nothing songYaml = case HM.toList $ _plans songYaml of
  [pair] -> Just pair
  _      -> Nothing
getPlan (Just p) songYaml = case HM.lookup p $ _plans songYaml of
  Just found -> Just (p, found)
  Nothing    -> Nothing

forceRW :: (MonadIO m) => FilePath -> StackTraceT m ()
forceRW f = stackIO $ do
  p <- Dir.getPermissions f
  Dir.setPermissions f $ Dir.setOwnerReadable True $ Dir.setOwnerWritable True p

makeRB3DTA :: (MonadIO m, SendMessage m, Hashable f) => SongYaml f -> Plan f -> TargetRB3 f -> Bool -> (DifficultyRB3, Maybe VocalCount) -> RBFile.Song (RBFile.FixedFile U.Beats) -> T.Text -> StackTraceT m D.SongPackage
makeRB3DTA songYaml plan rb3 isPS3 (DifficultyRB3{..}, vocalCount) song filename = do
  ((kickPV, snarePV, kitPV), _) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
  let thresh = 170 -- everything gets forced anyway
      (pstart, pend) = previewBounds songYaml song
      len = RBFile.songLengthMS song
      perctype = RBFile.getPercType song
      fullGenre = interpretGenre
        (_genre    $ _metadata songYaml)
        (_subgenre $ _metadata songYaml)
      lookupPart rank part parts = guard (rank /= 0) >> HM.lookup part (getParts parts)
      -- all the following are only used for Plan, not MoggPlan.
      -- we don't need to handle more than 1 game part mapping to the same flex part,
      -- because no specs will change - we'll just zero out the game parts
      channelIndices before inst = take (length inst) $ drop (length $ concat before) [0..]
      partChannels, drumChannels, bassChannels, guitarChannels, keysChannels, vocalChannels, crowdChannels, songChannels :: [(Double, Double)]
      partChannels = concat
        [ drumChannels
        , bassChannels
        , guitarChannels
        , keysChannels
        , vocalChannels
        ]
      drumChannels   = case rb3DrumsRank  of 0 -> []; _ -> kickPV ++ snarePV ++ kitPV
      bassChannels   = case rb3BassRank   of 0 -> []; _ -> computeSimplePart (rb3_Bass   rb3) plan songYaml
      guitarChannels = case rb3GuitarRank of 0 -> []; _ -> computeSimplePart (rb3_Guitar rb3) plan songYaml
      keysChannels   = case rb3KeysRank   of 0 -> []; _ -> computeSimplePart (rb3_Keys   rb3) plan songYaml
      vocalChannels  = case rb3VocalRank  of 0 -> []; _ -> computeSimplePart (rb3_Vocal  rb3) plan songYaml
      crowdChannels = case plan of
        MoggPlan{}   -> undefined -- not used
        Plan    {..} -> case _crowd of
          Nothing -> []
          Just _  -> [(-1, 0), (1, 0)]
      songChannels = [(-1, 0), (1, 0)]
      -- If there are 6 channels in total, the actual mogg will have an extra 7th to avoid oggenc 5.1 issue.
      -- Leaving off pan/vol/core for the last channel is fine in RB3, but may cause issues with RB4 (ForgeTool).
      extend6 seven xs = if length xs == 6 then xs <> [seven] else xs
  songName <- replaceCharsRB False $ targetTitle songYaml $ RB3 rb3
  artistName <- replaceCharsRB False $ getArtist $ _metadata songYaml
  albumName <- mapM (replaceCharsRB False) $ _album $ _metadata songYaml
  return D.SongPackage
    { D.name = songName
    , D.artist = Just artistName
    , D.master = not $ _cover $ _metadata songYaml
    , D.songId = Just $ case rb3_SongID rb3 of
      SongIDSymbol s   -> Right s
      SongIDInt i      -> Left $ fromIntegral i
      SongIDAutoSymbol -> if isPS3
        then Left $ fromIntegral $ hashRB3 songYaml rb3 -- PS3 needs real number ID
        else Right filename
      SongIDAutoInt    -> Left $ fromIntegral $ hashRB3 songYaml rb3
    , D.song = D.Song
      { D.songName = "songs/" <> filename <> "/" <> filename
      , D.tracksCount = Nothing
      , D.tracks = D.DictList $ map (second $ map fromIntegral) $ filter (not . null . snd) $ case plan of
        MoggPlan{..} -> let
          getChannels rank fpart = maybe [] (concat . toList) $ lookupPart rank fpart _moggParts
          -- * the below trick does not work. RB3 freezes if a part doesn't have any channels.
          -- * so instead above we just allow doubling up on channels.
          -- * this works ok; the audio will cut out if either player misses, and whammy does not bend pitch.
          -- allParts = map ($ rb3) [rb3_Drums, rb3_Bass, rb3_Guitar, rb3_Keys, rb3_Vocal]
          -- getChannels rank fpart = case filter (== fpart) allParts of
          --   _ : _ : _ -> [] -- more than 1 game part maps to this flex part
          --   _         -> maybe [] (concat . toList) $ lookupPart rank fpart _moggParts
          in sortOn snd -- sorting numerically for ForgeTool (RB4) compatibility
            [ ("drum"  , getChannels rb3DrumsRank  $ rb3_Drums  rb3)
            , ("bass"  , getChannels rb3BassRank   $ rb3_Bass   rb3)
            , ("guitar", getChannels rb3GuitarRank $ rb3_Guitar rb3)
            , ("keys"  , getChannels rb3KeysRank   $ rb3_Keys   rb3)
            , ("vocals", getChannels rb3VocalRank  $ rb3_Vocal  rb3)
            ]
        Plan{} ->
          [ ("drum"  , channelIndices [] drumChannels)
          , ("bass"  , channelIndices [drumChannels] bassChannels)
          , ("guitar", channelIndices [drumChannels, bassChannels] guitarChannels)
          , ("keys"  , channelIndices [drumChannels, bassChannels, guitarChannels] keysChannels)
          , ("vocals", channelIndices [drumChannels, bassChannels, guitarChannels, keysChannels] vocalChannels)
          ]
      , D.vocalParts = Just $ case vocalCount of
        Nothing     -> 0
        Just Vocal1 -> 1
        Just Vocal2 -> 2
        Just Vocal3 -> 3
      , D.pans = map realToFrac $ case plan of
        MoggPlan{..} -> _pans
        Plan{}       -> extend6 0 $ map fst $ partChannels ++ crowdChannels ++ songChannels
      , D.vols = map realToFrac $ case plan of
        MoggPlan{..} -> _vols
        Plan{}       -> extend6 0 $ map snd $ partChannels ++ crowdChannels ++ songChannels
      , D.cores = case plan of
        MoggPlan{..} -> map (const (-1)) _pans
        Plan{}       -> extend6 (-1) $ map (const (-1)) $ partChannels ++ crowdChannels ++ songChannels
        -- TODO: 1 for guitar channels?
      , D.drumSolo = D.DrumSounds $ T.words $ case fmap drumsLayout $ getPart (rb3_Drums rb3) songYaml >>= partDrums of
        Nothing             -> "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
        Just StandardLayout -> "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
        Just FlipYBToms     -> "kick.cue snare.cue tom2.cue tom1.cue crash.cue"
      , D.drumFreestyle = D.DrumSounds $ T.words
        "kick.cue snare.cue hat.cue ride.cue crash.cue"
      , D.crowdChannels = let
        chans = case plan of
          MoggPlan{..} -> _moggCrowd
          Plan{}       -> take (length crowdChannels) [length partChannels ..]
        in guard (not $ null chans) >> Just (map fromIntegral chans)
      , D.hopoThreshold = Just thresh
      , D.muteVolume = Nothing
      , D.muteVolumeVocals = Nothing
      , D.midiFile = Nothing
      }
    , D.bank = Just $ case perctype of
      Nothing               -> "sfx/tambourine_bank.milo"
      Just Magma.Tambourine -> "sfx/tambourine_bank.milo"
      Just Magma.Cowbell    -> "sfx/cowbell_bank.milo"
      Just Magma.Handclap   -> "sfx/handclap_bank.milo"
    , D.drumBank = Just $ case fmap drumsKit $ getPart (rb3_Drums rb3) songYaml >>= partDrums of
      Nothing            -> "sfx/kit01_bank.milo"
      Just HardRockKit   -> "sfx/kit01_bank.milo"
      Just ArenaKit      -> "sfx/kit02_bank.milo"
      Just VintageKit    -> "sfx/kit03_bank.milo"
      Just TrashyKit     -> "sfx/kit04_bank.milo"
      Just ElectronicKit -> "sfx/kit05_bank.milo"
    , D.animTempo = _animTempo $ _global songYaml
    , D.bandFailCue = Nothing
    , D.songScrollSpeed = 2300
    , D.preview = (fromIntegral pstart, fromIntegral pend)
    , D.songLength = Just $ fromIntegral len
    , D.rank = HM.fromList
      [ ("drum"       , rb3DrumsRank    )
      , ("bass"       , rb3BassRank     )
      , ("guitar"     , rb3GuitarRank   )
      , ("vocals"     , rb3VocalRank    )
      , ("keys"       , rb3KeysRank     )
      , ("real_keys"  , rb3ProKeysRank  )
      , ("real_guitar", rb3ProGuitarRank)
      , ("real_bass"  , rb3ProBassRank  )
      , ("band"       , rb3BandRank     )
      ]
    , D.solo = let
      kwds :: [T.Text]
      kwds = concat
        [ ["guitar" | RBFile.hasSolo Guitar song]
        , ["bass" | RBFile.hasSolo Bass song]
        , ["drum" | RBFile.hasSolo Drums song]
        , ["keys" | RBFile.hasSolo Keys song]
        , ["vocal_percussion" | RBFile.hasSolo Vocal song]
        ]
      in guard (not $ null kwds) >> Just kwds
    , D.songFormat = 10
    , D.version = fromMaybe 1 $ rb3_Version rb3
    , D.fake = Nothing
    , D.gameOrigin = Just $ if rb3_Harmonix rb3 then "rb3_dlc" else "ugc_plus"
    , D.ugc = Nothing
    , D.rating = case (isPS3, fromIntegral $ fromEnum (_rating $ _metadata songYaml) + 1) of
      (True, 4) -> 2 -- Unrated (on RB2 at least) causes it to be locked in game on PS3
      (_   , x) -> x
    , D.genre = Just $ rbn2Genre fullGenre
    , D.subGenre = Just $ "subgenre_" <> rbn2Subgenre fullGenre
    , D.vocalGender = Just $ fromMaybe Magma.Female $ getPart (rb3_Vocal rb3) songYaml >>= partVocal >>= vocalGender
    -- TODO is it safe to have no vocal_gender?
    , D.shortVersion = Nothing
    , D.yearReleased = Just $ fromIntegral $ getYear $ _metadata songYaml
    , D.yearRecorded = Nothing
    -- confirmed: you can have (album_art 1) with no album_name/album_track_number
    , D.albumArt = Just $ isJust $ _fileAlbumArt $ _metadata songYaml
    -- haven't tested behavior if you have album_name but no album_track_number
    , D.albumName = albumName
    , D.albumTrackNumber = fmap fromIntegral $ _trackNumber $ _metadata songYaml
    , D.packName = Nothing
    , D.vocalTonicNote = fmap songKey $ _key $ _metadata songYaml
    , D.songTonality = fmap songTonality $ _key $ _metadata songYaml
    , D.songKey = Nothing
    , D.tuningOffsetCents = Just $ fromIntegral $ _tuningCents plan
    , D.realGuitarTuning = flip fmap (getPart (rb3_Guitar rb3) songYaml >>= partProGuitar) $ \pg ->
      map fromIntegral $ encodeTuningOffsets (pgTuning pg) TypeGuitar
    , D.realBassTuning = flip fmap (getPart (rb3_Bass rb3) songYaml >>= partProGuitar) $ \pg ->
      map fromIntegral $ encodeTuningOffsets (pgTuning pg) TypeBass
    , D.guidePitchVolume = Just (-3)
    , D.encoding = Just "utf8"
    , D.extraAuthoring = Nothing
    , D.alternatePath = Nothing
    , D.context = Nothing
    , D.decade = Nothing
    , D.downloaded = Nothing
    , D.basePoints = Nothing
    , D.videoVenues = Nothing
    , D.dateReleased = Nothing
    , D.dateRecorded = Nothing
    }

printOverdrive :: FilePath -> StackTraceT (QueueLog Action) ()
printOverdrive mid = do
  song <- shakeMIDI mid
  let _ = song :: RBFile.Song (RBFile.OnyxFile U.Beats)
  od <- calculateUnisons <$> getOverdrive (RBFile.s_tracks song)
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 od) $ \(posn, unison) -> do
    let posn' = showPosition (RBFile.s_signatures song) posn
    if all (== 0) [ t | (t, _, _) <- NE.toList unison ]
      then lg $ posn' <> ": " <> printFlexParts [ inst | (_, inst, _) <- NE.toList unison ]
      else lg $ intercalate "\n" $ (posn' <> ":") : do
        (t, inst, _) <- NE.toList unison
        return $ "  (" <> show (realToFrac t :: Milli) <> ") " <> printFlexParts [inst]

makeC3 :: (Monad m) => SongYaml f -> Plan f -> TargetRB3 f -> RBFile.Song (RBFile.FixedFile U.Beats) -> T.Text -> StackTraceT m C3.C3
makeC3 songYaml plan rb3 midi pkg = do
  let (pstart, _) = previewBounds songYaml midi
      DifficultyRB3{..} = difficultyRB3 rb3 songYaml
      title = targetTitle songYaml $ RB3 rb3
      numSongID = case rb3_SongID rb3 of
        SongIDInt i -> Just i
        _           -> Nothing
      hasCrowd = case plan of
        MoggPlan{..} -> not $ null _moggCrowd
        Plan{..}     -> isJust _crowd
  return C3.C3
    { C3.song = fromMaybe (getTitle $ _metadata songYaml) $ tgt_Title $ rb3_Common rb3
    , C3.artist = getArtist $ _metadata songYaml
    , C3.album = getAlbum $ _metadata songYaml
    , C3.customID = pkg
    , C3.version = fromIntegral $ fromMaybe 1 $ rb3_Version rb3
    , C3.isMaster = not $ _cover $ _metadata songYaml
    , C3.encodingQuality = 5
    , C3.crowdAudio = guard hasCrowd >> Just "crowd.wav"
    , C3.crowdVol = guard hasCrowd >> Just 0
    , C3.is2xBass = rb3_2xBassPedal rb3
    , C3.rhythmKeys = _rhythmKeys $ _metadata songYaml
    , C3.rhythmBass = _rhythmBass $ _metadata songYaml
    , C3.karaoke = getKaraoke plan
    , C3.multitrack = getMultitrack plan
    , C3.convert = _convert $ _metadata songYaml
    , C3.expertOnly = _expertOnly $ _metadata songYaml
    , C3.proBassDiff = case rb3ProBassRank of 0 -> Nothing; r -> Just $ fromIntegral r
    , C3.proBassTuning4 = flip fmap (getPart (rb3_Bass rb3) songYaml >>= partProGuitar) $ \pg -> T.concat
      [ "(real_bass_tuning ("
      , T.unwords $ map (T.pack . show) $ encodeTuningOffsets (pgTuning pg) TypeBass
      , "))"
      ]
    , C3.proGuitarDiff = case rb3ProGuitarRank of 0 -> Nothing; r -> Just $ fromIntegral r
    , C3.proGuitarTuning = flip fmap (getPart (rb3_Guitar rb3) songYaml >>= partProGuitar) $ \pg -> T.concat
      [ "(real_guitar_tuning ("
      , T.unwords $ map (T.pack . show) $ encodeTuningOffsets (pgTuning pg) TypeGuitar
      , "))"
      ]
    , C3.disableProKeys = case getPart (rb3_Keys rb3) songYaml of
      Nothing   -> False
      Just part -> isJust (partGRYBO part) && isNothing (partProKeys part)
    , C3.tonicNote = fmap songKey $ _key $ _metadata songYaml
    , C3.tuningCents = 0
    , C3.songRating = fromEnum (_rating $ _metadata songYaml) + 1
    , C3.drumKitSFX = maybe 0 (fromEnum . drumsKit) $ getPart (rb3_Drums rb3) songYaml >>= partDrums
    , C3.hopoThresholdIndex = 2 -- 170 ticks (everything gets forced anyway)
    , C3.muteVol = -96
    , C3.vocalMuteVol = -12
    , C3.soloDrums = RBFile.hasSolo Drums midi
    , C3.soloGuitar = RBFile.hasSolo Guitar midi
    , C3.soloBass = RBFile.hasSolo Bass midi
    , C3.soloKeys = RBFile.hasSolo Keys midi
    , C3.soloVocals = RBFile.hasSolo Vocal midi
    , C3.songPreview = Just $ fromIntegral pstart
    , C3.checkTempoMap = True
    , C3.wiiMode = False
    , C3.doDrumMixEvents = True -- is this a good idea?
    , C3.packageDisplay = getArtist (_metadata songYaml) <> " - " <> title
    , C3.packageDescription = "Created with Magma: C3 Roks Edition (forums.customscreators.com) and ONYX (git.io/onyx)."
    , C3.songAlbumArt = "cover.bmp"
    , C3.packageThumb = ""
    , C3.encodeANSI = True  -- is this right?
    , C3.encodeUTF8 = False -- is this right?
    , C3.useNumericID = isJust numSongID
    , C3.uniqueNumericID = case numSongID of
      Nothing -> ""
      Just i  -> T.pack $ show i
    , C3.uniqueNumericID2X = "" -- will use later if we ever create combined 1x/2x C3 Magma projects
    , C3.toDoList = C3.defaultToDo
    }

-- Magma RBProj rules
makeMagmaProj :: SongYaml f -> TargetRB3 f -> Plan f -> (DifficultyRB3, Maybe VocalCount) -> T.Text -> FilePath -> Action T.Text -> Staction Magma.RBProj
makeMagmaProj songYaml rb3 plan (DifficultyRB3{..}, voxCount) pkg mid thisTitle = do
  song <- shakeMIDI mid
  ((kickPVs, snarePVs, kitPVs), mixMode) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
  let (pstart, _) = previewBounds songYaml song
      maxPStart = 570000 :: Int -- 9:30.000
      fullGenre = interpretGenre
        (_genre    $ _metadata songYaml)
        (_subgenre $ _metadata songYaml)
      perctype = RBFile.getPercType song
      silentDryVox :: Int -> Magma.DryVoxPart
      silentDryVox n = Magma.DryVoxPart
        { Magma.dryVoxFile = "dryvox" <> T.pack (show n) <> ".wav"
        , Magma.dryVoxEnabled = True
        }
      emptyDryVox = Magma.DryVoxPart
        { Magma.dryVoxFile = ""
        , Magma.dryVoxEnabled = False
        }
      disabledFile = Magma.AudioFile
        { Magma.audioEnabled = False
        , Magma.channels = 0
        , Magma.pan = []
        , Magma.vol = []
        , Magma.audioFile = ""
        }
      pvFile :: [(Double, Double)] -> T.Text -> Magma.AudioFile
      pvFile pvs f = Magma.AudioFile
        { Magma.audioEnabled = True
        , Magma.channels = fromIntegral $ length pvs
        , Magma.pan = map (realToFrac . fst) pvs
        , Magma.vol = map (realToFrac . snd) pvs
        , Magma.audioFile = f
        }
  title <- T.map (\case '"' -> '\''; c -> c) <$> shk thisTitle
  pstart' <- if pstart > maxPStart
    then do
      warn $ "Preview start time of " ++ show pstart ++ "ms too late for C3 Magma; changed to " ++ show maxPStart ++ "ms"
      return maxPStart
    else return pstart
  songName <- replaceCharsRB True title
  artistName <- replaceCharsRB True $ getArtist $ _metadata songYaml
  albumName <- replaceCharsRB True $ getAlbum $ _metadata songYaml
  let fixString = T.strip . T.map (\case '"' -> '\''; c -> c)
  return Magma.RBProj
    { Magma.project = Magma.Project
      { Magma.toolVersion = "110411_A"
      , Magma.projectVersion = 24
      , Magma.metadata = Magma.Metadata
        -- "song_name: This field must be less than 100 characters."
        -- also, can't begin or end with whitespace
        { Magma.songName = T.strip $ T.take 99 $ fixString songName
        -- "artist_name: This field must be less than 75 characters."
        , Magma.artistName = T.strip $ T.take 74 $ fixString artistName
        , Magma.genre = rbn2Genre fullGenre
        , Magma.subGenre = "subgenre_" <> rbn2Subgenre fullGenre
        , Magma.yearReleased = fromIntegral $ max 1960 $ getYear $ _metadata songYaml
        -- "album_name: This field must be less than 75 characters."
        , Magma.albumName = T.strip $ T.take 74 $ fixString albumName
        -- "author: This field must be less than 75 characters."
        , Magma.author = T.strip $ T.take 74 $ fixString $ getAuthor $ _metadata songYaml
        , Magma.releaseLabel = "Onyxite Customs"
        , Magma.country = "ugc_country_us"
        , Magma.price = 160
        , Magma.trackNumber = fromIntegral $ getTrackNumber $ _metadata songYaml
        , Magma.hasAlbum = True
        }
      , Magma.gamedata = Magma.Gamedata
        { Magma.previewStartMs = fromIntegral pstart'
        , Magma.rankDrum    = max 1 rb3DrumsTier
        , Magma.rankBass    = max 1 rb3BassTier
        , Magma.rankGuitar  = max 1 rb3GuitarTier
        , Magma.rankVocals  = max 1 rb3VocalTier
        , Magma.rankKeys    = max 1 rb3KeysTier
        , Magma.rankProKeys = max 1 rb3ProKeysTier
        , Magma.rankBand    = max 1 rb3BandTier
        , Magma.vocalScrollSpeed = 2300
        , Magma.animTempo = case _animTempo $ _global songYaml of
          Left  D.KTempoSlow   -> 16
          Left  D.KTempoMedium -> 32
          Left  D.KTempoFast   -> 64
          Right n              -> n
        , Magma.vocalGender = fromMaybe Magma.Female $ getPart (rb3_Vocal rb3) songYaml >>= partVocal >>= vocalGender
        , Magma.vocalPercussion = fromMaybe Magma.Tambourine perctype
        , Magma.vocalParts = case voxCount of
          Nothing     -> 0
          Just Vocal1 -> 1
          Just Vocal2 -> 2
          Just Vocal3 -> 3
        , Magma.guidePitchVolume = -3
        }
      , Magma.languages = let
        lang s = elem s $ _languages $ _metadata songYaml
        eng = lang "English"
        fre = lang "French"
        ita = lang "Italian"
        spa = lang "Spanish"
        ger = lang "German"
        jap = lang "Japanese"
        in Magma.Languages
          { Magma.english  = Just $ eng || not (or [eng, fre, ita, spa, ger, jap])
          , Magma.french   = Just fre
          , Magma.italian  = Just ita
          , Magma.spanish  = Just spa
          , Magma.german   = Just ger
          , Magma.japanese = Just jap
          }
      , Magma.destinationFile = T.pack $ T.unpack pkg <.> "rba"
      , Magma.midi = Magma.Midi
        { Magma.midiFile = "notes.mid"
        , Magma.autogenTheme = Left $ fromMaybe Magma.DefaultTheme $ _autogenTheme $ _global songYaml
        }
      , Magma.dryVox = Magma.DryVox
        { Magma.part0 = case voxCount of
          Nothing     -> emptyDryVox
          Just Vocal1 -> silentDryVox 0
          _           -> silentDryVox 1
        , Magma.part1 = if voxCount == Just Vocal2 || voxCount == Just Vocal3
          then silentDryVox 2
          else emptyDryVox
        , Magma.part2 = if voxCount == Just Vocal3
          then silentDryVox 3
          else emptyDryVox
        , Magma.dryVoxFileRB2 = Nothing
        , Magma.tuningOffsetCents = fromIntegral $ _tuningCents plan -- TODO should do both this and c3 cents?
        }
      , Magma.albumArt = Magma.AlbumArt "cover.bmp"
      , Magma.tracks = Magma.Tracks
        { Magma.drumLayout = case mixMode of
          RBDrums.D0 -> Magma.Kit
          RBDrums.D1 -> Magma.KitKickSnare
          RBDrums.D2 -> Magma.KitKickSnare
          RBDrums.D3 -> Magma.KitKickSnare
          RBDrums.D4 -> Magma.KitKick
        , Magma.drumKick = if rb3DrumsRank == 0 || mixMode == RBDrums.D0
          then disabledFile
          else pvFile kickPVs "kick.wav"
        , Magma.drumSnare = if rb3DrumsRank == 0 || elem mixMode [RBDrums.D0, RBDrums.D4]
          then disabledFile
          else pvFile snarePVs "snare.wav"
        , Magma.drumKit = if rb3DrumsRank == 0
          then disabledFile
          else pvFile kitPVs "drums.wav"
        , Magma.bass = if rb3BassRank == 0
          then disabledFile
          else pvFile (computeSimplePart (rb3_Bass rb3) plan songYaml) "bass.wav"
        , Magma.guitar = if rb3GuitarRank == 0
          then disabledFile
          else pvFile (computeSimplePart (rb3_Guitar rb3) plan songYaml) "guitar.wav"
        , Magma.vocals = if rb3VocalRank == 0
          then disabledFile
          else pvFile (computeSimplePart (rb3_Vocal rb3) plan songYaml) "vocal.wav"
        , Magma.keys = if rb3KeysRank == 0
          then disabledFile
          else pvFile (computeSimplePart (rb3_Keys rb3) plan songYaml) "keys.wav"
        , Magma.backing = pvFile [(-1, 0), (1, 0)] "song-countin.wav"
        }
      }
    }

shakeBuildFiles :: (MonadIO m) => [FilePath] -> FilePath -> [FilePath] -> StackTraceT (QueueLog m) ()
shakeBuildFiles audioDirs yamlPath = shakeBuild audioDirs yamlPath []

shakeBuild :: (MonadIO m) => [FilePath] -> FilePath -> [(T.Text, Target FilePath)] -> [FilePath] -> StackTraceT (QueueLog m) ()
shakeBuild audioDirs yamlPathRel extraTargets buildables = do

  yamlPath <- stackIO $ Dir.canonicalizePath yamlPathRel
  songYaml <- loadYaml yamlPath

  let fullGenre = interpretGenre
        (_genre    $ _metadata songYaml)
        (_subgenre $ _metadata songYaml)

  checkDefined songYaml

  exeTime <- stackIO $ getExecutablePath >>= Dir.getModificationTime
  yamlTime <- stackIO $ Dir.getModificationTime yamlPath
  let projVersion = show exeTime ++ "," ++ show yamlTime

  audioLib <- newAudioLibrary
  forM_ audioDirs $ \dir -> do
    p <- parseAbsDir dir
    addAudioDir audioLib p

  writeMsg <- lift $ QueueLog ask
  let yamlDir = takeDirectory yamlPath
      rel f = yamlDir </> f
      ourShakeOptions = shakeOptions
        { shakeThreads = 0
        , shakeFiles = rel "gen"
        , shakeVersion = projVersion
        , shakeOutput = \verb str -> if verb <= Warn
          then writeMsg (MessageWarning, Message str [])
          else if verb <= Info
            then writeMsg (MessageLog, Message str [])
            else return ()
        }

  do

    shakeEmbed ourShakeOptions $ do

      phony "yaml"  $ lg $ show songYaml
      phony "audio" $ lg $ show audioDirs

      -- Find and convert all Jammit audio into the work directory
      let jammitAudioParts = map J.Only    [minBound .. maxBound]
                          ++ map J.Without [minBound .. maxBound]
      forM_ (HM.toList $ _jammit songYaml) $ \(jammitName, jammitQuery) ->
        forM_ jammitAudioParts $ \audpart ->
          rel (jammitPath jammitName audpart) %> \out -> do
            inside ("Looking for the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart) $ do
              let title  = fromMaybe (getTitle  $ _metadata songYaml) $ _jammitTitle  jammitQuery
                  artist = fromMaybe (getArtist $ _metadata songYaml) $ _jammitArtist jammitQuery
                  inst   = fromJammitInstrument $ J.audioPartToInstrument audpart
              p <- searchJammit audioLib (title, artist, inst)
              result <- stackIO $ fmap J.getAudioParts $ J.loadLibrary $ toFilePath p
              case [ jcfx | (audpart', jcfx) <- result, audpart == audpart' ] of
                jcfx : _ -> do
                  lg $ "Found the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
                  stackIO $ J.runAudio [jcfx] [] out
                []       -> fail "Couldn't find a necessary Jammit track"

      -- Cover art
      let loadRGB8 = case _fileAlbumArt $ _metadata songYaml of
            Just img -> do
              shk $ need [img]
              stackIO $ if takeExtension img `elem` [".png_xbox", ".png_wii"]
                then pixelMap dropTransparency . readRBImage False <$> BL.readFile img
                else if takeExtension img == ".png_ps3"
                  then pixelMap dropTransparency . readRBImage True <$> BL.readFile img
                  else readImage img >>= \case
                    Left  err -> fail $ "Failed to load cover art (" ++ img ++ "): " ++ err
                    Right dyn -> return $ convertRGB8 dyn
            Nothing -> stackIO onyxAlbum
      rel "gen/cover.bmp"      %> \out -> loadRGB8 >>= stackIO . writeBitmap  out . STBIR.resize STBIR.defaultOptions 256 256
      rel "gen/cover.png"      %> \out -> loadRGB8 >>= stackIO . writePng     out . STBIR.resize STBIR.defaultOptions 256 256
      rel "gen/cover-full.png" %> \out -> loadRGB8 >>= stackIO . writePng     out
      let hmxImageTypes =
            [ (".png_xbox", PNGXbox)
            , (".png_ps3" , PNGPS3 )
            , (".png_wii" , PNGWii )
            ]
      forM_ hmxImageTypes $ \(ext, pngType) -> do
        rel ("gen/cover" <> ext) %> \out -> case _fileAlbumArt $ _metadata songYaml of
          Just f | takeExtension f == ext -> do
            shk $ copyFile' f out
            forceRW out
          _      -> loadRGB8 >>= stackIO . BL.writeFile out . toDXT1File pngType

      rel "gen/notes.mid" %> \out -> shk $ do
        let f = rel $ _fileMidi $ _global songYaml
        doesFileExist f >>= \b -> if b
          then copyFile' f out
          else saveMIDI out RBFile.Song
            { RBFile.s_tempos = U.tempoMapFromBPS RTB.empty
            , RBFile.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
            , RBFile.s_tracks = mempty :: RBFile.OnyxFile U.Beats
            }

      let audioDependPath name = rel $ "gen/audio" </> T.unpack name <.> "wav"
          audioDepend name = do
            let path = audioDependPath name
            shk $ need [path]
            return path
      forM_ (HM.toList $ _audio songYaml) $ \(name, _) -> do
        audioDependPath name %> \out -> do
          src <- manualLeaf yamlDir audioLib audioDepend songYaml $ Named name
          buildAudio src out

      let getAudioLength :: T.Text -> Plan f -> Staction U.Seconds
          getAudioLength planName = \case
            MoggPlan{} -> do
              let ogg = rel $ "gen/plan" </> T.unpack planName </> "audio.ogg"
              shk $ need [ogg]
              liftIO $ audioSeconds ogg
            Plan{..} -> let
              parts = fmap _planExpr $ concat
                [ toList _song
                , toList _crowd
                , toList _planParts >>= toList
                ]
              in case NE.nonEmpty parts of
                Nothing -> return 0
                Just parts' -> do
                  src <- mapM (manualLeaf yamlDir audioLib audioDepend songYaml) (Mix parts') >>= lift . lift . buildSource . join
                  let _ = src :: AudioSource (ResourceT IO) Float
                  return $ realToFrac $ fromIntegral (frames src) / rate src

          adjustSpec :: Bool -> [(Double, Double)] -> [(Double, Double)]
          adjustSpec True  spec     = spec
          adjustSpec False [(0, 0)] = [(0, 0)]
          adjustSpec False _        = [(-1, 0), (1, 0)]

          padAudio :: (Monad m) => Int -> AudioSource m Float -> AudioSource m Float
          padAudio pad src = if frames src == 0
            then src
            else padStart (Seconds $ realToFrac pad) src
          setAudioLength :: (Monad m) => U.Seconds -> AudioSource m Float -> AudioSource m Float
          setAudioLength len src = let
            currentLength = fromIntegral (frames src) / rate src
            requiredLength = realToFrac len
            in case compare currentLength requiredLength of
              EQ -> src
              LT -> padEnd (Seconds $ requiredLength - currentLength) src
              GT -> takeStart (Seconds requiredLength) src
          -- Silences out an audio stream if more than 1 game part maps to the same flex part
          zeroIfMultiple fparts fpart src = case filter (== fpart) fparts of
            _ : _ : _ -> takeStart (Frames 0) src
            _         -> src

          oggWavForPlan planName = rel $ "gen/plan" </> T.unpack planName </> "audio.wav"

          writeKick, writeSnare, writeKit, writeSimplePart
            :: [RBFile.FlexPartName] -> TargetCommon -> RBFile.Song f -> Int -> Bool -> T.Text -> Plan FilePath -> RBFile.FlexPartName -> Integer -> FilePath -> Staction ()
          writeKick gameParts tgt mid pad supportsOffMono planName plan fpart rank out = do
            ((spec', _, _), _) <- computeDrumsPart fpart plan songYaml
            let spec = adjustSpec supportsOffMono spec'
            src <- case plan of
              MoggPlan{..} -> channelsToSpec spec (oggWavForPlan planName) (zip _pans _vols) $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _moggParts of
                  Just (PartDrumKit kick _ _) -> fromMaybe [] kick
                  _                           -> []
              Plan{..}     -> buildAudioToSpec yamlDir audioLib audioDepend songYaml spec $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _planParts of
                  Just (PartDrumKit kick _ _) -> kick
                  _                           -> Nothing
            runAudio (clampIfSilent $ zeroIfMultiple gameParts fpart $ padAudio pad $ applyTargetAudio tgt mid src) out
          writeSnare gameParts tgt mid pad supportsOffMono planName plan fpart rank out = do
            ((_, spec', _), _) <- computeDrumsPart fpart plan songYaml
            let spec = adjustSpec supportsOffMono spec'
            src <- case plan of
              MoggPlan{..} -> channelsToSpec spec (oggWavForPlan planName) (zip _pans _vols) $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _moggParts of
                  Just (PartDrumKit _ snare _) -> fromMaybe [] snare
                  _                            -> []
              Plan{..}     -> buildAudioToSpec yamlDir audioLib audioDepend songYaml spec $ do
                guard $ rank /= 0
                case HM.lookup fpart $ getParts _planParts of
                  Just (PartDrumKit _ snare _) -> snare
                  _                            -> Nothing
            runAudio (clampIfSilent $ zeroIfMultiple gameParts fpart $ padAudio pad $ applyTargetAudio tgt mid src) out
          writeKit gameParts tgt mid pad supportsOffMono planName plan fpart rank out = do
            ((_, _, spec'), mixMode) <- computeDrumsPart fpart plan songYaml
            let spec = adjustSpec supportsOffMono spec'
            src <- case plan of
              MoggPlan{..} -> let
                build = channelsToSpec spec (oggWavForPlan planName) (zip _pans _vols)
                indexSets = do
                  guard $ rank /= 0
                  case HM.lookup fpart $ getParts _moggParts of
                    Just (PartDrumKit kick snare kit) -> case mixMode of
                      RBDrums.D0 -> toList kick <> toList snare <> [kit]
                      _          -> [kit]
                    Just (PartSingle             kit) -> [kit]
                    _                                 -> []
                in mapM build indexSets >>= \case
                  []     -> build []
                  s : ss -> return $ foldr mix s ss
              Plan{..}     -> let
                build = buildAudioToSpec yamlDir audioLib audioDepend songYaml spec
                exprs = do
                  guard $ rank /= 0
                  case HM.lookup fpart $ getParts _planParts of
                    Just (PartDrumKit kick snare kit) -> case mixMode of
                      RBDrums.D0 -> toList kick <> toList snare <> [kit]
                      _          -> [kit]
                    Just (PartSingle             kit) -> [kit]
                    _                                 -> []
                in mapM (build . Just) exprs >>= \case
                  []     -> build Nothing
                  s : ss -> return $ foldr mix s ss
            runAudio (clampIfSilent $ zeroIfMultiple gameParts fpart $ padAudio pad $ applyTargetAudio tgt mid src) out
          getPartSource :: (MonadResource m) => [(Double, Double)] -> T.Text -> Plan FilePath -> RBFile.FlexPartName -> Integer -> Staction (AudioSource m Float)
          getPartSource spec planName plan fpart rank = case plan of
            MoggPlan{..} -> channelsToSpec spec (oggWavForPlan planName) (zip _pans _vols) $ do
              guard $ rank /= 0
              toList (HM.lookup fpart $ getParts _moggParts) >>= toList >>= toList
            Plan{..} -> buildPartAudioToSpec yamlDir audioLib audioDepend songYaml spec $ do
              guard $ rank /= 0
              HM.lookup fpart $ getParts _planParts
          writeStereoParts gameParts tgt mid pad planName plan fpartranks out = do
            let spec = [(-1, 0), (1, 0)]
            srcs <- forM fpartranks $ \(fpart, rank)
              -> zeroIfMultiple gameParts fpart
              <$> getPartSource spec planName plan fpart rank
            src <- case srcs of
              []     -> buildAudioToSpec yamlDir audioLib audioDepend songYaml spec Nothing
              s : ss -> return $ foldr mix s ss
            runAudio (clampIfSilent $ padAudio pad $ applyTargetAudio tgt mid src) out
          writeSimplePart gameParts tgt mid pad supportsOffMono planName plan fpart rank out = do
            let spec = adjustSpec supportsOffMono $ computeSimplePart fpart plan songYaml
            src <- getPartSource spec planName plan fpart rank
            runAudio (clampIfSilent $ zeroIfMultiple gameParts fpart $ padAudio pad $ applyTargetAudio tgt mid src) out
          writeCrowd tgt mid pad planName plan out = do
            src <- case plan of
              MoggPlan{..} -> channelsToSpec [(-1, 0), (1, 0)] (oggWavForPlan planName) (zip _pans _vols) _moggCrowd
              Plan{..}     -> buildAudioToSpec yamlDir audioLib audioDepend songYaml [(-1, 0), (1, 0)] _crowd
            runAudio (clampIfSilent $ padAudio pad $ applyTargetAudio tgt mid src) out
          sourceSongCountin :: (MonadResource m) => TargetCommon -> RBFile.Song f -> Int -> Bool -> T.Text -> Plan g -> [(RBFile.FlexPartName, Integer)] -> Staction (AudioSource m Float)
          sourceSongCountin tgt mid pad includeCountin planName plan fparts = do
            let usedParts' = [ fpart | (fpart, rank) <- fparts, rank /= 0 ]
                usedParts =
                  [ fpart
                  | fpart <- usedParts'
                  , case filter (== fpart) usedParts' of
                    -- if more than 1 game part maps to this flex part,
                    -- the flex part's audio should go in backing track
                    _ : _ : _ -> False
                    _         -> True
                  ]
                spec = [(-1, 0), (1, 0)]
            src <- case plan of
              MoggPlan{..} -> channelsToSpec spec (oggWavForPlan planName) (zip _pans _vols) $ let
                channelsFor fpart = toList (HM.lookup fpart $ getParts _moggParts) >>= toList >>= toList
                usedChannels = concatMap channelsFor usedParts ++ _moggCrowd
                in filter (`notElem` usedChannels) [0 .. length _pans - 1]
              Plan{..} -> let
                unusedParts = do
                  (fpart, pa) <- HM.toList $ getParts _planParts
                  guard $ notElem fpart usedParts
                  return pa
                partAudios = maybe id (\pa -> (PartSingle pa :)) _song unusedParts
                countinPath = rel $ "gen/plan" </> T.unpack planName </> "countin.wav"
                in do
                  unusedSrcs <- mapM (buildPartAudioToSpec yamlDir audioLib audioDepend songYaml spec . Just) partAudios
                  if includeCountin
                    then do
                      countinSrc <- shk $ buildSource $ Input countinPath
                      return $ foldr mix countinSrc unusedSrcs
                    else case unusedSrcs of
                      []     -> buildPartAudioToSpec yamlDir audioLib audioDepend songYaml spec Nothing
                      s : ss -> return $ foldr mix s ss
            return $ padAudio pad $ applyTargetAudio tgt mid src
          writeSongCountin :: TargetCommon -> RBFile.Song f -> Int -> Bool -> T.Text -> Plan g -> [(RBFile.FlexPartName, Integer)] -> FilePath -> Staction ()
          writeSongCountin tgt mid pad includeCountin planName plan fparts out = do
            src <- sourceSongCountin tgt mid pad includeCountin planName plan fparts
            runAudio (clampIfSilent src) out

          rbRules :: FilePath -> TargetRB3 FilePath -> Maybe TargetRB2 -> QueueLog Rules ()
          rbRules dir rb3 mrb2 = do
            let pkg :: (IsString a) => a
                pkg = fromString $ T.unpack $ makeShortName (hashRB3 songYaml rb3) songYaml
            (planName, plan) <- case getPlan (tgt_Plan $ rb3_Common rb3) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show rb3
              Just pair -> return pair
            let planDir = rel $ "gen/plan" </> T.unpack planName

            let pathMagmaKick        = dir </> "magma/kick.wav"
                pathMagmaSnare       = dir </> "magma/snare.wav"
                pathMagmaDrums       = dir </> "magma/drums.wav"
                pathMagmaBass        = dir </> "magma/bass.wav"
                pathMagmaGuitar      = dir </> "magma/guitar.wav"
                pathMagmaKeys        = dir </> "magma/keys.wav"
                pathMagmaVocal       = dir </> "magma/vocal.wav"
                pathMagmaCrowd       = dir </> "magma/crowd.wav"
                pathMagmaDryvox0     = dir </> "magma/dryvox0.wav"
                pathMagmaDryvox1     = dir </> "magma/dryvox1.wav"
                pathMagmaDryvox2     = dir </> "magma/dryvox2.wav"
                pathMagmaDryvox3     = dir </> "magma/dryvox3.wav"
                pathMagmaDryvoxSine  = dir </> "magma/dryvox-sine.wav"
                pathMagmaSong        = dir </> "magma/song-countin.wav"
                pathMagmaCover       = dir </> "magma/cover.bmp"
                pathMagmaCoverV1     = dir </> "magma/cover-v1.bmp"
                pathMagmaMid         = dir </> "magma/notes.mid"
                pathMagmaRPP         = dir </> "magma/notes.RPP"
                pathMagmaMidV1       = dir </> "magma/notes-v1.mid"
                pathMagmaProj        = dir </> "magma/magma.rbproj"
                pathMagmaProjV1      = dir </> "magma/magma-v1.rbproj"
                pathMagmaC3          = dir </> "magma/magma.c3"
                pathMagmaSetup       = dir </> "magma"
                pathMagmaRba         = dir </> "magma.rba"
                pathMagmaRbaV1       = dir </> "magma-v1.rba"
                pathMagmaExport      = dir </> "notes-magma-export.mid"
                pathMagmaExport2     = dir </> "notes-magma-added.mid"
                pathMagmaDummyMono   = dir </> "magma/dummy-mono.wav"
                pathMagmaDummyStereo = dir </> "magma/dummy-stereo.wav"
                pathMagmaPad         = dir </> "magma/pad.txt"
                pathMagmaEditedParts = dir </> "magma/edited-parts.txt"

            let magmaParts = map ($ rb3) [rb3_Drums, rb3_Bass, rb3_Guitar, rb3_Keys, rb3_Vocal]
                loadEditedParts :: Staction (DifficultyRB3, Maybe VocalCount)
                loadEditedParts = shk $ read <$> readFile' pathMagmaEditedParts
                loadMidiResults :: Staction (RBFile.Song (RBFile.RawFile U.Beats), DifficultyRB3, Maybe VocalCount, Int)
                loadMidiResults = do
                  mid <- shakeMIDI $ planDir </> "raw.mid" :: Staction (RBFile.Song (RBFile.RawFile U.Beats))
                  (diffs, vc) <- loadEditedParts
                  pad <- shk $ read <$> readFile' pathMagmaPad
                  return (mid, diffs, vc, pad)
            pathMagmaKick   %> \out -> do
              (mid, DifficultyRB3{..}, _, pad) <- loadMidiResults
              writeKick        magmaParts (rb3_Common rb3) mid pad True planName plan (rb3_Drums  rb3) rb3DrumsRank out
            pathMagmaSnare  %> \out -> do
              (mid, DifficultyRB3{..}, _, pad) <- loadMidiResults
              writeSnare       magmaParts (rb3_Common rb3) mid pad True planName plan (rb3_Drums  rb3) rb3DrumsRank out
            pathMagmaDrums  %> \out -> do
              (mid, DifficultyRB3{..}, _, pad) <- loadMidiResults
              writeKit         magmaParts (rb3_Common rb3) mid pad True planName plan (rb3_Drums  rb3) rb3DrumsRank out
            pathMagmaBass   %> \out -> do
              (mid, DifficultyRB3{..}, _, pad) <- loadMidiResults
              writeSimplePart  magmaParts (rb3_Common rb3) mid pad True planName plan (rb3_Bass   rb3) rb3BassRank out
            pathMagmaGuitar %> \out -> do
              (mid, DifficultyRB3{..}, _, pad) <- loadMidiResults
              writeSimplePart  magmaParts (rb3_Common rb3) mid pad True planName plan (rb3_Guitar rb3) rb3GuitarRank out
            pathMagmaKeys   %> \out -> do
              (mid, DifficultyRB3{..}, _, pad) <- loadMidiResults
              writeSimplePart  magmaParts (rb3_Common rb3) mid pad True planName plan (rb3_Keys   rb3) rb3KeysRank out
            pathMagmaVocal  %> \out -> do
              (mid, DifficultyRB3{..}, _, pad) <- loadMidiResults
              writeSimplePart  magmaParts (rb3_Common rb3) mid pad True planName plan (rb3_Vocal  rb3) rb3VocalRank out
            pathMagmaCrowd  %> \out -> do
              (mid, DifficultyRB3{}, _, pad) <- loadMidiResults
              writeCrowd                  (rb3_Common rb3) mid pad      planName plan out
            pathMagmaSong   %> \out -> do
              (mid, DifficultyRB3{..}, _, pad) <- loadMidiResults
              writeSongCountin            (rb3_Common rb3) mid pad True planName plan
                [ (rb3_Drums  rb3, rb3DrumsRank )
                , (rb3_Guitar rb3, rb3GuitarRank)
                , (rb3_Bass   rb3, rb3BassRank  )
                , (rb3_Keys   rb3, rb3KeysRank  )
                , (rb3_Vocal  rb3, rb3VocalRank )
                ] out
            let saveClip m out vox = do
                  let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
                      clip = clipDryVox $ U.applyTempoTrack (RBFile.s_tempos m)
                        $ fmap isJust $ vocalTubes vox
                  unclippedVox <- shk $ buildSource $ Input pathMagmaVocal
                  unclipped <- case frames unclippedVox of
                    0 -> shk $ buildSource $ Input pathMagmaSong
                    _ -> return unclippedVox
                  lg $ "Writing a clipped dry vocals file to " ++ out
                  stackIO $ runResourceT $ sinkSnd out fmt $ toDryVoxFormat $ clip unclipped
                  lg $ "Finished writing dry vocals to " ++ out
            pathMagmaDryvox0 %> \out -> do
              m <- shakeMIDI pathMagmaMid
              saveClip m out $ RBFile.fixedPartVocals $ RBFile.s_tracks m
            pathMagmaDryvox1 %> \out -> do
              m <- shakeMIDI pathMagmaMid
              saveClip m out $ RBFile.fixedHarm1 $ RBFile.s_tracks m
            pathMagmaDryvox2 %> \out -> do
              m <- shakeMIDI pathMagmaMid
              saveClip m out $ RBFile.fixedHarm2 $ RBFile.s_tracks m
            pathMagmaDryvox3 %> \out -> do
              m <- shakeMIDI pathMagmaMid
              saveClip m out $ RBFile.fixedHarm3 $ RBFile.s_tracks m
            pathMagmaDryvoxSine %> \out -> do
              m <- shakeMIDI pathMagmaMid
              let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
              liftIO $ runResourceT $ sinkSnd out fmt $ RB2.dryVoxAudio m
            pathMagmaDummyMono   %> buildAudio (Silence 1 $ Seconds 31) -- we set preview start to 0:00 so these can be short
            pathMagmaDummyStereo %> buildAudio (Silence 2 $ Seconds 31)
            pathMagmaCover %> shk . copyFile' (rel "gen/cover.bmp")
            pathMagmaCoverV1 %> \out -> liftIO $ writeBitmap out $ generateImage (\_ _ -> PixelRGB8 0 0 255) 256 256
            let title = targetTitle songYaml $ RB3 rb3
            pathMagmaProj %> \out -> do
              editedParts <- loadEditedParts
              p <- makeMagmaProj songYaml rb3 plan editedParts pkg pathMagmaMid $ return title
              liftIO $ D.writeFileDTA_latin1 out $ D.serialize (valueId D.stackChunks) p
            pathMagmaC3 %> \out -> do
              midi <- shakeMIDI pathMagmaMid
              c3 <- makeC3 songYaml plan rb3 midi pkg
              liftIO $ B.writeFile out $ TE.encodeUtf8 $ C3.showC3 c3
            let magmaNeededAudio = do
                  ((kickSpec, snareSpec, _), _) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
                  return $ concat
                    [ guard (maybe False (/= def) $ getPart (rb3_Drums  rb3) songYaml) >> concat
                      [ [pathMagmaDrums]
                      , [pathMagmaKick | not $ null kickSpec]
                      , [pathMagmaSnare | not $ null snareSpec]
                      ]
                    , guard (maybe False (/= def) $ getPart (rb3_Bass   rb3) songYaml) >> [pathMagmaBass  ]
                    , guard (maybe False (/= def) $ getPart (rb3_Guitar rb3) songYaml) >> [pathMagmaGuitar]
                    , guard (maybe False (/= def) $ getPart (rb3_Keys   rb3) songYaml) >> [pathMagmaKeys  ]
                    , case fmap vocalCount $ getPart (rb3_Vocal rb3) songYaml >>= partVocal of
                      Nothing     -> []
                      Just Vocal1 -> [pathMagmaVocal, pathMagmaDryvox0]
                      Just Vocal2 -> [pathMagmaVocal, pathMagmaDryvox1, pathMagmaDryvox2]
                      Just Vocal3 -> [pathMagmaVocal, pathMagmaDryvox1, pathMagmaDryvox2, pathMagmaDryvox3]
                    , [pathMagmaSong, pathMagmaCrowd]
                    ]
            pathMagmaRPP %> \out -> do
              auds <- magmaNeededAudio
              let auds' = filter (`notElem` [pathMagmaDryvox1, pathMagmaDryvox2, pathMagmaDryvox3]) auds
                  tunings = TuningInfo
                    { tuningGuitars = do
                      (fpart, part) <- HM.toList $ getParts $ _parts songYaml
                      fpart' <- toList $ lookup fpart
                        [ (rb3_Guitar rb3, RBFile.FlexGuitar)
                        , (rb3_Bass   rb3, RBFile.FlexBass  )
                        ]
                      pg <- toList $ partProGuitar part
                      return (fpart', pgTuning pg)
                    , tuningCents = _tuningCents plan
                    }
              makeReaperShake tunings pathMagmaMid pathMagmaMid auds' out
            phony pathMagmaSetup $ do
              -- Just make all the Magma prereqs, but don't actually run Magma
              auds <- magmaNeededAudio
              shk $ need $ auds ++ [pathMagmaCover, pathMagmaMid, pathMagmaProj, pathMagmaC3, pathMagmaRPP]
            pathMagmaRba %> \out -> do
              shk $ need [pathMagmaSetup]
              lg "# Running Magma v2 (C3)"
              mapStackTraceT (liftIO . runResourceT) (Magma.runMagma pathMagmaProj out) >>= lg
            let blackVenueTrack = mempty
                  { venueCameraRB3        = RTB.singleton 0 V3_coop_all_far
                  , venuePostProcessRB3   = RTB.singleton 0 V3_film_b_w
                  , venueLighting         = RTB.singleton 0 Lighting_blackout_fast
                  }
            pathMagmaExport %> \out -> do
              shk $ need [pathMagmaMid, pathMagmaProj]
              let magma = mapStackTraceT (liftIO . runResourceT) (Magma.runMagmaMIDI pathMagmaProj out) >>= lg
                  fallback = do
                    userMid <- shakeMIDI pathMagmaMid
                    saveMIDI out userMid
                      { RBFile.s_tracks = (RBFile.s_tracks userMid)
                        { RBFile.fixedVenue = blackVenueTrack
                        -- TODO sections if midi didn't supply any
                        }
                      }
              case rb3_Magma rb3 of
                MagmaRequire -> do
                  lg "# Running Magma v2 to export MIDI"
                  magma
                MagmaTry -> do
                  lg "# Running Magma v2 to export MIDI (with fallback)"
                  errorToWarning magma >>= \case
                    Nothing -> do
                      lg "Falling back to black venue MIDI due to a Magma error"
                      fallback
                    Just () -> return ()
                MagmaDisable -> fallback
            let midRealSections :: RBFile.Song (RBFile.OnyxFile U.Beats) -> Staction (RTB.T U.Beats T.Text)
                midRealSections = notSingleSection . fmap snd . eventsSections . RBFile.onyxEvents . RBFile.s_tracks
                -- also applies the computed pad + tempo hacks
                getRealSections' :: Staction (RTB.T U.Beats T.Text)
                getRealSections' = do
                  raw <- fmap (applyTargetMIDI $ rb3_Common rb3) $ shakeMIDI $ planDir </> "raw.mid"
                  let sects = fmap snd $ eventsSections $ RBFile.onyxEvents $ RBFile.s_tracks raw
                  (_, _, _, RB3.TrackAdjust adjuster) <- RB3.magmaLegalTempos
                    (sum (RTB.getTimes sects) + 20) -- whatever
                    (RBFile.s_tempos raw)
                    (RBFile.s_signatures raw)
                  padSeconds <- shk $ read <$> readFile' pathMagmaPad
                  let padBeats = padSeconds * 2
                  notSingleSection $ RTB.delay (fromInteger padBeats) $ adjuster sects
                notSingleSection rtb = case RTB.toPairList rtb of
                  [_] -> do
                    warn "Only one practice section event; removing it"
                    return RTB.empty
                  _   -> return rtb
            pathMagmaExport2 %> \out -> do
              -- Using Magma's "export MIDI" option overwrites animations/venue
              -- with autogenerated ones, even if they were actually authored.
              -- We already generate moods and drum animations ourselves,
              -- so the only things we need to get from Magma are venue,
              -- and percent sections.
              userMid <- shakeMIDI pathMagmaMid
              magmaMid <- shakeMIDI pathMagmaExport
              sects <- getRealSections'
              let trackOr x y = if x == mergeEmpty then y else x
                  user = RBFile.s_tracks userMid
                  magma = RBFile.s_tracks magmaMid
                  reauthor f = f user `trackOr` f magma
              saveMIDI out $ userMid
                { RBFile.s_tracks = user
                  { RBFile.fixedVenue = case _autogenTheme $ _global songYaml of
                    Nothing -> blackVenueTrack
                    Just _  -> let
                      onlyLightingPP venue = mempty
                        { venueSpotKeys = venueSpotKeys venue
                        , venueSpotVocal = venueSpotVocal venue
                        , venueSpotGuitar = venueSpotGuitar venue
                        , venueSpotDrums = venueSpotDrums venue
                        , venueSpotBass = venueSpotBass venue
                        , venuePostProcessRB3 = venuePostProcessRB3 venue
                        , venuePostProcessRB2 = venuePostProcessRB2 venue
                        , venueLighting = venueLighting venue
                        , venueLightingCommands = venueLightingCommands venue
                        }
                      onlyCamera venue = mempty
                        { venueCameraRB3 = venueCameraRB3 venue
                        , venueCameraRB2 = venueCameraRB2 venue
                        , venueDirectedRB2 = venueDirectedRB2 venue
                        }
                      onlyOther venue = mempty
                        { venueSingGuitar = venueSingGuitar venue
                        , venueSingDrums = venueSingDrums venue
                        , venueSingBass = venueSingBass venue
                        , venueBonusFX = venueBonusFX venue
                        , venueBonusFXOptional = venueBonusFXOptional venue
                        , venueFog = venueFog venue
                        }
                      in mconcat
                        [ reauthor $ onlyLightingPP . RBFile.fixedVenue
                        , reauthor $ onlyCamera . RBFile.fixedVenue
                        , reauthor $ onlyOther . RBFile.fixedVenue
                        ]
                  , RBFile.fixedEvents = if RTB.null sects
                    then RBFile.fixedEvents magma
                    else (RBFile.fixedEvents magma)
                      { eventsSections = fmap makeRB3Section sects
                      }
                  }
                }

            [pathMagmaMid, pathMagmaPad, pathMagmaEditedParts] &%> \_ -> do
              input <- shakeMIDI $ planDir </> "raw.mid"
              (_, mixMode) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
              sects <- ATB.toPairList . RTB.toAbsoluteEventList 0 <$> midRealSections input
              let (magmaSects, invalid) = makeRBN2Sections sects
                  magmaSects' = RTB.fromAbsoluteEventList $ ATB.fromPairList magmaSects
                  adjustEvents trks = trks
                    { RBFile.onyxEvents = (RBFile.onyxEvents trks)
                      { eventsSections = magmaSects'
                      }
                    }
                  input' = input { RBFile.s_tracks = adjustEvents $ RBFile.s_tracks input }
              (output, diffs, vc, pad) <- case plan of
                MoggPlan{} -> do
                  (output, diffs, vc) <- RB3.processRB3
                    rb3
                    songYaml
                    (applyTargetMIDI (rb3_Common rb3) input')
                    mixMode
                    (applyTargetLength (rb3_Common rb3) input <$> getAudioLength planName plan)
                  return (output, diffs, vc, 0)
                Plan{} -> RB3.processRB3Pad
                  rb3
                  songYaml
                  (applyTargetMIDI (rb3_Common rb3) input')
                  mixMode
                  (applyTargetLength (rb3_Common rb3) input <$> getAudioLength planName plan)
              liftIO $ writeFile pathMagmaPad $ show pad
              liftIO $ writeFile pathMagmaEditedParts $ show (diffs, vc)
              case invalid of
                [] -> return ()
                _  -> lg $ "The following sections were swapped out for Magma (but will be readded in CON output): " ++ show invalid
              saveMIDI pathMagmaMid output

            let pathDta = dir </> "stfs/songs/songs.dta"
                pathMid = dir </> "stfs/songs" </> pkg </> pkg <.> "mid"
                pathOgg = dir </> "audio.ogg"
                pathMogg = dir </> "stfs/songs" </> pkg </> pkg <.> "mogg"
                pathPng = dir </> "stfs/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                pathMilo = dir </> "stfs/songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
                pathCon = dir </> "rb3con"

            pathDta %> \out -> do
              song <- shakeMIDI pathMid
              editedParts <- loadEditedParts
              songPkg <- makeRB3DTA songYaml plan rb3 False editedParts song pkg
              liftIO $ writeUtf8CRLF out $ prettyDTA pkg songPkg $ makeC3DTAComments (_metadata songYaml) plan rb3
            pathMid %> shk . copyFile' pathMagmaExport2
            pathOgg %> \out -> case plan of
              MoggPlan{} -> do
                -- TODO apply segment boundaries
                let speed = fromMaybe 1 $ tgt_Speed $ rb3_Common rb3
                pad <- shk $ read <$> readFile' (dir </> "magma/pad.txt")
                case (speed, pad :: Int) of
                  (1, 0) -> shk $ copyFile' (planDir </> "audio.ogg") out
                  _      -> do
                    input <- shk $ buildSource $ Input $ planDir </> "audio.ogg"
                    let src = padStart (Seconds $ realToFrac pad)
                          $ stretchFullSmart (1 / speed) 1 input
                    runAudio src out
              Plan{..} -> do
                (_, mixMode) <- computeDrumsPart (rb3_Drums rb3) plan songYaml
                (DifficultyRB3{..}, _) <- loadEditedParts
                let partsBeforeSong = concat
                      [ [pathMagmaKick   | rb3DrumsRank  /= 0 && mixMode /= RBDrums.D0]
                      , [pathMagmaSnare  | rb3DrumsRank  /= 0 && notElem mixMode [RBDrums.D0, RBDrums.D4]]
                      , [pathMagmaDrums  | rb3DrumsRank  /= 0]
                      , [pathMagmaBass   | rb3BassRank   /= 0]
                      , [pathMagmaGuitar | rb3GuitarRank /= 0]
                      , [pathMagmaKeys   | rb3KeysRank   /= 0]
                      , [pathMagmaVocal  | rb3VocalRank  /= 0]
                      , [pathMagmaCrowd  | isJust _crowd]
                      ]
                    parts = case NE.nonEmpty partsBeforeSong of
                      Nothing -> return pathMagmaSong
                      Just ne -> ne <> return pathMagmaSong
                src <- shk $ buildSource $ Merge $ fmap Input parts
                runAudio src out
            pathMogg %> \out -> case plan of
              MoggPlan{} -> do
                -- TODO apply segment boundaries
                let speed = fromMaybe 1 $ tgt_Speed $ rb3_Common rb3
                pad <- shk $ read <$> readFile' (dir </> "magma/pad.txt")
                case (speed, pad :: Int) of
                  (1, 0) -> shk $ copyFile' (planDir </> "audio.mogg") out
                  _      -> do
                    shk $ need [pathOgg]
                    mapStackTraceT (liftIO . runResourceT) $ oggToMogg pathOgg out
              Plan{} -> do
                shk $ need [pathOgg]
                mapStackTraceT (liftIO . runResourceT) $ oggToMogg pathOgg out
            pathPng  %> shk . copyFile' (rel "gen/cover.png_xbox")
            pathMilo %> \out -> case getPart (rb3_Vocal rb3) songYaml >>= partVocal of
              -- TODO apply segment boundaries
              -- TODO add member assignments and anim style in BandSongPref, and anim style
              -- TODO include rb3 format venue in milo (with speed/pad adjustments)
              Nothing   -> stackIO emptyMilo >>= \mt -> shk $ copyFile' mt out
              Just pvox -> do
                let srcs = case (vocalLipsyncRB3 pvox, vocalCount pvox) of
                      (Just lrb3, _     ) -> lipsyncSources lrb3
                      (Nothing  , Vocal1) -> [LipsyncVocal Nothing]
                      (Nothing  , Vocal2) -> [LipsyncVocal $ Just Vocal1, LipsyncVocal $ Just Vocal2]
                      (Nothing  , Vocal3) -> [LipsyncVocal $ Just Vocal1, LipsyncVocal $ Just Vocal2, LipsyncVocal $ Just Vocal3]
                midi <- shakeMIDI $ planDir </> "raw.mid"
                vmap <- loadVisemesRB3
                pad <- shk $ read <$> readFile' (dir </> "magma/pad.txt")
                let vox = RBFile.getFlexPart (rb3_Vocal rb3) $ RBFile.s_tracks midi
                    lip = lipsyncFromMIDITrack vmap . mapTrack (U.applyTempoTrack $ RBFile.s_tempos midi)
                    auto = autoLipsync defaultTransition vmap englishSyllables . mapTrack (U.applyTempoTrack $ RBFile.s_tempos midi)
                    write = stackIO . BL.writeFile out
                    padSeconds = fromIntegral (pad :: Int) :: U.Seconds
                    speed = realToFrac $ fromMaybe 1 $ tgt_Speed $ rb3_Common rb3 :: Rational
                    fromSource = fmap (lipsyncPad padSeconds . lipsyncAdjustSpeed speed) . \case
                      LipsyncTrack1 -> return $ lip $ RBFile.onyxLipsync1 vox
                      LipsyncTrack2 -> return $ lip $ RBFile.onyxLipsync2 vox
                      LipsyncTrack3 -> return $ lip $ RBFile.onyxLipsync3 vox
                      LipsyncTrack4 -> return $ lip $ RBFile.onyxLipsync4 vox
                      LipsyncVocal mvc -> return $ auto $ case mvc of
                        Nothing     -> RBFile.onyxPartVocals vox
                        Just Vocal1 -> RBFile.onyxHarm1 vox
                        Just Vocal2 -> RBFile.onyxHarm2 vox
                        Just Vocal3 -> RBFile.onyxHarm3 vox
                      LipsyncFile f -> stackIO (BL.fromStrict <$> B.readFile f) >>= runGetM parseLipsync
                lips <- mapM fromSource srcs
                case lips of
                  []                    -> stackIO emptyMilo >>= \mt -> shk $ copyFile' mt out
                  [l1]                  -> write $ magmaMilo $ MagmaLipsync1 l1
                  [l1, l2]              -> write $ magmaMilo $ MagmaLipsync2 l1 l2
                  [l1, l2, l3]          -> write $ magmaMilo $ MagmaLipsync3 l1 l2 l3
                  l1 : l2 : l3 : l4 : _ -> write $ magmaMilo $ MagmaLipsync4 l1 l2 l3 l4
            pathCon %> \out -> do
              let files = [pathDta, pathMid, pathMogg, pathMilo]
                    ++ [pathPng | isJust $ _fileAlbumArt $ _metadata songYaml]
              shk $ need files
              lg "# Producing RB3 CON file"
              mapStackTraceT (mapQueueLog $ liftIO . runResourceT) $ rb3pkg
                (getArtist (_metadata songYaml) <> " - " <> title)
                (T.pack $ "Compiled by Onyx Music Game Toolkit version " <> showVersion version)
                (dir </> "stfs")
                out

            let rb3ps3Root = dir </> "rb3-ps3"
                rb3ps3DTA = rb3ps3Root </> "songs/songs.dta"
                rb3ps3Mogg = rb3ps3Root </> "songs" </> pkg </> pkg <.> "mogg"
                rb3ps3Mid = rb3ps3Root </> "songs" </> pkg </> pkg <.> "mid.edat"
                rb3ps3Art = rb3ps3Root </> "songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_ps3")
                rb3ps3Milo = rb3ps3Root </> "songs" </> pkg </> "gen" </> pkg <.> "milo_ps3"
                -- don't think we need weights.bin or .pan
                rb3ps3Pkg = dir </> "rb3-ps3.pkg"
                rb3ps3Folder = makePS3Name (hashRB3 songYaml rb3) songYaml
                rb3ps3EDATConfig = rb3CustomMidEdatConfig rb3ps3Folder
                rb3ps3ContentID = npdContentID rb3ps3EDATConfig

            rb3ps3DTA %> \out -> do
              song <- shakeMIDI pathMid
              editedParts <- loadEditedParts
              songPkg <- makeRB3DTA songYaml plan rb3 True editedParts song pkg
              liftIO $ writeUtf8CRLF out $ prettyDTA pkg songPkg $ makeC3DTAComments (_metadata songYaml) plan rb3
            rb3ps3Mid %> \out -> if rb3_PS3Encrypt rb3
              then do
                shk $ need [pathMid]
                fin  <- shortWindowsPath False pathMid
                fout <- shortWindowsPath True  out
                stackIO $ packNPData rb3ps3EDATConfig fin fout $ B8.pack pkg <> ".mid.edat"
              else shk $ copyFile' pathMid out
            rb3ps3Art %> shk . copyFile' (rel "gen/cover.png_ps3")
            rb3ps3Mogg %> \out -> do
              -- PS3 RB3 can't play unencrypted moggs
              shk $ need [pathMogg]
              moggType <- stackIO $ withBinaryFile pathMogg ReadMode $ \h -> B.hGet h 1
              fin  <- shortWindowsPath False pathMogg
              fout <- shortWindowsPath True  out
              case B.unpack moggType of
                [0xA] -> stackIO $ encryptRB1 fin fout
                _     -> shk $ copyFile' pathMogg out
            rb3ps3Milo %> shk . copyFile' pathMilo
            phony rb3ps3Root $ do
              shk $ need [rb3ps3DTA, rb3ps3Mogg, rb3ps3Mid, rb3ps3Art, rb3ps3Milo]

            let crawlFolderBytes p
                  =   stackIO
                  $   crawlFolder p
                  >>= return . first TE.encodeUtf8
            rb3ps3Pkg %> \out -> do
              shk $ need [rb3ps3Root]
              let container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
              main <- container "USRDIR" . container rb3ps3Folder <$> crawlFolderBytes rb3ps3Root
              extra <- stackIO (getResourcesPath "pkg-contents/rb3") >>= crawlFolderBytes
              stackIO $ makePKG rb3ps3ContentID (main <> extra) >>= BL.writeFile out

            -- Guitar rules
            dir </> "protar-mpa.mid" %> \out -> do
              input <- shakeMIDI pathMagmaMid
              let gtr17   = RBFile.onyxPartRealGuitar   $ RBFile.getFlexPart (rb3_Guitar rb3) $ RBFile.s_tracks input
                  gtr22   = RBFile.onyxPartRealGuitar22 $ RBFile.getFlexPart (rb3_Guitar rb3) $ RBFile.s_tracks input
                  bass17  = RBFile.onyxPartRealGuitar   $ RBFile.getFlexPart (rb3_Bass   rb3) $ RBFile.s_tracks input
                  bass22  = RBFile.onyxPartRealGuitar22 $ RBFile.getFlexPart (rb3_Bass   rb3) $ RBFile.s_tracks input
                  pgThres = maybe 170 pgHopoThreshold $ getPart (rb3_Guitar rb3) songYaml >>= partProGuitar
                  pbThres = maybe 170 pgHopoThreshold $ getPart (rb3_Bass   rb3) songYaml >>= partProGuitar
                  playTrack thres cont name t = let
                    expert = fromMaybe mempty $ Map.lookup Expert $ pgDifficulties t
                    auto = PGPlay.autoplay (fromIntegral thres / 480) (RBFile.s_tempos input) expert
                    msgToSysEx msg
                      = E.SystemExclusive $ SysEx.Regular $ PGPlay.sendCommand (cont, msg) ++ [0xF7]
                    in U.setTrackName name $ msgToSysEx <$> auto
              saveMIDI out input
                { RBFile.s_tracks = RBFile.RawFile
                    [ playTrack pgThres PGPlay.Mustang "GTR17"  $ if nullPG gtr17  then gtr22  else gtr17
                    , playTrack pgThres PGPlay.Squier  "GTR22"  $ if nullPG gtr22  then gtr17  else gtr22
                    , playTrack pbThres PGPlay.Mustang "BASS17" $ if nullPG bass17 then bass22 else bass17
                    , playTrack pbThres PGPlay.Squier  "BASS22" $ if nullPG bass22 then bass17 else bass22
                    ]
                }

            case mrb2 of
              Nothing -> return ()
              Just rb2 -> do

                pathMagmaMidV1 %> \out -> shakeMIDI pathMagmaMid >>= RB2.convertMIDI >>= saveMIDI out

                pathMagmaProjV1 %> \out -> do
                  editedParts <- loadEditedParts
                  p <- makeMagmaProj songYaml rb3 plan editedParts pkg pathMagmaMid $ return title
                  let makeDummy (Magma.Tracks dl dkt dk ds b g v k bck) = Magma.Tracks
                        dl
                        (makeDummyKeep dkt)
                        (makeDummyKeep dk)
                        (makeDummyKeep ds)
                        (makeDummyMono b)
                        (makeDummyMono g)
                        (makeDummyMono v)
                        (makeDummyMono k) -- doesn't matter
                        (makeDummyMono bck)
                      makeDummyMono af = af
                        { Magma.audioFile = "dummy-mono.wav"
                        , Magma.channels = 1
                        , Magma.pan = [0]
                        , Magma.vol = [0]
                        }
                      makeDummyKeep af = case Magma.channels af of
                        1 -> af
                          { Magma.audioFile = "dummy-mono.wav"
                          }
                        _ -> af
                          { Magma.audioFile = "dummy-stereo.wav"
                          , Magma.channels = 2
                          , Magma.pan = [-1, 1]
                          , Magma.vol = [0, 0]
                          }
                  liftIO $ D.writeFileDTA_latin1 out $ D.serialize (valueId D.stackChunks) p
                    { Magma.project = (Magma.project p)
                      { Magma.albumArt = Magma.AlbumArt "cover-v1.bmp"
                      , Magma.midi = (Magma.midi $ Magma.project p)
                        { Magma.midiFile = "notes-v1.mid"
                        }
                      , Magma.projectVersion = 5
                      , Magma.languages = let
                          lang s = elem s $ _languages $ _metadata songYaml
                          eng = lang "English"
                          fre = lang "French"
                          ita = lang "Italian"
                          spa = lang "Spanish"
                          in Magma.Languages
                            { Magma.english  = Just $ eng || not (or [eng, fre, ita, spa])
                            , Magma.french   = Just fre
                            , Magma.italian  = Just ita
                            , Magma.spanish  = Just spa
                            , Magma.german   = Nothing
                            , Magma.japanese = Nothing
                            }
                      , Magma.dryVox = (Magma.dryVox $ Magma.project p)
                        { Magma.dryVoxFileRB2 = Just "dryvox-sine.wav"
                        }
                      , Magma.tracks = makeDummy $ Magma.tracks $ Magma.project p
                      , Magma.metadata = (Magma.metadata $ Magma.project p)
                        { Magma.genre = rbn1Genre fullGenre
                        , Magma.subGenre = "subgenre_" <> rbn1Subgenre fullGenre
                        , Magma.author = T.strip $ T.take 75 $ Magma.author $ Magma.metadata $ Magma.project p
                        }
                      , Magma.gamedata = (Magma.gamedata $ Magma.project p)
                        { Magma.previewStartMs = 0 -- for dummy audio. will reset after magma
                        }
                      }
                    }
                  -- TODO patch this
                  -- ERROR: Metadata Compiler: [...] track_number: value is 16000, which is greater than the maximum value of 99.

                pathMagmaRbaV1 %> \out -> do
                  shk $ need [pathMagmaDummyMono, pathMagmaDummyStereo, pathMagmaDryvoxSine, pathMagmaCoverV1, pathMagmaMidV1, pathMagmaProjV1]
                  lg "# Running Magma v1 (without 10 min limit)"
                  errorToWarning (mapStackTraceT (liftIO . runResourceT) $ Magma.runMagmaV1 pathMagmaProjV1 out) >>= \case
                    Just output -> lg output
                    Nothing     -> do
                      lg "Magma v1 failed; optimistically bypassing."
                      stackIO $ B.writeFile out B.empty

                -- Magma v1 rba to con
                do
                  let doesRBAExist = do
                        shk $ need [pathMagmaRbaV1]
                        stackIO $ (/= 0) <$> withBinaryFile pathMagmaRbaV1 ReadMode hFileSize
                      rb2CON = dir </> "rb2con"
                      rb2OriginalDTA = dir </> "rb2-original.dta"
                      rb2DTA = dir </> "rb2/songs/songs.dta"
                      rb2Mogg = dir </> "rb2/songs" </> pkg </> pkg <.> "mogg"
                      rb2Mid = dir </> "rb2/songs" </> pkg </> pkg <.> "mid"
                      rb2Art = dir </> "rb2/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                      rb2Weights = dir </> "rb2/songs" </> pkg </> "gen" </> (pkg ++ "_weights.bin")
                      rb2Milo = dir </> "rb2/songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
                      rb2Pan = dir </> "rb2/songs" </> pkg </> pkg <.> "pan"
                      fixDict = HM.fromList . fixAssoc . HM.toList
                      fixDictList = D.DictList . fixAssoc . D.fromDictList
                      fixAssoc = mapMaybe $ \(k, v) -> case k of
                        "guitar" -> Just (k, v)
                        "bass"   -> Just (k, v)
                        "keys"   -> Nothing
                        "drum"   -> Just (k, v)
                        "vocals" -> Just (k, v)
                        "band"   -> Just (k, v)
                        _        -> Nothing
                  rb2OriginalDTA %> \out -> do
                    ex <- doesRBAExist
                    if ex
                      then Magma.getRBAFile 0 pathMagmaRbaV1 out
                      else do
                        shk $ need [pathDta]
                        (_, rb3DTA, _) <- readRB3DTA pathDta
                        let newDTA :: D.SongPackage
                            newDTA = D.SongPackage
                              { D.name = D.name rb3DTA
                              , D.artist = D.artist rb3DTA
                              , D.master = not $ _cover $ _metadata songYaml
                              , D.song = D.Song
                                -- most of this gets rewritten later anyway
                                { D.songName = D.songName $ D.song rb3DTA
                                , D.tracksCount = Nothing
                                , D.tracks = D.tracks $ D.song rb3DTA
                                , D.pans = D.pans $ D.song rb3DTA
                                , D.vols = D.vols $ D.song rb3DTA
                                , D.cores = D.cores $ D.song rb3DTA
                                , D.drumSolo = D.drumSolo $ D.song rb3DTA -- needed
                                , D.drumFreestyle = D.drumFreestyle $ D.song rb3DTA -- needed
                                , D.midiFile = D.midiFile $ D.song rb3DTA
                                -- not used
                                , D.vocalParts = Nothing
                                , D.crowdChannels = Nothing
                                , D.hopoThreshold = Nothing
                                , D.muteVolume = Nothing
                                , D.muteVolumeVocals = Nothing
                                }
                              , D.songScrollSpeed = D.songScrollSpeed rb3DTA
                              , D.bank = D.bank rb3DTA
                              , D.animTempo = D.animTempo rb3DTA
                              , D.songLength = D.songLength rb3DTA
                              , D.preview = D.preview rb3DTA
                              , D.rank = fixDict $ D.rank rb3DTA
                              , D.genre = Just $ rbn1Genre fullGenre
                              , D.decade = Just $ case D.yearReleased rb3DTA of
                                Nothing -> "the10s"
                                Just y
                                  | 1960 <= y && y < 1970 -> "the60s"
                                  | 1970 <= y && y < 1980 -> "the70s"
                                  | 1980 <= y && y < 1990 -> "the80s"
                                  | 1990 <= y && y < 2000 -> "the90s"
                                  | 2000 <= y && y < 2010 -> "the00s"
                                  | 2010 <= y && y < 2020 -> "the10s"
                                  | otherwise -> "the10s"
                              , D.vocalGender = D.vocalGender rb3DTA
                              , D.version = 0
                              , D.fake = Nothing
                              , D.downloaded = Just True
                              , D.songFormat = 4
                              , D.albumArt = Just True
                              , D.yearReleased = D.yearReleased rb3DTA
                              , D.yearRecorded = D.yearRecorded rb3DTA
                              , D.basePoints = Just 0 -- TODO why did I put this?
                              , D.videoVenues = Nothing
                              , D.dateReleased = Nothing
                              , D.dateRecorded = Nothing
                              , D.rating = D.rating rb3DTA
                              , D.subGenre = Just $ "subgenre_" <> rbn1Subgenre fullGenre
                              , D.songId = D.songId rb3DTA
                              , D.tuningOffsetCents = D.tuningOffsetCents rb3DTA
                              , D.context = Just 2000
                              , D.gameOrigin = Just "rb2"
                              , D.ugc = Just True
                              , D.albumName = D.albumName rb3DTA
                              , D.albumTrackNumber = D.albumTrackNumber rb3DTA
                              , D.packName = D.packName rb3DTA
                              -- not present
                              , D.drumBank = Nothing
                              , D.bandFailCue = Nothing
                              , D.solo = Nothing
                              , D.shortVersion = Nothing
                              , D.vocalTonicNote = Nothing
                              , D.songTonality = Nothing
                              , D.songKey = Nothing
                              , D.realGuitarTuning = Nothing
                              , D.realBassTuning = Nothing
                              , D.guidePitchVolume = Nothing
                              , D.encoding = Nothing
                              , D.extraAuthoring = Nothing
                              , D.alternatePath = Nothing
                              }
                        liftIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0 [D.Parens (D.Tree 0 (D.Sym pkg : makeValue D.stackChunks newDTA))]
                  let writeRB2DTA isPS3 out = do
                        shk $ need [rb2OriginalDTA, pathDta]
                        (_, magmaDTA, _) <- readRB3DTA rb2OriginalDTA
                        (_, rb3DTA, _) <- readRB3DTA pathDta
                        let newDTA :: D.SongPackage
                            newDTA = magmaDTA
                              { D.name = targetTitle songYaml $ RB2 rb2
                              , D.artist = D.artist rb3DTA
                              , D.albumName = D.albumName rb3DTA
                              , D.master = not $ _cover $ _metadata songYaml
                              , D.version = 0
                              -- if version is not 0, you get a message
                              -- "can't play this song until all players in your session purchase it!"
                              , D.song = (D.song magmaDTA)
                                { D.tracksCount = Nothing
                                , D.tracks = fixDictList $ D.tracks $ D.song rb3DTA
                                , D.midiFile = Just $ "songs/" <> pkg <> "/" <> pkg <> ".mid"
                                , D.songName = "songs/" <> pkg <> "/" <> pkg
                                , D.pans = D.pans $ D.song rb3DTA
                                , D.vols = D.vols $ D.song rb3DTA
                                , D.cores = D.cores $ D.song rb3DTA
                                , D.crowdChannels = D.crowdChannels $ D.song rb3DTA
                                }
                              , D.songId = Just $ case rb2_SongID rb2 of
                                  SongIDSymbol s   -> Right s -- could override on PS3 but shouldn't happen
                                  SongIDInt i      -> Left $ fromIntegral i
                                  SongIDAutoSymbol -> if isPS3
                                    then Left $ fromIntegral $ hashRB3 songYaml rb3 -- PS3 needs real number ID
                                    else Right pkg
                                  SongIDAutoInt    -> Left $ fromIntegral $ hashRB3 songYaml rb3
                              , D.preview = D.preview rb3DTA -- because we told magma preview was at 0s earlier
                              , D.songLength = D.songLength rb3DTA -- magma v1 set this to 31s from the audio file lengths
                              , D.rating = case (isPS3, D.rating rb3DTA) of
                                (True, 4) -> 2 -- Unrated causes it to be locked in game on PS3
                                (_   , x) -> x
                              }
                        liftIO $ writeLatin1CRLF out $ prettyDTA pkg newDTA $ makeC3DTAComments (_metadata songYaml) plan rb3
                  rb2DTA %> writeRB2DTA False
                  rb2Mid %> \out -> do
                    ex <- doesRBAExist
                    RBFile.Song tempos sigs trks <- if ex
                      then do
                        shk $ need [pathMagmaRbaV1]
                        liftIO $ Magma.getRBAFile 1 pathMagmaRbaV1 out
                        RBFile.loadMIDI out
                      else shakeMIDI pathMagmaMidV1
                    sects <- getRealSections'
                    let mid = RBFile.Song tempos sigs trks
                          { RBFile.fixedEvents = if RTB.null sects
                            then RBFile.fixedEvents trks
                            else (RBFile.fixedEvents trks)
                              { eventsSections = fmap makeRB2Section sects
                              }
                          , RBFile.fixedVenue = if RBFile.fixedVenue trks == mempty
                            then VenueTrack
                              { venueCameraRB3        = RTB.empty
                              , venueCameraRB2        = RTB.flatten $ RTB.singleton 0
                                [ CameraCut
                                , FocusBass
                                , FocusDrums
                                , FocusGuitar
                                , FocusVocal
                                , NoBehind
                                , OnlyFar
                                , NoClose
                                ]
                              , venueDirectedRB2      = RTB.empty
                              , venueSingGuitar       = RTB.empty
                              , venueSingDrums        = RTB.empty
                              , venueSingBass         = RTB.empty
                              , venueSpotKeys         = RTB.empty
                              , venueSpotVocal        = RTB.empty
                              , venueSpotGuitar       = RTB.empty
                              , venueSpotDrums        = RTB.empty
                              , venueSpotBass         = RTB.empty
                              , venuePostProcessRB3   = RTB.empty
                              , venuePostProcessRB2   = RTB.singleton 0 V2_video_security
                              , venueLighting         = RTB.singleton 0 Lighting_
                              , venueLightingCommands = RTB.empty
                              , venueLightingMode     = RTB.singleton 0 ModeVerse
                              , venueBonusFX          = RTB.empty
                              , venueBonusFXOptional  = RTB.empty
                              , venueFog              = RTB.empty
                              }
                            else RBFile.fixedVenue trks
                          }
                    saveMIDI out mid
                  rb2Mogg %> shk . copyFile' pathMogg
                  rb2Milo %> \out -> do
                    -- TODO replace this with our own lipsync milo, ignore magma
                    ex <- doesRBAExist
                    if ex
                      then stackIO $ Magma.getRBAFile 3 pathMagmaRbaV1 out
                      else stackIO emptyMiloRB2 >>= \mt -> shk $ copyFile' mt out
                  rb2Weights %> \out -> do
                    ex <- doesRBAExist
                    if ex
                      then stackIO $ Magma.getRBAFile 5 pathMagmaRbaV1 out
                      else stackIO emptyWeightsRB2 >>= \mt -> shk $ copyFile' mt out
                  rb2Art %> shk . copyFile' (rel "gen/cover.png_xbox")
                  rb2Pan %> \out -> liftIO $ B.writeFile out B.empty
                  rb2CON %> \out -> do
                    shk $ need [rb2DTA, rb2Mogg, rb2Mid, rb2Art, rb2Weights, rb2Milo, rb2Pan]
                    lg "# Producing RB2 CON file"
                    mapStackTraceT (mapQueueLog $ liftIO . runResourceT) $ rb2pkg
                      (getArtist (_metadata songYaml) <> " - " <> targetTitle songYaml (RB2 rb2))
                      (T.pack $ "Compiled by Onyx Music Game Toolkit version " <> showVersion version)
                      (dir </> "rb2")
                      out

                  let rb2ps3Root = dir </> "rb2-ps3"
                      rb2ps3DTA = rb2ps3Root </> "songs/songs.dta"
                      rb2ps3Mogg = rb2ps3Root </> "songs" </> pkg </> pkg <.> "mogg"
                      rb2ps3Mid = rb2ps3Root </> "songs" </> pkg </> pkg <.> "mid.edat"
                      rb2ps3Art = rb2ps3Root </> "songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_ps3")
                      rb2ps3Milo = rb2ps3Root </> "songs" </> pkg </> "gen" </> pkg <.> "milo_ps3"
                      rb2ps3Weights = rb2ps3Root </> "songs" </> pkg </> "gen" </> (pkg ++ "_weights.bin")
                      rb2ps3Pan = rb2ps3Root </> "songs" </> pkg </> pkg <.> "pan"
                      rb2ps3Pkg = dir </> "rb2-ps3.pkg"
                      rb2ps3Folder = makePS3Name (hashRB3 songYaml rb3) songYaml
                      rb2ps3EDATConfig = rb2CustomMidEdatConfig rb2ps3Folder
                      rb2ps3ContentID = npdContentID rb2ps3EDATConfig

                  rb2ps3DTA %> writeRB2DTA True
                  rb2ps3Mid %> \out -> if rb2_PS3Encrypt rb2
                    then do
                      shk $ need [rb2Mid]
                      fin  <- shortWindowsPath False rb2Mid
                      fout <- shortWindowsPath True  out
                      stackIO $ packNPData rb2ps3EDATConfig fin fout $ B8.pack pkg <> ".mid.edat"
                    else shk $ copyFile' rb2Mid out
                  rb2ps3Art %> shk . copyFile' (rel "gen/cover.png_ps3")
                  rb2ps3Mogg %> \out -> do
                    -- PS3 RB3 can't play unencrypted moggs (RB2 might be fine, but we may as well be compatible)
                    shk $ need [rb2Mogg]
                    moggType <- stackIO $ withBinaryFile rb2Mogg ReadMode $ \h -> B.hGet h 1
                    fin  <- shortWindowsPath False rb2Mogg
                    fout <- shortWindowsPath True  out
                    case B.unpack moggType of
                      [0xA] -> stackIO $ encryptRB1 fin fout
                      _     -> shk $ copyFile' rb2Mogg out
                  rb2ps3Weights %> shk . copyFile' rb2Weights
                  rb2ps3Milo %> shk . copyFile' rb2Milo
                  rb2ps3Pan %> shk . copyFile' rb2Pan
                  phony rb2ps3Root $ do
                    shk $ need [rb2ps3DTA, rb2ps3Mogg, rb2ps3Mid, rb2ps3Art, rb2ps3Weights, rb2ps3Milo, rb2ps3Pan]

                  rb2ps3Pkg %> \out -> do
                    shk $ need [rb2ps3Root]
                    let container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
                    main <- container "USRDIR" . container rb2ps3Folder <$> crawlFolderBytes rb2ps3Root
                    extra <- stackIO (getResourcesPath "pkg-contents/rb2") >>= crawlFolderBytes
                    stackIO $ makePKG rb2ps3ContentID (main <> extra) >>= BL.writeFile out

      forM_ (extraTargets ++ HM.toList (_targets songYaml)) $ \(targetName, target) -> do
        let dir = rel $ "gen/target" </> T.unpack targetName
        case target of
          RB3 rb3 -> rbRules dir rb3 Nothing
          RB2 rb2 -> let
            rb3 = TargetRB3
              { rb3_Common = rb2_Common rb2
              , rb3_2xBassPedal = rb2_2xBassPedal rb2
              , rb3_SongID = rb2_SongID rb2
              , rb3_Version = rb2_Version rb2
              , rb3_Guitar = rb2_Guitar rb2
              , rb3_Bass = rb2_Bass rb2
              , rb3_Drums = rb2_Drums rb2
              , rb3_Vocal = rb2_Vocal rb2
              , rb3_Keys = RBFile.FlexExtra "undefined"
              , rb3_Harmonix = False
              , rb3_Magma = rb2_Magma rb2
              , rb3_PS3Encrypt = rb2_PS3Encrypt rb2
              }
            in rbRules dir rb3 $ Just rb2
          GH2 gh2 -> do

            (planName, plan) <- case getPlan (tgt_Plan $ gh2_Common gh2) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh2
              Just pair -> return pair
            let planDir = rel $ "gen/plan" </> T.unpack planName
                defaultID = hashGH2 songYaml gh2
                defaultLBP = defaultID + 1
                defaultLBW = defaultID + 2
                key = fromMaybe (makeShortName defaultID songYaml) $ gh2_Key gh2
                pkg = T.unpack key

            let loadPartAudioCheck = case plan of
                  Plan{..}     -> return $ \part -> HM.member part $ getParts _planParts
                  MoggPlan{..} -> do
                    silentChans <- shk $ read <$> readFile' (planDir </> "silent-channels.txt")
                    return $ \part -> case HM.lookup part $ getParts _moggParts of
                      Nothing    -> False
                      Just chans -> any (`notElem` (silentChans :: [Int])) $ concat $ toList chans

            [dir </> "gh2/notes.mid", dir </> "gh2/coop_max_scores.dta", dir </> "gh2/pad.txt"] &%> \[out, coop, pad] -> do
              input <- shakeMIDI $ planDir </> "processed.mid"
              hasAudio <- loadPartAudioCheck
              audio <- computeGH2Audio songYaml gh2 hasAudio
              (mid, padSeconds) <- midiRB3toGH2 songYaml gh2 audio
                (applyTargetMIDI (gh2_Common gh2) input)
                (getAudioLength planName plan)
              saveMIDI out mid
              let p1 = gh2PartGuitar $ RBFile.s_tracks mid
                  p2 = case gh2_Coop gh2 of
                    GH2Bass   -> gh2PartBass   $ RBFile.s_tracks mid
                    GH2Rhythm -> gh2PartRhythm $ RBFile.s_tracks mid
                  scores = map (\diff -> gh2Base diff p1 + gh2Base diff p2) [Easy .. Expert]
                  dta = "(" <> T.unpack key <> " (" <> unwords (map show scores) <> "))"
              stackIO $ writeFile coop dta
              stackIO $ writeFile pad $ show padSeconds

            let loadGH2Midi = shakeMIDI $ dir </> "gh2/notes.mid" :: Staction (RBFile.Song (GH2File U.Beats))
                correctAudioLength mid = do
                  endTime <- case RTB.filter (== GH2.End) $ GH2.eventsOther $ gh2Events $ RBFile.s_tracks mid of
                    RNil       -> fatal "panic! couldn't find [end] event in GH2 output midi"
                    Wait t _ _ -> return $ U.applyTempoMap (RBFile.s_tempos mid) t
                  return $ endTime + 5
                  -- previously we went 0.5s past [end], but that still had issues,
                  -- particularly in practice mode when playing the last section
                gh2SourceGeneral lowRateSilence withSources = do
                  hasAudio <- loadPartAudioCheck
                  audio <- computeGH2Audio songYaml gh2 hasAudio
                  mid <- loadGH2Midi
                  srcs <- forM (gh2AudioSections audio) $ \case
                    GH2PartStereo part -> getPartSource [(-1, 0), (1, 0)] planName plan part 1
                    -- This halves the volume, so we set vols in .dta to compensate
                    GH2PartMono part -> applyVolsMono [0] <$> getPartSource [(-1, 0), (1, 0)] planName plan part 1
                    GH2Band -> sourceSongCountin (gh2_Common gh2) mid 0 True planName plan $ concat
                      [ [(gh2LeadTrack audio, 1)]
                      , [(gh2CoopTrack audio, 1)]
                      , maybe [] (\t -> [(t, 1)]) $ gh2DrumTrack audio
                      ]
                    GH2Silent -> return $ silent (Seconds 0) (if lowRateSilence then 11025 else 44100) 1
                  pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
                  audioLen <- correctAudioLength mid
                  let applyOffset = case compare (gh2_Offset gh2) 0 of
                        EQ -> id
                        GT -> dropStart $ Seconds          $ gh2_Offset gh2
                        LT -> padStart  $ Seconds $ negate $ gh2_Offset gh2
                      toEachSource
                        = setAudioLength audioLen
                        . applyOffset
                        . padAudio pad
                        . applyTargetAudio (gh2_Common gh2) mid
                  return $ fmap toEachSource $ withSources srcs
                -- for vgs, separate sources so silence can be encoded at low sample rate
                gh2SourcesVGS = gh2SourceGeneral True id
                -- for mogg, single source
                gh2Source = fmap runIdentity $ gh2SourceGeneral False $ Identity . foldr1 merge

            dir </> "gh2/audio.vgs" %> \out -> do
              srcs <- gh2SourcesVGS
              stackIO $ runResourceT $ writeVGSMultiRate out $ map (mapSamples integralSample) srcs

            dir </> "gh2/audio_empty.vgs" %> \out -> do
              audioLen <- loadGH2Midi >>= correctAudioLength
              stackIO $ runResourceT $ writeVGS out
                $ silent (Seconds $ realToFrac audioLen) 1024 1
            forM_ ([90, 75, 60] :: [Int]) $ \speed -> do
              dir </> ("gh2/audio_p" ++ show speed ++ ".vgs") %> \out -> do
                hasAudio <- loadPartAudioCheck
                audio <- computeGH2Audio songYaml gh2 hasAudio
                mid <- loadGH2Midi
                (src, dupe) <- case gh2Practice audio of
                  [Nothing, Nothing] -> do
                    -- just compute it once and duplicate later
                    src <- applyVolsMono [0] <$> shk (buildSource $ Input $ planDir </> "everything.wav")
                    return (src, True)
                  _ -> do
                    srcs <- forM (gh2Practice audio) $ \case
                      Nothing   -> applyVolsMono [0] <$> shk (buildSource $ Input $ planDir </> "everything.wav")
                      Just part -> applyVolsMono [0] <$> getPartSource [(-1, 0), (1, 0)] planName plan part 1
                    return (foldr1 merge srcs, False)
                pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
                rate <- case speed of
                  60 -> return 19875
                  75 -> return 16125
                  90 -> return 13500
                  50 -> return 24000
                  65 -> return 18375
                  85 -> return 14250
                  _  -> fatal $ "No known rate for GH2 practice speed: " ++ show speed ++ "%"
                lg $ "Writing GH2 practice audio for " ++ show speed ++ "% speed"
                stackIO $ runResourceT $ writeVGS out
                  $ (if dupe then remapChannels [Just 0, Just 0] else id)
                  $ mapSamples integralSample
                  $ resampleTo rate SincMediumQuality
                  $ stretchFull 1 (100 / fromIntegral speed)
                  $ padAudio pad
                  $ applyTargetAudio (gh2_Common gh2) mid src
                lg $ "Finished writing GH2 practice audio for " ++ show speed ++ "% speed"

            [dir </> "gh2/songs.dta", dir </> "gh2/songs-dx2.dta", dir </> "gh2/songs-inner.dta", dir </> "gh2/songs-inner-dx2.dta"] &%> \[out, outDX2, outInner, outInnerDX2] -> do
              input <- shakeMIDI $ planDir </> "processed.mid"
              hasAudio <- loadPartAudioCheck
              audio <- computeGH2Audio songYaml gh2 hasAudio
              let inner isDX2 = D.serialize (valueId D.stackChunks) $ makeGH2DTA
                    songYaml
                    key
                    (previewBounds songYaml (input :: RBFile.Song (RBFile.OnyxFile U.Beats)))
                    gh2
                    audio
                    (targetTitle songYaml target)
                    isDX2
                  innerStandard = inner False
                  innerDX2      = inner True
              stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
                [ D.Parens $ D.Tree 0 $ D.Sym key : D.treeChunks (D.topTree innerStandard) ]
              stackIO $ D.writeFileDTA_latin1 outDX2 $ D.DTA 0 $ D.Tree 0
                [ D.Parens $ D.Tree 0 $ D.Sym key : D.treeChunks (D.topTree innerDX2) ]
              stackIO $ D.writeFileDTA_latin1 outInner innerStandard
              stackIO $ D.writeFileDTA_latin1 outInnerDX2 innerDX2

            dir </> "gh2/lipsync.voc" %> \out -> do
              midi <- shakeMIDI $ planDir </> "raw.mid"
              let vox = RBFile.getFlexPart (gh2_Vocal gh2) $ RBFile.s_tracks midi
                  auto = gh2Lipsync englishSyllables . mapTrack (U.applyTempoTrack $ RBFile.s_tempos midi)
              stackIO $ BL.writeFile out $ runPut $ putVocFile
                $ auto $ RBFile.onyxPartVocals vox

            dir </> "gh2/symbol" %> \out -> do
              stackIO $ B.writeFile out $ B8.pack pkg

            -- TODO give this the "distressed photo" look like the other bonus songs
            dir </> "gh2/cover.png_ps2" %> \out -> do
              img <- loadRGB8
              stackIO $ BL.writeFile out $ toHMXPS2 img

            phony (dir </> "gh2") $ shk $ need $
              [ dir </> "gh2/notes.mid"
              , dir </> "gh2/audio.vgs"
              , dir </> "gh2/songs.dta"
              , dir </> "gh2/songs-inner.dta"
              , dir </> "gh2/lipsync.voc"
              , dir </> "gh2/coop_max_scores.dta"
              , dir </> "gh2/symbol"
              , dir </> "gh2/cover.png_ps2"
              ] <> if gh2_PracticeAudio gh2
                then
                  [ dir </> "gh2/audio_p90.vgs"
                  , dir </> "gh2/audio_p75.vgs"
                  , dir </> "gh2/audio_p60.vgs"
                  ]
                else [dir </> "gh2/audio_empty.vgs"]

            dir </> "stfs/config/contexts.dta" %> \out -> do
              let ctx = fromMaybe defaultID $ gh2_Context gh2
              stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
                [ D.Parens $ D.Tree 0
                  [ D.Sym key
                  , D.Int $ fromIntegral ctx
                  ]
                ]
            dir </> "stfs/config/coop_max_scores.dta" %> \out -> do
              shk $ copyFile' (dir </> "gh2/coop_max_scores.dta") out
            dir </> "stfs/config/leaderboards.dta" %> \out -> do
              let (lbp, lbw) = fromMaybe (defaultLBP, defaultLBW) $ gh2_Leaderboard gh2
              stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
                [ D.Parens $ D.Tree 0
                  [ D.Sym key
                  , D.Parens $ D.Tree 0
                    [ D.Int $ fromIntegral lbp
                    , D.Int $ fromIntegral lbw
                    ]
                  ]
                ]
            dir </> "stfs/config/songs.dta" %> \out -> do
              input <- shakeMIDI $ planDir </> "raw.mid"
              hasAudio <- loadPartAudioCheck
              audio <- computeGH2Audio songYaml gh2 hasAudio
              let songPackage = makeGH2DTA360
                    songYaml
                    key
                    (previewBounds songYaml (input :: RBFile.Song (RBFile.OnyxFile U.Beats)))
                    gh2
                    audio
                    (targetTitle songYaml target)
              stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
                [ D.Parens $ D.Tree 0
                  $ D.Sym key
                  : makeValue (valueId D.stackChunks) songPackage
                ]
            dir </> "stfs/songs" </> pkg </> pkg <.> "mid" %> \out -> do
              shk $ copyFile' (dir </> "gh2/notes.mid") out
            dir </> "audio.ogg" %> \out -> do
              src <- gh2Source
              runAudio src out
            dir </> "stfs/songs" </> pkg </> pkg <.> "mogg" %> \out -> do
              shk $ need [dir </> "audio.ogg"]
              oggToMogg (dir </> "audio.ogg") out
            dir </> "stfs/songs" </> pkg </> pkg <.> "voc" %> \out -> do
              shk $ copyFile' (dir </> "gh2/lipsync.voc") out
            dir </> "gh2live" %> \out -> do
              shk $ need
                [ dir </> "stfs/config/contexts.dta"
                , dir </> "stfs/config/coop_max_scores.dta"
                , dir </> "stfs/config/leaderboards.dta"
                , dir </> "stfs/config/songs.dta"
                , dir </> "stfs/songs" </> pkg </> pkg <.> "mid"
                , dir </> "stfs/songs" </> pkg </> pkg <.> "mogg"
                , dir </> "stfs/songs" </> pkg </> pkg <.> "voc"
                ]
              lg "# Producing GH2 LIVE file"
              mapStackTraceT (mapQueueLog $ liftIO . runResourceT) $ gh2pkg
                (getArtist (_metadata songYaml) <> " - " <> targetTitle songYaml target)
                (T.pack $ "Compiled by Onyx Music Game Toolkit version " <> showVersion version)
                (dir </> "stfs")
                out

          PS ps -> do

            (planName, plan) <- case getPlan (tgt_Plan $ ps_Common ps) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show ps
              Just pair -> return pair
            let planDir = rel $ "gen/plan" </> T.unpack planName
                pathPSEditedParts = dir </> "edited-parts.txt"
                loadEditedParts :: Staction (DifficultyPS, Maybe VocalCount)
                loadEditedParts = shk $ read <$> readFile' pathPSEditedParts

            [dir </> "ps/notes.mid", pathPSEditedParts] &%> \[out, parts] -> do
              input <- shakeMIDI $ planDir </> "raw.mid"
              (_, mixMode) <- computeDrumsPart (ps_Drums ps) plan songYaml
              (output, diffs, vc) <- RB3.processPS
                ps
                songYaml
                (applyTargetMIDI (ps_Common ps) input)
                mixMode
                (applyTargetLength (ps_Common ps) input <$> getAudioLength planName plan)
              saveMIDI out output
              liftIO $ writeFile parts $ show (diffs, vc)

            dir </> "ps/expert+.mid" %> \out -> do
              song <- shakeMIDI $ dir </> "ps/notes.mid"
              saveMIDI out song
                { RBFile.s_tracks = (RBFile.s_tracks song)
                  { RBFile.fixedPartDrums = RBDrums.expertWith2x
                    $ RBFile.fixedPartDrums $ RBFile.s_tracks song
                  }
                }

            dir </> "ps/song.ini" %> \out -> do
              raw <- shakeMIDI $ planDir </> "raw.mid"
              song <- shakeMIDI $ dir </> "ps/notes.mid"
              (DifficultyPS{..}, _) <- loadEditedParts
              let (pstart, _) = previewBounds songYaml (raw :: RBFile.Song (RBFile.OnyxFile U.Beats))
                  len = RBFile.songLengthMS song
                  pd = getPart (ps_Drums ps) songYaml >>= partDrums
                  dmode = fmap drumsMode pd
                  DifficultyRB3{..} = psDifficultyRB3
                  allFives =
                    [ RBFile.fixedPartGuitar     $ RBFile.s_tracks song
                    , RBFile.fixedPartBass       $ RBFile.s_tracks song
                    , RBFile.fixedPartKeys       $ RBFile.s_tracks song
                    , RBFile.fixedPartRhythm     $ RBFile.s_tracks song
                    , RBFile.fixedPartGuitarCoop $ RBFile.s_tracks song
                    ]
              FoF.saveSong out FoF.Song
                { FoF.artist           = _artist $ _metadata songYaml
                , FoF.name             = Just $ targetTitle songYaml target
                , FoF.album            = _album $ _metadata songYaml
                , FoF.charter          = _author $ _metadata songYaml
                , FoF.year             = _year $ _metadata songYaml
                , FoF.genre            = Just $ fofGenre fullGenre
                , FoF.proDrums         = flip fmap dmode $ \case
                  DrumsPro  -> True
                  DrumsReal -> True
                  DrumsFull -> True
                  Drums4    -> False
                  Drums5    -> False
                , FoF.fiveLaneDrums    = Nothing
                -- for consistency we will just use the flipped midi layout,
                -- where 100 is green and 101 is orange
                , FoF.drumFallbackBlue = pd >>= \case
                  PartDrums{ drumsMode = Drums5, drumsFallback = FallbackBlue } -> Just True
                  _                                                             -> Nothing
                , FoF.songLength       = Just len
                , FoF.previewStartTime = Just pstart
                -- difficulty tiers go from 0 to 6, or -1 for no part
                , FoF.diffBand         = Just $ fromIntegral $ rb3BandTier      - 1
                , FoF.diffGuitar       = Just $ fromIntegral $ rb3GuitarTier    - 1
                , FoF.diffGuitarGHL    = Just $ fromIntegral $ chGuitarGHLTier  - 1
                , FoF.diffBass         = Just $ fromIntegral $ rb3BassTier      - 1
                , FoF.diffBassGHL      = Just $ fromIntegral $ chBassGHLTier    - 1
                , FoF.diffDrums        = Just $ fromIntegral $ rb3DrumsTier     - 1
                , FoF.diffDrumsReal    = Just $ case dmode of
                  Just DrumsPro  -> fromIntegral $ rb3DrumsTier - 1
                  Just DrumsReal -> fromIntegral $ rb3DrumsTier - 1
                  _              -> -1
                , FoF.diffKeys         = Just $ fromIntegral $ rb3KeysTier      - 1
                , FoF.diffKeysReal     = Just $ fromIntegral $ rb3ProKeysTier   - 1
                , FoF.diffVocals       = Just $ fromIntegral $ rb3VocalTier     - 1
                , FoF.diffVocalsHarm   = Just $ fromIntegral $ rb3VocalTier     - 1
                , FoF.diffDance        = Just (-1)
                , FoF.diffBassReal     = Just $ fromIntegral $ rb3ProBassTier   - 1
                , FoF.diffGuitarReal   = Just $ fromIntegral $ rb3ProGuitarTier - 1
                -- TODO: are the 22-fret difficulties needed?
                , FoF.diffBassReal22   = Just $ fromIntegral $ rb3ProBassTier   - 1
                , FoF.diffGuitarReal22 = Just $ fromIntegral $ rb3ProGuitarTier - 1
                , FoF.diffGuitarCoop   = Just $ fromIntegral $ psGuitarCoopTier - 1
                , FoF.diffRhythm       = Just $ fromIntegral $ psRhythmTier     - 1
                , FoF.diffDrumsRealPS  = Just (-1)
                , FoF.diffKeysRealPS   = Just (-1)
                , FoF.delay            = Nothing
                , FoF.starPowerNote    = Just 116
                , FoF.eighthNoteHOPO   = Nothing
                , FoF.hopoFrequency    = Nothing
                , FoF.track            = _trackNumber $ _metadata songYaml
                , FoF.sysexSlider      = Just $ or $ do
                  five <- allFives
                  fd <- toList $ fiveDifficulties five
                  return $ not $ RTB.null $ fiveTap fd
                , FoF.sysexOpenBass    = Just $ or $ do
                  five <- allFives
                  fd <- toList $ fiveDifficulties five
                  return $ not $ RTB.null $ fiveOpen fd
                , FoF.loadingPhrase    = ps_LoadingPhrase ps
                , FoF.cassetteColor    = Nothing
                , FoF.tags             = guard (_cover $ _metadata songYaml) >> Just "cover"
                 -- TODO fill these in if we have a video
                , FoF.video            = Nothing
                , FoF.videoStartTime   = Nothing
                , FoF.videoEndTime     = Nothing
                , FoF.videoLoop        = Nothing
                }

            let psParts = map ($ ps) [ps_Drums, ps_Guitar, ps_Bass, ps_Keys, ps_Vocal, ps_Rhythm, ps_GuitarCoop]
                eitherDiff x y = if x == 0 then y else x
                loadPSMidi :: Staction (RBFile.Song (RBFile.OnyxFile U.Beats), DifficultyPS, DifficultyRB3)
                loadPSMidi = do
                  (diffs, _) <- loadEditedParts
                  mid <- shakeMIDI $ planDir </> "raw.mid"
                  return (mid, diffs, psDifficultyRB3 diffs)
            -- TODO make all of these end at [end] for maximum compatibility
            --   (prevents early endings in PS and practice audio glitch in CH)
            -- TODO for mix mode 4 (kick + kit), we should create only
            --   drums_1 (kick) and drums_2 (kit). currently we create
            --   drums_1 (kick) drums_2 (snare, empty) drums_3 (kit)
            dir </> "ps/drums.ogg"   %> \out -> do
              (mid, DifficultyPS{}, DifficultyRB3{..}) <- loadPSMidi
              writeStereoParts psParts (ps_Common ps) mid 0 planName plan [(ps_Drums  ps, rb3DrumsRank)] out
            dir </> "ps/drums_1.ogg" %> \out -> do
              (mid, DifficultyPS{}, DifficultyRB3{..}) <- loadPSMidi
              writeKick  psParts (ps_Common ps) mid 0 False planName plan  (ps_Drums  ps) rb3DrumsRank out
            dir </> "ps/drums_2.ogg" %> \out -> do
              (mid, DifficultyPS{}, DifficultyRB3{..}) <- loadPSMidi
              writeSnare psParts (ps_Common ps) mid 0 False planName plan  (ps_Drums  ps) rb3DrumsRank out
            dir </> "ps/drums_3.ogg" %> \out -> do
              (mid, DifficultyPS{}, DifficultyRB3{..}) <- loadPSMidi
              writeKit   psParts (ps_Common ps) mid 0 False planName plan  (ps_Drums  ps) rb3DrumsRank out
            dir </> "ps/guitar.ogg"  %> \out -> do
              (mid, DifficultyPS{..}, DifficultyRB3{..}) <- loadPSMidi
              writeStereoParts psParts (ps_Common ps) mid 0 planName plan
                [(ps_Guitar ps, eitherDiff rb3GuitarRank chGuitarGHLTier), (ps_GuitarCoop ps, psGuitarCoopTier)]
                out
            dir </> "ps/keys.ogg"    %> \out -> do
              (mid, DifficultyPS{}, DifficultyRB3{..}) <- loadPSMidi
              writeStereoParts psParts (ps_Common ps) mid 0 planName plan
                [(ps_Keys ps, rb3KeysRank)]
                out
            dir </> "ps/rhythm.ogg"  %> \out -> do
              (mid, DifficultyPS{..}, DifficultyRB3{..}) <- loadPSMidi
              writeStereoParts psParts (ps_Common ps) mid 0 planName plan
                [(ps_Bass ps, eitherDiff rb3BassRank chBassGHLTier), (ps_Rhythm ps, psRhythmTier)]
                out
            dir </> "ps/vocals.ogg"  %> \out -> do
              (mid, DifficultyPS{}, DifficultyRB3{..}) <- loadPSMidi
              writeStereoParts psParts (ps_Common ps) mid 0 planName plan
                [(ps_Vocal  ps, rb3VocalRank)]
                out
            dir </> "ps/crowd.ogg"   %> \out -> do
              (mid, DifficultyPS{}, DifficultyRB3{}) <- loadPSMidi
              writeCrowd       (ps_Common ps) mid 0 planName plan out
            dir </> "ps/song.ogg"    %> \out -> do
              (mid, DifficultyPS{..}, DifficultyRB3{..}) <- loadPSMidi
              writeSongCountin (ps_Common ps) mid 0 True planName plan
                [ (ps_Drums      ps, rb3DrumsTier    )
                , (ps_Guitar     ps, eitherDiff rb3GuitarRank chGuitarGHLTier)
                , (ps_GuitarCoop ps, psGuitarCoopTier)
                , (ps_Bass       ps, eitherDiff rb3BassRank chBassGHLTier)
                , (ps_Rhythm     ps, psRhythmTier    )
                , (ps_Keys       ps, rb3KeysTier     )
                , (ps_Vocal      ps, rb3VocalTier    )
                ] out
            useJPEG <- case _fileAlbumArt $ _metadata songYaml of
              Just img | elem (takeExtension img) [".jpg", ".jpeg"] -> do
                dir </> "ps/album.jpg" %> shk . copyFile' img
                return True
              _ -> return False
            dir </> "ps/album.png"   %> shk . copyFile' (rel "gen/cover-full.png")
            bgimg <- forM (_fileBackgroundImage $ _global songYaml) $ \f -> do
              let psImage = "background" <> takeExtension f
              dir </> "ps" </> psImage %> shk . copyFile' (rel f)
              return psImage
            phony (dir </> "ps") $ do
              (_, mixMode) <- computeDrumsPart (ps_Drums ps) plan songYaml
              shk $ need $ map (\f -> dir </> "ps" </> f) $ concat
                -- TODO replace (/= def), should actually check whether the right PS play mode is present
                [ ["song.ini", "notes.mid", "song.ogg", if useJPEG then "album.jpg" else "album.png"]
                , ["expert+.mid"
                  | maybe False ((/= Kicks1x) . drumsKicks)
                  $ getPart (ps_Drums ps) songYaml >>= partDrums
                  ]
                , ["drums.ogg"   | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode == RBDrums.D0 && case plan of
                    Plan{..} -> HM.member (ps_Drums ps) $ getParts _planParts
                    _        -> True
                  ]
                , ["drums_1.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
                , ["drums_2.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
                , ["drums_3.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
                -- TODO also check ps_GuitarCoop
                , ["guitar.ogg"  | maybe False (/= def) (getPart (ps_Guitar ps) songYaml) && case plan of
                    Plan{..} -> HM.member (ps_Guitar ps) $ getParts _planParts
                    _        -> True
                  ]
                , ["keys.ogg"    | maybe False (/= def) (getPart (ps_Keys ps) songYaml) && case plan of
                    Plan{..} -> HM.member (ps_Keys ps) $ getParts _planParts
                    _        -> True
                  ]
                -- TODO also check ps_Rhythm
                , ["rhythm.ogg"  | maybe False (/= def) (getPart (ps_Bass ps) songYaml) && case plan of
                    Plan{..} -> HM.member (ps_Bass ps) $ getParts _planParts
                    _        -> True
                  ]
                , ["vocals.ogg"  | maybe False (/= def) (getPart (ps_Vocal ps) songYaml) && case plan of
                    Plan{..} -> HM.member (ps_Vocal ps) $ getParts _planParts
                    _        -> True
                  ]
                , ["crowd.ogg"   | case plan of
                    Plan{..}     -> isJust _crowd
                    MoggPlan{..} -> not $ null _moggCrowd
                  ]
                , toList bgimg
                ]
            dir </> "ps.zip" %> \out -> do
              let d = dir </> "ps"
              shk $ need [d]
              files <- shk $ map (d </>) <$> getDirectoryContents d
              let folderInZip = T.unpack $ validFileNamePiece NameRulePC
                    $ getArtist (_metadata songYaml) <> " - " <> targetTitle songYaml target
              z <- stackIO $ Zip.addFilesToArchive [Zip.OptLocation folderInZip False] Zip.emptyArchive files
              stackIO $ BL.writeFile out $ Zip.fromArchive z

          Melody tgt -> do

            (planName, _) <- case getPlan (tgt_Plan $ tgt_Common tgt) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show tgt
              Just pair -> return pair
            let planDir = rel $ "gen/plan" </> T.unpack planName
                midraw = planDir </> "raw.mid"

            -- Melody's Escape customs
            let melodyAudio = dir </> "melody/audio.ogg"
                melodyChart = dir </> "melody/song.track"
            -- TODO support audio speed
            melodyAudio %> shk . copyFile' (planDir </> "everything.ogg")
            melodyChart %> \out -> do
              shk $ need [midraw, melodyAudio]
              mid <- shakeMIDI midraw
              melody <- liftIO
                $ Melody.randomNotes
                $ maybe mempty RBFile.onyxMelody
                $ Map.lookup (tgt_Part tgt)
                $ RBFile.onyxParts
                $ RBFile.s_tracks mid
              info <- liftIO $ Snd.getFileInfo melodyAudio
              let secs = realToFrac (Snd.frames info) / realToFrac (Snd.samplerate info) :: U.Seconds
                  str = unlines
                    [ "1.02"
                    , intercalate ";"
                      [ show (Melody.secondsToTicks secs)
                      , show (realToFrac secs :: Centi)
                      , "420"
                      , "4"
                      ]
                    , Melody.writeTransitions (RBFile.s_tempos mid) melody
                    , Melody.writeNotes (RBFile.s_tempos mid) melody
                    ]
              liftIO $ writeFile out str
            phony (dir </> "melody") $ shk $ need [melodyAudio, melodyChart]

          RS rs -> do

            (planName, plan) <- case getPlan (tgt_Plan $ rs_Common rs) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show rs
              Just pair -> return pair
            let planDir = rel $ "gen/plan" </> T.unpack planName

            let isBass = \case
                  RSArrSlot _ RSBass -> True
                  _                  -> False
                presentPlayable = do
                  (arrSlot, fpart) <- rs_Arrangements rs
                  -- TODO warn if arrangement does not have pro guitar mode
                  pg <- maybe [] (toList . partProGuitar) $ getPart fpart songYaml
                  return (fpart, RSPlayable arrSlot pg)
                presentParts = presentPlayable <> do
                  let fpart = rs_Vocal rs
                  pv <- maybe [] (toList . partVocal) $ getPart fpart songYaml
                  return (fpart, RSVocal pv)
                rsPadding = dir </> "padding.txt"
                rsAnchors = dir </> "anchors.mid"
                rsProject = dir </> "cst/project.dlc.xml"
                rsAudio   = dir </> "cst/audio.wav"
                rsPreview = dir </> "cst/audio_preview.wav" -- this has to be named same as audio + "_preview" for CST to load it
                rsArt     = dir </> "cst/cover.png"
                rsArr p arrSlot = let
                  suffix = case arrSlot of
                    RSVocal _         -> "vocals" -- NOTE this is required! CST breaks if the filename does not have "vocals"
                    -- see https://github.com/rscustom/rocksmith-custom-song-toolkit/blob/fa63cc4e0075/RocksmithToolkitLib/XML/Song2014.cs#L347
                    -- similarly, we'll need "showlights" when that is implemented
                    RSPlayable slot _ -> if isBass slot then "b" else "g"
                  in dir </> "cst/" <> T.unpack (RBFile.getPartName p) <> "." <> suffix <> ".arr.xml"

            phony (dir </> "cst") $ shk $ need $
              [rsProject, rsAudio, rsPreview, rsArt] ++ [ rsArr p arrSlot | (p, arrSlot) <- presentParts ]

            rsPadding %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let firstNoteBeats = do
                    (fpart, RSPlayable _ _) <- presentParts
                    let opart = RBFile.getFlexPart fpart $ RBFile.s_tracks mid
                    trk <- [RBFile.onyxPartRSBass opart, RBFile.onyxPartRSGuitar opart]
                    Wait dt _ _ <- [rsNotes trk]
                    return dt
                  targetTime = 10 :: U.Seconds
                  firstNoteTime = case NE.nonEmpty firstNoteBeats of
                    Nothing -> targetTime
                    Just ts -> U.applyTempoMap (RBFile.s_tempos mid) $ minimum ts
              stackIO $ writeFile out $ show $ if firstNoteTime >= targetTime
                then 0
                else realToFrac $ targetTime - firstNoteTime :: Milli

            rsAnchors %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let eachTrack trk = if RTB.null $ rsNotes trk
                    then return trk
                    else do
                      rso <- buildRS (RBFile.s_tempos mid) 0 trk
                      return $ backportAnchors (RBFile.s_tempos mid) trk rso
              newParts <- forM (RBFile.onyxParts $ RBFile.s_tracks mid) $ \opart -> do
                gtr  <- eachTrack $ RBFile.onyxPartRSGuitar opart
                bass <- eachTrack $ RBFile.onyxPartRSBass   opart
                return opart
                  { RBFile.onyxPartRSGuitar = gtr
                  , RBFile.onyxPartRSBass   = bass
                  }
              saveMIDI out $ mid
                { RBFile.s_tracks = (RBFile.s_tracks mid)
                  { RBFile.onyxParts = newParts
                  }
                }

            forM_ presentParts $ \(fpart, arrSlot) -> do
              rsArr fpart arrSlot %> \out -> do
                mid <- shakeMIDI $ planDir </> "processed.mid"
                pad <- shk $ (realToFrac :: Milli -> U.Seconds) . read <$> readFile' rsPadding
                case arrSlot of
                  RSVocal _pv -> do
                    let opart = RBFile.getFlexPart fpart $ RBFile.s_tracks mid
                        trk = if nullVox $ RBFile.onyxPartVocals opart
                          then RBFile.onyxHarm1 opart
                          else RBFile.onyxPartVocals opart
                        vox = buildRSVocals (RBFile.s_tempos mid) trk
                    Arr.writePart out $ Arr.addPadding pad $ Arr.PartVocals vox
                  RSPlayable slot pg -> do
                    mapM_ (shk . need . toList) $ pgTones pg
                    toneKeys <- forM (pgTones pg) $ mapM $ fmap CST.t14_Key . CST.parseTone
                    -- TODO the first beat event needs to be a barline,
                    -- otherwise DDC fails to run!
                    -- also, notes can't go past the last beat event, or they disappear.
                    let ebeats = V.fromList $ numberBars 1 $ ATB.toPairList
                          $ RTB.toAbsoluteEventList 0
                          $ U.applyTempoTrack (RBFile.s_tempos mid)
                          $ beatLines $ RBFile.onyxBeat $ RBFile.s_tracks mid
                        numberBars _       [] = []
                        numberBars measure ((t, Beat) : rest)
                          = Arr.Ebeat t Nothing : numberBars measure rest
                        numberBars measure ((t, Bar) : rest)
                          = Arr.Ebeat t (Just measure) : numberBars (measure + 1) rest
                        tuning0 = case (isBass slot, pgTuningRSBass pg) of
                          (True, Just tun) -> tun
                          _                -> pgTuning pg
                        tuning1 = map (+ gtrGlobal tuning0)
                          $ encodeTuningOffsets tuning0 (if isBass slot then TypeBass else TypeGuitar)
                        tuning2 = tuning1 <> repeat (last tuning1) -- so 5-string bass has a consistent dummy top string
                        octaveDown = head tuning2 < (if isBass slot then -4 else -7)
                        -- NOTE: the cutoff is -4 for bass because for some reason CST fails
                        -- when trying to apply the low bass fix
                        tuning3 = map (+ if octaveDown then 12 else 0) tuning2
                        lengthBeats = RBFile.songLengthBeats mid
                        lengthSeconds = U.applyTempoMap (RBFile.s_tempos mid) lengthBeats
                    rso <- let
                      opart = RBFile.getFlexPart fpart $ RBFile.s_tracks mid
                      trk = if isBass slot
                        then RBFile.onyxPartRSBass   opart
                        else RBFile.onyxPartRSGuitar opart
                        -- TODO maybe support using bass track for a guitar slot
                      in buildRS (RBFile.s_tempos mid) (gtrCapo tuning0) trk
                    let allNotes = Arr.lvl_notes $ rso_level rso
                    time <- stackIO getZonedTime
                    Arr.writePart out $ Arr.addPadding pad $ Arr.PartArrangement Arr.Arrangement
                      { Arr.arr_version                = 7 -- this is what EOF has currently
                      , Arr.arr_title                  = targetTitle songYaml target
                      , Arr.arr_arrangement            = case slot of
                        RSArrSlot _ RSLead        -> "Lead"
                        RSArrSlot _ RSRhythm      -> "Rhythm"
                        RSArrSlot _ RSComboLead   -> "Combo"
                        RSArrSlot _ RSComboRhythm -> "Combo"
                        RSArrSlot _ RSBass        -> "Bass"
                      , Arr.arr_part                   = 1 -- TODO what is this?
                      , Arr.arr_offset                 = 0
                      , Arr.arr_centOffset             = _tuningCents plan + if octaveDown then -1200 else 0
                      , Arr.arr_songLength             = lengthSeconds
                      , Arr.arr_lastConversionDateTime = T.pack $ formatTime defaultTimeLocale
                        "%-m-%d-%y %-H:%M"
                        time
                      , Arr.arr_startBeat              = 0
                      , Arr.arr_averageTempo           = U.makeTempo lengthBeats lengthSeconds
                      , Arr.arr_tuning                 = Arr.Tuning
                        { Arr.tuning_string0 = fromMaybe 0 $ listToMaybe tuning3
                        , Arr.tuning_string1 = fromMaybe 0 $ listToMaybe $ drop 1 tuning3
                        , Arr.tuning_string2 = fromMaybe 0 $ listToMaybe $ drop 2 tuning3
                        , Arr.tuning_string3 = fromMaybe 0 $ listToMaybe $ drop 3 tuning3
                        , Arr.tuning_string4 = fromMaybe 0 $ listToMaybe $ drop 4 tuning3
                        , Arr.tuning_string5 = fromMaybe 0 $ listToMaybe $ drop 5 tuning3
                        }
                      , Arr.arr_capo                   = gtrCapo tuning0
                      , Arr.arr_artistName             = getArtist $ _metadata songYaml
                      , Arr.arr_artistNameSort         = getArtist $ _metadata songYaml -- TODO
                      , Arr.arr_albumName              = getAlbum $ _metadata songYaml
                      , Arr.arr_albumYear              = _year $ _metadata songYaml
                      , Arr.arr_crowdSpeed             = 1
                      , Arr.arr_arrangementProperties  = Arr.ArrangementProperties
                        { Arr.ap_represent         = True -- this is always true in arrangement xmls, but false for bonus (+ vocal/lights) in the project xml?
                        , Arr.ap_bonusArr          = case slot of
                          RSArrSlot RSDefault   _ -> False
                          RSArrSlot RSBonus     _ -> True
                          RSArrSlot RSAlternate _ -> True
                        , Arr.ap_standardTuning    = all (== 0) tuning1
                        , Arr.ap_nonStandardChords = False -- TODO :: Bool
                        , Arr.ap_barreChords       = False -- TODO :: Bool
                        , Arr.ap_powerChords       = False -- TODO :: Bool
                        , Arr.ap_dropDPower        = False -- TODO :: Bool
                        , Arr.ap_openChords        = False -- TODO :: Bool
                        , Arr.ap_fingerPicking     = False -- TODO :: Bool
                        , Arr.ap_pickDirection     = any (isJust . Arr.n_pickDirection) allNotes
                        , Arr.ap_doubleStops       = False -- TODO :: Bool
                        , Arr.ap_palmMutes         = any Arr.n_palmMute allNotes
                        , Arr.ap_harmonics         = any Arr.n_harmonic allNotes
                        , Arr.ap_pinchHarmonics    = any Arr.n_harmonicPinch allNotes
                        , Arr.ap_hopo              = any Arr.n_hopo allNotes
                        , Arr.ap_tremolo           = any Arr.n_tremolo allNotes
                        , Arr.ap_slides            = any (isJust . Arr.n_slideTo) allNotes
                        , Arr.ap_unpitchedSlides   = any (isJust . Arr.n_slideUnpitchTo) allNotes
                        , Arr.ap_bends             = any (\n -> isJust (Arr.n_bend n) || not (V.null $ Arr.n_bendValues n)) allNotes
                        , Arr.ap_tapping           = any Arr.n_tap allNotes
                        , Arr.ap_vibrato           = any (isJust . Arr.n_vibrato) allNotes
                        , Arr.ap_fretHandMutes     = any Arr.n_mute allNotes
                        , Arr.ap_slapPop           = any (\n -> any isJust [Arr.n_slap n, Arr.n_pluck n]) allNotes
                        , Arr.ap_twoFingerPicking  = False -- TODO :: Bool
                        , Arr.ap_fifthsAndOctaves  = False -- TODO :: Bool
                        , Arr.ap_syncopation       = False -- TODO :: Bool
                        , Arr.ap_bassPick          = pgPickedBass pg
                        , Arr.ap_sustain           = any (isJust . Arr.n_sustain) allNotes
                        -- TODO what does Combo select for these?
                        , Arr.ap_pathLead          = case slot of
                          RSArrSlot _ RSLead -> True
                          _                  -> False
                        , Arr.ap_pathRhythm        = case slot of
                          RSArrSlot _ RSRhythm -> True
                          _                    -> False
                        , Arr.ap_pathBass          = case slot of
                          RSArrSlot _ RSBass -> True
                          _                  -> False
                        , Arr.ap_routeMask         = Nothing
                        }
                      , Arr.arr_phrases                = rso_phrases rso
                      , Arr.arr_phraseIterations       = rso_phraseIterations rso
                      , Arr.arr_chordTemplates         = rso_chordTemplates rso
                      , Arr.arr_tonebase               = fmap rsFileToneBase toneKeys
                      , Arr.arr_tonea                  = toneKeys >>= rsFileToneA
                      , Arr.arr_toneb                  = toneKeys >>= rsFileToneB
                      , Arr.arr_tonec                  = toneKeys >>= rsFileToneC
                      , Arr.arr_toned                  = toneKeys >>= rsFileToneD
                      , Arr.arr_tones
                        = V.fromList
                        $ map (\(t, letter) -> let
                          in Arr.Tone
                            { tone_time = t
                            , tone_id   = Just $ fromEnum letter
                            , tone_name = fromMaybe "" $ toneKeys >>= case letter of
                              ToneA -> rsFileToneA
                              ToneB -> rsFileToneB
                              ToneC -> rsFileToneC
                              ToneD -> rsFileToneD
                            }
                          )
                        $ ATB.toPairList
                        $ RTB.toAbsoluteEventList 0
                        $ rso_tones rso
                      , Arr.arr_ebeats                 = ebeats
                      , Arr.arr_sections               = rso_sections rso
                      , Arr.arr_events                 = mempty -- TODO :: V.Vector Event
                      , Arr.arr_transcriptionTrack     = Arr.Level
                        { Arr.lvl_difficulty    = -1
                        , Arr.lvl_notes         = mempty
                        , Arr.lvl_chords        = mempty
                        , Arr.lvl_fretHandMutes = mempty
                        , Arr.lvl_anchors       = mempty
                        , Arr.lvl_handShapes    = mempty
                        }
                      , Arr.arr_levels                 = V.singleton $ rso_level rso
                      }

            rsAudio %> \out -> do
              pad <- shk $ (realToFrac :: Milli -> Seconds) . read <$> readFile' rsPadding
              let wav = planDir </> "everything.wav"
              case pad of
                0 -> shk $ copyFile' wav out
                _ -> buildAudio (Pad Start (Seconds pad) $ Input wav) out
            rsPreview %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let (pstart, pend) = previewBounds songYaml (mid :: RBFile.Song (RBFile.OnyxFile U.Beats))
                  fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
                  previewExpr
                    = Fade End (Seconds 5)
                    $ Fade Start (Seconds 2)
                    $ Take Start (fromMS $ pend - pstart)
                    $ Drop Start (fromMS pstart)
                    $ Input (planDir </> "everything.wav")
              buildAudio previewExpr out
            rsArt %> shk . copyFile' (rel "gen/cover-full.png")
            rsProject %> \out -> do
              let allTonePaths = nubOrd $ do
                    (_, RSPlayable _ pg) <- presentParts
                    tones <- toList $ pgTones pg
                    toList tones
              shk $ need $ [ rsArr fpart arrSlot | (fpart, arrSlot) <- presentParts ] ++ allTonePaths
              allTones <- forM allTonePaths $ \f -> do
                tone <- CST.parseTone f
                return (f, tone)
              parsedArrFiles <- forM presentParts $ \(fpart, arrSlot) -> do
                contents <- Arr.parseFile $ rsArr fpart arrSlot
                return (fpart, arrSlot, contents)
              let averageTempo = listToMaybe $ do
                    (_, _, Arr.PartArrangement arr) <- parsedArrFiles
                    return $ Arr.arr_averageTempo arr
              arrangements <- forM (zip [0..] parsedArrFiles) $ \(i, (fpart, arrSlot, contents)) -> do
                -- TODO ArrangementSort doesn't appear to work for multiples of the same arrangement label.
                -- Instead it sorts by the arrangement ID UUID!
                -- So, we could just use the first character or so to encode `i`.
                arrID <- stackIO UUID.nextRandom
                songFileID <- stackIO UUID.nextRandom
                songXmlID <- stackIO UUID.nextRandom
                arrMasterID <- stackIO $ randomRIO (0, maxBound :: Int32) -- this matches the range CST uses (C# Random.Next method)
                return $ case contents of
                  Arr.PartVocals _ -> CST.Arrangement
                    { CST.arr_ArrangementName      = "Vocals"
                    , CST.arr_ArrangementPropeties = Nothing
                    , CST.arr_ArrangementSort      = i
                    , CST.arr_ArrangementType      = "Vocal"
                    , CST.arr_BonusArr             = False
                    , CST.arr_CapoFret             = 0
                    , CST.arr_GlyphsXmlPath        = Nothing
                    , CST.arr_Id                   = UUID.toText arrID
                    , CST.arr_LyricsArtPath        = Nothing
                    , CST.arr_MasterId             = arrMasterID
                    , CST.arr_Metronome            = "None"
                    , CST.arr_PluckedType          = "NotPicked"
                    , CST.arr_Represent            = False
                    , CST.arr_RouteMask            = "None"
                    , CST.arr_ScrollSpeed          = 13
                    , CST.arr_Sng2014              = Nothing
                    , CST.arr_SongFile             = CST.AggregateGraph
                      { CST.ag_UUID    = UUID.toText songFileID
                      , CST.ag_File    = ""
                      , CST.ag_Version = Nothing
                      }
                    , CST.arr_SongXml              = CST.AggregateGraph
                      { CST.ag_UUID    = UUID.toText songXmlID
                      , CST.ag_File    = T.pack $ takeFileName $ rsArr fpart arrSlot
                      , CST.ag_Version = Nothing
                      }
                    , CST.arr_ToneA                = ""
                    , CST.arr_ToneB                = ""
                    , CST.arr_ToneBase             = "" -- is this fine?
                    , CST.arr_ToneC                = ""
                    , CST.arr_ToneD                = ""
                    , CST.arr_ToneMultiplayer      = Nothing
                    , CST.arr_Tuning               = "E Standard"
                    , CST.arr_TuningPitch          = 440
                    , CST.arr_TuningStrings        = CST.TuningStrings
                      { CST.ts_String0 = 0
                      , CST.ts_String1 = 0
                      , CST.ts_String2 = 0
                      , CST.ts_String3 = 0
                      , CST.ts_String4 = 0
                      , CST.ts_String5 = 0
                      }
                    }
                  Arr.PartArrangement arr -> CST.Arrangement
                    { CST.arr_ArrangementName      = Arr.arr_arrangement arr
                    , CST.arr_ArrangementPropeties = Just CST.ArrangementPropeties
                      { CST.ap_BarreChords       = Arr.ap_barreChords       $ Arr.arr_arrangementProperties arr
                      , CST.ap_BassPick          = Arr.ap_bassPick          $ Arr.arr_arrangementProperties arr
                      , CST.ap_Bends             = Arr.ap_bends             $ Arr.arr_arrangementProperties arr
                      , CST.ap_DoubleStops       = Arr.ap_doubleStops       $ Arr.arr_arrangementProperties arr
                      , CST.ap_DropDPower        = Arr.ap_dropDPower        $ Arr.arr_arrangementProperties arr
                      , CST.ap_FifthsAndOctaves  = Arr.ap_fifthsAndOctaves  $ Arr.arr_arrangementProperties arr
                      , CST.ap_FingerPicking     = Arr.ap_fingerPicking     $ Arr.arr_arrangementProperties arr
                      , CST.ap_FretHandMutes     = Arr.ap_fretHandMutes     $ Arr.arr_arrangementProperties arr
                      , CST.ap_Harmonics         = Arr.ap_harmonics         $ Arr.arr_arrangementProperties arr
                      , CST.ap_Hopo              = Arr.ap_hopo              $ Arr.arr_arrangementProperties arr
                      , CST.ap_NonStandardChords = Arr.ap_nonStandardChords $ Arr.arr_arrangementProperties arr
                      , CST.ap_OpenChords        = Arr.ap_openChords        $ Arr.arr_arrangementProperties arr
                      , CST.ap_PalmMutes         = Arr.ap_palmMutes         $ Arr.arr_arrangementProperties arr
                      , CST.ap_PickDirection     = Arr.ap_pickDirection     $ Arr.arr_arrangementProperties arr
                      , CST.ap_PinchHarmonics    = Arr.ap_pinchHarmonics    $ Arr.arr_arrangementProperties arr
                      , CST.ap_PowerChords       = Arr.ap_powerChords       $ Arr.arr_arrangementProperties arr
                      , CST.ap_Represent         = case arrSlot of
                        RSPlayable (RSArrSlot RSDefault _) _ -> True
                        _                                    -> False -- vocals, bonus/alt arrangements
                      , CST.ap_SlapPop           = Arr.ap_slapPop           $ Arr.arr_arrangementProperties arr
                      , CST.ap_Slides            = Arr.ap_slides            $ Arr.arr_arrangementProperties arr
                      , CST.ap_StandardTuning    = Arr.ap_standardTuning    $ Arr.arr_arrangementProperties arr
                      , CST.ap_Sustain           = Arr.ap_sustain           $ Arr.arr_arrangementProperties arr
                      , CST.ap_Syncopation       = Arr.ap_syncopation       $ Arr.arr_arrangementProperties arr
                      , CST.ap_Tapping           = Arr.ap_tapping           $ Arr.arr_arrangementProperties arr
                      , CST.ap_Tremolo           = Arr.ap_tremolo           $ Arr.arr_arrangementProperties arr
                      , CST.ap_TwoFingerPicking  = Arr.ap_twoFingerPicking  $ Arr.arr_arrangementProperties arr
                      , CST.ap_UnpitchedSlides   = Arr.ap_unpitchedSlides   $ Arr.arr_arrangementProperties arr
                      , CST.ap_Vibrato           = Arr.ap_vibrato           $ Arr.arr_arrangementProperties arr
                      , CST.ap_BonusArr          = Arr.ap_bonusArr          $ Arr.arr_arrangementProperties arr
                      , CST.ap_PathBass          = Arr.ap_pathBass          $ Arr.arr_arrangementProperties arr
                      , CST.ap_PathLead          = Arr.ap_pathLead          $ Arr.arr_arrangementProperties arr
                      , CST.ap_PathRhythm        = Arr.ap_pathRhythm        $ Arr.arr_arrangementProperties arr
                      , CST.ap_Metronome         = False
                      , CST.ap_RouteMask         = case arrSlot of
                        RSPlayable (RSArrSlot _ RSLead       ) _ -> 1
                        RSPlayable (RSArrSlot _ RSRhythm     ) _ -> 2
                        RSPlayable (RSArrSlot _ RSComboLead  ) _ -> 1
                        RSPlayable (RSArrSlot _ RSComboRhythm) _ -> 2
                        RSPlayable (RSArrSlot _ RSBass       ) _ -> 4
                        RSVocal    _                             -> 0 -- shouldn't happen (vocal/showlight have a nil ArrangementPropeties)
                      }
                    , CST.arr_ArrangementSort      = i
                    , CST.arr_ArrangementType      = case arrSlot of
                      RSPlayable (RSArrSlot _ RSBass) _ -> "Bass"
                      RSPlayable (RSArrSlot _ _     ) _ -> "Guitar"
                      RSVocal                         _ -> "Vocal"
                      -- last one is "ShowLight"
                    , CST.arr_BonusArr             = case arrSlot of
                      RSPlayable (RSArrSlot RSBonus     _) _ -> True
                      RSPlayable (RSArrSlot RSAlternate _) _ -> False -- alt: both represent and bonus set to false
                      _                                      -> False
                    , CST.arr_CapoFret             = Arr.arr_capo arr
                    , CST.arr_GlyphsXmlPath        = Nothing
                    , CST.arr_Id                   = UUID.toText arrID
                    , CST.arr_LyricsArtPath        = Nothing
                    , CST.arr_MasterId             = arrMasterID
                    , CST.arr_Metronome            = "None"
                    , CST.arr_PluckedType          = if Arr.ap_bassPick $ Arr.arr_arrangementProperties arr
                      then "Picked"
                      else "NotPicked"
                    , CST.arr_Represent            = case arrSlot of
                      RSPlayable (RSArrSlot RSDefault _) _ -> True
                      _                                    -> False -- vocals, bonus/alt arrangements
                    , CST.arr_RouteMask            = case arrSlot of
                      RSPlayable (RSArrSlot _ RSLead       ) _ -> "Lead"
                      RSPlayable (RSArrSlot _ RSRhythm     ) _ -> "Rhythm"
                      RSPlayable (RSArrSlot _ RSBass       ) _ -> "Bass"
                      RSPlayable (RSArrSlot _ RSComboLead  ) _ -> "Lead"
                      RSPlayable (RSArrSlot _ RSComboRhythm) _ -> "Rhythm"
                      RSVocal _                                -> "None"
                      -- showlights are also "None"
                    , CST.arr_ScrollSpeed          = 13 -- default?
                    , CST.arr_Sng2014              = Nothing
                    , CST.arr_SongFile             = CST.AggregateGraph
                      { CST.ag_UUID    = UUID.toText songFileID
                      , CST.ag_File    = ""
                      , CST.ag_Version = Nothing
                      }
                    , CST.arr_SongXml              = CST.AggregateGraph
                      { CST.ag_UUID    = UUID.toText songXmlID
                      , CST.ag_File    = T.pack $ takeFileName $ rsArr fpart arrSlot
                      , CST.ag_Version = Nothing
                      }
                    , CST.arr_ToneA                = fromMaybe "" $ Arr.arr_tonea arr
                    , CST.arr_ToneB                = fromMaybe "" $ Arr.arr_toneb arr
                    , CST.arr_ToneBase             = fromMaybe "" $ Arr.arr_tonebase arr
                    , CST.arr_ToneC                = fromMaybe "" $ Arr.arr_tonec arr
                    , CST.arr_ToneD                = fromMaybe "" $ Arr.arr_toned arr
                    , CST.arr_ToneMultiplayer      = Nothing
                    , CST.arr_Tuning               = let
                      rs2014Tunings =
                        [ ([0, 0, 0, 0, 0, 0], "E Standard")
                        , ([-2, 0, 0, 0, 0, 0], "Drop D")
                        , ([-2, 0, 0, -1, -2, -2], "Open D")
                        , ([0, 0, 2, 2, 2, 0], "Open A")
                        , ([-2, -2, 0, 0, 0, -2], "Open G")
                        , ([0, 2, 2, 1, 0, 0], "Open E")
                        , ([-1, -1, -1, -1, -1, -1], "Eb Standard")
                        , ([-3, -1, -1, -1, -1, -1], "Eb Drop Db")
                        , ([-2, -2, -2, -2, -2, -2], "D Standard")
                        , ([-4, -2, -2, -2, -2, -2], "D Drop C")
                        , ([-3, -3, -3, -3, -3, -3], "C# Standard")
                        , ([-4, -4, -4, -4, -4, -4], "C Standard")
                        ]
                      thisTuning =
                        [ Arr.tuning_string0 $ Arr.arr_tuning arr
                        , Arr.tuning_string1 $ Arr.arr_tuning arr
                        , Arr.tuning_string2 $ Arr.arr_tuning arr
                        , Arr.tuning_string3 $ Arr.arr_tuning arr
                        , Arr.tuning_string4 $ Arr.arr_tuning arr
                        , Arr.tuning_string5 $ Arr.arr_tuning arr
                        ]
                      in fromMaybe "Custom Tuning" $ lookup thisTuning rs2014Tunings
                    , CST.arr_TuningPitch          = let
                      -- we use round instead of realToFrac to make sure that e.g. -1200 cents becomes 220 Hz and not 219.999
                      hertzDouble = 440 * (2 ** (1 / 12)) ** (fromIntegral (Arr.arr_centOffset arr) / 100) :: Double
                      in MkFixed $ round $ 1000 * hertzDouble :: Milli
                    , CST.arr_TuningStrings        = CST.TuningStrings
                      { CST.ts_String0 = Arr.tuning_string0 $ Arr.arr_tuning arr
                      , CST.ts_String1 = Arr.tuning_string1 $ Arr.arr_tuning arr
                      , CST.ts_String2 = Arr.tuning_string2 $ Arr.arr_tuning arr
                      , CST.ts_String3 = Arr.tuning_string3 $ Arr.arr_tuning arr
                      , CST.ts_String4 = Arr.tuning_string4 $ Arr.arr_tuning arr
                      , CST.ts_String5 = Arr.tuning_string5 $ Arr.arr_tuning arr
                      }
                    }
              key <- case rs_SongKey rs of
                Nothing -> stackIO $ T.pack . ("OnyxCST" <>) . show <$> randomRIO (0, maxBound :: Int32)
                -- TODO maybe autogenerate a CST-like key, e.g. OnyASAMACrimsonRoseandaGinToni
                Just k  -> if T.length k <= 30
                  then return k
                  else do
                    warn $ "RS song key of " <> show k <> " truncated, longer than max length of 30 characters"
                    return $ T.take 30 k
                {-
                  Info from CST source on song key (+ tone key):

                  Limited to a maximum length of 30 charactures, minimum of 6 charactures for uniqueness
                  Only Ascii Alpha and Numeric may be used
                  No spaces, no special characters, no puncuation
                  All alpha lower, upper, or mixed case are allowed
                  All numeric is allowed
                -}
              shk $ need [rsAudio]
              volume <- stackIO $ audioIntegratedVolume rsAudio
              when (isNothing volume) $ warn "Unable to calculate integrated volume of audio file"
              CST.writeProject out CST.DLCPackageData
                { CST.dlc_AlbumArtPath      = "cover.png"
                , CST.dlc_AppId             = 248750 -- Cherub Rock ID
                , CST.dlc_Arrangements      = V.fromList arrangements
                , CST.dlc_ArtFiles          = Nothing
                , CST.dlc_DefaultShowlights = False
                , CST.dlc_GameVersion       = "RS2014"
                , CST.dlc_Inlay             = Nothing
                , CST.dlc_Mac               = False
                , CST.dlc_Name              = key
                , CST.dlc_OggPath           = "audio.wav"
                , CST.dlc_OggPreviewPath    = "audio_preview.wav"
                , CST.dlc_OggQuality        = 5
                , CST.dlc_PS3               = False
                , CST.dlc_Pc                = True
                , CST.dlc_PreviewVolume     = -5 -- default?
                , CST.dlc_SignatureType     = "CON"
                , CST.dlc_SongInfo          = let
                  -- not sure why brackets aren't allowed, CST removes them on compile
                  textReplace = T.replace "[" "(" . T.replace "]" ")"
                  in CST.SongInfo
                    { CST.si_Album               = textReplace $ getAlbum $ _metadata songYaml
                    , CST.si_AlbumSort           = textReplace $ getAlbum $ _metadata songYaml -- TODO
                    , CST.si_Artist              = textReplace $ getArtist $ _metadata songYaml
                    , CST.si_ArtistSort          = textReplace $ getArtist $ _metadata songYaml -- TODO
                    , CST.si_AverageTempo        = maybe 120 {- shouldn't happen -} ((round :: U.BPS -> Int) . (* 60)) averageTempo
                    , CST.si_JapaneseArtistName  = case _artistJP $ _metadata songYaml of
                      Nothing -> ""
                      Just s  -> textReplace s
                    , CST.si_JapaneseSongName    = maybe "" textReplace $ targetTitleJP songYaml target
                    , CST.si_SongDisplayName     = textReplace $ targetTitle songYaml target
                    , CST.si_SongDisplayNameSort = textReplace $ targetTitle songYaml target
                    , CST.si_SongYear            = fromMaybe 1960 $ _year $ _metadata songYaml -- TODO see if this can be empty
                    }
                    {-
                      Info from CST source on sortable text (might not need to do this though, CST appears to edit them itself):

                      ( ) are always stripped
                      / is replaced with a space
                      - usage is inconsistent (so for consistency remove it)
                      , is stripped (in titles)
                      ' is not stripped
                      . and ? usage are inconsistent (so for consistency leave these)
                      Abbreviations/symbols like 'Mr.' and '&' are replaced with words
                      Diacritics are replaced with their ASCII approximations if available
                    -}
                , CST.dlc_Tones             = mempty
                , CST.dlc_TonesRS2014       = V.fromList $ map snd allTones
                , CST.dlc_ToolkitInfo       = CST.ToolkitInfo
                  { CST.tk_PackageAuthor  = Nothing -- TODO can this be filled in to override the author entered in the toolkit?
                  , CST.tk_PackageComment = Just $ "(Project generated by Onyx v" <> T.pack (showVersion version) <> ")"
                  , CST.tk_PackageRating  = Nothing
                  , CST.tk_PackageVersion = Just $ fromMaybe "1.0" $ rs_Version rs
                  , CST.tk_ToolkitVersion = Nothing
                  }
                , CST.dlc_Version           = "" -- TODO this should be e.g. "Toolkit Version 2.9.2.1-5a8cb74e"
                , CST.dlc_Volume            = case volume of
                  Nothing -> -7 -- default in CST
                  Just v  -> realToFrac $ (-16) - v -- -16 is the target ODLC uses
                , CST.dlc_XBox360           = False
                , CST.dlc_XBox360Licenses   = mempty
                }

          GH5 gh5 -> do

            (planName, plan) <- case getPlan (tgt_Plan $ gh5_Common gh5) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh5
              Just pair -> return pair
            let planDir = rel $ "gen/plan" </> T.unpack planName

            let hashed = hashGH5 songYaml gh5
                songID = fromMaybe hashed $ gh5_SongID gh5
                cdl = "cdl" <> show (fromMaybe hashed $ gh5_CDL gh5)
                songKey = "dlc" <> show songID
                songKeyQB = qbKeyCRC $ B8.pack songKey
                -- Limiting to one-byte chars because I don't know the right way to hash chars beyond U+00FF
                packageInfo = T.map (\c -> if fromEnum c <= 0xFF then c else '_')
                  $ targetTitle songYaml target <> " (" <> getArtist (_metadata songYaml) <> ")"
                -- We put the cdl in as well, otherwise 2 titles that share the first 42 chars can conflict
                -- (for example, a long-title song converted to 2 different speeds)
                packageTitle = T.pack cdl <> " " <> packageInfo
                packageTitles = [packageTitle, "", packageTitle, packageTitle, packageTitle, packageTitle]
                packageDescs = let s = "Custom song created by Onyx Music Game Toolkit" in [s, "", s, s, s, s]
                -- "Emo Edge Track Pack" becomes "emo_edge_track_pack"
                -- "\"Addicted\"" becomes "_addicted_"
                -- "GH: Warriors of Rock 1 Track Pack" becomes "gh__warriors_of_rock_1_track_pack"
                (titleHashHex, titleHash) = packageNameHash packageTitle

                -- more IDs that might need to be unique
                manifestQBFilenameKey
                  : textQBFilenameKey
                  : textQS1FilenameKey
                  : textQS2FilenameKey
                  : textQS3FilenameKey
                  : textQS4FilenameKey
                  : textQS5FilenameKey
                  : songQBFilenameKey
                  : songQSFilenameKey
                  : songNoteFilenameKey
                  : songQB2FilenameKey
                  : songPerfFilenameKey
                  : _
                  = [songKeyQB + 1 ..]

            dir </> "cmanifest.pak.xen" %> \out -> stackIO $ BL.writeFile out
              $ worFileManifest titleHashHex (T.pack cdl) manifestQBFilenameKey [fromIntegral songID]

            dir </> "cdl.pak.xen" %> \out -> stackIO $ BL.writeFile out worFileBarePak

            dir </> "cdl_text.pak.xen" %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let _ = mid :: RBFile.Song (RBFile.OnyxFile U.Beats)
                  makeQSPair s = let s' = worMetadataString s in (qsKey s', s')
                  -- not sure what the \L does; it works without it but we'll just match official songs
                  titleQS  = makeQSPair $ "\\L" <> targetTitle songYaml target
                  artistQS = makeQSPair $ "\\L" <> getArtist (_metadata songYaml)
                  albumQS  = makeQSPair $ getAlbum  $ _metadata songYaml
                  qs = makeQS [titleQS, artistQS, albumQS]
                  difficulties = difficultyGH5 gh5 songYaml
                  genre = worGenre $ interpretGenre
                    (_genre    $ _metadata songYaml)
                    (_subgenre $ _metadata songYaml)
                  qb =
                    [ QBSectionArray 3796209450 textQBFilenameKey $
                      QBArrayOfQbKey [songKeyQB]
                    , QBSectionStruct 4087958085 textQBFilenameKey
                      [ QBStructHeader
                      , QBStructItemStruct songKeyQB
                        [ QBStructHeader
                        , QBStructItemQbKey (qbKeyCRC "checksum") songKeyQB
                        , QBStructItemString (qbKeyCRC "name") $ B8.pack songKey
                        , QBStructItemQbKeyStringQs (qbKeyCRC "title") $ fst titleQS
                        , QBStructItemQbKeyStringQs (qbKeyCRC "artist") $ fst artistQS
                        , QBStructItemQbKeyString 2026561191 2714706322 -- dunno
                        , QBStructItemInteger 2916764328 1 -- dunno
                        , QBStructItemInteger (qbKeyCRC "year") $ fromIntegral $ getYear $ _metadata songYaml
                        , QBStructItemQbKeyStringQs (qbKeyCRC "album_title") $ fst albumQS
                        , QBStructItemQbKey 1732896360 3045815699 -- dunno
                        , QBStructItemQbKey (qbKeyCRC "genre") $ qbWoRGenre genre
                        , QBStructItemInteger (qbKeyCRC "leaderboard") 0 -- does setting this to 0 work?
                        , QBStructItemInteger (qbKeyCRC "duration")
                          (fromIntegral $ quot (RBFile.songLengthMS mid + 500) 1000) -- this is just displayed in song list
                        , QBStructItemInteger (qbKeyCRC "flags") 0 -- what is this?
                        , QBStructItemInteger (qbKeyCRC "double_kick") $
                          case getPart (gh5_Drums gh5) songYaml >>= partDrums of
                            Nothing -> 0
                            Just pd -> case drumsKicks pd of
                              Kicks1x   -> 0
                              Kicks2x   -> 1
                              KicksBoth -> 1
                        -- meaning of these seems clear but not sure what criteria you'd use to set them
                        , QBStructItemInteger (qbKeyCRC "thin_fretbar_8note_params_low_bpm") 1
                        , QBStructItemInteger (qbKeyCRC "thin_fretbar_8note_params_high_bpm") 150
                        , QBStructItemInteger (qbKeyCRC "thin_fretbar_16note_params_low_bpm") 1
                        , QBStructItemInteger (qbKeyCRC "thin_fretbar_16note_params_high_bpm") 120
                        , QBStructItemInteger 437674840 $ fromIntegral $ gh5GuitarTier difficulties
                        , QBStructItemInteger 3733500155 $ fromIntegral $ gh5BassTier difficulties
                        , QBStructItemInteger 945984381 $ fromIntegral $ gh5VocalsTier difficulties
                        , QBStructItemInteger 178662704 $ fromIntegral $ gh5DrumsTier difficulties
                        , QBStructItemInteger 3512970546 10 -- what is this?
                        -- maybe we could figure out the options for these and match them to the RB kit options?
                        , QBStructItemString (qbKeyCRC "snare") "ModernRock"
                        , QBStructItemString (qbKeyCRC "kick") "ModernRock"
                        , QBStructItemString (qbKeyCRC "tom1") "ModernRock"
                        , QBStructItemString (qbKeyCRC "tom2") "ModernRock"
                        , QBStructItemString (qbKeyCRC "hihat") "ModernRock"
                        , QBStructItemString (qbKeyCRC "cymbal") "ModernRock"
                        , QBStructItemString (qbKeyCRC "drum_kit") "ModernRock"
                        , QBStructItemString 4094319878 "Sticks_Normal" -- think this is for the countin hits
                        , QBStructItemFloat 1179677752 0 -- dunno
                        -- - QBStructItemStruct:
                        --   - vocals_pitch_score_shift
                        --   - - QBStructHeader
                        --     - QBStructItemInteger:
                        --       - cents
                        --       - 30
                        ]
                      ]
                    ]
              stackIO $ BL.writeFile out $ worFileTextPak
                (textQBFilenameKey, putQB qb)
                (textQS1FilenameKey, textQS2FilenameKey, textQS3FilenameKey, textQS4FilenameKey, textQS5FilenameKey, qs)

            dir </> "song.pak.xen" %> \out -> do
              shk $ need [dir </> "ghwor.note"]
              note <- stackIO $ BL.readFile $ dir </> "ghwor.note"
              qsSections <- stackIO $ BL.readFile $ dir </> "ghwor.qs"
              qsIDs <- case parseQS qsSections of
                Just pairs -> return $ map fst pairs
                Nothing    -> fatal "Couldn't reparse practice sections .qs file"
              perf <- stackIO $ ghWoRSamplePerf >>= BL.readFile
              let perf' = BL.take 4 perf <> runPut (putWord32be songKeyQB) <> BL.drop 8 perf
                  qb =
                    [ QBSectionArray 1441618440 songQBFilenameKey $ QBArrayOfInteger []
                    , QBSectionArray 2961626425 songQBFilenameKey $ QBArrayOfFloatRaw []
                    , QBSectionArray 3180084209 songQBFilenameKey $ QBArrayOfInteger []
                    , QBSectionArray 3250951858 songQBFilenameKey $ QBArrayOfFloatRaw []
                    , QBSectionArray 2096871117 songQBFilenameKey $ QBArrayOfInteger []
                    , QBSectionArray 1487281764 songQBFilenameKey $ QBArrayOfStruct
                      [ [ QBStructHeader
                        , QBStructItemInteger (qbKeyCRC "time") 0
                        , QBStructItemQbKey (qbKeyCRC "scr") 1861295691
                        , QBStructItemStruct (qbKeyCRC "params")
                          [ QBStructHeader
                          , QBStructItemInteger (qbKeyCRC "time") 3
                          ]
                        ]
                      ]
                    , QBSectionArray 926843683 songQBFilenameKey $ QBArrayOfStruct []
                    , QBSectionArray 183728976 songQBFilenameKey $ QBArrayOfQbKeyStringQs qsIDs
                    ]
                  nodes =
                    [ ( Node {nodeFileType = qbKeyCRC ".qb", nodeOffset = 0, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQBFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , putQB qb
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".qs.en", nodeOffset = 1, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , qsSections
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".qs.fr", nodeOffset = 2, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , qsSections
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".qs.it", nodeOffset = 3, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , qsSections
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".qs.de", nodeOffset = 4, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , qsSections
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".qs.es", nodeOffset = 5, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , qsSections
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".note", nodeOffset = 6, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songNoteFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , note
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".qb", nodeOffset = 7, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = 4208822249, nodeFilenameCRC = 662273024, nodeUnknown = 0, nodeFlags = 0}
                      -- nodeFilenameKey and nodeFilenameCRC here are same across songs
                      , putQB [QBSectionInteger 2519306321 4208822249 5377]
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".qb", nodeOffset = 8, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQB2FilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , putQB [QBSectionArray 1148198227 3748754942 $ QBArrayOfStruct []]
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".perf", nodeOffset = 9, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songPerfFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0}
                      , perf'
                      )
                    , ( Node {nodeFileType = qbKeyCRC ".last", nodeOffset = 10, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = 2306521930, nodeFilenameCRC = 1794739921, nodeUnknown = 0, nodeFlags = 0}
                      , BL.replicate 4 0xAB
                      )
                    ]
              stackIO $ BL.writeFile out $ buildPak nodes

            [dir </> "ghwor.note", dir </> "ghwor.qs"] &%> \[outNote, outQS] -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              (note, qs) <- makeGHWoRNote songYaml gh5
                (applyTargetMIDI (gh5_Common gh5) mid)
                $ getAudioLength planName plan
              stackIO $ BL.writeFile outNote $ runPut $
                putNote songKeyQB $ makeWoRNoteFile note
              stackIO $ BL.writeFile outQS $ makeQS $ HM.toList qs

            -- Not supporting stems yet due to FSB generator issue;
            -- it will fail with memory errors on large WAVs, so we have to keep them small.
            -- However they do have to be the full length of the song!
            -- Otherwise pausing doesn't pause the audio once you pass the end of any of the FSBs.
            dir </> "audio1.wav" %> \out -> do
              len <- getAudioLength planName plan
              runAudio (applySpeedAudio (gh5_Common gh5) $ silent (Seconds $ realToFrac len) 1000 8) out
            dir </> "audio2.wav" %> \out -> do
              len <- getAudioLength planName plan
              runAudio (applySpeedAudio (gh5_Common gh5) $ silent (Seconds $ realToFrac len) 1000 6) out
            dir </> "audio3.wav" %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let _ = mid :: RBFile.Song (RBFile.OnyxFile U.Beats)
              src <- shk $ buildSource $ Merge
                $ Input (planDir </> "everything.wav")
                :| [Silence 2 $ Seconds 0]
              runAudio (applyTargetAudio (gh5_Common gh5) mid src) out

            dir </> "preview.wav" %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let (pstart, pend) = previewBounds songYaml (mid :: RBFile.Song (RBFile.OnyxFile U.Beats))
                  fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
              src <- shk $ buildSource
                $ Gain 0.5 -- just guessing at this. without it previews are too loud
                $ Fade End (Seconds 5)
                $ Fade Start (Seconds 2)
                $ Take Start (fromMS $ pend - pstart)
                $ Drop Start (fromMS pstart)
                $ Input (planDir </> "everything.wav")
              runAudio (applySpeedAudio (gh5_Common gh5) src) out

            forM_ ["audio1", "audio2", "audio3", "preview"] $ \audio -> do
              let wav = dir </> audio <.> "wav"
                  fsb = dir </> audio <.> "fsb"
              fsb %> \out -> do
                shk $ need [wav]
                xma <- mapStackTraceT (mapQueueLog $ liftIO . runResourceT) $ makeXMAPieces $ Right wav
                ghBandFSB xma >>= stackIO . BL.writeFile out . emitFSB
              fsb <.> "xen" %> \out -> do
                shk $ need [fsb]
                bs <- stackIO $ B.readFile fsb
                case ghworEncrypt bs of
                  Nothing  -> fatal "Unable to encrypt .fsb to .fsb.xen"
                  Just enc -> stackIO $ B.writeFile out enc

            dir </> "ghworlive" %> \out -> do
              let files =
                    [ ("cmanifest_" <> titleHash <> ".pak.xen", dir </> "cmanifest.pak.xen")
                    , (cdl <> ".pak.xen", dir </> "cdl.pak.xen")
                    , (cdl <> "_text.pak.xen", dir </> "cdl_text.pak.xen")
                    , ("b" <> songKey <> "_song.pak.xen", dir </> "song.pak.xen")
                    , ("a" <> songKey <> "_preview.fsb.xen", dir </> "preview.fsb.xen")
                    , ("a" <> songKey <> "_1.fsb.xen", dir </> "audio1.fsb.xen")
                    , ("a" <> songKey <> "_2.fsb.xen", dir </> "audio2.fsb.xen")
                    , ("a" <> songKey <> "_3.fsb.xen", dir </> "audio3.fsb.xen")
                    ]
                  folder = Folder
                    { folderSubfolders = []
                    , folderFiles = map (\(dest, src) -> (T.pack dest, fileReadable src)) files
                    }
              shk $ need $ map snd files
              thumb <- stackIO $ ghWoRthumbnail >>= B.readFile
              stackIO $ makeCONReadable CreateOptions
                { createNames = packageTitles
                , createDescriptions = packageDescs
                , createTitleID = 0x41560883
                , createTitleName = "Guitar Hero : Warriors of Rock"
                , createThumb = thumb
                , createTitleThumb = thumb
                , createLicenses = [LicenseEntry (-1) 1 1, LicenseEntry (-1) 1 0]
                , createMediaID       = 0
                , createVersion       = 0
                , createBaseVersion   = 0
                , createTransferFlags = 0xC0
                , createLIVE = True
                } folder out

          GH3 gh3 -> do

            (planName, plan) <- case getPlan (tgt_Plan $ gh3_Common gh3) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh3
              Just pair -> return pair
            let planDir = rel $ "gen/plan" </> T.unpack planName

            let hashed = hashGH3 songYaml gh3
                songID = fromMaybe hashed $ gh3_SongID gh3
                dl = "dl" <> show (fromMaybe hashed $ gh3_DL gh3)

            let coopPart = case gh3_Coop gh3 of
                  GH2Bass   -> gh3_Bass   gh3
                  GH2Rhythm -> gh3_Rhythm gh3
                gh3Parts = [gh3_Guitar gh3, coopPart]
                loadPSMidi :: Staction (RBFile.Song (RBFile.OnyxFile U.Beats))
                loadPSMidi = shakeMIDI $ planDir </> "raw.mid"

            let pathGuitar  = dir </> "guitar.wav"
                pathRhythm  = dir </> "rhythm.wav"
                pathSong    = dir </> "song.wav"
                pathPreview = dir </> "preview.wav"
            pathGuitar %> \out -> do
              mid <- loadPSMidi
              writeStereoParts gh3Parts (gh3_Common gh3) mid 0 planName plan
                [(gh3_Guitar gh3, 1)]
                out
            pathRhythm %> \out -> do
              mid <- loadPSMidi
              writeStereoParts gh3Parts (gh3_Common gh3) mid 0 planName plan
                [(coopPart, 1)]
                out
            pathSong %> \out -> do
              mid <- loadPSMidi
              writeSongCountin (gh3_Common gh3) mid 0 True planName plan
                [ (gh3_Guitar gh3, 1)
                , (coopPart      , 1)
                ] out
            pathPreview %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let (pstart, pend) = previewBounds songYaml (mid :: RBFile.Song (RBFile.OnyxFile U.Beats))
                  fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
                  previewExpr
                    = Fade End (Seconds 5)
                    $ Fade Start (Seconds 2)
                    $ Take Start (fromMS $ pend - pstart)
                    $ Drop Start (fromMS pstart)
                    $ Input (planDir </> "everything.wav")
              buildAudio previewExpr out

            let pathFsb = dir </> "audio.fsb"
                pathFsbXen = dir </> "gh3" </> ("DLC" <> show songID <> ".fsb.xen")
            pathFsb %> \out -> do
              shk $ need [pathGuitar, pathPreview, pathRhythm, pathSong]
              makeGH3FSB pathGuitar pathPreview pathRhythm pathSong out
            pathFsbXen %> \out -> do
              shk $ need [pathFsb]
              fsb <- stackIO $ BL.readFile pathFsb
              stackIO $ BL.writeFile out $ gh3Encrypt fsb

            phony (dir </> "gh3") $ do
              shk $ need [pathFsbXen]

          DTX dtx -> do

            (planName, plan) <- case getPlan (tgt_Plan $ dtx_Common dtx) songYaml of
              Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show dtx
              Just pair -> return pair
            let planDir = rel $ "gen/plan" </> T.unpack planName

            let dtxPartDrums  = case getPart (dtx_Drums dtx) songYaml >>= partDrums of
                  Just pd -> Just (dtx_Drums  dtx, pd)
                  Nothing -> Nothing
                dtxPartGuitar = case getPart (dtx_Guitar dtx) songYaml >>= partGRYBO of
                  Just pg -> Just (dtx_Guitar dtx, pg)
                  Nothing -> Nothing
                dtxPartBass   = case getPart (dtx_Bass dtx) songYaml >>= partGRYBO of
                  Just pg -> Just (dtx_Bass dtx, pg)
                  Nothing -> Nothing

            dir </> "dtx/empty.wav" %> \out -> do
              buildAudio (Silence 1 $ Seconds 0) out

            dir </> "dtx/bgm.ogg" %> \out -> do
              let wav = planDir </> "everything.wav"
              buildAudio (Input wav) out

            dir </> "dtx/preview.ogg" %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let (pstart, pend) = previewBounds songYaml (mid :: RBFile.Song (RBFile.OnyxFile U.Beats))
                  fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
                  previewExpr
                    = Fade End (Seconds 5)
                    $ Fade Start (Seconds 2)
                    $ Take Start (fromMS $ pend - pstart)
                    $ Drop Start (fromMS pstart)
                    $ Input (planDir </> "everything.wav")
              buildAudio previewExpr out

            artPath <- case _fileAlbumArt $ _metadata songYaml of
              Just img | elem (takeExtension img) [".jpg", ".jpeg"] -> do
                dir </> "dtx/cover.jpg" %> shk . copyFile' img
                return "cover.jpg"
              _ -> return "cover.png"
            dir </> "dtx/cover.png" %> shk . copyFile' (rel "gen/cover-full.png")

            mapping <- forM (dtxPartDrums >>= \(_, pd) -> drumsFileDTXKit pd) $ \f -> do
              bs <- liftIO $ B.readFile f
              case readMaybe $ T.unpack $ decodeGeneral bs of
                Nothing -> fail $ "Couldn't parse mapping of full drums to DTX template from: " <> f
                Just m  -> return (f, m)
            template <- forM mapping $ \(f, DTX.DTXMapping templateRel _) -> liftIO $ do
              let templateFixed = takeDirectory f </> templateRel
              templateDTX <- DTX.readDTXLines DTX.FormatDTX <$> DTX.loadDTXLines templateFixed
              return (templateFixed, templateDTX)

            dir </> "dtx/mstr.dtx" %> \out -> do
              mid <- shakeMIDI $ planDir </> "processed.mid"
              let bgmChip   = "0X" -- TODO read this from BGMWAV in mapping
                  emptyChip = "ZZ"
                  makeGuitarBass = \case
                    Nothing          -> RTB.empty
                    Just (part, _pg) -> let
                      notes
                        = maybe RTB.empty (guitarify' . openNotes')
                        $ Map.lookup Expert
                        $ fiveDifficulties
                        $ maybe mempty (fst . RBFile.selectGuitarTrack RBFile.FiveTypeGuitarExt)
                        $ Map.lookup part
                        $ RBFile.onyxParts
                        $ RBFile.s_tracks mid
                      toDTXNotes = fmap $ \(mcolors, _len) -> (sort $ catMaybes mcolors, emptyChip)
                      in toDTXNotes notes
              liftIO $ B.writeFile out $ TE.encodeUtf16LE $ T.cons '\xFEFF' $ DTX.makeDTX DTX.DTX
                { DTX.dtx_TITLE         = Just $ targetTitle songYaml target
                , DTX.dtx_ARTIST        = Just $ getArtist $ _metadata songYaml
                , DTX.dtx_PREIMAGE      = Just artPath
                , DTX.dtx_COMMENT       = Nothing
                , DTX.dtx_GENRE         = _genre $ _metadata songYaml
                , DTX.dtx_PREVIEW       = Just "preview.ogg"
                , DTX.dtx_STAGEFILE     = Nothing
                , DTX.dtx_DLEVEL        = Nothing
                , DTX.dtx_GLEVEL        = Nothing
                , DTX.dtx_BLEVEL        = Nothing
                , DTX.dtx_DLVDEC        = Nothing
                , DTX.dtx_GLVDEC        = Nothing
                , DTX.dtx_BLVDEC        = Nothing
                , DTX.dtx_WAV           = let
                  initWAV = HM.fromList
                    [ (emptyChip, "empty.wav")
                    , (bgmChip  , "bgm.ogg"  )
                    ]
                  -- TODO maybe make sure all template WAV paths are local
                  in maybe initWAV (HM.union initWAV . DTX.dtx_WAV . snd) template
                , DTX.dtx_VOLUME        = maybe HM.empty (DTX.dtx_VOLUME . snd) template
                , DTX.dtx_PAN           = maybe HM.empty (DTX.dtx_PAN    . snd) template
                , DTX.dtx_AVI           = HM.empty
                , DTX.dtx_MeasureMap    = RBFile.s_signatures mid
                , DTX.dtx_TempoMap      = RBFile.s_tempos mid
                , DTX.dtx_Drums         = case dtxPartDrums of
                  Nothing          -> RTB.empty
                  Just (part, _pd) -> let
                    -- TODO split flams
                    -- TODO figure out what to do for Left Bass
                    fullNotes
                      = FD.getDifficulty Nothing
                      $ maybe mempty RBFile.onyxPartFullDrums
                      $ Map.lookup part
                      $ RBFile.onyxParts
                      $ RBFile.s_tracks mid
                    toDTXNotes = fmap $ \fdn -> let
                      lane = case FD.fdn_gem fdn of
                        FD.Kick      -> DTX.BassDrum
                        FD.Snare     -> DTX.Snare
                        FD.Hihat     -> case FD.fdn_type fdn of
                          FD.GemHihatOpen -> DTX.HihatOpen
                          _               -> DTX.HihatClose
                        FD.HihatFoot -> DTX.LeftPedal
                        FD.CrashL    -> DTX.LeftCymbal
                        FD.Tom1      -> DTX.HighTom
                        FD.Tom2      -> DTX.LowTom
                        FD.Tom3      -> DTX.FloorTom
                        FD.CrashR    -> DTX.Cymbal
                        FD.Ride      -> DTX.RideCymbal
                      chip = fromMaybe emptyChip $ mapping >>= \(_, m) -> DTX.lookupDTXMapping m fdn
                      in (lane, chip)
                    in toDTXNotes fullNotes
                , DTX.dtx_DrumsDummy    = RTB.empty
                , DTX.dtx_Guitar        = makeGuitarBass dtxPartGuitar
                , DTX.dtx_GuitarWailing = RTB.empty
                , DTX.dtx_Bass          = makeGuitarBass dtxPartBass
                , DTX.dtx_BassWailing   = RTB.empty
                , DTX.dtx_BGM           = RTB.singleton 0 bgmChip
                , DTX.dtx_BGMExtra      = HM.empty
                , DTX.dtx_Video         = RTB.empty
                }

            phony (dir </> "dtx") $ do
              shk $ need
                [ dir </> "dtx/empty.wav"
                , dir </> "dtx/bgm.ogg"
                , dir </> "dtx/preview.ogg"
                , dir </> "dtx" </> artPath
                , dir </> "dtx/mstr.dtx"
                ]
              forM_ template $ \(templatePath, templateDTX) -> do
                forM_ (HM.toList $ DTX.dtx_WAV templateDTX) $ \(_, path) -> do
                  -- again, maybe make sure path is local
                  when (path /= "bgm.ogg") $ do
                    shk $ copyFile'
                      (takeDirectory templatePath </> path)
                      (rel $ dir </> "dtx" </> path)

          Konga _ -> return () -- TODO

      forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do

        let dir = rel $ "gen/plan" </> T.unpack planName

        -- plan audio, currently only used for REAPER project
        let allPlanParts :: [(RBFile.FlexPartName, PartAudio ())]
            allPlanParts = case plan of
              Plan{..}     -> HM.toList $ getParts $ void <$> _planParts
              MoggPlan{..} -> do
                (fpart, pa) <- HM.toList $ getParts _moggParts
                guard $ not $ null $ concat $ toList pa
                return (fpart, void pa)
            dummyMIDI :: RBFile.Song (RBFile.OnyxFile U.Beats)
            dummyMIDI = RBFile.Song
              { RBFile.s_tempos = U.tempoMapFromBPS RTB.empty
              , RBFile.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
              , RBFile.s_tracks = mempty :: RBFile.OnyxFile U.Beats
              }
        dir </> "song.wav" %> writeSongCountin def dummyMIDI 0 False planName plan [ (fpart, 1) | (fpart, _) <- allPlanParts ]
        dir </> "crowd.wav" %> writeCrowd def dummyMIDI 0 planName plan
        forM_ allPlanParts $ \(fpart, pa) -> do
          let name = T.unpack $ RBFile.getPartName fpart
          case pa of
            PartSingle () -> do
              dir </> name <.> "wav" %> writeSimplePart [fpart] def dummyMIDI 0 False planName plan fpart 1
            PartDrumKit mkick msnare () -> do
              forM_ mkick $ \() -> do
                dir </> (name ++ "-kick") <.> "wav" %>
                  writeKick [fpart] def dummyMIDI 0 False planName plan fpart 1
              forM_ msnare $ \() -> do
                dir </> (name ++ "-snare") <.> "wav" %>
                  writeSnare [fpart] def dummyMIDI 0 False planName plan fpart 1
              dir </> (name ++ "-kit") <.> "wav" %>
                writeKit [fpart] def dummyMIDI 0 False planName plan fpart 1
        let allPlanAudio :: [FilePath]
            allPlanAudio = map (dir </>) $ concat
              [ [ "song.wav" ]
              , sort $ allPlanParts >>= \(fpart, pa) -> let
                name = T.unpack $ RBFile.getPartName fpart
                in case pa of
                  PartSingle () -> [name <.> "wav"]
                  PartDrumKit mkick msnare () -> concat
                    [ map (\() -> (name ++ "-kick") <.> "wav") $ toList mkick
                    , map (\() -> (name ++ "-snare") <.> "wav") $ toList msnare
                    , [(name ++ "-kit") <.> "wav"]
                    ]
              , [ "crowd.wav"
                | case plan of Plan{..} -> isJust _crowd; MoggPlan{..} -> not $ null _moggCrowd
                ]
              ]

        -- REAPER project
        rel ("notes-" ++ T.unpack planName ++ ".RPP") %> \out -> do
          let tempo = rel $ fromMaybe "gen/notes.mid" $ _fileTempo plan
              tunings = TuningInfo
                { tuningGuitars = do
                  (fpart, part) <- HM.toList $ getParts $ _parts songYaml
                  pg <- toList $ partProGuitar part
                  return (fpart, pgTuning pg)
                , tuningCents = _tuningCents plan
                }
          makeReaperShake tunings (rel "gen/notes.mid") tempo allPlanAudio out

        dir </> "web/song.js" %> \out -> do
          let json = dir </> "display.json"
          s <- shk $ readFile' json
          let s' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
              js = "window.onyxSong = " ++ s' ++ ";\n"
          liftIO $ writeFile out js
        phony (dir </> "web") $ do
          stackIO $ Dir.createDirectoryIfMissing True $ dir </> "web"
          stackIO webDisplay >>= (`copyDirRecursive` (dir </> "web"))
          shk $ need
            [ dir </> "web/audio-mp3.js"
            , dir </> "web/audio-ogg.js"
            , dir </> "web/song.js"
            ]
        forM_ ["mp3", "ogg"] $ \ext -> do
          dir </> ("web/audio-" <> ext) <.> "js" %> \out -> do
            let audio = dir </> "preview-audio" <.> ext
            shk $ need [audio]
            bs <- stackIO $ BL.readFile audio
            stackIO $ BL.writeFile out $ "window.audioBin = \"" <> B64.encode bs <> "\";\n"

        dir </> "everything.wav" %> \out -> case plan of
          MoggPlan{..} -> do
            src <- shk $ buildSource $ Input $ dir </> "audio.ogg"
            let vols = zipWith f [0..] _vols
                f i vol = if elem i _moggCrowd then -99 else vol
            runAudio (applyPansVols (map realToFrac _pans) (map realToFrac vols) src) out
          Plan{..} -> do
            let planAudios = concat
                  [ toList _song
                  , toList _planParts >>= toList
                  ]
            srcs <- mapM (buildAudioToSpec yamlDir audioLib audioDepend songYaml [(-1, 0), (1, 0)] . Just) planAudios
            count <- shk $ buildSource $ Input $ dir </> "countin.wav"
            runAudio (foldr mix count srcs) out
        dir </> "everything.ogg" %> buildAudio (Input $ dir </> "everything.wav")

        -- MIDI files

        let midprocessed = dir </> "processed.mid"
            midraw = dir </> "raw.mid"
            display = dir </> "display.json"
        midraw %> \out -> do
          lg "Loading the MIDI file..."
          input <- shakeMIDI $ rel "gen/notes.mid"
          let _ = input :: RBFile.Song (RBFile.RawFile U.Beats)
          tempos <- fmap RBFile.s_tempos $ case _fileTempo plan of
            Nothing -> return input
            Just m  -> shakeMIDI m
          saveMIDI out input { RBFile.s_tempos = tempos }
        midprocessed %> \out -> do
          -- basically just autogen a BEAT track
          input <- shakeMIDI midraw
          output <- RB3.processTiming input $ getAudioLength planName plan
          saveMIDI out output

        display %> \out -> do
          song <- shakeMIDI midprocessed
          liftIO $ BL.writeFile out $ makeDisplay songYaml song

        -- count-in audio
        dir </> "countin.wav" %> \out -> do
          let hits = case plan of MoggPlan{} -> []; Plan{..} -> case _countin of Countin h -> h
          src <- buildAudioToSpec yamlDir audioLib audioDepend songYaml [(-1, 0), (1, 0)] =<< case NE.nonEmpty hits of
            Nothing    -> return Nothing
            Just hits' -> Just . (\expr -> PlanAudio expr [] []) <$> do
              mid <- shakeMIDI $ dir </> "raw.mid"
              let _ = mid :: RBFile.Song (RBFile.RawFile U.Beats)
              return $ Mix $ flip fmap hits' $ \(posn, aud) -> let
                time = Seconds $ realToFrac $ case posn of
                  Left  mb   -> U.applyTempoMap (RBFile.s_tempos mid) $ U.unapplyMeasureMap (RBFile.s_signatures mid) mb
                  Right secs -> secs
                in Pad Start time aud
          runAudio src out

        -- Getting MOGG/OGG from MoggPlan
        let ogg  = dir </> "audio.ogg"
            wav  = dir </> "audio.wav"
            mogg = dir </> "audio.mogg"
        case plan of
          Plan{} -> return ()
          MoggPlan{..} -> do
            ogg %> \out -> do
              shk $ need [mogg]
              moggToOgg mogg out
            wav %> buildAudio (Input ogg)
            mogg %> \out -> do
              p <- inside "Searching for MOGG file" $ case (_fileMOGG, _moggMD5) of
                (Nothing, Nothing ) -> fatal "No file path or MD5 hash specified for MOGG file"
                (Just f , _       ) -> return f -- TODO maybe check md5 if it is specified
                (Nothing, Just md5) -> toFilePath <$> searchMOGG audioLib md5
              lg $ "Found the MOGG file: " <> p
              -- TODO: check if it's actually an OGG (starts with OggS)
              shk $ copyFile' p out
              forceRW out
            (dir </> "silent-channels.txt") %> \out -> do
              src <- lift $ lift $ buildSource $ Input ogg
              chans <- stackIO $ runResourceT $ runConduit $ emptyChannels src
              stackIO $ writeFile out $ show chans

        -- Audio files for the online preview app
        dir </> "preview-audio.mp3" %> \out -> do
          src <- lift $ lift $ buildSource $ Input $ dir </> "everything.wav"
          stackIO $ runResourceT $ decentMP3 out src
        dir </> "preview-audio.ogg" %> \out -> do
          src <- lift $ lift $ buildSource $ Input $ dir </> "everything.wav"
          stackIO $ runResourceT $ decentVorbis out src

        -- Warn about notes that might hang off before a pro keys range shift
        phony (dir </> "hanging") $ do
          song <- shakeMIDI midprocessed
          lg $ T.unpack $ closeShiftsFile song

        -- Print out a summary of (non-vocal) overdrive and unison phrases
        phony (dir </> "overdrive") $ printOverdrive midprocessed

        {-
          -- Check for some extra problems that Magma doesn't catch.
          phony (pedalDir </> "problems") $ do
            song <- RBFile.loadMIDI $ pedalDir </> "notes.mid"
            let problems = RB3.findProblems song
            mapM_ putNormal problems
            unless (null problems) $ fail "At least 1 problem was found in the MIDI."
        -}

      lift $ want $ map rel buildables

data RSArrangementType g v
  = RSPlayable RSArrSlot g
  | RSVocal v
  -- TODO showlight

shk :: Action a -> StackTraceT (QueueLog Action) a
shk = lift . lift
