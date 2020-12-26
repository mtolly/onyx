{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.SongCache where

import           Control.Monad
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Codec.Class
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.DTA.Serialize             (DictList (..))
import           Data.DTA.Serialize.Magma       (Gender (..))
import qualified Data.DTA.Serialize.RB3         as D
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (elemIndex, sort)
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Maybe                     (fromMaybe)
import           Data.SimpleHandle              (Folder (..))
import           Data.SimpleHandle              (findByteString)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           PrettyDTA
import           Resources                      (rb3Thumbnail)
import           STFS.Package
import           System.Directory               (doesFileExist, listDirectory)
import           System.FilePath                (takeDirectory, (</>))
import qualified System.IO                      as IO

-- C3 parse code for reference:
-- https://github.com/RBTools/CON-Tools/blob/14dd538ee929093808b67e96691620321d41aa54/C3%20Tools/SetlistManager.cs#L426

lenArray :: BinaryCodec a -> BinaryCodec [a]
lenArray c = Codec
  { codecIn = do
    len <- getWord32le
    replicateM (fromIntegral len) $ codecIn c
  , codecOut = fmapArg $ \xs -> do
    putWord32le $ fromIntegral $ length xs
    forM_ xs $ codecOut c
  }

string :: BinaryCodec T.Text
string = Codec
  -- verified that these are UTF-8
  { codecIn = do
    len <- getWord32le
    TE.decodeUtf8 <$> getByteString (fromIntegral len)
  , codecOut = fmapArg $ \t -> do
    let bs = TE.encodeUtf8 t
    putWord32le $ fromIntegral $ B.length bs
    putByteString bs
  }

floatle :: BinaryCodec Float
floatle = Codec getFloatle (fmapArg putFloatle)

data SongCache = SongCache
  { sc_Version    :: Word32 -- 0x0C
  , sc_Files      :: [FileEntry] -- order can get entirely scrambled on each load
  , sc_Songs      :: [SongEntry] -- new songs get randomly added in the middle, but existing entries maintain order
  , sc_NewSongIDs :: [Word32] -- the 20 most recent songs for "newly obtained"
  , sc_FileNames  :: [T.Text] -- this gets new filenames tacked onto the end
  , sc_FileNames2 :: [FileNames2Entry] -- order matches that of sc_Files
  } deriving (Eq, Show)

instance Bin SongCache where
  bin = do
    sc_Version    <- sc_Version    =. word32le
    sc_Files      <- sc_Files      =. lenArray bin
    sc_Songs      <- sc_Songs      =. lenArray bin
    sc_NewSongIDs <- sc_NewSongIDs =. lenArray word32le
    sc_FileNames  <- sc_FileNames  =. lenArray string
    sc_FileNames2 <- sc_FileNames2 =. lenArray bin
    return SongCache{..}

data FileEntry = FileEntry
  { fe_Name    :: T.Text
  , fe_Mystery :: Word32 -- 1 on every song in my cache. maybe index into multi-song pack?
  , fe_SongID  :: Word32
  } deriving (Eq, Show)

instance Bin FileEntry where
  bin = do
    fe_Name    <- fe_Name    =. string
    fe_Mystery <- fe_Mystery =. word32le
    fe_SongID  <- fe_SongID  =. word32le
    return FileEntry{..}

data SongEntry = SongEntry
  -- examples of mystery entries taken from first one in my list (another dimension 2x)
  { se_SongID           :: Word32
  , se_Mystery1         :: Word32 -- 0x11 on every song in my cache
  , se_Mystery2         :: Word32 -- 0x06 on every song in my cache
  , se_Version          :: Word16 -- 'version' from dta
  , se_SongID2          :: Word32
  , se_Mystery3         :: Word8 -- 0 on every song in my cache
  , se_GameOrigin       :: T.Text -- "ugc_plus", "rb3_dlc", etc.
  , se_PreviewStart     :: Float -- milliseconds
  , se_PreviewEnd       :: Float -- milliseconds
  , se_Key1             :: T.Text -- e.g. "o139827711"
  , se_Mystery4         :: Word32 -- usually 0, newly added (?) songs might have 1, 2, 3, 4, 5. fresh cache they're all 0
  , se_Mystery5         :: Word32 -- 1 on every song in my cache
  , se_Key2             :: T.Text -- e.g. "o139827711"
  , se_Path             :: T.Text -- e.g. "songs/o139827711/o139827711"
  , se_Mystery6         :: Word32 -- 0 on every song in my cache
  , se_VocalParts       :: Word32 -- 0 (no vocals), 1, 2, 3
  , se_HopoThreshold    :: Word32 -- usually 0xAA (170)
  , se_MuteVolume       :: Float -- usually -16.0 (default) or -96.0 (c3 magma)
  , se_MuteVolumeVocals :: Float -- usually -12.0
  , se_Pans             :: [Float]
  , se_Vols             :: [Float]
  , se_Cores            :: [Int32]
  , se_CrowdChannels    :: [Word32]
  , se_DrumSolo         :: [T.Text]
  , se_DrumFreestyle    :: [T.Text]
  , se_Tracks           :: [TracksEntry]
  , se_Mystery7         :: Word32 -- 0 on every song in my cache
  , se_Title            :: T.Text
  , se_Artist           :: T.Text
  , se_Album            :: T.Text
  , se_AlbumTrackNumber :: Word32
  , se_MysteryDates     :: B.ByteString -- 10 bytes: 00 01 00 63 | 00 00 | 00 01 00 63 (0x63 == 99, year is 1999. two dates for re-records)
  , se_Genre            :: T.Text
  , se_Mystery8         :: Word32 -- 0 on every song in my cache
  , se_Mystery9         :: Word32 -- 0 on every song in my cache
  , se_Ranks            :: [RankEntry]
  , se_Rating           :: Word32 -- 1 FF, 2 SR, 3 M, 4 unrated
  , se_Mystery10        :: Word16 -- usually 0x40 0xC0 (49216), 0 in DLC, 0xBF 0x80 (49024) in IMM's green day songs
  , se_SongScrollSpeed  :: Float -- 00 C0 0F 45 (2300.0), occasionally authors tweak this
  , se_Mystery11        :: Word16 -- 0 on every song in my cache
  , se_Mystery12        :: Word16 -- usually 0, some songs have: 16904,16800,49568,16824,16672,16840,49608,16908,16880 (look like bitfields?)
  , se_Bank             :: T.Text
  , se_DrumBank         :: T.Text
  , se_VocalTonicNote   :: Int32 -- 0 to 11, or -1 (not set)
  , se_Mystery13        :: Int32 -- -1 on every song in my cache, could be song_key
  , se_SongTonality     :: Int32 -- 0, 1, or -1 (not set)
  , se_SongLength       :: Word32 -- milliseconds
  , se_HasAlbumArt      :: Word8
  , se_Master           :: Word8 -- 1 if master, 0 if cover
  -- c3 tools says next 5 bytes are 6 bytes if wii file
  , se_Mystery14        :: Word8 -- 0 on every song in my cache
  , se_AnimTempo        :: Word32 -- 16 slow, 32 medium, 64 fast
  , se_VocalGender      :: T.Text
  , se_GuitarTuning     :: [Int32] -- 6 entries (no length)
  , se_BassTuning       :: [Int32] -- 4 entries (no length)
  , se_Mystery15        :: Word8 -- 0 on every song in my cache
  , se_Solo             :: [T.Text] -- ["guitar", "keys"]
  } deriving (Eq, Show)

instance Bin SongEntry where
  bin = do
    se_SongID           <- se_SongID           =. word32le
    se_Mystery1         <- se_Mystery1         =. word32le
    se_Mystery2         <- se_Mystery2         =. word32le
    se_Version          <- se_Version          =. word16le
    se_SongID2          <- se_SongID2          =. word32le
    se_Mystery3         <- se_Mystery3         =. word8
    se_GameOrigin       <- se_GameOrigin       =. string
    se_PreviewStart     <- se_PreviewStart     =. floatle
    se_PreviewEnd       <- se_PreviewEnd       =. floatle
    se_Key1             <- se_Key1             =. string
    se_Mystery4         <- se_Mystery4         =. word32le
    se_Mystery5         <- se_Mystery5         =. word32le
    se_Key2             <- se_Key2             =. string
    se_Path             <- se_Path             =. string
    se_Mystery6         <- se_Mystery6         =. word32le
    se_VocalParts       <- se_VocalParts       =. word32le
    se_HopoThreshold    <- se_HopoThreshold    =. word32le
    se_MuteVolume       <- se_MuteVolume       =. floatle
    se_MuteVolumeVocals <- se_MuteVolumeVocals =. floatle
    se_Pans             <- se_Pans             =. lenArray floatle
    se_Vols             <- se_Vols             =. lenArray floatle
    se_Cores            <- se_Cores            =. lenArray int32le
    se_CrowdChannels    <- se_CrowdChannels    =. lenArray word32le
    se_DrumSolo         <- se_DrumSolo         =. lenArray string
    se_DrumFreestyle    <- se_DrumFreestyle    =. lenArray string
    se_Tracks           <- se_Tracks           =. lenArray bin
    se_Mystery7         <- se_Mystery7         =. word32le
    se_Title            <- se_Title            =. string
    se_Artist           <- se_Artist           =. string
    se_Album            <- se_Album            =. string
    se_AlbumTrackNumber <- se_AlbumTrackNumber =. word32le
    se_MysteryDates     <- se_MysteryDates     =. byteString 10
    se_Genre            <- se_Genre            =. string
    se_Mystery8         <- se_Mystery8         =. word32le
    se_Mystery9         <- se_Mystery9         =. word32le
    se_Ranks            <- se_Ranks            =. lenArray bin
    se_Rating           <- se_Rating           =. word32le
    se_Mystery10        <- se_Mystery10        =. word16le
    se_SongScrollSpeed  <- se_SongScrollSpeed  =. floatle
    se_Mystery11        <- se_Mystery11        =. word16le
    se_Mystery12        <- se_Mystery12        =. word16le
    se_Bank             <- se_Bank             =. string
    se_DrumBank         <- se_DrumBank         =. string
    se_VocalTonicNote   <- se_VocalTonicNote   =. int32le
    se_Mystery13        <- se_Mystery13        =. int32le
    se_SongTonality     <- se_SongTonality     =. int32le
    se_SongLength       <- se_SongLength       =. word32le
    se_HasAlbumArt      <- se_HasAlbumArt      =. word8
    se_Master           <- se_Master           =. word8
    se_Mystery14        <- se_Mystery14        =. word8
    se_AnimTempo        <- se_AnimTempo        =. word32le
    se_VocalGender      <- se_VocalGender      =. string
    se_GuitarTuning     <- se_GuitarTuning     =. fixedArray 6 int32le
    se_BassTuning       <- se_BassTuning       =. fixedArray 4 int32le
    se_Mystery15        <- se_Mystery15        =. word8
    se_Solo             <- se_Solo             =. lenArray string
    return SongEntry{..}

data TracksEntry = TracksEntry
  { te_Instrument :: Word32 -- 0 drum, 2 bass, 1 guitar, 4 keys, probably 3 vox
  , te_Channels   :: [Word32]
  } deriving (Eq, Show)

instance Bin TracksEntry where
  bin = do
    te_Instrument <- te_Instrument =. word32le
    te_Channels   <- te_Channels   =. lenArray word32le
    return TracksEntry{..}

data RankEntry = RankEntry
  { re_Part :: T.Text
  , re_Rank :: Float
  } deriving (Eq, Show)

instance Bin RankEntry where
  bin = do
    re_Part <- re_Part =. string
    re_Rank <- re_Rank =. floatle
    return RankEntry{..}

data FileNames2Entry = FileNames2Entry
  { fn2_Path    :: T.Text
  , fn2_Mystery :: Word32 -- 0 on every song in my cache
  } deriving (Eq, Show)

instance Bin FileNames2Entry where
  bin = do
    fn2_Path    <- fn2_Path    =. string
    fn2_Mystery <- fn2_Mystery =. word32le
    return FileNames2Entry{..}

-- | Orders keys to match an existing list, and puts new ones at the end (sorted).
matchOrder :: (Ord a, Ord b) => [a] -> [(a, b)] -> [(a, b)]
matchOrder template xs = let
  xs' = do
    pair@(x, _) <- xs
    let xi = case elemIndex x template of
          Just i  -> Left i
          Nothing -> Right x
    return (xi, pair)
  in map snd $ sort xs'

applyDTA :: DTASingle -> SongEntry -> SongEntry
applyDTA single entry = let
  pkg = dtaSongPackage single
  songID orig = case D.songId pkg of
    Just (Left i) -> fromIntegral i
    _             -> orig
  in entry
    { se_SongID           = songID $ se_SongID entry
    , se_Version          = fromIntegral $ D.version pkg
    , se_SongID2          = songID $ se_SongID2 entry
    , se_GameOrigin       = fromMaybe (se_GameOrigin entry) $ D.gameOrigin pkg
    , se_PreviewStart     = fromIntegral $ fst $ D.preview pkg
    , se_PreviewEnd       = fromIntegral $ snd $ D.preview pkg
    , se_Key1             = dtaTopKey single
    , se_Key2             = dtaTopKey single
    , se_Path             = D.songName $ D.song pkg
    , se_VocalParts       = maybe (se_VocalParts entry) fromIntegral $ D.vocalParts $ D.song pkg
    , se_HopoThreshold    = maybe (se_HopoThreshold entry) fromIntegral $ D.hopoThreshold $ D.song pkg
    , se_MuteVolume       = fromMaybe (-16) $ D.muteVolume $ D.song pkg
    , se_MuteVolumeVocals = fromMaybe (-12) $ D.muteVolumeVocals $ D.song pkg
    , se_Pans             = D.pans $ D.song pkg
    , se_Vols             = D.vols $ D.song pkg
    , se_Cores            = map fromIntegral $ D.cores $ D.song pkg
    , se_CrowdChannels    = maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
    , se_DrumSolo         = D.seqs $ D.drumSolo $ D.song pkg
    , se_DrumFreestyle    = D.seqs $ D.drumFreestyle $ D.song pkg
    , se_Tracks           = do
      (inst, chans) <- matchOrder (map te_Instrument $ se_Tracks entry) $ do
        (inst, chans) <- fromDictList $ D.tracks $ D.song pkg
        case inst of
          "drum"   -> [(0, chans)]
          "guitar" -> [(1, chans)]
          "bass"   -> [(2, chans)]
          "vocals" -> [(3, chans)]
          "keys"   -> [(4, chans)]
          _        -> [] -- unrecognized instrument
      return TracksEntry { te_Instrument = inst, te_Channels = map fromIntegral chans }
    , se_Title            = D.name pkg
    , se_Artist           = fromMaybe "" $ D.artist pkg
    , se_Album            = fromMaybe (se_Album entry) $ D.albumName pkg
    , se_AlbumTrackNumber = maybe (se_AlbumTrackNumber entry) fromIntegral $ D.albumTrackNumber pkg
    -- TODO se_MysteryDates
    , se_Genre            = fromMaybe "" $ D.genre pkg
    , se_Ranks            = do
      (t, i) <- matchOrder (map re_Part $ se_Ranks entry) $ HM.toList $ D.rank pkg
      return RankEntry { re_Part = t, re_Rank = fromIntegral i }
    , se_Rating           = fromIntegral $ D.rating pkg
    , se_SongScrollSpeed  = fromIntegral $ D.songScrollSpeed pkg
    , se_Bank             = fromMaybe (se_Bank entry) $ D.bank pkg
    , se_DrumBank         = fromMaybe (se_DrumBank entry) $ D.drumBank pkg
    , se_VocalTonicNote   = maybe (-1) (fromIntegral . fromEnum) $ D.vocalTonicNote pkg
    , se_SongTonality     = maybe (-1) (fromIntegral . fromEnum) $ D.songTonality pkg
    , se_SongLength       = maybe (se_SongLength entry) fromIntegral $ D.songLength pkg
    , se_HasAlbumArt      = maybe (se_HasAlbumArt entry) (\b -> if b then 1 else 0) $ D.albumArt pkg
    , se_Master           = if D.master pkg then 1 else 0
    , se_AnimTempo        = case D.animTempo pkg of
      Right i              -> fromIntegral i
      Left  D.KTempoSlow   -> 16
      Left  D.KTempoMedium -> 32
      Left  D.KTempoFast   -> 64
    , se_VocalGender      = case D.vocalGender pkg of
      Just Female -> "female"
      Just Male   -> "male"
      Nothing     -> se_VocalGender entry
    , se_GuitarTuning     = take 6 $
      maybe [] (map fromIntegral) (D.realGuitarTuning pkg) ++ repeat 0
    , se_BassTuning       = take 4 $
      maybe [] (map fromIntegral) (D.realBassTuning pkg) ++ repeat 0
    , se_Solo             = flip map (fromMaybe [] $ D.solo pkg) $ \case
      "vocal_percussion" -> "vocals"
      inst               -> inst
    }

fixSongCache :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m ()
fixSongCache path = do
  mcache <- stackIO $ getSTFSFolder path >>= findByteString (pure "songcache")
  origCache <- case mcache of
    Nothing -> fatal "Couldn't find song cache file inside STFS"
    Just bs -> case runGetOrFail (codecIn bin) bs of
      Left  (_, _, err  ) -> fatal $ "Couldn't parse song cache file: " <> err
      Right (_, _, cache) -> return (cache :: SongCache)
  cons <- fmap (filter $ \s -> take 1 s /= ".")
    $ stackIO $ listDirectory $ takeDirectory path
  let checkCON cache con = do
        let conFull = takeDirectory path </> con
        lg con
        stackIO (doesFileExist conFull) >>= \case
          False -> return cache
          True  -> do
            magic <- stackIO $ IO.withBinaryFile conFull IO.ReadMode $ \h -> B.hGet h 4
            if elem magic ["CON ", "LIVE"]
              then do
                dta <- fmap join $ errorToWarning $ stackIO $ getSTFSFolder conFull
                  >>= findByteString ("songs" :| pure "songs.dta")
                case dta of
                  Nothing -> return cache
                  Just bs -> do
                    songs <- map fst <$> readDTASingles (BL.toStrict bs)
                    foldM (checkSingle $ T.pack con) cache (zip [1..] songs)
              else return cache
      checkSingle name cache (i, single) = do
        let numericID = case D.songId $ dtaSongPackage single of
              Just (Left nid) -> Just $ fromIntegral nid
              _               -> Nothing
        case [ fe_SongID fe | fe <- sc_Files cache, fe_Name fe == name, fe_Mystery fe == i ] of
          [] -> case numericID of
            Just nid -> checkSongID name nid single cache
            Nothing  -> return cache -- non-numeric ID, no generated ID, nothing to do
          cid : _ -> case numericID of
            Just nid -> if nid == cid
              then checkSongID name nid single cache
              else do
                lg $ T.unpack name <> ": updated song ID from " <> show cid <> " to " <> show nid
                checkSongID name nid single cache
                  { sc_Files = flip map (sc_Files cache) $ \fe ->
                    -- replace the existing numeric ID with a new one
                    if fe_Name fe == name && fe_Mystery fe == i
                      then fe { fe_SongID = nid }
                      else fe
                  }
            Nothing -> checkSongID name cid single cache -- keep the generated ID
      checkSongID name sid single cache = do
        newSongs <- forM (sc_Songs cache) $ \se ->
          if se_SongID se == sid
            then do
              let se' = applyDTA single se
              when (se /= se') $ lg $ T.unpack name <> " [" <> show sid <> "]: updated metadata"
              return se'
            else return se
        return cache { sc_Songs = newSongs }
  newCache <- foldM checkCON origCache cons
  if origCache == newCache
    then lg "Cache is up to date, nothing to save."
    else do
      lg "Saving updated cache file."
      thumb <- stackIO $ rb3Thumbnail >>= B.readFile
      stackIO $ makeCONMemory CreateOptions
        { createName = "Rock Band 3 Song Cache"
        , createDescription = ""
        , createTitleID = 0x45410914
        , createTitleName = "Rock Band 3"
        , createThumb = thumb
        , createTitleThumb = thumb
        , createLicense = LicenseEntry (-1) 0 0
        , createMediaID       = 1338582383
        , createVersion       = 1025
        , createBaseVersion   = 1
        , createTransferFlags = 64
        } Folder
          { folderSubfolders = []
          , folderFiles = [("songcache", runPut $ void $ codecOut bin newCache)]
          } path
