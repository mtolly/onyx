{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Harmonix.Ark.GH2
( sortSongs, SongSort(..)
, replaceSong
, GameGH(..), detectGameGH
, GH2DXExtra(..), readSongListGH2Extra
, readSongListGH2, readSongListGH1
, createHdrArk
, GH2InstallLocation(..)
, addBonusSongGH2, addBonusSongGH1
, GH2Installation(..)
) where

import           Codec.Compression.GZip          (decompress)
import           Control.Applicative             ((<|>))
import           Control.Monad.Extra             (concatForM, firstJustM, forM,
                                                  forM_, guard, void)
import           Control.Monad.Trans.State       (runStateT)
import           Data.Binary.Put                 (putInt32le, putWord32le,
                                                  runPut)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as B8
import qualified Data.ByteString.Lazy            as BL
import           Data.Foldable                   (toList)
import qualified Data.HashMap.Strict             as HM
import           Data.Int                        (Int32)
import           Data.List.Extra                 (nubOrd, sortOn)
import           Data.List.NonEmpty              (NonEmpty ((:|)))
import           Data.Maybe
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeLatin1)
import           Onyx.Harmonix.Ark
import           Onyx.Harmonix.Ark.Amplitude     (FoundFile (..),
                                                  makeStringBank,
                                                  traverseFolder)
import           Onyx.Harmonix.Ark.ArkTool       hiding (FileEntry)
import qualified Onyx.Harmonix.DTA               as D
import qualified Onyx.Harmonix.DTA.Run           as Run
import qualified Onyx.Harmonix.DTA.Serialize     as D
import qualified Onyx.Harmonix.DTA.Serialize.GH1 as GH1
import qualified Onyx.Harmonix.DTA.Serialize.GH2 as GH2
import           Onyx.StackTrace
import           Onyx.Util.Handle                (Folder (..), Readable,
                                                  findFile, findFolder,
                                                  handleToByteString, useHandle)
import qualified System.IO                       as IO
import           System.IO.Temp                  (withSystemTempFile)

-- Haskell extract/repack stuff

createHdrArk :: FilePath -> FilePath -> FilePath -> IO ()
createHdrArk dout hdr ark = do
  files <- traverseFolder dout
  let strings = nubOrd $ files >>= \ff -> ff.arkName : toList ff.arkParent
      (stringBank, offsets) = makeStringBank strings
      stringToIndex = HM.fromList $ zip strings [0..]
      getStringIndex str = case HM.lookup str stringToIndex of
        Nothing -> fail "panic! couldn't find string offset in bank"
        Just i  -> return i
      fileCount = length files
      stringCount = HM.size stringToIndex
  (entries, arkSize) <- IO.withBinaryFile ark IO.WriteMode $ \h -> do
    entries <- forM files $ \ff -> do
      posn <- IO.hTell h
      contents <- BL.fromStrict <$> B.readFile ff.onDisk
      BL.hPut h contents
      return FileEntry
        { offset = fromIntegral posn
        , name = ff.arkName
        , folder = ff.arkParent
        , size = fromIntegral $ BL.length contents
        , inflate = if ff.gzip -- should always be false in GH files
          then fromIntegral $ BL.length $ decompress contents
          else 0
        }
    arkSize <- IO.hTell h
    return (entries, arkSize)
  IO.withBinaryFile hdr IO.WriteMode $ \h -> do
    let w32 = BL.hPut h . runPut . putWord32le
        i32 = BL.hPut h . runPut . putInt32le
    w32 3
    w32 1
    w32 1
    w32 $ fromIntegral arkSize
    w32 $ fromIntegral $ BL.length stringBank
    BL.hPut h stringBank
    w32 $ fromIntegral stringCount
    mapM_ w32 offsets
    w32 $ fromIntegral fileCount
    forM_ entries $ \entry -> do
      w32 $ fromIntegral entry.offset
      getStringIndex entry.name >>= i32
      maybe (return (-1)) getStringIndex entry.folder >>= i32
      w32 entry.size
      w32 entry.inflate

data GH2DXExtra = GH2DXExtra
  { songalbum      :: Maybe T.Text
  , author         :: Maybe T.Text
  , songyear       :: Maybe T.Text
  , songgenre      :: Maybe T.Text
  , songorigin     :: Maybe T.Text
  , songduration   :: Maybe Int32
  , songguitarrank :: Maybe Int32
  , songbassrank   :: Maybe Int32
  , songrhythmrank :: Maybe Int32
  , songdrumrank   :: Maybe Int32
  , songartist     :: Maybe T.Text
  }

-- Parses song info by preprocessing and evaluating the DTA file.
-- * Extracts extra GH2DX info by evaluating {set ...} commands
-- * For songs with GH2DX "easter eggs", duplicates a song into multiple
--   songs for each possible combination of egg flag variables
readSongListGH2Extra :: (SendMessage m) => D.DTA T.Text -> StackTraceT m [(T.Text, GH2.SongPackage, GH2DXExtra)]
readSongListGH2Extra (D.DTA _ (D.Tree _ chunks)) = do
  preproc <- Run.runPreprocess chunks
  let isIgnore = \case
        -- this uses 'TRUE' in the .dta, but we preprocessed it to 1
        D.Parens (D.Tree _ [D.Sym "validate_ignore", D.Int 1]) -> True
        _                                                      -> False
      -- Look for GH2DX 2.0 scripting like: {if_else $egg5 "Speed Test (HalfDuck Cover)" "Speed Test"}
      -- so we can duplicate songs into: (songkey ...) (songkey_egg5 ...)
      findEggs :: [D.Chunk T.Text] -> [T.Text]
      findEggs xs = xs >>= \case
        D.Parens (D.Tree _ sub)                                -> findEggs sub
        D.Braces (D.Tree _ [D.Sym "if_else", D.Var var, _, _]) -> [var]
        _                                                      -> []
      eggCombinations :: [T.Text] -> [(T.Text, [D.Chunk T.Text])]
      eggCombinations eggs = do
        combo <- sequence $ map (\egg -> [(egg, False), (egg, True)]) eggs
        let comboName = T.concat [ "_" <> egg | (egg, True) <- combo ]
            comboSets = flip map combo $ \(egg, b) -> D.Braces $ D.Tree 0
              [ D.Sym "set"
              , D.Var egg
              , D.Int $ if b then 1 else 0
              ]
        return (comboName, comboSets)
  fmap concat $ forM preproc $ \chunk -> case chunk of
    D.Parens (D.Tree _ (D.Sym shortName : rest)) -> if any isIgnore rest
      then return []
      else do
        let combos = case eggCombinations $ nubOrd $ findEggs rest of
              [] -> [("", [])]
              xs -> xs
        concatForM combos $ \(suffix, setters) -> do
          let runner = do
                mapM_ Run.evaluateDTA setters
                Run.evaluateDTA $ D.Parens $ D.Tree 0 $ D.Sym (shortName <> suffix) : rest
          (newChunks, finalState) <- flip mapStackTraceT runner $ \x -> do
            (result, finalState) <- runStateT x Run.initDTAState
            return $ (, finalState) <$> result
          pairs <- fmap D.fromDictList
            $ D.unserialize (D.chunksDictList D.chunkSym D.stackChunks)
            $ D.DTA 0 $ D.Tree 0 newChunks
          let retrieveText k = case HM.lookup k finalState.variables of
                Just [D.String x] -> Just x
                _                 -> Nothing
              retrieveInt k = case HM.lookup k finalState.variables of
                Just [D.Int x] -> Just x
                _              -> Nothing
              extra = GH2DXExtra
                { songalbum      = retrieveText "songalbum"
                , author         = retrieveText "author"
                , songyear       = retrieveText "songyear"
                , songgenre      = retrieveText "songgenre"
                , songorigin     = retrieveText "songorigin"
                , songduration   = retrieveInt  "songduration"
                , songguitarrank = retrieveInt  "songguitarrank"
                , songbassrank   = retrieveInt  "songbassrank"
                , songrhythmrank = retrieveInt  "songrhythmrank"
                , songdrumrank   = retrieveInt  "songdrumrank"
                , songartist     = retrieveText "songartist"
                }
          return [ (k, pkg, extra) | (k, pkg) <- pairs ]
    _ -> return []

readSongListGH2 :: (SendMessage m) => D.DTA B.ByteString -> StackTraceT m [(T.Text, GH2.SongPackage)]
readSongListGH2
  = fmap (map $ \(t, pkg, _extra) -> (t, pkg))
  . readSongListGH2Extra
  . fmap decodeLatin1

readSongListGH1 :: (SendMessage m) => D.DTA B.ByteString -> StackTraceT m [(T.Text, GH1.SongPackage)]
readSongListGH1 dta = let
  editDTB d = d { D.topTree = editTree $ D.topTree d }
  editTree t = t { D.treeChunks = filter keepChunk $ D.treeChunks t }
  keepChunk = \case
    D.Parens _ -> True
    _          -> False
  in fmap D.fromDictList
    $ D.unserialize (D.chunksDictList D.chunkSym D.stackChunks)
    $ editDTB $ decodeLatin1 <$> dta

-- ArkTool stuff

data GameGH = GameGH1 | GameGH2 | GameGH2DX2 | GameRB
  deriving (Show)

-- TODO: find better way to do this.
-- for comparison, Guitar Wizard looked for existence of
-- "songs/aceofspades/aceofspades.mid", etc., which seems less resilient
detectGameGH
  :: Folder T.Text Readable
  -> IO (Maybe GameGH)
detectGameGH gen = do
  (hdr, arks) <- loadGEN gen
  let folder = entryFolder hdr
      mids = do
        songDir <- toList $ findFolder ["songs"] folder
        song <- map snd $ folderSubfolders songDir
        (name, entry) <- folderFiles song
        guard $ ".mid" `B.isSuffixOf` name
        return entry
      checkEntry entry = do
        -- could parse whole midi but this is fine
        r <- readFileEntry hdr arks entry
        bs <- BL.toStrict <$> useHandle r handleToByteString
        return $ if
          | "T1 GEMS"     `B.isInfixOf` bs -> Just GameGH1
          | "PART GUITAR" `B.isInfixOf` bs -> Just $ if isStockGH2 then GameGH2 else GameGH2DX2
          | otherwise                      -> Nothing
      isStockGH2 = isJust $ do
        configGen <- findFolder ["config", "gen"] folder
        lookup "gh2_version.dtb" $ folderFiles configGen
  case findFile ("songs" :| ["gen", "songs.dtb"]) folder of
    Just _  -> return $ Just GameRB
    Nothing -> firstJustM checkEntry mids

data GH2InstallLocation
  = GH2Replace B.ByteString
  | GH2AddTier Int
  | GH2AddBonus

-- | Replaces a song in Guitar Hero II (U).
replaceSong
  :: FilePath -- ^ the @GEN@ folder containing ark and hdr files
  -> B.ByteString -- ^ the folder name (and DTB symbol) of the song to replace
  -> [D.Chunk B.ByteString] -- ^ the DTB snippet to insert into @config\/gen\/songs.dtb@
  -> [(B.ByteString, FilePath)] -- ^ the files to copy into the song folder, e.g. @("yyz.mid", "some/dir/notes.mid")@
  -> IO ()
replaceSong gen sym snippet files = withArk gen $ \ark -> do
  withSystemTempFile "songs.dtb" $ \fdtb hdl -> do
    IO.hClose hdl
    ark_GetFile ark fdtb "config/gen/songs.dtb" True
    D.DTA z (D.Tree _ chunks) <- D.readFileDTB fdtb
    let adjust chunk = case chunk of
          D.Parens (D.Tree _ (D.Sym k : _)) | k == sym ->
            D.Parens $ D.Tree 0 $ D.Sym k : map adjustSnippet snippet
          _ -> chunk
        adjustSnippet = \case
          D.String s   -> D.String $ adjustString s
          D.Parens t   -> D.Parens $ adjustTree t
          D.Braces t   -> D.Braces $ adjustTree t
          D.Brackets t -> D.Brackets $ adjustTree t
          chunk        -> chunk
        adjustTree (D.Tree tid cks) = D.Tree tid $ map adjustSnippet cks
        adjustString str = case B.breakSubstring "$SONGKEY" str of
          (h, t) | not $ B.null t -> adjustString $ h <> sym <> B.drop 8 t
          _                       -> str
    D.writeFileDTB fdtb $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 $ map adjust chunks
    ark_ReplaceAFile ark fdtb "config/gen/songs.dtb" True
    entries <- searchFiles ark $ "songs/" <> sym <> "/*"
    forM_ entries $ \entry -> do
      name <- ark_Arkname entry
      ark_RemoveFile ark name
    forM_ files $ \(arkName, localPath) -> do
      let arkPath = "songs/" <> sym <> "/" <> arkName
      ark_AddFile ark localPath arkPath True -- encryption doesn't matter
    ark_Save ark

data GH2Installation = GH2Installation
  { gen              :: FilePath
  , symbol           :: B.ByteString -- ^ folder name, and key symbol in songs.dta
  , song             :: [D.Chunk B.ByteString] -- ^ info for songs.dta
  , coop_max_scores  :: [Int]
  -- TODO maybe contexts and leaderboards for Xbox only
  , shop_title       :: Maybe B.ByteString
  , shop_description :: Maybe B.ByteString
  , author           :: Maybe B.ByteString
  , album_art        :: Maybe FilePath
  , files            :: [(B.ByteString, FilePath)] -- ^ files to copy into the song folder, e.g. @("songsym.mid", "some/dir/notes.mid")@
  , sort_            :: Maybe SongSort -- ^ should bonus songs be sorted alphabetically
  , loading_phrase   :: Maybe B.ByteString
  , gh2Deluxe        :: Bool
  }

-- | Adds a song to a GH2 ARK, and registers it as a bonus song with price 0.
-- Returns a list of warnings for missing optional .dtb files.
addBonusSongGH2 :: GH2Installation -> IO [String]
addBonusSongGH2 gh2i = withArk gh2i.gen $ \ark -> do
  withSystemTempFile "songs.dtb"             $ \fdtb1 hdl1 -> do
    withSystemTempFile "coop_max_scores.dtb" $ \fdtb2 hdl2 -> do
      withSystemTempFile "store.dtb"         $ \fdtb3 hdl3 -> do
        withSystemTempFile "locale.dtb"      $ \fdtb4 hdl4 -> do
          IO.hClose hdl1
          IO.hClose hdl2
          IO.hClose hdl3
          IO.hClose hdl4
          let editDTB tmp path f = ark_TryGetFile ark tmp path True >>= \case
                True -> do
                  ark_GetFile ark tmp path True
                  D.DTA z (D.Tree _ chunks) <- D.readFileDTB tmp
                  chunks' <- f chunks
                  D.writeFileDTB tmp $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 chunks'
                  ark_ReplaceAFile ark tmp path True
                  return $ Just chunks'
                False -> return Nothing
              tryEditDTB tmp path f = editDTB tmp path f >>= \case
                Just _  -> return []
                Nothing -> return ["Couldn't find optional .dtb: " <> B8.unpack path]
          songResult <- editDTB fdtb1 "config/gen/songs.dtb" $ \chunks -> do
            return $ D.Parens (D.Tree 0 (D.Sym gh2i.symbol : gh2i.song)) : chunks
          newSongs <- maybe
            (fail "Failed to extract config/gen/songs.dtb when installing a new song")
            return
            songResult
          warnings <- fmap concat $ sequence
            [ tryEditDTB fdtb2 "config/gen/coop_max_scores.dtb" $ \chunks -> do
                return $ D.Parens (D.Tree 0 [D.Sym gh2i.symbol, D.Parens $ D.Tree 0 (map (D.Int . fromIntegral) gh2i.coop_max_scores)]) : chunks
            , tryEditDTB fdtb3 "config/gen/store.dtb" $ \chunks -> do
                forM chunks $ \case
                  D.Parens (D.Tree _ bonus@(D.Sym "song" : _)) -> do
                    let bonus' = bonus <> [D.Parens $ D.Tree 0 [D.Sym gh2i.symbol, D.Parens $ D.Tree 0 [D.Sym "price", D.Int 0]]]
                    bonusSorted <- case gh2i.sort_ of
                      -- TODO handle warnings/errors better
                      Just ss -> logStdout (readSongListGH2 $ D.DTA 0 $ D.Tree 0 newSongs) >>= \case
                        Right newSongList -> return $ let
                          sorter = sortSongs ss $ \case
                            D.Parens (D.Tree _ [D.Sym sym, _]) -> case lookup (T.pack $ B8.unpack sym) newSongList of
                              Just pkg -> (GH2.name pkg, GH2.artist pkg)
                              Nothing  -> (T.pack $ show sym, "")
                            _                                  -> ("", "")
                          in sorter bonus'
                        Left _ -> return bonus'
                      Nothing -> return bonus'
                    return $ D.Parens $ D.Tree 0 bonusSorted
                  chunk -> return chunk
            , tryEditDTB fdtb4 "ui/eng/gen/locale.dtb" $ \chunks -> do
                return $ catMaybes
                  [ (\x -> D.Parens (D.Tree 0 [D.Sym gh2i.symbol                    , D.String x])) <$> gh2i.shop_title
                  , (\x -> D.Parens (D.Tree 0 [D.Sym $ gh2i.symbol <> "_shop_desc"  , D.String x])) <$> gh2i.shop_description
                  , (\x -> D.Parens (D.Tree 0 [D.Sym $ gh2i.symbol <> "_author"     , D.String x])) <$> (guard gh2i.gh2Deluxe >> gh2i.author)
                  , (\x -> D.Parens (D.Tree 0 [D.Sym $ "loading_tip_" <> gh2i.symbol, D.String x])) <$> gh2i.loading_phrase
                  ] <> chunks
            ]
          forM_ gh2i.files $ \(arkName, localPath) -> do
            let arkPath = "songs/" <> gh2i.symbol <> "/" <> arkName
            ark_AddFile ark localPath arkPath True -- encryption doesn't matter
          forM_ gh2i.album_art $ \img -> do
            ark_AddFile ark img ("ui/image/og/gen/us_logo_" <> gh2i.symbol <> "_keep.png_ps2") True
          ark_Save ark
          return warnings

-- | Adds a song to a GH1 ARK, and registers it as a bonus song with price 0.
addBonusSongGH1 :: GH2Installation -> IO ()
addBonusSongGH1 gh2i = withArk gh2i.gen $ \ark -> do
  withSystemTempFile "songs.dtb"      $ \fdtb1 hdl1 -> do
    withSystemTempFile "store.dtb"    $ \fdtb2 hdl2 -> do
      withSystemTempFile "locale.dtb" $ \fdtb3 hdl3 -> do
        IO.hClose hdl1
        IO.hClose hdl2
        IO.hClose hdl3
        let editDTB tmp path f = do
              ark_GetFile ark tmp path True
              D.DTA z (D.Tree _ chunks) <- D.readFileDTB tmp
              chunks' <- f chunks
              D.writeFileDTB tmp $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 chunks'
              ark_ReplaceAFile ark tmp path True
              return chunks'
        newSongs <- editDTB fdtb1 "config/gen/songs.dtb" $ \chunks -> do
          let (before, after) = case chunks of
                -- put new songs after "#include ../charsys/band_chars.dta"
                inc@(D.Include _) : rest -> ([inc], rest  )
                _                        -> ([]   , chunks)
          return $ before <> (D.Parens (D.Tree 0 (D.Sym gh2i.symbol : gh2i.song)) : after)
        void $ editDTB fdtb2 "config/gen/store.dtb" $ \chunks -> do
          forM chunks $ \case
            D.Parens (D.Tree _ bonus@(D.Sym "song" : _)) -> do
              let bonus' = bonus <> [D.Parens $ D.Tree 0 [D.Sym gh2i.symbol, D.Parens $ D.Tree 0 [D.Sym "price", D.Int 0]]]
              bonusSorted <- case gh2i.sort_ of
                -- TODO handle warnings/errors better
                Just ss -> logStdout (readSongListGH1 $ D.DTA 0 $ D.Tree 0 newSongs) >>= \case
                  Right newSongList -> return $ let
                    sorter = sortSongs ss $ \case
                      D.Parens (D.Tree _ [D.Sym sym, _]) -> case lookup (T.pack $ B8.unpack sym) newSongList of
                        Just pkg -> (GH1.name pkg, GH1.artist pkg)
                        Nothing  -> (T.pack $ show sym, "")
                      _                                  -> ("", "")
                    in sorter bonus'
                  Left _ -> return bonus'
                Nothing -> return bonus'
              return $ D.Parens $ D.Tree 0 bonusSorted
            chunk -> return chunk
        void $ editDTB fdtb3 "ghui/eng/gen/locale.dtb" $ \chunks -> do
          return $ catMaybes
            [ (\x -> D.Parens (D.Tree 0 [D.Sym $ gh2i.symbol <> "_shop_desc"  , D.String x])) <$> gh2i.shop_description
            , (\x -> D.Parens (D.Tree 0 [D.Sym $ "loading_tip_" <> gh2i.symbol, D.String x])) <$> gh2i.loading_phrase
            ] <> chunks
        forM_ gh2i.files $ \(arkName, localPath) -> do
          let arkPath = "songs/" <> gh2i.symbol <> "/" <> arkName
          ark_AddFile ark localPath arkPath True -- encryption doesn't matter
        ark_Save ark

data SongSort
  = SongSortTitleArtist
  | SongSortArtistTitle

sortSongs :: SongSort -> (a -> (T.Text, T.Text)) -> [a] -> [a]
sortSongs ss getTitleArtist = sortOn $ \song -> let
  (title, artist) = getTitleArtist song
  sortForm x = let
    folded = T.toCaseFold x
    in fromMaybe folded
      $   T.stripPrefix "the " folded
      <|> T.stripPrefix "a "   folded
      <|> T.stripPrefix "an "  folded
  in case ss of
    SongSortTitleArtist -> (sortForm title , sortForm artist)
    SongSortArtistTitle -> (sortForm artist, sortForm title )
