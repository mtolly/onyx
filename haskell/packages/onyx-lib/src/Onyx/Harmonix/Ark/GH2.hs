{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
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
import           Onyx.Harmonix.Ark.ArkTool
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
  let strings = nubOrd $ files >>= \ff -> ff_arkName ff : toList (ff_arkParent ff)
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
      contents <- BL.fromStrict <$> B.readFile (ff_onDisk ff)
      BL.hPut h contents
      return FileEntry
        { fe_offset = fromIntegral posn
        , fe_name = ff_arkName ff
        , fe_folder = ff_arkParent ff
        , fe_size = fromIntegral $ BL.length contents
        , fe_inflate = if ff_gzip ff -- should always be false in GH files
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
      w32 $ fromIntegral $ fe_offset entry
      getStringIndex (fe_name entry) >>= i32
      maybe (return (-1)) getStringIndex (fe_folder entry) >>= i32
      w32 $ fe_size entry
      w32 $ fe_inflate entry

data GH2DXExtra = GH2DXExtra
  { gh2dx_songalbum      :: Maybe T.Text
  , gh2dx_author         :: Maybe T.Text
  , gh2dx_songyear       :: Maybe T.Text
  , gh2dx_songgenre      :: Maybe T.Text
  , gh2dx_songorigin     :: Maybe T.Text
  , gh2dx_songduration   :: Maybe Int32
  , gh2dx_songguitarrank :: Maybe Int32
  , gh2dx_songbassrank   :: Maybe Int32
  , gh2dx_songrhythmrank :: Maybe Int32
  , gh2dx_songdrumrank   :: Maybe Int32
  , gh2dx_songartist     :: Maybe T.Text
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
                { gh2dx_songalbum      = retrieveText "songalbum"
                , gh2dx_author         = retrieveText "author"
                , gh2dx_songyear       = retrieveText "songyear"
                , gh2dx_songgenre      = retrieveText "songgenre"
                , gh2dx_songorigin     = retrieveText "songorigin"
                , gh2dx_songduration   = retrieveInt  "songduration"
                , gh2dx_songguitarrank = retrieveInt  "songguitarrank"
                , gh2dx_songbassrank   = retrieveInt  "songbassrank"
                , gh2dx_songrhythmrank = retrieveInt  "songrhythmrank"
                , gh2dx_songdrumrank   = retrieveInt  "songdrumrank"
                , gh2dx_songartist     = retrieveText "songartist"
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
    ark_GetFile' ark fdtb "config/gen/songs.dtb" True
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
    ark_ReplaceAFile' ark fdtb "config/gen/songs.dtb" True
    entries <- searchFiles ark $ "songs/" <> sym <> "/*"
    forM_ entries $ \entry -> do
      name <- ark_Arkname entry
      ark_RemoveFile' ark name
    forM_ files $ \(arkName, localPath) -> do
      let arkPath = "songs/" <> sym <> "/" <> arkName
      ark_AddFile' ark localPath arkPath True -- encryption doesn't matter
    ark_Save' ark

data GH2Installation = GH2Installation
  { gh2i_GEN              :: FilePath
  , gh2i_symbol           :: B.ByteString -- ^ folder name, and key symbol in songs.dta
  , gh2i_song             :: [D.Chunk B.ByteString] -- ^ info for songs.dta
  , gh2i_coop_max_scores  :: [Int]
  -- TODO maybe contexts and leaderboards for Xbox only
  , gh2i_shop_title       :: Maybe B.ByteString
  , gh2i_shop_description :: Maybe B.ByteString
  , gh2i_author           :: Maybe B.ByteString
  , gh2i_album_art        :: Maybe FilePath
  , gh2i_files            :: [(B.ByteString, FilePath)] -- ^ files to copy into the song folder, e.g. @("songsym.mid", "some/dir/notes.mid")@
  , gh2i_sort             :: Maybe SongSort -- ^ should bonus songs be sorted alphabetically
  , gh2i_loading_phrase   :: Maybe B.ByteString
  }

-- | Adds a song to a GH2 ARK, and registers it as a bonus song with price 0.
addBonusSongGH2 :: GH2Installation -> IO ()
addBonusSongGH2 GH2Installation{..} = withArk gh2i_GEN $ \ark -> do
  withSystemTempFile "songs.dtb"             $ \fdtb1 hdl1 -> do
    withSystemTempFile "coop_max_scores.dtb" $ \fdtb2 hdl2 -> do
      withSystemTempFile "store.dtb"         $ \fdtb3 hdl3 -> do
        withSystemTempFile "locale.dtb"      $ \fdtb4 hdl4 -> do
          IO.hClose hdl1
          IO.hClose hdl2
          IO.hClose hdl3
          IO.hClose hdl4
          let editDTB tmp path f = do
                ark_GetFile' ark tmp path True
                D.DTA z (D.Tree _ chunks) <- D.readFileDTB tmp
                chunks' <- f chunks
                D.writeFileDTB tmp $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 chunks'
                ark_ReplaceAFile' ark tmp path True
                return chunks'
          newSongs <- editDTB fdtb1 "config/gen/songs.dtb" $ \chunks -> do
            return $ D.Parens (D.Tree 0 (D.Sym gh2i_symbol : gh2i_song)) : chunks
          void $ editDTB fdtb2 "config/gen/coop_max_scores.dtb" $ \chunks -> do
            return $ D.Parens (D.Tree 0 [D.Sym gh2i_symbol, D.Parens $ D.Tree 0 (map (D.Int . fromIntegral) gh2i_coop_max_scores)]) : chunks
          void $ editDTB fdtb3 "config/gen/store.dtb" $ \chunks -> do
            forM chunks $ \case
              D.Parens (D.Tree _ bonus@(D.Sym "song" : _)) -> do
                let bonus' = bonus <> [D.Parens $ D.Tree 0 [D.Sym gh2i_symbol, D.Parens $ D.Tree 0 [D.Sym "price", D.Int 0]]]
                bonusSorted <- case gh2i_sort of
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
          void $ editDTB fdtb4 "ui/eng/gen/locale.dtb" $ \chunks -> do
            return $ catMaybes
              [ (\x -> D.Parens (D.Tree 0 [D.Sym gh2i_symbol                    , D.String x])) <$> gh2i_shop_title
              , (\x -> D.Parens (D.Tree 0 [D.Sym $ gh2i_symbol <> "_shop_desc"  , D.String x])) <$> gh2i_shop_description
              , (\x -> D.Parens (D.Tree 0 [D.Sym $ gh2i_symbol <> "_author"     , D.String x])) <$> gh2i_author -- only used by GH2DX
              , (\x -> D.Parens (D.Tree 0 [D.Sym $ "loading_tip_" <> gh2i_symbol, D.String x])) <$> gh2i_loading_phrase
              ] <> chunks
          forM_ gh2i_files $ \(arkName, localPath) -> do
            let arkPath = "songs/" <> gh2i_symbol <> "/" <> arkName
            ark_AddFile' ark localPath arkPath True -- encryption doesn't matter
          forM_ gh2i_album_art $ \img -> do
            ark_AddFile' ark img ("ui/image/og/gen/us_logo_" <> gh2i_symbol <> "_keep.png_ps2") True
          ark_Save' ark

-- | Adds a song to a GH1 ARK, and registers it as a bonus song with price 0.
addBonusSongGH1 :: GH2Installation -> IO ()
addBonusSongGH1 GH2Installation{..} = withArk gh2i_GEN $ \ark -> do
  withSystemTempFile "songs.dtb"      $ \fdtb1 hdl1 -> do
    withSystemTempFile "store.dtb"    $ \fdtb2 hdl2 -> do
      withSystemTempFile "locale.dtb" $ \fdtb3 hdl3 -> do
        IO.hClose hdl1
        IO.hClose hdl2
        IO.hClose hdl3
        let editDTB tmp path f = do
              ark_GetFile' ark tmp path True
              D.DTA z (D.Tree _ chunks) <- D.readFileDTB tmp
              chunks' <- f chunks
              D.writeFileDTB tmp $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 chunks'
              ark_ReplaceAFile' ark tmp path True
              return chunks'
        newSongs <- editDTB fdtb1 "config/gen/songs.dtb" $ \chunks -> do
          let (before, after) = case chunks of
                -- put new songs after "#include ../charsys/band_chars.dta"
                inc@(D.Include _) : rest -> ([inc], rest  )
                _                        -> ([]   , chunks)
          return $ before <> (D.Parens (D.Tree 0 (D.Sym gh2i_symbol : gh2i_song)) : after)
        void $ editDTB fdtb2 "config/gen/store.dtb" $ \chunks -> do
          forM chunks $ \case
            D.Parens (D.Tree _ bonus@(D.Sym "song" : _)) -> do
              let bonus' = bonus <> [D.Parens $ D.Tree 0 [D.Sym gh2i_symbol, D.Parens $ D.Tree 0 [D.Sym "price", D.Int 0]]]
              bonusSorted <- case gh2i_sort of
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
            [ (\x -> D.Parens (D.Tree 0 [D.Sym $ gh2i_symbol <> "_shop_desc"  , D.String x])) <$> gh2i_shop_description
            , (\x -> D.Parens (D.Tree 0 [D.Sym $ "loading_tip_" <> gh2i_symbol, D.String x])) <$> gh2i_loading_phrase
            ] <> chunks
        forM_ gh2i_files $ \(arkName, localPath) -> do
          let arkPath = "songs/" <> gh2i_symbol <> "/" <> arkName
          ark_AddFile' ark localPath arkPath True -- encryption doesn't matter
        ark_Save' ark

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
