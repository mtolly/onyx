{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.Ark (replaceSong, GameGH(..), detectGameGH, readSongListGH2, readSongListGH1, createHdrArk, GH2InstallLocation(..), addBonusSongGH2, addBonusSongGH1, GH2Installation(..)) where

import           Amplitude.PS2.Ark              (FoundFile (..), makeStringBank,
                                                 traverseFolder)
import           ArkTool
import           Codec.Compression.GZip         (decompress)
import           Control.Monad.Extra            (firstJustM, forM, forM_, guard,
                                                 void)
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Put                (putInt32le, putWord32le,
                                                 runPut)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import qualified Data.DTA                       as D
import qualified Data.DTA.Serialize             as D
import qualified Data.DTA.Serialize.GH1         as GH1
import qualified Data.DTA.Serialize.GH2         as GH2
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as HM
import           Data.List.Extra                (nubOrd, sortOn)
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Maybe
import           Data.SimpleHandle              (Folder (..), findFile,
                                                 findFolder, handleToByteString,
                                                 useHandle)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeLatin1)
import           Harmonix.Ark
import           OSFiles                        (fixFileCase)
import qualified System.IO                      as IO
import           System.IO.Temp                 (withSystemTempFile)

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

readSongListGH2 :: (SendMessage m) => D.DTA B.ByteString -> StackTraceT m [(T.Text, GH2.SongPackage)]
readSongListGH2 dta = let
  editDTB d = d { D.topTree = editTree $ D.topTree d }
  editTree t = t { D.treeChunks = concatMap createEggs $ filter keepChunk $ D.treeChunks t }
  keepChunk = \case
    D.Parens tree -> not $ any isIgnore $ D.treeChunks tree
    _             -> False
  isIgnore = \case
    D.Parens (D.Tree _ [D.Sym "validate_ignore", D.Sym "TRUE"]) -> True
    _                                                           -> False
  -- Look for GH2DX 2.0 scripting like: {if_else $egg5 "Speed Test (HalfDuck Cover)" "Speed Test"}
  -- and duplicate songs into: (songkey ...) (songkey_egg5 ...)
  createEggs = \case
    D.Parens (D.Tree x (D.Sym topKey : chunks)) -> do
      pattern <- sequence $ map (\v -> [(v, False), (v, True)]) $ nubOrd $ findEggs chunks
      let topKey' = T.intercalate "_" $ topKey : map fst (filter snd pattern)
          chunks' = foldr ($) chunks $ map (uncurry evalEgg) pattern
      return $ D.Parens $ D.Tree x $ D.Sym topKey' : chunks'
    x -> return x -- shouldn't happen
  findEggs chunks = chunks >>= \case
    D.Parens (D.Tree _ sub)                                -> findEggs sub
    D.Braces (D.Tree _ [D.Sym "if_else", D.Var var, _, _]) -> [var]
    _                                                      -> []
  evalEgg var bool = map $ \case
    D.Parens (D.Tree x sub) -> D.Parens $ D.Tree x $ evalEgg var bool sub
    D.Braces (D.Tree _ [D.Sym "if_else", D.Var var', t, f]) | var == var'
      -> if bool then t else f
    x -> x
  in fmap D.fromDictList
    $ D.unserialize (D.chunksDictList D.chunkSym D.stackChunks)
    $ editDTB $ decodeLatin1 <$> dta

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
  :: FilePath
  -> IO (Maybe GameGH)
detectGameGH inputHdr = do
  hdrPath <- fixFileCase inputHdr
  hdr <- BL.readFile hdrPath >>= readHdr
  arks <- getArkReadables hdr hdrPath
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
  , gh2i_sort             :: Bool -- ^ should bonus songs be sorted alphabetically
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
                bonusSorted <- if gh2i_sort
                  -- TODO handle warnings/errors better
                  then logStdout (readSongListGH2 $ D.DTA 0 $ D.Tree 0 newSongs) >>= \case
                    Right newSongList -> let
                      -- TODO use case fold sort (don't put lowercase letters after all uppercase)
                      f = \case
                        D.Parens (D.Tree _ [D.Sym sym, _]) -> case lookup (T.pack $ B8.unpack sym) newSongList of
                          Just pkg -> GH2.name pkg
                          Nothing  -> ""
                        _                                  -> ""
                      in return $ sortOn f bonus'
                    Left _ -> return bonus'
                  else return bonus'
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
              bonusSorted <- if gh2i_sort
                -- TODO handle warnings/errors better
                then logStdout (readSongListGH1 $ D.DTA 0 $ D.Tree 0 newSongs) >>= \case
                  Right newSongList -> let
                    -- TODO use case fold sort (don't put lowercase letters after all uppercase)
                    f = \case
                      D.Parens (D.Tree _ [D.Sym sym, _]) -> case lookup (T.pack $ B8.unpack sym) newSongList of
                        Just pkg -> GH1.name pkg
                        Nothing  -> ""
                      _                                  -> ""
                    in return $ sortOn f bonus'
                  Left _ -> return bonus'
                else return bonus'
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
