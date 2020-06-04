{-# LANGUAGE OverloadedStrings #-}
module Rocksmith.Import where

import           Control.Monad                  (forM)
import           Control.Monad.Trans.Resource   (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.Aeson                     as A
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import           Rocksmith.Base                 (parseFile)
import           Rocksmith.PSARC
import qualified System.Directory               as Dir
import           System.FilePath                ((<.>), (</>))

importRS :: (SendMessage m, MonadResource m) => FilePath -> FilePath -> StackTraceT m ()
importRS psarc dout = tempDir "onyx_rocksmith" $ \temp -> do
  stackIO $ extractPSARC psarc temp
  manifestDirs <- stackIO $ Dir.listDirectory $ temp </> "manifests"
  song <- case manifestDirs of
    -- TODO support index into multiple songs
    [song] -> return song
    _      -> fatal "Not exactly 1 song in .psarc"
  lg $ "Manifest folder: " <> song
  manifest <- stackIO (A.eitherDecodeFileStrict $ temp </> "manifests" </> song </> song <.> "hsan") >>= either fatal return
  let prop k rec = case rec of
        A.Object o -> case HM.lookup k o of
          Nothing -> fatal $ "No key " <> show k <> " in object"
          Just v  -> return v
        _ -> fatal $ "Tried to read key " <> show k <> " of non-object"
      urn urnType o = case o of
        A.String s -> case T.stripPrefix urnType s of
          Just s' -> return s'
          Nothing -> fatal $ "Couldn't read urn of type " <> show urnType
        _ -> fatal $ "Tried to read urn " <> show urnType <> " from non-string"
  A.Object entries <- prop "Entries" manifest
  lg $ show $ HM.keys entries
  parts <- forM (HM.toList entries) $ \(k, entry) -> inside ("Entry " <> T.unpack k) $ do
    attrs <- prop "Attributes" entry
    partJsonPath <- prop "ManifestUrn" attrs >>= urn "urn:database:json-db:"
    part <- stackIO (A.eitherDecodeFileStrict $ temp </> "manifests" </> song </> T.unpack partJsonPath <.> "json") >>= either fatal return
    partAttrs <- prop "Entries" part >>= prop k >>= prop "Attributes"
    partXmlPath <- prop "SongXml" partAttrs >>= urn "urn:application:xml:"
    bnkPath <- prop "SongBank" partAttrs >>= urn ""
    arr <- parseFile $ temp </> "songs/arr" </> T.unpack partXmlPath <.> "xml"
    lg $ take 100 $ show arr
    return ()
  {-
  1. pick manifests/foo
  2. load manifests/foo/foo.hsan (json)
  3. parts located in values of the object.Entries object
  4. for each part:
    1. part.Attributes.ManifestUrn is "urn:database:json-db:thepartname"
    2. load manifests/foo/thepartname.json
    3. object.Entries should only have one key-value pair?
    4. values to read to find the song files:
      thatValue.Attributes.SongXml is "urn:application:xml:something"
        points to songs/arr/something.xml
      thatValue.Attributes.ShowlightsXML is "urn:application:xml:something"
        points to songs/arr/something.xml
      thatValue.Attributes.SongAsset is "urn:application:musicgame-song:something"
        points to songs/bin/generic/something.sng
      thatValue.Attributes.SongBank is "something.bnk"
        points to audio/windows/something.bnk
        (assuming windows could be xbox/ps/mac or something)
      thatValue.Attributes.AlbumArt is "urn:image:dds:album_cuspikotaroppap"
        points to gfxassets/album_art/something_size.dds
        where size is 64, 128, 256
        (this is also in the .hsan file)
      (note that album art and audio could technically be different between parts,
        but we don't need to support that for now)
    5. use extractRSOgg to follow .bnk and extract .ogg
    6. existing DXT code should work with .dds with some small additions
  -}
  return ()
