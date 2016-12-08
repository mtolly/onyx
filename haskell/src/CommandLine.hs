{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module CommandLine where

import           Config
import qualified Control.Exception              as Exc
import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Reader     (runReaderT)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer
import           Data.Binary                    (Binary, decodeFileOrFail)
import qualified Data.ByteString.Lazy           as BL
import           Data.ByteString.Lazy.Char8     ()
import qualified Data.DTA                       as D
import qualified Data.DTA.Serialize.Magma       as RBProj
import qualified Data.DTA.Serialize.RB3         as D
import qualified Data.DTA.Serialize2            as D
import qualified Data.HashMap.Strict            as Map
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           JSONData                       (traceJSON)
import           Magma                          (oggToMogg, runMagma)
import           MoggDecrypt                    (moggToOgg)
import           PrettyDTA                      (readRB3DTA)
import           ProKeysRanges                  (closeShiftsFile)
import           Reaper.Build                   (makeReaperIO)
import           Reductions                     (simpleReduce)
import qualified RockBand.File                  as RBFile
import qualified Sound.MIDI.File.Load           as Load
import           STFS.Extract                   (extractSTFS)
import           System.Directory               (doesDirectoryExist,
                                                 doesFileExist)
import           System.FilePath                (takeExtension, takeFileName,
                                                 (-<.>), (</>))
import           System.IO.Error                (ioeGetErrorString, tryIOError)
import           X360                           (rb2pkg, rb3pkg)
import           YAMLTree                       (readYAMLTreeStack)

type Match = Writer [Matcher]

data Matcher = Matcher
  { matcherTest  :: T.Text -> StackTraceT IO (Match ())
  , matcherClick :: Click
  , matcherDesc  :: T.Text
  }

-- | What happens when clicking a button in the GUI?
data Click
  = ClickText T.Text -- ^ Add the given text to the end of the command
  | ClickPick [T.Text] -- ^ Launch an Open File dialog, with patterns
  | ClickPickDir -- ^ Launch an Open Folder dialog
  | ClickSave -- ^ Launch a Save File dialog
  | ClickEnd (IO T.Text) -- ^ Do a thing

matchSongYml :: (FilePath -> SongYaml -> Match ()) -> Match ()
matchSongYml cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> cont (T.unpack t) <$> do
    yaml <- readYAMLTreeStack $ T.unpack t
    mapStackTraceT (`runReaderT` yaml) traceJSON
  , matcherClick = ClickPick ["*.yml", "*.yaml"]
  , matcherDesc = "An Onyx song.yml file"
  }

matchTarget :: SongYaml -> (T.Text -> Target -> Match ()) -> Match ()
matchTarget songYaml cont
  = forM_ (Map.toList $ _targets songYaml) $ \(targetName, target)
  -> word targetName "Target" $ cont targetName target

matchPlan :: SongYaml -> (T.Text -> Plan -> Match ()) -> Match ()
matchPlan songYaml cont
  = forM_ (Map.toList $ _plans songYaml) $ \(planName, plan)
  -> word planName "Plan" $ cont planName plan

word :: T.Text -> T.Text -> Match () -> Match ()
word t desc cont = tell $ (: []) $ Matcher
  { matcherTest = \t' -> if t == t'
    then return cont
    else fatal $ "Expected " <> T.unpack t
  , matcherClick = ClickText t
  , matcherDesc = desc
  }

matchBinary :: (Binary a) => T.Text -> (FilePath -> a -> Match ()) -> Match ()
matchBinary desc cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> do
    let f = T.unpack t
    liftIO (doesFileExist f) >>= \case
      True -> return ()
      False -> fatal "File does not exist"
    liftIO (decodeFileOrFail f) >>= \case
      Left err -> inside f $ fatal (show err)
      Right x -> return $ cont f x
  , matcherClick = ClickPick []
  , matcherDesc = desc
  }

matchMagma :: (FilePath -> RBProj.RBProj -> Match ()) -> Match ()
matchMagma cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> do
    let f = T.unpack t
    liftIO (doesFileExist f) >>= \case
      True -> return ()
      False -> fatal "File does not exist"
    -- TODO
    if takeExtension (T.unpack $ T.toLower t) == ".rbproj"
      then do
        dta <- liftIO $ D.readFileDTA f
        rbproj <- D.unserialize D.format dta
        return $ cont f rbproj
      else fatal "Not a .rbproj file"
  , matcherClick = ClickPick ["*.rbproj"]
  , matcherDesc = "A Magma .rbproj file"
  }

outputFile :: T.Text -> (FilePath -> Match ()) -> Match ()
outputFile desc cont = tell $ (: []) $ Matcher
  { matcherTest = return . cont . T.unpack
  , matcherClick = ClickSave
  , matcherDesc = desc
  }

-- | Can be an input directory, or directory to save things into
matchDir :: T.Text -> (FilePath -> Match ()) -> Match ()
matchDir desc cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> liftIO (doesDirectoryExist $ T.unpack fp) >>= \case
    True  -> return $ cont $ T.unpack fp
    False -> fatal "Not a directory"
  , matcherClick = ClickPickDir
  , matcherDesc = desc
  }

end :: T.Text -> IO T.Text -> Match ()
end desc act = tell $ (: []) $ Matcher
  { matcherTest = const $ fatal "Expected end of input"
  , matcherClick = ClickEnd act
  , matcherDesc = desc
  }

matchSTFS :: (FilePath -> Match ()) -> Match ()
matchSTFS cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> liftIO (tryIOError $ BL.readFile $ T.unpack fp) >>= \case
    Left ioe -> fatal $ ioeGetErrorString ioe
    Right lbs -> if BL.take 4 lbs `elem` ["CON ", "LIVE"]
      then return $ cont $ T.unpack fp
      else fatal "Not an STFS (CON/LIVE) file"
  , matcherClick = ClickPick ["*_rb3con"]
  , matcherDesc = "An STFS (CON/LIVE) Xbox 360 package"
  }

matchMIDI :: (FilePath -> Match ()) -> Match ()
matchMIDI cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> liftIO (tryIOError $ BL.readFile $ T.unpack fp) >>= \case
    Left ioe -> fatal $ ioeGetErrorString ioe
    Right lbs -> if BL.take 4 lbs == "MThd"
      then return $ cont $ T.unpack fp
      else fatal "Not a MIDI file"
  , matcherClick = ClickPick ["*.mid", "*.midi"]
  , matcherDesc = "A Standard MIDI File (SMF)"
  }

matchFoF :: (FilePath -> Match ()) -> Match ()
matchFoF cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> if takeFileName (T.unpack $ T.toLower fp) == "song.ini"
    then return $ cont $ T.unpack fp
    else fatal "Not a song.ini"
  , matcherClick = ClickPick ["*.ini"]
  , matcherDesc = "A song.ini for Frets on Fire or Phase Shift"
  }

matchMOGG :: (FilePath -> Match ()) -> Match ()
matchMOGG cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> if takeExtension (T.unpack $ T.toLower fp) == ".mogg"
    then return $ cont $ T.unpack fp
    else fatal "Not a MOGG file"
  , matcherClick = ClickPick ["*.mogg"]
  , matcherDesc = "A Rock Band MOGG file"
  }

matchOGG :: (FilePath -> Match ()) -> Match ()
matchOGG cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> if takeExtension (T.unpack $ T.toLower fp) == ".ogg"
    then return $ cont $ T.unpack fp
    else fatal "Not an OGG file"
  , matcherClick = ClickPick ["*.ogg"]
  , matcherDesc = "An OGG Vorbis file"
  }

matchRBA :: (FilePath -> Match ()) -> Match ()
matchRBA cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> if takeExtension (T.unpack $ T.toLower fp) == ".rba"
    then return $ cont $ T.unpack fp
    else fatal "Not an RBA file"
  , matcherClick = ClickPick ["*.rba"]
  , matcherDesc = "A Magma RBA file"
  }

getInfoForSTFS :: FilePath -> FilePath -> IO (T.Text, T.Text)
getInfoForSTFS dir stfs = do
  let getDTAInfo = do
        (_, pkg, _) <- readRB3DTA $ dir </> "songs/songs.dta"
        return (D.name pkg, D.name pkg <> " (" <> D.artist pkg <> ")")
      handler1 :: Exc.IOException -> IO (T.Text, T.Text)
      handler1 _ = return (T.pack $ takeFileName stfs, T.pack stfs)
      handler2 :: Exc.ErrorCall -> IO (T.Text, T.Text)
      handler2 _ = return (T.pack $ takeFileName stfs, T.pack stfs)
  getDTAInfo `Exc.catch` handler1 `Exc.catch` handler2

withDefaultFilename :: FilePath -> T.Text -> (FilePath -> IO T.Text) -> Match ()
withDefaultFilename f desc cont = do
  end (T.pack f) $ cont f
  outputFile desc $ \out -> end "" $ cont out

commandLine :: Match ()
commandLine = do
  matchSongYml $ \yamlPath songYaml -> do
    matchPlan songYaml $ \planName plan -> do
      undefined
    matchTarget songYaml $ \targetName target -> do
      undefined
  matchMagma $ \rbprojPath rbproj -> do
    word "rba" "Create an RBA file for Audition mode" $ do
      end "Use the RBA path listed in the .rbproj" $ do
        undefined
      outputFile "RBA file to save to" $ \rba -> do
        end "" $ do
          T.pack <$> runMagma rbprojPath rba
    word "con" "Create an Xbox 360 CON-STFS package" $ do
      end "Use the CON path listed in the .rbproj" $ do
        undefined
      outputFile "CON file to save to" $ \con -> do
        end "" $ do
          undefined
  matchSTFS $ \stfs -> do
    word "install" "Install the CON file to an Xbox 360 USB drive" $ do
      end "Find the plugged-in Xbox 360 drive automatically" $ do
        undefined
      matchDir "The USB drive to install to" $ \out -> do
        end "" $ do
          undefined
    word "player" "Generates a browser chart preview app" $ do
      end "Opens the preview in a browser" $ do
        undefined
      outputFile "The new folder to create for the preview app" $ \out -> do
        end "" $ do
          undefined
    word "unstfs" "Extract the contents of the STFS package" $ do
      withDefaultFilename (stfs ++ "_extract") "New folder to extract to" $ \dir -> do
        extractSTFS stfs dir
        return ""
  matchRBA $ \rba -> do
    undefined
  matchMIDI $ \mid -> do
    word "reduce" "Fill in missing difficulties in the MIDI" $ do
      withDefaultFilename (mid -<.> "reduced.mid") "New MIDI location" $ \new -> do
        simpleReduce mid new
        return ""
    word "rpp" "Convert the MIDI to a Reaper project (.RPP)" $ do
      withDefaultFilename (mid -<.> "RPP") "New RPP location" $ \rpp -> do
        makeReaperIO mid mid [] rpp
        return ""
    word "hanging" "List pro keys range shifts with hanging notes" $ do
      end "" $ do
        song <- Load.fromFile mid >>= printStackTraceIO . RBFile.readMIDIFile
        return $ T.pack $ closeShiftsFile song
  matchDir "A folder" $ \dir -> do
    word "stfs" "Make an RB3 CON package from the folder" $ do
      withDefaultFilename (dir ++ "_rb3con") "New CON location" $ \stfs -> do
        (title, desc) <- getInfoForSTFS dir stfs
        T.pack <$> rb3pkg title desc dir stfs
    word "stfs-rb2" "Make an RB2 CON package from the folder" $ do
      withDefaultFilename (dir ++ "_rb2con") "New CON location" $ \stfs -> do
        (title, desc) <- getInfoForSTFS dir stfs
        T.pack <$> rb2pkg title desc dir stfs
  matchMOGG $ \mogg -> do
    word "unmogg" "Unwrap an unencrypted MOGG file into an OGG Vorbis file" $ do
      withDefaultFilename (mogg -<.> "ogg") "New OGG location" $ \ogg -> do
        moggToOgg mogg ogg
        return ""
  matchOGG $ \ogg -> do
    word "mogg" "Wrap an OGG Vorbis file into an unencrypted MOGG file" $ do
      withDefaultFilename (ogg -<.> "mogg") "New OGG location" $ \mogg -> do
        oggToMogg ogg mogg
        return ""
  matchFoF $ \ini -> do
    undefined
