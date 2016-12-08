{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CommandLine where

import qualified Data.Text as T
import Control.Monad.Trans.Writer
import Control.Monad (forM_, void)
import Config
import qualified Data.HashMap.Strict as Map
import YAMLTree (readYAMLTreeStack)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.StackTrace (runStackTraceT, mapStackTraceT)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 ()
import System.IO.Error (tryIOError, ioeGetErrorString)
import JSONData (traceJSON)
import System.FilePath (takeFileName, takeExtension)
import System.Directory (doesDirectoryExist)
import Magma (runMagma)
import Reaper.Build (makeReaperIO)

type Match = Writer [Matcher]

data MatchResult
  = NoMatch
  | MatchError T.Text
  | MatchSuccess (Match ())

data Matcher = Matcher
  { matcherTest  :: T.Text -> IO MatchResult
  , matcherClick :: Click
  , matcherDesc  :: T.Text
  }

-- | What happens when clicking a button in the GUI?
data Click
  = ClickText T.Text -- ^ Add the given text to the end of the command
  | ClickPick [T.Text] -- ^ Launch an Open File dialog, with patterns
  | ClickPickDir -- ^ Launch an Open Folder dialog
  | ClickSave -- ^ Launch a Save File dialog
  | ClickEnd (IO ()) -- ^ Do a thing

matchSongYml :: (FilePath -> SongYaml -> Match ()) -> Match ()
matchSongYml cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> do
    res <- runStackTraceT $ do
      yaml <- readYAMLTreeStack $ T.unpack t
      mapStackTraceT (`runReaderT` yaml) traceJSON
    return $ case res of
      (Left _, _) -> MatchError "Error reading YAML file" -- TODO
      (Right songYaml, _) -> MatchSuccess $ cont (T.unpack t) songYaml
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
  { matcherTest = \t' -> return $ if t == t' then MatchSuccess cont else NoMatch
  , matcherClick = ClickText t
  , matcherDesc = desc
  }

matchMagma :: (FilePath -> Match ()) -> Match ()
matchMagma cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> return $ if takeExtension (T.unpack $ T.toLower fp) == ".rbproj"
    then MatchSuccess $ cont $ T.unpack fp
    else MatchError "Not a .rbproj file"
  , matcherClick = ClickPick ["*.rbproj"]
  , matcherDesc = "A Magma .rbproj file"
  }

outputFile :: T.Text -> (FilePath -> Match ()) -> Match ()
outputFile desc cont = tell $ (: []) $ Matcher
  { matcherTest = return . MatchSuccess . cont . T.unpack
  , matcherClick = ClickSave
  , matcherDesc = desc
  }

-- | Can be an input directory, or directory to save things into
matchDir :: T.Text -> (FilePath -> Match ()) -> Match ()
matchDir desc cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> doesDirectoryExist (T.unpack fp) >>= return . \case
    True  -> MatchSuccess $ cont $ T.unpack fp
    False -> MatchError "Not a directory"
  , matcherClick = ClickPickDir
  , matcherDesc = desc
  }

end :: T.Text -> IO () -> Match ()
end desc act = tell $ (: []) $ Matcher
  { matcherTest = const $ return NoMatch
  , matcherClick = ClickEnd act
  , matcherDesc = desc
  }

matchCON :: (FilePath -> Match ()) -> Match ()
matchCON cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> tryIOError (BL.readFile $ T.unpack fp) >>= return . \case
    Left ioe -> MatchError $ T.pack $ ioeGetErrorString ioe
    Right lbs -> if BL.take 4 lbs `elem` ["CON ", "LIVE"]
      then MatchSuccess $ cont $ T.unpack fp
      else MatchError "Not an STFS (CON/LIVE) file"
  , matcherClick = ClickPick ["*_rb3con"]
  , matcherDesc = "An STFS (CON/LIVE) Xbox 360 package"
  }

matchMIDI :: (FilePath -> Match ()) -> Match ()
matchMIDI cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> tryIOError (BL.readFile $ T.unpack fp) >>= return . \case
    Left ioe -> MatchError $ T.pack $ ioeGetErrorString ioe
    Right lbs -> if BL.take 4 lbs == "MThd"
      then MatchSuccess $ cont $ T.unpack fp
      else MatchError "Not a MIDI file"
  , matcherClick = ClickPick ["*.mid", "*.midi"]
  , matcherDesc = "A Standard MIDI File (SMF)"
  }

matchFoF :: (FilePath -> Match ()) -> Match ()
matchFoF cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> return $ if takeFileName (T.unpack $ T.toLower fp) == "song.ini"
    then MatchSuccess $ cont $ T.unpack fp
    else MatchError "Not a song.ini"
  , matcherClick = ClickPick ["*.ini"]
  , matcherDesc = "A song.ini for Frets on Fire or Phase Shift"
  }

commandLine :: Match ()
commandLine = do
  matchSongYml $ \yamlPath songYaml -> do
    matchPlan songYaml $ \planName plan -> do
      undefined
    matchTarget songYaml $ \targetName target -> do
      undefined
  matchMagma $ \rbproj -> do
    word "rba" "Create an RBA file for Audition mode" $ do
      end "Use the RBA path listed in the .rbproj" $ do
        undefined
      outputFile "RBA file to save to" $ \rba -> do
        end "" $ do
          void $ runMagma rbproj rba
    word "con" "Create an Xbox 360 CON-STFS package" $ do
      end "Use the CON path listed in the .rbproj" $ do
        undefined
      outputFile "CON file to save to" $ \con -> do
        end "" $ do
          undefined
  matchCON $ \con -> do
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
  matchMIDI $ \mid -> do
    word "reduce" "Fill in missing difficulties in the MIDI" $ do
      end "Modifies the MIDI in-place" $ do
        undefined
      outputFile "The new MIDI location" $ \out -> do
        end "" $ do
          undefined
    word "rpp" "Convert the MIDI to a Reaper project (.RPP)" $ do
      end "Places somefile.RPP next to somefile.mid" $ do
        undefined
      outputFile "The RPP location" $ \rpp -> do
        end "" $ do
          makeReaperIO mid mid [] rpp
    word "hanging" "List pro keys range shifts with hanging notes" $ do
      end "" $ do
        undefined
  matchFoF $ \ini -> do
    undefined
