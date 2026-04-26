{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Data.List                          (isPrefixOf)
import           Distribution.PackageDescription    as PD
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup          (ConfigFlags)
import           System.FilePath                    (takeExtension)
import           System.Process                     (readProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

myConfHook
  :: (PD.GenericPackageDescription, PD.HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
myConfHook (description, buildInfo) flags = do

  -- borrowing this logic from fltkhs to apply fltk-config output to cabal

  -- TODO `words` doesn't handle paths with spaces? unlikely to be an issue though
  allFlags <- words <$> readProcess "../../dependencies/root/bin/fltk-config"
    -- seems like our current fltk build only makes static .a anyway?
    -- fine either static/dynamic but this way we can just add as extra-libraries dependency
    ["--ldflags", "--use-gl", "--use-images"] ""
  let applyFlags (f : fs) bi | "-l" `isPrefixOf` f
        = applyFlags fs $ bi <> mempty { extraLibs = [drop 2 f], extraGHCiLibs = [drop 2 f] }
      applyFlags (f : fs) bi | "-L" `isPrefixOf` f
        = applyFlags fs $ bi <> mempty { extraLibDirs = [drop 2 f] }
      applyFlags ("-framework" : f : fs) bi
        = applyFlags fs $ bi <> mempty { frameworks = [f] }
      applyFlags ("-framework-path" : f : fs) bi
        = applyFlags fs $ bi <> mempty { extraFrameworkDirs = [f] }
      applyFlags (f : fs) bi = applyFlags fs $ if archivePath f
        then bi -- shouldn't see currently since using --ldflags not --ldstaticflags
        else bi <> mempty { ldOptions = [f] }
      applyFlags [] bi = bi

      archivePath :: FilePath -> Bool
      archivePath f = takeExtension f == ".a"

  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
  return localBuildInfo
    { localPkgDescr = packageDescription
      { PD.executables = flip map (PD.executables packageDescription) $ \exe -> exe
        { PD.buildInfo = applyFlags allFlags $ PD.buildInfo exe
        }
      }
    }
