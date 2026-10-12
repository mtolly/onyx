{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           Control.Monad                      (forM_)
import qualified Data.ByteString                    as B
import           Data.List                          (isPrefixOf, sort)
import           Distribution.PackageDescription    as PD
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup          (BuildFlags, ConfigFlags)
import           System.Directory                   (createDirectoryIfMissing,
                                                     doesDirectoryExist,
                                                     doesFileExist,
                                                     listDirectory,
                                                     makeAbsolute,
                                                     removeDirectoryRecursive)
import           System.FilePath                    (takeExtension, (<.>),
                                                     (</>))
import           System.Info                        (os)
import           System.Process                     (CreateProcess (..), proc,
                                                     readCreateProcess,
                                                     readProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook  = myConfHook
  , buildHook = myBuildHook
  }

--------------------------------------------------------------------------------
-- FLUID code generation

-- | Where the .fl layouts live, relative to the package directory.
fluidDir :: FilePath
fluidDir = "fluid"

-- | Where fluid writes the .cxx/.h it generates. Not checked into git; the
-- build always regenerates it from the .fl files.
genDir :: FilePath
genDir = "gen"

fluidProgram :: FilePath
fluidProgram = ".." </> ".." </> "dependencies" </> "root" </> "bin" </> "fluid"

-- | Base names (no extension) of every layout under 'fluidDir'.
listFluidLayouts :: IO [String]
listFluidLayouts = do
  exists <- doesDirectoryExist fluidDir
  if not exists then return [] else do
    fs <- listDirectory fluidDir
    return $ sort [ take (length f - 3) f | f <- fs, takeExtension f == ".fl" ]

-- | Run fluid over every .fl file, leaving the results in 'genDir'.
--
-- Output is written into a staging directory first and only copied over when
-- the contents actually differ, so that an unchanged layout doesn't retrigger
-- a C++ (and therefore Haskell) recompile on every build.
--
-- fluid emits @#include "<the -h argument>"@ verbatim, so it has to run with
-- its working directory set to the output directory and be given bare names.
generateFluid :: IO ()
generateFluid = do
  names <- listFluidLayouts
  if null names then return () else do
    let stageDir = genDir </> "stage"
    createDirectoryIfMissing True stageDir
    -- resolved before we set cwd below, which would otherwise be what a
    -- relative program path is looked up against
    fluidExe <- makeAbsolute fluidProgram
    forM_ names $ \name -> do
      let flFile = ".." </> ".." </> fluidDir </> name <.> "fl"
      _ <- readCreateProcess
        (proc fluidExe ["-c", "-o", name <.> "cxx", "-h", name <.> "h", flFile])
          { cwd = Just stageDir }
        ""
      forM_ [name <.> "cxx", name <.> "h"] $ \f ->
        copyIfChanged (stageDir </> f) (genDir </> f)
    removeDirectoryRecursive stageDir

copyIfChanged :: FilePath -> FilePath -> IO ()
copyIfChanged src dst = do
  new <- B.readFile src
  exists <- doesFileExist dst
  same <- if exists then (new ==) <$> B.readFile dst else return False
  if same then return () else B.writeFile dst new

-- | Add the generated sources to a component's build info. The file list is
-- fixed at configure time, so adding a brand new .fl file needs a reconfigure;
-- editing an existing one only needs a build.
addFluidSources :: [String] -> BuildInfo -> BuildInfo
addFluidSources names bi = bi <> mempty
  { cxxSources  = [ genDir </> name <.> "cxx" | name <- names ]
  , includeDirs = [ genDir ]
  }

--------------------------------------------------------------------------------

myBuildHook
  :: PD.PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
myBuildHook description localBuildInfo hooks flags = do
  generateFluid
  buildHook simpleUserHooks description localBuildInfo hooks flags

myConfHook
  :: (PD.GenericPackageDescription, PD.HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
myConfHook (description, buildInfo) flags = do

  -- generate first, so the .cxx files exist by the time Cabal looks at them
  generateFluid
  fluidNames <- listFluidLayouts

  -- borrowing this logic from fltkhs to apply fltk-config output to cabal

  let (prog, args) = case os of
        -- fltk-config is a shell script!
        "mingw32" -> ("sh", ["../../dependencies/root/bin/fltk-config"])
        _         -> ("../../dependencies/root/bin/fltk-config", [])
  -- TODO `words` doesn't handle paths with spaces? unlikely to be an issue though
  allFlags <- words <$> readProcess prog
    -- seems like our current fltk build only makes static .a anyway?
    -- fine either static/dynamic but this way we can just add as extra-libraries dependency
    (args <> ["--ldflags", "--use-gl", "--use-images"]) ""
  let applyFlags (f : fs) bi | "-l" `isPrefixOf` f
        = applyFlags fs $ bi <> mempty { extraLibs = [drop 2 f], extraGHCiLibs = [drop 2 f] }
      applyFlags (f : fs) bi | "-L" `isPrefixOf` f
        = applyFlags fs $ bi <> mempty { extraLibDirs = [drop 2 f] }
      applyFlags ("-framework" : f : fs) bi
        = applyFlags fs $ bi <> mempty { frameworks = [f] }
      applyFlags ("-weak_framework" : f : fs) bi
        -- doesn't look like Cabal supports "weak"
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
        { PD.buildInfo
          -- only apply to the GUI executable, not CLI
          = if PD.unUnqualComponentName (PD.exeName exe) == "onyx"
            then addFluidSources fluidNames $ applyFlags allFlags $ PD.buildInfo exe
            else PD.buildInfo exe
        }
      }
    }
