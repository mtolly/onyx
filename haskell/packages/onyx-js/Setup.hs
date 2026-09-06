module Main (main) where

import           Data.Functor                       (void)
import           Data.Maybe                         (fromJust)
import           Distribution.PackageDescription    as PD
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PreProcess
import           Distribution.Simple.Setup          (ConfigFlags, CopyFlags,
                                                     copyDest,
                                                     fromFlagOrDefault)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo,
                                                     emptyHookedBuildInfo)
import           Distribution.Types.Library         (Library (..))
import           System.Directory                   (copyFile,
                                                     createDirectoryIfMissing,
                                                     getCurrentDirectory)
import           System.Environment                 (lookupEnv, setEnv)
import           System.Info                        (os)
import           System.Process                     (CreateProcess (..),
                                                     callProcess, proc,
                                                     readCreateProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preConf = myPreConf, confHook = myConfHook, copyHook = myCopyHook }

-- On my Stack system, for an external package such as a git reference, this gives me something like
-- $HOME/.stack/snapshots/x86_64-linux-tinfo6/fccfa9932e1adafc2949e626b62baeceff08655250fc8a4a10c9ee215c88d179/9.6.2/lib
staticLibOutputDir :: CopyDest -> PackageDescription -> LocalBuildInfo -> FilePath
staticLibOutputDir copyDest' packageDescription localBuildInfo
  = flibdir $ absoluteComponentInstallDirs
    packageDescription
    localBuildInfo
    (localUnitId localBuildInfo)
    copyDest'

-- Run make in the library folder to produce the static library
myPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo
myPreConf _ _ = do
  let buildDir = "quickjs"
  -- library needs to be position-independent, otherwise pulling quickjs-libc.o
  -- out of the archive fails to link
  existing <- lookupEnv "CFLAGS"
  setEnv "CFLAGS" $ maybe "-fPIC" (<> " -fPIC") existing
  -- Windows needs to be set to use Clang, otherwise it tries gcc and fails
  let makeConfig = ["CONFIG_CLANG=y" | os == "mingw32"]
  void $ readCreateProcess (proc "make" ("libquickjs.a" : makeConfig)) { cwd = Just buildDir } ""
  -- copy only the .hs-referenced headers; if we add the whole quickjs root
  -- directory then e.g. VERSION can clobber other random C/C++ files
  -- downstream that #include <version> on case-insensitive systems
  createDirectoryIfMissing True "quickjs-include"
  mapM_ (\h -> copyFile ("quickjs/" <> h) ("quickjs-include/" <> h))
    ["quickjs.h", "quickjs-libc.h"]
  -- Return empty HookedBuildInfo as we are not modifying build info here.
  return emptyHookedBuildInfo

-- Add the directories containing the header files and the static library to the package.
-- Need to jump through hoops because a relative folder is not permitted in extra-lib-dirs
myConfHook
  :: (PD.GenericPackageDescription, PD.HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
myConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  dir <- getCurrentDirectory
  return localBuildInfo
    { localPkgDescr = packageDescription
      { PD.library = Just library
        { PD.libBuildInfo = libraryBuildInfo
          { PD.includeDirs
            = (dir ++ "/quickjs-include")
            : PD.includeDirs libraryBuildInfo
          , PD.extraLibDirs
            = (dir ++ "/quickjs") -- this is needed while building this package
            : staticLibOutputDir NoCopyDest packageDescription localBuildInfo -- this is the permanent place we copy to, needed while linking downstream
            : PD.extraLibDirs libraryBuildInfo
          }
        }
      }
    }

myCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
myCopyHook packageDescription localBuildInfo userHooks copyFlags = do
  copyHook simpleUserHooks packageDescription localBuildInfo userHooks copyFlags
  let outDir = staticLibOutputDir
        (fromFlagOrDefault NoCopyDest (copyDest copyFlags))
        packageDescription
        localBuildInfo
  createDirectoryIfMissing True outDir
  copyFile "quickjs/libquickjs.a" (outDir <> "/libquickjs.a")
