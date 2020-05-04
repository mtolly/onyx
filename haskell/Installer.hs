{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad              (forM_, unless)
import qualified Data.Set                   as Set
import           Data.String                (IsString (..))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Version               (showVersion)
import           Development.NSIS
import           Paths_onyxite_customs_tool (version)
import           System.Directory           (listDirectory, removeFile)
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeExtension, (</>), takeFileName, takeDirectory)
import           System.IO                  (hPutStrLn, stderr)
import           System.Process             (readProcess)

versionString :: (IsString a) => a
versionString = fromString $ showVersion version

main :: IO ()
main = getArgs >>= \args -> case args of

  ["changes"] -> do

    -- Make sure I wrote up the changes for this version
    changes <- T.readFile "CHANGES.md"
    unless (versionString `T.isInfixOf` changes) $ do
      error $ "No changelog written for version " ++ versionString

  ["version-print"] -> do

    -- Write out the version to use it for naming the Mac .zip
    putStr versionString

  "version-write" : files -> do

    -- Insert version string into files specified on command line
    forM_ files $ \f -> do
      txt <- T.readFile f
      date <- case versionString of
        [y1, y2, y3, y4, m1, m2, d1, d2] -> return
          [y1, y2, y3, y4, '-', m1, m2, '-', d1, d2]
        _ -> error "Version string not in 8-character date format"
      T.writeFile f
        $ T.replace "_ONYXDATE_" (T.pack date)
        $ T.replace "_ONYXVERSION_" versionString txt

  ["dlls", exe] -> do

    -- Find which .dll files Onyx requires, delete the others
    let dir = takeDirectory exe
        go (obj : objs) dlls = if Set.member obj dlls
          then go objs dlls
          else do
            lns <- readProcess "ldd" [dir </> obj] ""
            let objdlls
                  = map T.unpack
                  $ concatMap (take 1 . T.words)
                  $ filter ("haskell" `T.isInfixOf`)
                  $ T.lines $ T.pack lns
            go (objs ++ objdlls) (Set.insert obj dlls)
        go [] dlls = return dlls
    dlls <- go [takeFileName exe] Set.empty
    alldlls <- filter ((== ".dll") . takeExtension) <$> listDirectory dir
    forM_ (filter (`Set.notMember` dlls) alldlls) $ \dll -> do
      removeFile $ dir </> dll

  ["nsis"] -> writeFile "installer.nsi" $ nsis $ do

    -- Create Windows installer script
    name "Onyx Music Game Toolkit"
    outFile $ fromString $ "onyx-" ++ versionString ++ "-windows-x64.exe"
    installDir "$PROGRAMFILES64/OnyxToolkit"
    installDirRegKey HKLM "SOFTWARE/OnyxToolkit" "Install_Dir"
    requestExecutionLevel Admin

    page $ License "LICENSE.txt"
    page Components
    page Directory
    page InstFiles
    -- hack to run onyx without admin privilege so drag and drop works
    event "LaunchApplication" $ do
      exec "\"$WINDIR/explorer.exe\" \"$INSTDIR/onyx.exe\""
    unsafeInjectGlobal "!define MUI_FINISHPAGE_RUN_FUNCTION LaunchApplication"
    page $ Finish finishOptions
      { finRunText = "Run Onyx"
      , finRun = " " -- should be empty this works I guess
      , finReadmeText = "View README"
      , finReadme = "$INSTDIR/README.txt"
      , finReadmeChecked = True
      }

    unpage Confirm
    unpage InstFiles

    _ <- section "Onyx" [Required] $ do
      setOutPath "$INSTDIR"
      -- delete existing resource folder and older resource locations
      rmdir [Recursive] "$INSTDIR/onyx-resource"
      rmdir [Recursive] "$INSTDIR/onyx-resources"
      rmdir [Recursive] "$INSTDIR/magma-v1"
      rmdir [Recursive] "$INSTDIR/magma-v2"
      rmdir [Recursive] "$INSTDIR/magma-ogg2mogg"
      rmdir [Recursive] "$INSTDIR/magma-common"
      delete [] "$INSTDIR/*.dll"
      delete [] "$INSTDIR/itaijidict"
      delete [] "$INSTDIR/kanwadict"
      -- copy the files
      file [Recursive] "win/*"
      -- write install path
      writeRegStr HKLM "SOFTWARE/OnyxToolkit" "Install_Dir" "$INSTDIR"
      -- uninstall keys
      writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/OnyxToolkit" "DisplayName" "Onyx Music Game Toolkit"
      writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/OnyxToolkit" "UninstallString" "\"$INSTDIR/uninstall.exe\""
      writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/OnyxToolkit" "NoModify" 1
      writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/OnyxToolkit" "NoRepair" 1
      writeUninstaller "uninstall.exe"

    _ <- section "Start Menu Shortcuts" [] $ do
      createDirectory "$SMPROGRAMS/Onyx Music Game Toolkit"
      createShortcut "$SMPROGRAMS/Onyx Music Game Toolkit/Uninstall.lnk"
        [ Target "$INSTDIR/uninstall.exe"
        , IconFile "$INSTDIR/uninstall.exe"
        , IconIndex 0
        ]
      createShortcut "$SMPROGRAMS/Onyx Music Game Toolkit/Onyx.lnk"
        [ Target "$INSTDIR/onyx.exe"
        , IconFile "$INSTDIR/onyx.exe"
        , IconIndex 0
        ]

    uninstall $ do
      -- Remove registry keys
      deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/OnyxToolkit"
      deleteRegKey HKLM "SOFTWARE/OnyxToolkit"
      -- Remove directories used
      rmdir [Recursive] "$SMPROGRAMS/Onyx Music Game Toolkit"
      rmdir [Recursive] "$INSTDIR"
      rmdir [Recursive] "$LOCALAPPDATA/onyx-log"

  _ -> do
    hPutStrLn stderr "Invalid command."
    exitFailure
