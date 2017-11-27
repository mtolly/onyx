{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad              (forM_, unless)
import           Data.String                (IsString (..))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Version               (showVersion)
import           Development.NSIS
import           Paths_onyxite_customs_tool (version)
import           System.Environment         (getArgs)

versionString :: (IsString a) => a
versionString = fromString $ showVersion version

main :: IO ()
main = do

  -- Make sure I wrote up the changes for this version
  changes <- T.readFile "CHANGES.md"
  unless (versionString `T.isInfixOf` changes) $ do
    error $ "No changelog written for version " ++ versionString

  -- Insert version string into files specified on command line
  args <- getArgs
  forM_ args $ \arg -> do
    txt <- T.readFile arg
    T.writeFile arg $ T.replace "_ONYXVERSION_" versionString txt

  -- Create Windows installer script
  writeFile "installer.nsi" $ nsis $ do

    name "Onyx Music Game Toolkit"
    outFile $ fromString $ "onyx-" ++ versionString ++ "-win32-x64.exe"
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
