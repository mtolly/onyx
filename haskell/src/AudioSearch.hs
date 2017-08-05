module AudioSearch where

data SearchState = SearchState
  { dirsToScan   :: [FilePath]
  , filesToScan  :: [FilePath]
  , scannedFiles :: Map.Map T.Text FilePath
  }



newAudioSearch :: m AudioSearch
newAudioSearch = undefined
