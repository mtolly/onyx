{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Util.Handle where

import           Control.Concurrent.MVar       (modifyMVar, newMVar)
import           Control.Exception             (bracket, throwIO)
import           Control.Monad                 (forM, forM_, guard, unless)
import           Control.Monad.Trans.Resource  (MonadResource, allocate,
                                                release)
import           Data.Bifunctor                (Bifunctor (..))
import qualified Data.ByteString               as B
import           Data.ByteString.Internal      (memcpy)
import qualified Data.ByteString.Lazy          as BL
import           Data.ByteString.Lazy.Internal (smallChunkSize)
import           Data.ByteString.Unsafe        (unsafeUseAsCStringLen)
import           Data.Either                   (lefts, rights)
import           Data.IORef
import           Data.List                     (find, sort)
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes, fromMaybe)
import qualified Data.Text                     as T
import           Data.Typeable                 (Typeable)
import           Foreign                       (castPtr)
import           GHC.IO.Buffer                 (newByteBuffer)
import           GHC.IO.BufferedIO
import qualified GHC.IO.Device                 as D
import           GHC.IO.Exception              (unsupportedOperation)
import qualified GHC.IO.Handle                 as H
import           GHC.IO.Handle.Types           (Handle (..))
import qualified System.Directory              as Dir
import           System.FilePath               ((</>))
import           System.IO                     (IOMode (ReadMode, WriteMode),
                                                SeekMode (..), hClose,
                                                hFileSize, hSeek, hTell,
                                                openBinaryFile, withBinaryFile)

data SimpleHandle = SimpleHandle
  { shSize  :: Integer
  , shSeek  :: Integer -> IO ()
  , shTell  :: IO Integer
  , shClose :: IO ()
  , shRead  :: Integer -> IO B.ByteString
  } deriving (Typeable)

instance D.IODevice SimpleHandle where
  ready _ False _ = return True
  ready _ True  _ = return False
  close           = shClose
  devType _       = return D.RegularFile
  getSize         = return . shSize
  seek sh mode n  = do
    n' <- case mode of
      AbsoluteSeek -> return n
      RelativeSeek -> (+ n) <$> shTell sh
      SeekFromEnd  -> return $ shSize sh - n
    shSeek sh n'
    return n'
  isSeekable _    = return True
  isTerminal _    = return False
  tell            = shTell

instance D.RawIO SimpleHandle where
  read sh p _ n = do
    pos <- shTell sh
    let maxBytes = shSize sh - pos
    bs <- shRead sh $ min (fromIntegral n) maxBytes
    unsafeUseAsCStringLen bs $ \(p', len) -> memcpy p (castPtr p') len
    return $ B.length bs
  readNonBlocking sh p w64 n = (\bytes -> guard (bytes /= 0) >> Just bytes) <$> D.read sh p w64 n
  write            _ _ _ _ = throwIO unsupportedOperation
  writeNonBlocking _ _ _ _ = throwIO unsupportedOperation

instance BufferedIO SimpleHandle where
  newBuffer _dev state = newByteBuffer 8192 state
  fillReadBuffer       = readBuf
  fillReadBuffer0      = readBufNonBlocking
  flushWriteBuffer     = writeBuf
  flushWriteBuffer0    = writeBufNonBlocking

data Readable = Readable
  { rOpen     :: IO Handle
  , rFilePath :: Maybe FilePath
  }

instance Show Readable where
  show r = case rFilePath r of
    Nothing -> "Readable{...}"
    Just f  -> "Readable{" <> show f <> "}"

fileReadable :: FilePath -> Readable
fileReadable f = Readable
  { rOpen     = openBinaryFile f ReadMode
  , rFilePath = Just f
  }

-- this should only be used on read-only handles
simplifyHandle :: Handle -> IO SimpleHandle
simplifyHandle h = do
  len <- hFileSize h
  return SimpleHandle
    { shSize  = len
    , shSeek  = hSeek h AbsoluteSeek
    , shTell  = hTell h
    , shClose = hClose h
    , shRead  = B.hGet h . fromIntegral
    }

openSimpleHandle :: String -> SimpleHandle -> IO Handle
openSimpleHandle str sh = H.mkFileHandle
  sh
  str
  ReadMode
  Nothing
  H.noNewlineTranslation

makeHandle :: String -> IO SimpleHandle -> Readable
makeHandle str iosh = Readable
  { rFilePath = Nothing
  , rOpen = iosh >>= openSimpleHandle str
  }

byteStringSimpleHandle :: BL.ByteString -> IO SimpleHandle
byteStringSimpleHandle bs = do
  posn <- newIORef 0
  let len = fromIntegral $ BL.length bs
  return SimpleHandle
    { shSize = len
    , shSeek = writeIORef posn
    , shTell = readIORef posn
    , shClose = return ()
    , shRead = \n -> do
      p <- readIORef posn
      writeIORef posn $ p + n
      return $ BL.toStrict $ BL.take (fromIntegral n) $ BL.drop (fromIntegral p) bs
    }

handleToByteString :: Handle -> IO BL.ByteString
handleToByteString h = do
  len <- hFileSize h
  hSeek h AbsoluteSeek 0
  BL.hGet h $ fromIntegral len

appendSimpleHandle :: SimpleHandle -> SimpleHandle -> IO SimpleHandle
appendSimpleHandle x y = do
  posn <- newIORef 0
  return SimpleHandle
    { shSize = shSize x + shSize y
    , shSeek = writeIORef posn
    , shTell = readIORef posn
    , shClose = shClose x >> shClose y
    , shRead = \n -> do
      p <- readIORef posn
      writeIORef posn $ p + n
      let segmentX = do
            guard $ p < shSize x
            Just $ do
              shSeek x p
              shRead x $ min (shSize x - p) n
          segmentY = do
            guard $ p + n > shSize x
            Just $ do
              shSeek y $ max 0 (p - shSize x)
              shRead y $ n - max 0 (shSize x - p)
      fmap B.concat $ sequence $ catMaybes [segmentX, segmentY]
    }

useHandle :: Readable -> (Handle -> IO a) -> IO a
useHandle readable = bracket (rOpen readable) hClose

resourceHandle :: (MonadResource m) => Readable -> m (Handle, IO ())
resourceHandle r = do
  (rkey, h) <- allocate (rOpen r) hClose
  return (h, release rkey)

copyReadableToHandle :: Readable -> Handle -> IO ()
copyReadableToHandle r hout = useHandle r $ \h -> let
  go = do
    chunk <- B.hGet h smallChunkSize
    unless (B.null chunk) $ do
      B.hPut hout chunk
      go
  in go

saveReadable :: Readable -> FilePath -> IO ()
saveReadable r dest = case rFilePath r of
  Just src -> Dir.copyFile src dest
  Nothing  -> withBinaryFile dest WriteMode $ copyReadableToHandle r

saveReadables :: [Readable] -> FilePath -> IO ()
saveReadables rs dest = withBinaryFile dest WriteMode $ \h -> do
  mapM_ (`copyReadableToHandle` h) rs

saveHandleFolder :: Folder T.Text Readable -> FilePath -> IO ()
saveHandleFolder folder dest = do
  Dir.createDirectoryIfMissing False dest
  forM_ (folderSubfolders folder) $ \(name, sub) -> do
    let subdest = dest </> T.unpack name
    Dir.createDirectoryIfMissing False subdest
    saveHandleFolder sub subdest
  forM_ (folderFiles folder) $ \(name, file) -> do
    saveReadable file $ dest </> T.unpack name

data Folder name a = Folder
  { folderSubfolders :: [(name, Folder name a)]
  , folderFiles      :: [(name, a)]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor Folder where
  first f folder = Folder
    { folderSubfolders = map (bimap f $ first f) $ folderSubfolders folder
    , folderFiles = map (first f) $ folderFiles folder
    }
  second = fmap

instance (Ord name) => Semigroup (Folder name a) where
  x <> y = Folder
    -- merge subfolders
    { folderSubfolders = Map.toList $ Map.unionWith (<>) (Map.fromList $ folderSubfolders x) (Map.fromList $ folderSubfolders y)
    -- prefer the right argument in case of a duplicate file
    , folderFiles      = Map.toList $ Map.fromList $ folderFiles x <> folderFiles y
    }

instance (Ord name) => Monoid (Folder name a) where
  mempty = Folder [] []

allFolders :: Folder p a -> [NE.NonEmpty p]
allFolders folder = do
  (sub, subf) <- folderSubfolders folder
  pure sub : map (NE.cons sub) (allFolders subf)

allFiles :: Folder p a -> [(NE.NonEmpty p, a)]
allFiles folder = let
  inHere = do
    (name, file) <- folderFiles folder
    return (pure name, file)
  inSub = do
    (sub, subf) <- folderSubfolders folder
    (spath, file) <- allFiles subf
    return (NE.cons sub spath, file)
  in inHere <> inSub

splitPath :: T.Text -> Maybe (NE.NonEmpty T.Text)
splitPath t = NE.nonEmpty $ filter (not . T.null) $ T.splitOn "/" t >>= T.splitOn "\\"

unsplitPath :: NE.NonEmpty T.Text -> T.Text
unsplitPath = T.pack . foldr1 ((</>)) . fmap T.unpack

fromFiles :: (Ord p) => [(NE.NonEmpty p, a)] -> Folder p a
fromFiles pairs = Folder
  { folderFiles = [ (name, file) | (name :| [], file) <- pairs ]
  , folderSubfolders = do
    group <- NE.groupAllWith fst [ (name, (x :| xs, file)) | (name :| (x : xs), file) <- pairs ]
    let (sub, _) :| _ = group
    return (sub, fromFiles $ map snd $ NE.toList group)
  }

findFolder :: (Eq p) => [p] -> Folder p a -> Maybe (Folder p a)
findFolder []           folder = Just folder
findFolder (dir : rest) folder = do
  sub <- lookup dir $ folderSubfolders folder
  findFolder rest sub

-- Case-insensitive
findFolderCI :: [T.Text] -> Folder T.Text a -> Maybe (Folder T.Text a)
findFolderCI []           folder = Just folder
findFolderCI (dir : rest) folder = do
  sub <- lookup (T.toCaseFold dir) $ map (first T.toCaseFold) $ folderSubfolders folder
  findFolderCI rest sub

findFile :: (Eq p) => NE.NonEmpty p -> Folder p a -> Maybe a
findFile spath folder = case NE.uncons spath of
  (dir, Just rest) -> do
    sub <- lookup dir $ folderSubfolders folder
    findFile rest sub
  (name, Nothing) -> lookup name $ folderFiles folder

-- Case-insensitive
findFileCI :: NE.NonEmpty T.Text -> Folder T.Text a -> Maybe a
findFileCI spath folder = case NE.uncons spath of
  (dir, Just rest) -> do
    sub <- lookup' dir $ folderSubfolders folder
    findFileCI rest sub
  (name, Nothing) -> lookup' name $ folderFiles folder
  where lookup' x xs = let
          x' = T.toCaseFold x
          in fmap snd $ find (\(y, _) -> x' == T.toCaseFold y) xs

crawlFolder :: FilePath -> IO (Folder T.Text Readable)
crawlFolder f = do
  ents <- sort <$> Dir.listDirectory f
  results <- forM ents $ \ent -> Dir.doesFileExist (f </> ent) >>= \case
    True  -> return $ Left (T.pack ent, fileReadable $ f </> ent)
    False -> do
      sub <- crawlFolder $ f </> ent
      return $ Right (T.pack ent, sub)
  return Folder
    { folderFiles      = lefts  results
    , folderSubfolders = rights results
    }

findByteString :: (Eq p) => NE.NonEmpty p -> Folder p Readable -> IO (Maybe BL.ByteString)
findByteString spath folder
  = mapM (\readable -> useHandle readable handleToByteString)
  $ findFile spath folder

handleLabel :: Handle -> String
handleLabel = \case
  FileHandle   s _   -> s
  DuplexHandle s _ _ -> s

subHandle' :: (String -> String) -> Integer -> Maybe Integer -> IO Handle -> (Handle -> IO ()) -> Readable
subHandle' addLabel pos mlen open close = Readable
  { rFilePath = Nothing -- because it no longer refers to a simple file
  , rOpen = do
    h <- open
    origSize <- hFileSize h
    hSeek h AbsoluteSeek pos
    openSimpleHandle (addLabel $ handleLabel h) SimpleHandle
      { shSize  = fromMaybe (origSize - pos) mlen
      , shSeek  = \n -> hSeek h AbsoluteSeek $ n + pos
      , shTell  = (\(H.HandlePosn _ n) -> n - pos) <$> H.hGetPosn h
      , shClose = close h
      , shRead  = B.hGet h . fromIntegral
      }
  }

subHandle :: (String -> String) -> Integer -> Maybe Integer -> Readable -> Readable
subHandle addLabel pos mlen readable = subHandle' addLabel pos mlen (rOpen readable) hClose

transformBytes :: (BL.ByteString -> BL.ByteString) -> Readable -> Readable
transformBytes f r = Readable
  { rFilePath = Nothing
  , rOpen = do
    bs <- useHandle r handleToByteString
    rOpen $ makeHandle "" $ byteStringSimpleHandle $ f bs
  }

traverseFiles :: (Applicative f) => (t -> a -> f b) -> Folder t a -> f (Folder t b)
traverseFiles g folder = Folder
  <$> traverse (traverse $ traverseFiles g) (folderSubfolders folder)
  <*> traverse (\(name, x) -> (name,) <$> g name x) (folderFiles folder)

generateCachedReadable :: IO Readable -> IO Readable
generateCachedReadable generate = do
  v <- newMVar Nothing
  return Readable
    { rFilePath = Nothing
    , rOpen = do
      r <- modifyMVar v $ \mr -> do
        r <- maybe generate return mr
        return (Just r, r)
      rOpen r
    }
