{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Control.Monad.Trans.StackTrace where

import           Control.Applicative
import qualified Control.Exception            as Exc
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource (allocate, runResourceT)
import           Control.Monad.Trans.RWS
import           Data.Data
import           Data.Functor.Identity
import           Data.Monoid                  ((<>))
import qualified System.Directory             as Dir
import           System.Exit                  (ExitCode (..))
import           System.IO
import           System.IO.Temp               (createTempDirectory)
import           System.Process

-- | This can represent an error (required input was not found) or a warning
-- (given input was not completely recognized).
data Message = Message
  { messageString  :: String
  , messageContext :: [String] -- ^ The first element is the innermost context
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Exc.Exception Message where
  displayException (Message str ctx) = unlines
    $ str
    : "Context (innermost first):"
    : map ("  - " ++) ctx

newtype Messages = Messages { getMessages :: [Message] }
  deriving (Eq, Ord, Show, Read, Data, Typeable, Monoid)

instance Exc.Exception Messages where
  displayException = unlines . map Exc.displayException . getMessages

-- | Attaches warnings and fatal errors to a monad. Both warnings and errors
-- keep track of their \"call stack\" of where the message occurred.
newtype StackTraceT m a = StackTraceT
  { fromStackTraceT :: ExceptT Messages (RWST [String] () Messages m) a
  } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

type StackTrace = StackTraceT Identity

instance MonadTrans StackTraceT where
  lift = StackTraceT . lift . lift

warn :: (Monad m) => String -> StackTraceT m ()
warn s = warnMessage $ Message s []

warnMessage :: (Monad m) => Message -> StackTraceT m ()
warnMessage (Message s ctx) = StackTraceT $ lift $ do
  upper <- ask
  modify (<> Messages [Message s $ ctx ++ upper])

-- | Turns errors into warnings, and returns "Nothing".
optional :: (Monad m) => StackTraceT m a -> StackTraceT m (Maybe a)
optional p = fmap Just p `catch` \(Messages msgs) ->
  mapM_ warnMessage msgs >> return Nothing

optional' :: (Monad m) => StackTraceT m a -> StackTraceT m (Either Messages a)
optional' p = fmap Right p `catch` (return . Left)

fatalMessages :: (Monad m) => Messages -> StackTraceT m a
fatalMessages (Messages msgs) = StackTraceT $ do
  upper <- lift ask
  throwE $ Messages [ Message s (ctx ++ upper) | Message s ctx <- msgs ]

fatal :: (Monad m) => String -> StackTraceT m a
fatal s = fatalMessages $ Messages [Message s []]

catch :: (Monad m) => StackTraceT m a -> (Messages -> StackTraceT m a) -> StackTraceT m a
StackTraceT ex `catch` f = StackTraceT $ ex `catchE` (fromStackTraceT . f)

inside :: String -> StackTraceT m a -> StackTraceT m a
inside s (StackTraceT (ExceptT rdr)) = StackTraceT $ ExceptT $ local (s :) rdr

runStackTraceT :: (Monad m) => StackTraceT m a -> m (Either Messages a, Messages)
runStackTraceT (StackTraceT ex) = do
  (res, warns, ()) <- runRWST (runExceptT ex) [] (Messages [])
  return (res, warns)

processWarnings :: (Monad m) => StackTraceT m Messages
processWarnings = StackTraceT $ lift $ do
  warns <- get
  put $ Messages []
  return warns

printWarning :: (MonadIO m) => Message -> m ()
printWarning msg = liftIO $ hPutStr stderr $ "Warning: " ++ Exc.displayException msg

-- | Prints warnings to standard error, and then throws any errors as an exception.
printStackTraceIO :: (MonadIO m) => StackTraceT m a -> m a
printStackTraceIO p = runStackTraceT p >>= \(res, Messages warns) -> do
  mapM_ printWarning warns
  case res of
    Left msgs -> liftIO $ do
      hPutStrLn stderr "printStackTraceIO: fatal errors occurred!"
      Exc.throwIO msgs
    Right x -> return x

liftMaybe :: (Monad m, Show a) => (a -> m (Maybe b)) -> a -> StackTraceT m b
liftMaybe f x = lift (f x) >>= \case
  Nothing -> fatal $ "Unrecognized input: " ++ show x
  Just y  -> return y

mapStackTraceT
  :: (Monad m, Monad n)
  => (m (Either Messages a, Messages, ()) -> n (Either Messages b, Messages, ()))
  -> StackTraceT m a -> StackTraceT n b
mapStackTraceT f (StackTraceT st) = StackTraceT $ mapExceptT (mapRWST f) st

tempDir :: String -> (FilePath -> StackTraceT IO a) -> StackTraceT IO a
tempDir template cb = StackTraceT $ runResourceT $ do
  tmp <- liftIO Dir.getTemporaryDirectory
  let ignoringIOErrors ioe = ioe `Exc.catch` (\e -> const (return ()) (e :: IOError))
  (_, dir) <- allocate (createTempDirectory tmp template) (ignoringIOErrors . Dir.removeDirectoryRecursive)
  lift $ fromStackTraceT $ cb dir

stackProcess :: CreateProcess -> String -> StackTraceT IO String
stackProcess cp input = liftIO (readCreateProcessWithExitCode cp input) >>= \case
  (ExitSuccess  , out, _  ) -> return out
  (ExitFailure n, out, err) -> fatal $ unlines
    [ "process exited with code " ++ show n
    , "stdout:"
    , out
    , "stderr:"
    , err
    ]
