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
  { fromStackTraceT :: ExceptT Messages (RWST [String] Messages () m) a
  } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

type StackTrace = StackTraceT Identity

instance MonadTrans StackTraceT where
  lift = StackTraceT . lift . lift

warn :: (Monad m) => String -> StackTraceT m ()
warn s = StackTraceT $ lift $ do
  ctx <- ask
  tell $ Messages [Message s ctx]

-- | Turns errors into warnings, and returns "Nothing".
optional :: (Monad m) => StackTraceT m a -> StackTraceT m (Maybe a)
optional p = fmap Just p `catch` \errs ->
  StackTraceT (lift $ tell errs) >> return Nothing

fatalMessages :: (Monad m) => Messages -> StackTraceT m a
fatalMessages (Messages msgs) = StackTraceT $ do
  upper <- lift ask
  throwE $ Messages [ Message s (ctx ++ upper) | Message s ctx <- msgs ]

fatal :: (Monad m) => String -> StackTraceT m a
fatal s = fatalMessages $ Messages [Message s []]

catch :: (Monad m) => StackTraceT m a -> (Messages -> StackTraceT m a) -> StackTraceT m a
StackTraceT ex `catch` f = StackTraceT $ ex `catchE` (fromStackTraceT . f)

inside :: String -> StackTraceT m a -> StackTraceT m a
inside s (StackTraceT (ExceptT rwst)) = StackTraceT $ ExceptT $ local (s :) rwst

runStackTraceT :: (Monad m) => StackTraceT m a -> m (Either Messages a, Messages)
runStackTraceT (StackTraceT ex) = evalRWST (runExceptT ex) [] ()

runStackTrace :: StackTrace a -> (Either Messages a, Messages)
runStackTrace = runIdentity . runStackTraceT

-- | Prints warnings to standard error, and then throws any errors as an exception.
printStackTraceIO :: (MonadIO m) => StackTraceT m a -> m a
printStackTraceIO p = do
  (result, Messages warnings) <- runStackTraceT p
  liftIO $ forM_ warnings $ \msg -> hPutStr stderr $ "Warning: " ++ Exc.displayException msg
  case result of
    Left msgs -> liftIO $ do
      hPutStrLn stderr "printStackTraceIO: fatal errors occurred!"
      Exc.throwIO msgs
    Right x -> return x

liftMaybe :: (Monad m, Show a) => (a -> m (Maybe b)) -> a -> StackTraceT m b
liftMaybe f x = lift (f x) >>= \case
  Nothing -> fatal $ "Unrecognized input: " ++ show x
  Just y  -> return y

mapStackTraceT
  :: (m (Either Messages a, (), Messages) -> n (Either Messages b, (), Messages))
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
