{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Control.Monad.Trans.StackTrace
( Message(..), Messages(..)
, MessageLevel(..), SendMessage(..)
, PureLog(..), runPureLog, withPureLog
, QueueLog(..)
, StackTraceT(..)
, warn, warnMessage, sendMessage'
, errorToWarning, errorToEither
, fatal
, MonadError(..)
, inside
, runStackTraceT
, printWarning
, liftMaybe
, mapStackTraceT
, tempDir
, stackProcess
, stackCatchIO
, stackShowException
, stackIO
, shakeTrace
, (≡>)
) where

import           Control.Applicative
import qualified Control.Exception            as Exc
import           Control.Monad
import           Control.Monad.Catch          (MonadThrow (..))
import           Control.Monad.Except         (MonadError (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource (allocate, runResourceT)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.State (StateT)
import Data.Functor.Identity (Identity)
import qualified Data.ByteString.Char8        as B8
import           Data.Data
import qualified Development.Shake            as Shake
import qualified System.Directory             as Dir
import           System.Exit                  (ExitCode (..))
import           System.IO
import           System.IO.Temp               (createTempDirectory)
import           System.Process               (CreateProcess)
import           System.Process.ByteString    (readCreateProcessWithExitCode)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)

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

data MessageLevel = MessageLog | MessageWarning
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

class (Monad m) => SendMessage m where
  sendMessage :: MessageLevel -> Message -> m ()

newtype PureLog m a = PureLog { fromPureLog :: WriterT [(MessageLevel, Message)] m a }
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

instance MonadTrans PureLog where
  lift = PureLog . lift

instance (Monad m) => SendMessage (PureLog m) where
  sendMessage lvl msg = PureLog $ tell [(lvl, msg)]

runPureLog :: PureLog Identity a -> (a, [(MessageLevel, Message)])
runPureLog = runWriter . fromPureLog

withPureLog :: (SendMessage m, Monad n) => (n a -> m a) -> StackTraceT (PureLog n) a -> StackTraceT m a
withPureLog = undefined

newtype QueueLog m a = QueueLog { fromQueueLog :: ReaderT (TChan (MessageLevel, Message)) m a }
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

instance MonadTrans QueueLog where
  lift = QueueLog . lift

instance (MonadIO m) => SendMessage (QueueLog m) where
  sendMessage lvl msg = QueueLog $ ask >>= \q -> liftIO $ atomically $ writeTChan q (lvl, msg)

liftMessage :: (MonadTrans t, SendMessage m) => MessageLevel -> Message -> t m ()
liftMessage lvl msg = lift $ sendMessage lvl msg

instance (SendMessage m)           => SendMessage (ReaderT r m) where sendMessage = liftMessage
instance (SendMessage m, Monoid w) => SendMessage (WriterT w m) where sendMessage = liftMessage
instance (SendMessage m)           => SendMessage (StateT  s m) where sendMessage = liftMessage

newtype StackTraceT m a = StackTraceT
  { fromStackTraceT :: ExceptT Messages (ReaderT [String] m) a
  } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

instance MonadTrans StackTraceT where
  lift = StackTraceT . lift . lift

warn :: (SendMessage m) => String -> StackTraceT m ()
warn s = warnMessage $ Message s []

warnMessage :: (SendMessage m) => Message -> StackTraceT m ()
warnMessage = sendMessage' MessageWarning

sendMessage' :: (SendMessage m) => MessageLevel -> Message -> StackTraceT m ()
sendMessage' lvl (Message s ctx) = StackTraceT $ lift $ do
  upper <- ask
  lift $ sendMessage lvl $ Message s $ ctx ++ upper

errorToWarning :: (SendMessage m) => StackTraceT m a -> StackTraceT m (Maybe a)
errorToWarning p = errorToEither p >>= \case
  Left (Messages msgs) -> mapM_ warnMessage msgs >> return Nothing
  Right x              -> return $ Just x

errorToEither :: (Monad m) => StackTraceT m a -> StackTraceT m (Either Messages a)
errorToEither p = fmap Right p `catchError` (return . Left)

fatal :: (Monad m) => String -> StackTraceT m a
fatal s = throwError $ Messages [Message s []]

instance (Monad m) => MonadError Messages (StackTraceT m) where
  throwError (Messages msgs) = StackTraceT $ do
    upper <- lift ask
    throwE $ Messages [ Message s (ctx ++ upper) | Message s ctx <- msgs ]
  StackTraceT ex `catchError` f = StackTraceT $ ex `catchE` (fromStackTraceT . f)

instance (Monad m) => MonadThrow (StackTraceT m) where
  throwM = stackShowException

inside :: String -> StackTraceT m a -> StackTraceT m a
inside s (StackTraceT (ExceptT rdr)) = StackTraceT $ ExceptT $ local (s :) rdr

runStackTraceT :: (Monad m) => StackTraceT m a -> m (Either Messages a)
runStackTraceT (StackTraceT ex) = runReaderT (runExceptT ex) []

printWarning :: (MonadIO m) => Message -> m ()
printWarning msg = liftIO $ hPutStr stderr $ "Warning: " ++ Exc.displayException msg

liftMaybe :: (Monad m, Show a) => (a -> m (Maybe b)) -> a -> StackTraceT m b
liftMaybe f x = lift (f x) >>= \case
  Nothing -> fatal $ "Unrecognized input: " ++ show x
  Just y  -> return y

mapStackTraceT
  :: (Monad m, Monad n)
  => (m (Either Messages a) -> n (Either Messages b))
  -> StackTraceT m a -> StackTraceT n b
mapStackTraceT f (StackTraceT st) = StackTraceT $ mapExceptT (mapReaderT f) st

tempDir :: (MonadIO m) => String -> (FilePath -> StackTraceT IO a) -> StackTraceT m a
tempDir template cb = mapStackTraceT liftIO $ StackTraceT $ runResourceT $ do
  tmp <- liftIO Dir.getTemporaryDirectory
  let ignoringIOErrors ioe = ioe `Exc.catch` (\e -> const (return ()) (e :: IOError))
  (_, dir) <- allocate (createTempDirectory tmp template) (ignoringIOErrors . Dir.removeDirectoryRecursive)
  lift $ fromStackTraceT $ cb dir

stackProcess :: (MonadIO m) => CreateProcess -> StackTraceT m String
stackProcess cp = mapStackTraceT liftIO $ do
  -- Magma's output is Latin-1, so we read it as ByteString and B8.unpack.
  -- otherwise non-utf-8 chars crash with "invalid byte sequence".
  liftIO (readCreateProcessWithExitCode cp B8.empty) >>= \case
    (ExitSuccess  , out, _  ) -> return $ B8.unpack out
    (ExitFailure n, out, err) -> fatal $ unlines
      [ "process exited with code " ++ show n
      , "stdout:"
      , B8.unpack out
      , "stderr:"
      , B8.unpack err
      ]

stackCatchIO :: (MonadIO m, Exc.Exception e) => (e -> StackTraceT m a) -> IO a -> StackTraceT m a
stackCatchIO handler io = do
  exc <- liftIO $ fmap Right io `Exc.catch` (return . Left)
  either handler return exc

stackShowException :: (Exc.Exception e, Monad m) => e -> StackTraceT m a
stackShowException = fatal . Exc.displayException

-- | Like 'liftIO', but 'IOError' are caught and rethrown with 'fatal'.
stackIO :: (MonadIO m) => IO a -> StackTraceT m a
stackIO = stackCatchIO $ stackShowException . (id :: IOError -> IOError)

shakeEmbed :: (SendMessage m, MonadIO m) => Shake.ShakeOptions -> QueueLog Shake.Rules () -> StackTraceT m ()
shakeEmbed opts rules = do
  let handleShakeErr se = let
        go (layer : layers) exc = inside ("shake: " ++ layer) $ go layers exc
        go []               exc = case Exc.fromException exc of
          Nothing   -> stackShowException exc
          Just msgs -> throwError msgs
        in go (Shake.shakeExceptionStack se) (Shake.shakeExceptionInner se)
  withQueueLog' (stackCatchIO handleShakeErr . Shake.shake opts) rules

withQueueLog' :: (MonadIO m, SendMessage n, MonadIO n) =>
  (m a -> StackTraceT n b) -> QueueLog m a -> StackTraceT n b
withQueueLog' lifter act = do
  q <- liftIO $ atomically newTChan
  -- TODO fork off new thread to start processing messages
  x <- lifter $ runReaderT (fromQueueLog act) q
  -- TODO cancel the forked thread, process rest of queue
  return x

withQueueLog :: (MonadIO m, SendMessage n, MonadIO n) =>
  (StackTraceT m a -> StackTraceT n b) -> StackTraceT (QueueLog m) a -> StackTraceT n b
withQueueLog lifter act = do
  q <- liftIO $ atomically newTChan
  -- TODO fork off new thread to start processing messages
  x <- lifter $ mapStackTraceT ((`runReaderT` q) . fromQueueLog) act
  -- TODO cancel the forked thread, process rest of queue
  return x

actionWarn :: Message -> Shake.Action ()
actionWarn msg = Shake.putNormal $ "Warning: " ++ Exc.displayException msg

shakeTrace :: StackTraceT Shake.Action a -> Shake.Action a
shakeTrace stk = runStackTraceT stk >>= \res -> do
  case res of
    Right x  -> return x
    Left err -> liftIO $ Exc.throwIO err

(≡>) :: Shake.FilePattern -> (FilePath -> StackTraceT Shake.Action ()) -> Shake.Rules ()
pat ≡> f = pat Shake.%> shakeTrace . f
infix 1 ≡>
