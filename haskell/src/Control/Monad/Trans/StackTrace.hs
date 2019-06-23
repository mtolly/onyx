{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Monad.Trans.StackTrace
( Message(..), Messages(..)
, MessageLevel(..), SendMessage(..)
, PureLog(..), runPureLog, runPureLogT, withPureLog
, QueueLog(..), mapQueueLog
, StackTraceT(..)
, warn, warnMessage, sendMessage', lg
, errorToWarning, errorToEither
, fatal
, MonadError(..)
, inside
, runStackTraceT
, liftBracket, liftBracketLog, liftMaybe
, mapStackTraceT
, stracket, tempDir, withDir
, stackProcess
, stackCatchIO
, stackShowException
, stackIO
, shakeEmbed
, shakeTrace
, (%>), (&%>), phony
, Staction
, logIO, logStdout
) where

import           Control.Applicative
import qualified Control.Exception                as Exc
import           Control.Monad
import           Control.Monad.Catch              (MonadThrow (..))
import           Control.Monad.Except             (MonadError (..))
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift          (MonadUnliftIO (..),
                                                   UnliftIO (..))
import           Control.Monad.State              (MonadState (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.State.Lazy   as SL
import qualified Control.Monad.Trans.State.Strict as SS
import           Control.Monad.Trans.Writer
import qualified Data.ByteString.Char8            as B8
import           Data.Functor.Identity            (Identity)
import qualified Development.Shake                as Shake
import qualified System.Directory                 as Dir
import           System.Exit                      (ExitCode (..))
import qualified System.IO.Temp                   as Temp
import           System.Process                   (CreateProcess)
import           System.Process.ByteString        (readCreateProcessWithExitCode)

-- | This can represent an error (required input was not found) or a warning
-- (given input was not completely recognized).
data Message = Message
  { messageString  :: String
  , messageContext :: [String] -- ^ The first element is the innermost context
  } deriving (Eq, Ord, Show, Read)

instance Exc.Exception Message where
  displayException (Message str ctx) = unlines $ str : case ctx of
    [] -> []
    _  -> "Context (innermost first):" : map ("  - " ++) ctx

newtype Messages = Messages { getMessages :: [Message] }
  deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

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

runPureLogT :: (Monad m) => PureLog m a -> m (a, [(MessageLevel, Message)])
runPureLogT = runWriterT . fromPureLog

withPureLog :: (SendMessage m, Monad n) => (forall b. n b -> m b) -> PureLog n a -> m a
withPureLog f st = do
  (x, msgs) <- f $ runPureLogT st
  mapM_ (uncurry sendMessage) msgs
  return x

newtype QueueLog m a = QueueLog { fromQueueLog :: ReaderT ((MessageLevel, Message) -> IO ()) m a }
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

mapQueueLog :: (m a -> n b) -> QueueLog m a -> QueueLog n b
mapQueueLog f = QueueLog . mapReaderT f . fromQueueLog

instance (MonadUnliftIO m) => MonadUnliftIO (QueueLog m) where
  askUnliftIO = QueueLog $ fmap (\(UnliftIO f) -> UnliftIO $ f . fromQueueLog) askUnliftIO

instance MonadTrans QueueLog where
  lift = QueueLog . lift

instance (MonadThrow m) => MonadThrow (QueueLog m) where
  throwM = lift . throwM

instance (MonadResource m) => MonadResource (QueueLog m) where
  liftResourceT = lift . liftResourceT

instance (MonadIO m) => SendMessage (QueueLog m) where
  sendMessage lvl msg = QueueLog $ ask >>= liftIO . ($ (lvl, msg))

instance (MonadState s m) => MonadState s (QueueLog m) where
  get = QueueLog get
  put = QueueLog . put
  state = QueueLog . state

liftMessage :: (MonadTrans t, SendMessage m) => MessageLevel -> Message -> t m ()
liftMessage lvl msg = lift $ sendMessage lvl msg

instance (SendMessage m)           => SendMessage (ReaderT   r m) where sendMessage = liftMessage
instance (SendMessage m, Monoid w) => SendMessage (WriterT   w m) where sendMessage = liftMessage
instance (SendMessage m)           => SendMessage (SL.StateT s m) where sendMessage = liftMessage
instance (SendMessage m)           => SendMessage (SS.StateT s m) where sendMessage = liftMessage
instance (SendMessage m)           => SendMessage (ResourceT   m) where sendMessage = liftMessage

newtype StackTraceT m a = StackTraceT
  { fromStackTraceT :: ExceptT Messages (ReaderT [String] m) a
  } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

instance (MonadState s m) => MonadState s (StackTraceT m) where
  get = StackTraceT get
  put = StackTraceT . put
  state = StackTraceT . state

instance MonadTrans StackTraceT where
  lift = StackTraceT . lift . lift

instance (MonadResource m) => MonadResource (StackTraceT m) where
  liftResourceT = lift . liftResourceT

warn :: (SendMessage m) => String -> StackTraceT m ()
warn s = warnMessage $ Message s []

warnMessage :: (SendMessage m) => Message -> StackTraceT m ()
warnMessage = sendMessage' MessageWarning

sendMessage' :: (SendMessage m) => MessageLevel -> Message -> StackTraceT m ()
sendMessage' lvl (Message s ctx) = StackTraceT $ lift $ do
  upper <- ask
  lift $ sendMessage lvl $ Message s $ ctx ++ upper

lg :: (SendMessage m) => String -> StackTraceT m ()
lg s = sendMessage' MessageLog $ Message s []

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

liftBracket
  :: (MonadIO m)
  => (forall b. (a -> IO b) -> IO b)
  -> (a -> StackTraceT IO c)
  -> StackTraceT m c
liftBracket io st = do
  res <- liftIO $ io $ runStackTraceT . st
  either throwError return res

liftBracketLog
  :: (SendMessage m, MonadIO m)
  => (forall b. (a -> IO b) -> IO b)
  -> (a -> StackTraceT (PureLog IO) c)
  -> StackTraceT m c
liftBracketLog io st = do
  (res, msgs) <- liftIO $ io $ runWriterT . fromPureLog . runStackTraceT . st
  mapM_ (uncurry sendMessage') msgs
  either throwError return res

liftMaybe :: (Monad m, Show a) => (a -> m (Maybe b)) -> a -> StackTraceT m b
liftMaybe f x = lift (f x) >>= \case
  Nothing -> fatal $ "Unrecognized input: " ++ show x
  Just y  -> return y

mapStackTraceT
  :: (Monad m, Monad n)
  => (m (Either Messages a) -> n (Either Messages b))
  -> StackTraceT m a -> StackTraceT n b
mapStackTraceT f (StackTraceT st) = StackTraceT $ mapExceptT (mapReaderT f) st

stracket
  :: (MonadIO m)
  => IO a
  -> (a -> IO ())
  -> (a -> StackTraceT (QueueLog IO) b)
  -> StackTraceT (QueueLog m) b
stracket new del fn = mapStackTraceT (mapQueueLog (liftIO . runResourceT)) $ do
  (_, x) <- allocate new del
  mapStackTraceT (mapQueueLog lift) $ fn x

withDir :: (MonadIO m) => FilePath -> StackTraceT (QueueLog IO) a -> StackTraceT (QueueLog m) a
withDir d stk = do
  cwd <- stackIO Dir.getCurrentDirectory
  stracket
    (Dir.setCurrentDirectory d)
    (\() -> Dir.setCurrentDirectory cwd)
    (\() -> stk)

tempDir :: (MonadResource m) => String -> (FilePath -> m a) -> m a
tempDir template fn = do
  let ignoringIOErrors ioe = ioe `Exc.catch` (\e -> const (return ()) (e :: IOError))
  tmp <- liftIO Temp.getCanonicalTemporaryDirectory
  (key, dir) <- allocate
    (Temp.createTempDirectory tmp template)
    (ignoringIOErrors . Dir.removeDirectoryRecursive)
  fn dir <* release key

stackProcess :: (MonadIO m) => CreateProcess -> StackTraceT m String
stackProcess cp = mapStackTraceT liftIO $ do
  -- Magma's output is Latin-1, so we read it as ByteString and B8.unpack.
  -- otherwise non-utf-8 chars crash with "invalid byte sequence".
  liftIO (readCreateProcessWithExitCode cp B8.empty) >>= \case
    (ExitSuccess  , out, _  ) -> return $ stringNoCR out
    (ExitFailure n, out, err) -> fatal $ unlines
      [ "process exited with code " ++ show n
      , "stdout:"
      , stringNoCR out
      , "stderr:"
      , stringNoCR err
      ]
    where stringNoCR = filter (/= '\r') . B8.unpack

stackCatchIO :: (MonadIO m, Exc.Exception e) => (e -> StackTraceT m a) -> IO a -> StackTraceT m a
stackCatchIO handler io = do
  exc <- liftIO $ fmap Right io `Exc.catch` (return . Left)
  either handler return exc

stackShowException :: (Exc.Exception e, Monad m) => e -> StackTraceT m a
stackShowException = fatal . Exc.displayException

-- | Like 'liftIO', but 'IOError' are caught and rethrown with 'fatal'.
stackIO :: (MonadIO m) => IO a -> StackTraceT m a
stackIO = stackCatchIO $ stackShowException . (id :: IOError -> IOError)

shakeEmbed :: (MonadIO m) => Shake.ShakeOptions -> QueueLog Shake.Rules () -> StackTraceT (QueueLog m) ()
shakeEmbed opts rules = do
  let handleShakeErr se = let
        -- we translate ShakeException (which may or may not have a StackTraceT fatal inside)
        -- to a StackTraceT fatal with layers
        go (layer : layers) exc = inside ("shake: " ++ layer) $ go layers exc
        go []               exc = case Exc.fromException exc of
          Nothing   -> stackShowException exc
          Just msgs -> throwError msgs
        in go (Shake.shakeExceptionStack se) (Shake.shakeExceptionInner se)
  writeMsg <- lift $ QueueLog ask
  stackCatchIO handleShakeErr $ Shake.shake opts $ runReaderT (fromQueueLog rules) writeMsg

shakeTrace :: StackTraceT (QueueLog Shake.Action) a -> QueueLog Shake.Action a
shakeTrace stk = runStackTraceT stk >>= \res -> do
  case res of
    Right x  -> return x
    Left err -> liftIO $ Exc.throwIO err

(%>) :: Shake.FilePattern -> (FilePath -> StackTraceT (QueueLog Shake.Action) ()) -> QueueLog Shake.Rules ()
pat %> f = QueueLog $ ReaderT $ \q -> pat Shake.%> (`runReaderT` q) . fromQueueLog . shakeTrace . f
infix 1 %>

(&%>) :: [Shake.FilePattern] -> ([FilePath] -> StackTraceT (QueueLog Shake.Action) ()) -> QueueLog Shake.Rules ()
pat &%> f = QueueLog $ ReaderT $ \q -> pat Shake.&%> (`runReaderT` q) . fromQueueLog . shakeTrace . f
infix 1 &%>

phony :: FilePath -> StackTraceT (QueueLog Shake.Action) () -> QueueLog Shake.Rules ()
phony s act = QueueLog $ ReaderT $ \q -> do
  let act' = runReaderT (fromQueueLog $ shakeTrace act) q
  Shake.phony s act'
  Shake.phony (s ++ "/") act'

type Staction = StackTraceT (QueueLog Shake.Action)

logIO
  :: (MonadIO m)
  => ((MessageLevel, Message) -> IO ())
  -> StackTraceT (QueueLog m) a
  -> m (Either Messages a)
logIO logger task = runReaderT (fromQueueLog $ runStackTraceT task) logger

logStdout :: (MonadIO m) => StackTraceT (QueueLog m) a -> m (Either Messages a)
logStdout = logIO $ putStrLn . \case
  (MessageLog    , msg) -> messageString msg
  (MessageWarning, msg) -> "Warning: " ++ Exc.displayException msg
