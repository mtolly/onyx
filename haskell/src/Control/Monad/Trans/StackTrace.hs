{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Control.Monad.Trans.StackTrace
( Message(..), Messages(..)
, StackTraceT(..), StackTrace
, warn, warnMessage
, errorToWarning, errorToEither
, fatal
, MonadError(..)
, inside
, runStackTraceT
, processWarnings, printWarning
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
import           Control.Monad.Except         (MonadError (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource (allocate, runResourceT)
import           Control.Monad.Trans.RWS
import           Data.Data
import           Data.Functor.Identity
import           Data.Monoid                  ((<>))
import qualified Development.Shake            as Shake
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
errorToWarning :: (Monad m) => StackTraceT m a -> StackTraceT m (Maybe a)
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

liftMaybe :: (Monad m, Show a) => (a -> m (Maybe b)) -> a -> StackTraceT m b
liftMaybe f x = lift (f x) >>= \case
  Nothing -> fatal $ "Unrecognized input: " ++ show x
  Just y  -> return y

mapStackTraceT
  :: (Monad m, Monad n)
  => (m (Either Messages a, Messages, ()) -> n (Either Messages b, Messages, ()))
  -> StackTraceT m a -> StackTraceT n b
mapStackTraceT f (StackTraceT st) = StackTraceT $ mapExceptT (mapRWST f) st

tempDir :: (MonadIO m) => String -> (FilePath -> StackTraceT IO a) -> StackTraceT m a
tempDir template cb = mapStackTraceT liftIO $ StackTraceT $ runResourceT $ do
  tmp <- liftIO Dir.getTemporaryDirectory
  let ignoringIOErrors ioe = ioe `Exc.catch` (\e -> const (return ()) (e :: IOError))
  (_, dir) <- allocate (createTempDirectory tmp template) (ignoringIOErrors . Dir.removeDirectoryRecursive)
  lift $ fromStackTraceT $ cb dir

stackProcess :: (MonadIO m) => CreateProcess -> String -> StackTraceT m String
stackProcess cp input = mapStackTraceT liftIO $ do
  liftIO (readCreateProcessWithExitCode cp input) >>= \case
    (ExitSuccess  , out, _  ) -> return out
    (ExitFailure n, out, err) -> fatal $ unlines
      [ "process exited with code " ++ show n
      , "stdout:"
      , out
      , "stderr:"
      , err
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

actionWarn :: Message -> Shake.Action ()
actionWarn msg = Shake.putNormal $ "Warning: " ++ Exc.displayException msg

shakeTrace :: StackTraceT Shake.Action a -> Shake.Action a
shakeTrace stk = runStackTraceT stk >>= \(res, Messages warns) -> do
  mapM_ actionWarn warns
  case res of
    Right x  -> return x
    Left err -> liftIO $ Exc.throwIO err

(≡>) :: Shake.FilePattern -> (FilePath -> StackTraceT Shake.Action ()) -> Shake.Rules ()
pat ≡> f = pat Shake.%> shakeTrace . f
infix 1 ≡>
