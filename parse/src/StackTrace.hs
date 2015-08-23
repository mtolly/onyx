{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module StackTrace where

import Data.Functor.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.IO

-- | This can represent an error (required input was not found) or a warning
-- (given input was not completely recognized).
data Message = Message
  { messageString  :: String
  , messageContext :: [String] -- ^ The first element is the innermost context
  } deriving (Eq, Ord, Show, Read)

-- | Attaches warnings and fatal errors to a monad. Both warnings and errors
-- keep track of their \"call stack\" of where the message occurred.
newtype StackTraceT m a = StackTraceT
  { fromStackTraceT :: ExceptT [Message] (RWST [String] [Message] () m) a
  } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

type StackTrace = StackTraceT Identity

instance MonadTrans StackTraceT where
  lift = StackTraceT . lift . lift

warn :: (Monad m) => String -> StackTraceT m ()
warn s = StackTraceT $ lift $ do
  ctx <- ask
  tell [Message s ctx]

-- | Turns errors into warnings, and returns "Nothing".
optional :: (Monad m) => StackTraceT m a -> StackTraceT m (Maybe a)
optional p = liftM Just p `catch` \errs ->
  StackTraceT (lift $ tell errs) >> return Nothing

fatal :: (Monad m) => String -> StackTraceT m a
fatal s = StackTraceT $ do
  ctx <- lift ask
  throwE [Message s ctx]

catch :: (Monad m) => StackTraceT m a -> ([Message] -> StackTraceT m a) -> StackTraceT m a
StackTraceT ex `catch` f = StackTraceT $ ex `catchE` (fromStackTraceT . f)

inside :: (Monad m) => String -> StackTraceT m a -> StackTraceT m a
inside s (StackTraceT (ExceptT rwst)) = StackTraceT $ ExceptT $ local (s :) rwst

runStackTraceT :: (Monad m) => StackTraceT m a -> m (Either [Message] a, [Message])
runStackTraceT (StackTraceT ex) = evalRWST (runExceptT ex) [] ()

runStackTrace :: StackTrace a -> (Either [Message] a, [Message])
runStackTrace = runIdentity . runStackTraceT

-- | Prints the message and its context stack to standard error.
printMessage :: Message -> IO ()
printMessage (Message s ctx) = do
  hPutStrLn stderr s
  hPutStrLn stderr "Context (innermost first):"
  forM_ ctx $ \c -> hPutStrLn stderr $ "  — " ++ c

-- | Prints warnings and errors to standard error, and then throws an exception
-- if there were errors.
printStackTraceIO :: (MonadIO m) => StackTraceT m a -> m a
printStackTraceIO p = do
  (result, warnings) <- runStackTraceT p
  liftIO $ forM_ warnings $ \msg -> do
    hPutStr stderr "Warning: "
    printMessage msg
  case result of
    Left errors -> liftIO $ do
      forM_ errors $ \msg -> do
        hPutStr stderr "ERROR: "
        printMessage msg
      error "printStackTraceIO: fatal errors occurred"
    Right x -> return x

liftMaybe :: (Monad m, Show a) => (a -> m (Maybe b)) -> a -> StackTraceT m b
liftMaybe f x = lift (f x) >>= \case
  Nothing -> fatal $ "Unrecognized input: " ++ show x
  Just y  -> return y

mapStackTraceT :: (Monad m)
  => (m (Either [Message] a, (), [Message]) -> n (Either [Message] b, (), [Message]))
  -> StackTraceT m a -> StackTraceT n b
mapStackTraceT f (StackTraceT st) = StackTraceT $ mapExceptT (mapRWST f) st
