{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Parser where

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
newtype ParserT m a = ParserT
  { fromParserT :: ExceptT [Message] (RWST [String] [Message] () m) a
  } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

instance MonadTrans ParserT where
  lift = ParserT . lift . lift

warn :: (Monad m) => String -> ParserT m ()
warn s = ParserT $ lift $ do
  ctx <- ask
  tell [Message s ctx]

-- | Turns errors into warnings, and returns "Nothing".
optional :: (Monad m) => ParserT m a -> ParserT m (Maybe a)
optional p = liftM Just p `catch` \errs ->
  ParserT (lift $ tell errs) >> return Nothing

fatal :: (Monad m) => String -> ParserT m a
fatal s = ParserT $ do
  ctx <- lift ask
  throwE [Message s ctx]

catch :: (Monad m) => ParserT m a -> ([Message] -> ParserT m a) -> ParserT m a
ParserT ex `catch` f = ParserT $ ex `catchE` (fromParserT . f)

inside :: (Monad m) => String -> ParserT m a -> ParserT m a
inside s (ParserT (ExceptT rwst)) = ParserT $ ExceptT $ local (s :) rwst

runParserT :: (Monad m) => ParserT m a -> m (Either [Message] a, [Message])
runParserT (ParserT ex) = evalRWST (runExceptT ex) [] ()

-- | Prints the message and its context stack to standard error.
printMessage :: Message -> IO ()
printMessage (Message s ctx) = do
  hPutStrLn stderr s
  hPutStrLn stderr "Context (innermost first):"
  forM_ ctx $ \c -> hPutStrLn stderr $ "  — " ++ c

-- | Prints warnings and errors to standard error, and then throws an exception
-- if there were errors.
printParserIO :: (MonadIO m) => ParserT m a -> m a
printParserIO p = do
  (result, warnings) <- runParserT p
  liftIO $ forM_ warnings $ \msg -> do
    hPutStr stderr "Warning: "
    printMessage msg
  case result of
    Left errors -> liftIO $ do
      forM_ errors $ \msg -> do
        hPutStr stderr "ERROR: "
        printMessage msg
      error "printParserIO: fatal errors occurred"
    Right x -> return x

liftMaybe :: (Monad m, Show a) => (a -> m (Maybe b)) -> a -> ParserT m b
liftMaybe f x = lift (f x) >>= \case
  Nothing -> fatal $ "Unrecognized input: " ++ show x
  Just y  -> return y
