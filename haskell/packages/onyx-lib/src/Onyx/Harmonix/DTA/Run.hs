{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Harmonix.DTA.Run where

import           Control.Monad.Extra       (concatMapM, forM_, mapMaybeM, void,
                                            when, (>=>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as HM
import           Data.List.Extra           (foldl', unsnoc)
import           Data.String               (IsString)
import qualified Data.Text                 as T
import           Onyx.Harmonix.DTA.Base
import           Onyx.Harmonix.DTA.Parse   (parseStack)
import           Onyx.Harmonix.DTA.Scan    (scanStack)
import           Onyx.StackTrace

type Eval s m = StackTraceT (StateT s m)

-- preprocessing

newtype DTAPreprocess s = DTAPreprocess
  { defines :: HM.HashMap s (Chunk s)
  }

splitIfDef :: (SendMessage m, Show s) => String -> [Chunk s] -> Eval (DTAPreprocess s) m ([Chunk s], [Chunk s], [Chunk s])
splitIfDef type_ xs = do
  let getBranch = go (0 :: Int) []
      go 0 past future@(Else  : _) = return (reverse past, future)
      go 0 past future@(EndIf : _) = return (reverse past, future)
      go level past future = case future of
        x : rest -> if (case x of EndIf -> True; _ -> False)
          then go (level - 1) (x : past) rest
          else if (case x of IfDef{} -> True; IfNDef{} -> True; _ -> False)
            then go (level + 1) (x : past) rest
            else go level (x : past) rest
        [] -> return (reverse past, future) -- will be an error
  (true, xs1) <- getBranch xs
  case xs1 of
    Else : xs2 -> do
      (false, xs3) <- getBranch xs2
      case xs3 of
        EndIf : xs4 -> return (true, false, xs4)
        _ -> fatal $ "Couldn't parse " <> type_ <> " structure: " <> show xs
    EndIf : xs2 -> return (true, [], xs2)
    _ -> fatal $ "Couldn't parse " <> type_ <> " structure: " <> show xs

preprocessDTA :: (SendMessage m, Show s, Hashable s) => [Chunk s] -> Eval (DTAPreprocess s) m [Chunk s]
preprocessDTA (x : xs) = case x of

  Sym s -> lift (gets $ HM.lookup s . (.defines)) >>= \case
    Just s' -> preprocessDTA $ s' : xs
    Nothing -> (x :) <$> preprocessDTA xs

  Undef s -> do
    lift $ modify $ \pp -> pp
      { defines = HM.delete s pp.defines }
    preprocessDTA xs

  IfDef s -> do
    (t, f, after) <- splitIfDef "#ifdef" xs
    b <- lift $ gets $ HM.member s . (.defines)
    preprocessDTA $ (if b then t else f) <> after
  IfNDef s -> do
    (t, f, after) <- splitIfDef "#ifndef" xs
    b <- lift $ gets $ HM.member s . (.defines)
    preprocessDTA $ (if b then f else t) <> after
  Else -> do
    warn "Unmatched #else"
    preprocessDTA xs
  EndIf -> do
    warn "Unmatched #endif"
    preprocessDTA xs

  Define s -> case xs of
    [] -> do
      warn $ "#define does not have a value following it: " <> show x
      return []
    next : rest -> do
      lift $ modify $ \pp -> pp
        { defines = HM.insert s next pp.defines }
      preprocessDTA rest

  -- these require loading other files
  Include _ -> do
    warn $ "#include not implemented yet: " <> show x
    preprocessDTA xs
  Merge _ -> do
    warn $ "#merge not implemented yet: " <> show x
    preprocessDTA xs

  -- I don't know what this does
  Autorun -> preprocessDTA xs

  Parens (Tree n tree) -> do
    y  <- Parens . Tree n <$> preprocessDTA tree
    ys <- preprocessDTA xs
    return $ y : ys
  Braces (Tree n tree) -> do
    y  <- Braces . Tree n <$> preprocessDTA tree
    ys <- preprocessDTA xs
    return $ y : ys
  Brackets (Tree n tree) -> do
    y  <- Brackets . Tree n <$> preprocessDTA tree
    ys <- preprocessDTA xs
    return $ y : ys

  _ -> (x :) <$> preprocessDTA xs

preprocessDTA [] = return []

initPreprocess :: (IsString s, Hashable s) => DTAPreprocess s
initPreprocess = DTAPreprocess
  { defines = HM.fromList
    [ ("TRUE" , Int 1)
    , ("FALSE", Int 0)
    ]
  }

runPreprocess :: (SendMessage m, IsString s, Hashable s, Show s) => [Chunk s] -> StackTraceT m [Chunk s]
runPreprocess chunks = inside "DTA preprocessing" $ do
  mapStackTraceT (`evalStateT` initPreprocess) $ preprocessDTA chunks

-- evaluation

data DTAState m s = DTAState
  { functions :: HM.HashMap s ([Chunk s] -> Eval (DTAState m s) m [Chunk s])
  , variables :: HM.HashMap s [Chunk s]
  }

evaluateDTA :: (SendMessage m, Show s, Hashable s) => Chunk s -> Eval (DTAState m s) m [Chunk s]
evaluateDTA = \case

  Var v -> lift (gets $ HM.lookup v . (.variables)) >>= \case
    Just xs -> return xs
    Nothing -> do
      warn $ "Unbound variable " <> show v
      return []
  Parens (Tree n t) -> pure . Parens . Tree n <$> concatMapM evaluateDTA t
  Braces (Tree _ []) -> do
    warn "Empty {} braces"
    return []
  Braces (Tree _ (x : xs)) -> evaluateDTA x >>= \case
    [Sym s] -> lift (gets $ HM.lookup s . (.functions)) >>= \case
      Just func -> func xs
      Nothing -> do
        warn $ "Ignoring undefined function " <> show x
        return []
    notSym -> do
      warn $ "Function name did not evaluate to a symbol: " <> show x <> " -> " <> show notSym
      return []

  x@(Brackets _) -> do
    warn "[] brackets not implemented yet"
    return [x]
  x -> return [x]

evalReturnLast :: (SendMessage m, Show s, Hashable s) => [Chunk s] -> Eval (DTAState m s) m [Chunk s]
evalReturnLast chunks = case unsnoc chunks of
  Nothing            -> return []
  Just (steps, step) -> do
    mapM_ evaluateDTA steps
    evaluateDTA step

initDTAState :: (SendMessage m) => DTAState m T.Text
initDTAState = DTAState
  { variables = HM.empty
  , functions = HM.fromList

    -- basic stuff we need for some import logic
    [ ( "do"
      -- return-all behavior https://github.com/hmxmilohax/Guitar-Hero-II-Deluxe/blob/99dbd767b/_ark/ui/game.dta#L12-L16
      , concatMapM evaluateDTA
      )
    , ( "set"
      , \case
        Var v : args -> do
          results <- concatMapM evaluateDTA args
          lift $ modify $ \s -> s { variables = HM.insert v results s.variables }
          return []
        args -> do
          warn $ "{set} was not given $variable as first parameter: " <> show args
          return []
      )
    , ( "if"
      , \case
        cond : args -> do
          result <- evaluateDTA cond
          if toBool result
            then evalReturnLast args
            else return []
        [] -> do
          warn "{if} given no parameters"
          return []
      )
    , ( "unless"
      , \case
        cond : args -> do
          result <- evaluateDTA cond
          if toBool result
            then return []
            else evalReturnLast args
        [] -> do
          warn "{unless} given no parameters"
          return []
      )
    , ( "if_else"
      , \case
        [cond, t, f] -> do
          result <- evaluateDTA cond
          if toBool result
            then evaluateDTA t
            else evaluateDTA f
        args -> do
          warn $ "{if_else} not given 3 parameters: " <> show args
          return []
      )
    , ( "func"
      , \case
        Sym name : params : body -> case params of
          Parens (Tree _ params') -> case mapM (\case Var v -> Just v; _ -> Nothing) params' of
            Just paramNames -> let
              newFunc args = do
                when (length args /= length paramNames) $ warn $ unwords
                  [ "Function"
                  , show name
                  , "expected"
                  , show $ length paramNames
                  , "argument(s) but was given"
                  , show $ length args
                  ]
                args' <- mapM evaluateDTA args
                origState <- lift get
                lift $ put origState
                  { variables = HM.union (HM.fromList $ zip paramNames args') origState.variables }
                results <- evalReturnLast body
                lift $ put origState
                return results
              in do
                lift $ modify $ \s -> s { functions = HM.insert name newFunc s.functions }
                return []
            Nothing -> do
              warn $ "Non-variable in parameter list for {func}: " <> show params'
              return []
          _ -> do
            warn $ "Unrecognized parameter list format for {func}: " <> show params
            return []
        args -> do
          warn $ "Unrecognized parameters given to {func}: " <> show args
          return []
      )

    -- assuming math ops work like lisp
    , ( "+"
      , plusTimes "+" 0 (+) (+)
      )
    , ( "*"
      , plusTimes "*" 1 (*) (*)
      )
    , ( "-"
      , let
        step (Left  acc) (Left  x) = Left  $ acc - x
        step (Right acc) (Left  x) = Right $ acc - realToFrac x
        step (Left  acc) (Right x) = Right $ realToFrac acc - x
        step (Right acc) (Right x) = Right $ acc - x
        in concatMapM evaluateDTA >=> getNumbers "-" >=> \case
          []         -> warn "{-} given no arguments" >> return []
          [arg]      -> returnNumber $ either (Left . negate) (Right . negate) arg
          arg : args -> returnNumber $ foldl' step arg args
      )
    , ( "/"
      , let
        step acc (Left  x) = acc / realToFrac x
        step acc (Right x) = acc / x
        in concatMapM evaluateDTA >=> getNumbers "/" >=> \case
          []         -> warn "{/} given no arguments" >> return []
          [arg]      -> returnNumber $ Right $ recip $ either realToFrac id arg
          arg : args -> returnNumber $ Right $ foldl' step (either realToFrac id arg) args
      )

    , ( "!"
      , concatMapM evaluateDTA >=> \args -> returnBool $ not $ toBool args
      )
    , ( "&&"
      , let
        go = \case
          []     -> returnBool True
          x : xs -> evaluateDTA x >>= \cond -> if toBool cond
            then go xs
            else returnBool False
        in go
      )
    , ( "||"
      , let
        go = \case
          []     -> returnBool False
          x : xs -> evaluateDTA x >>= \cond -> if toBool cond
            then returnBool True
            else go xs
        in go
      )

    , ( "=="
      , concatMapM evaluateDTA >=> \case
        []         -> returnBool True
        arg : args -> returnBool $ all (== arg) args
      )
    , ( "!="
      , \args -> evaluateDTA $ call [Sym "!", call $ Sym "==" : args]
      )
    , ( "<"
      , comparison "<" (<) (<)
      )
    , ( ">"
      , comparison ">" (>) (>)
      )
    , ( "<="
      , comparison "<=" (<=) (<=)
      )
    , ( ">="
      , comparison ">=" (>=) (>=)
      )

    , ( "foreach_int"
      , \args -> do
        case args of
          var@(Var _) : start : end : body -> evaluateDTA start >>= \case
            [Int startIndex] -> evaluateDTA end >>= \case
              [Int endIndex] -> forM_ [startIndex .. endIndex - 1] $ \i -> do
                -- is the last index endIndex or endIndex - 1?
                void $ evaluateDTA $ call [Sym "set", var, Int i]
                mapM_ evaluateDTA body
              endResult -> warn $ "Non-int end parameter for {foreach_int}: " <> show endResult
            startResult -> warn $ "Non-int start parameter for {foreach_int}: " <> show startResult
          _ -> warn $ "Unrecognized arguments for {foreach_int}: " <> show args
        return []
      )

    -- for debugging
    , ( "onyx_print"
      , concatMapM evaluateDTA >=> \args -> do
        let printer = \case
              String          x -> lg $ T.unpack x
              Int             x -> lg $ show x
              Float           x -> lg $ show x
              Sym             x -> lg $ T.unpack x
              Unhandled         -> lg "kDataUnhandled"
              Parens (Tree _ t) -> mapM_ printer t
              _                 -> return ()
        mapM_ printer args
        return []
      )

    {-
    other stuff we could implement:
    'foreach' (don't know what 2nd param does)
    'sprint' 'sprintf'
    'elem'
    'int' (float -> int, see https://github.com/hmxmilohax/Guitar-Hero-II-Deluxe/blob/99dbd767b/_ark/ui/game.dta#L25 )
    -}

    ]

  } where
    getNumbers name = mapMaybeM $ \case
      Int   n -> return $ Just $ Left  n
      Float n -> return $ Just $ Right n
      arg     -> do
        warn $ "{" <> name <> "} expected a number but was given: " <> show arg
        return Nothing
    returnNumber x = return [either Int Float x]
    plusTimes name base intOp floatOp = let
      step (Left  acc) (Left  x) = Left  $ intOp acc x
      step (Right acc) (Left  x) = Right $ floatOp acc (realToFrac x)
      step (Left  acc) (Right x) = Right $ floatOp (realToFrac acc) x
      step (Right acc) (Right x) = Right $ floatOp acc x
      in concatMapM evaluateDTA >=> getNumbers name >=> returnNumber . foldl' step (Left base)
    call = Braces . Tree 0
    toBool = \case
      -- not actually sure which values are truthy
      []          -> False
      [Int    0 ] -> False
      [Float  0 ] -> False
      [String ""] -> False
      _           -> True
    returnBool b = return [Int $ if b then 1 else 0]
    comparison name intOp floatOp = concatMapM evaluateDTA >=> \case
      -- actually in lisp these can take more than 2 args
      [Int   x, Int   y] -> returnBool $ intOp x y
      [Int   x, Float y] -> returnBool $ floatOp (fromIntegral x) y
      [Float x, Int   y] -> returnBool $ floatOp x (fromIntegral y)
      [Float x, Float y] -> returnBool $ floatOp x y
      args -> do
        warn $ "Unexpected arguments for {" <> name <> "}: " <> show args
        return []

testRun :: T.Text -> IO (Either Messages [Chunk T.Text])
testRun txt = logStdout $ do
  DTA _ (Tree _ chunks) <- scanStack txt >>= parseStack
  prep <- mapStackTraceT (`evalStateT` initPreprocess) $ preprocessDTA chunks
  mapStackTraceT (`evalStateT` initDTAState) $ concatMapM evaluateDTA prep
