{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Onyx.JS where

import           Control.Exception         (bracket, bracket_, finally)
import           Control.Monad             (forM, forM_)
import qualified Data.Aeson                as A
import qualified Data.Aeson.Key            as K
import qualified Data.Aeson.KeyMap         as KM
import qualified Data.ByteString           as B
import qualified Data.ByteString.Short     as SB
import           Data.Coerce               (coerce)
import qualified Data.HashMap.Strict       as HM
import           Data.Int                  (Int64)
import qualified Data.Map                  as Map
import qualified Data.Scientific           as S
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Foreign         as TF
import qualified Data.Vector               as V
import           Data.Word                 (Word32)
import           Foreign                   (Ptr, alloca, allocaBytes, castPtr,
                                            nullPtr, peek, toBool)
import           Foreign.C                 (CDouble, CInt, CSize, CString)
import qualified Language.C.Inline         as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types          as C
import           System.IO.Unsafe          (unsafePerformIO)

data JSRuntime
data JSContext
data JSPropertyEnum

newtype JSValue = JSValue SB.ShortByteString
  deriving (Show)

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "JSRuntime", [t| JSRuntime |])
    , (C.TypeName "JSContext", [t| JSContext |])
    , (C.TypeName "JSPropertyEnum", [t| JSPropertyEnum |])
    , (C.TypeName "JSValue", [t| JSValue |])
    ]
  }

C.include "quickjs.h"
C.include "quickjs-libc.h"

--------------------------------------------------------------------------------

sizeOfJSValue :: Int
sizeOfJSValue = fromIntegral [C.pure| int { sizeof(JSValue) } |]

makeJSValue :: (Ptr JSValue -> IO ()) -> IO JSValue
makeJSValue f = allocaBytes sizeOfJSValue \p -> do
  f $ castPtr p
  JSValue <$> SB.packCStringLen (p, sizeOfJSValue)

withJSValue :: JSValue -> (Ptr JSValue -> IO a) -> IO a
withJSValue (JSValue b) f = SB.useAsCString b $ f . castPtr

{-# NOINLINE withJSValuePure #-}
withJSValuePure :: JSValue -> (Ptr JSValue -> IO a) -> a
withJSValuePure b f = unsafePerformIO $ withJSValue b f

js_FreeValue :: Ptr JSContext -> JSValue -> IO ()
js_FreeValue ctx v = withJSValue v \pv ->
  [C.block| void { JS_FreeValue($(JSContext* ctx), *$(JSValue* pv)); } |]

useValue :: Ptr JSContext -> JSValue -> IO a -> IO a
useValue ctx v f = finally f $ js_FreeValue ctx v

useValues :: Ptr JSContext -> [JSValue] -> IO a -> IO a
useValues ctx vs f = finally f $ mapM_ (js_FreeValue ctx) vs

jsValueToDouble :: Ptr JSContext -> JSValue -> IO (Maybe Double)
jsValueToDouble ctx v = withJSValue v \pv -> do
  isNum <- toBool <$> [C.exp| int { JS_IsNumber(*$(JSValue* pv)) } |]
  if isNum
    then alloca \pd -> do
      ret <- [C.exp| int { JS_ToFloat64($(JSContext* ctx), $(double* pd), *$(JSValue* pv)) } |]
      if ret == 0
        then Just . coerce <$> peek pd
        else fail "jsValueToDouble: number tag but got non-zero return code"
    else return Nothing

jsValueToText :: Ptr JSContext -> JSValue -> IO (Maybe T.Text)
jsValueToText ctx v = withJSValue v \pv -> do
  isStr <- toBool <$> [C.exp| int { JS_IsString(*$(JSValue* pv)) } |]
  if isStr
    then bracket
      [C.exp| const char* { JS_ToCString($(JSContext* ctx), *$(JSValue* pv)) } |]
      (\ps -> [C.block| void { JS_FreeCString($(JSContext* ctx), $(char* ps)); } |])
      \ps -> if ps == nullPtr
        then fail "jsValueToText: got null pointer"
        -- need text 2.1.2: else Just <$> TF.peekCString ps
        -- packCString copies; the pointer is freed when we leave the bracket
        else do
          bs <- B.packCString ps
          return $ Just $! TE.decodeUtf8 bs
    else return Nothing

jsValueToBool :: Ptr JSContext -> JSValue -> IO (Maybe Bool)
jsValueToBool ctx v = withJSValue v \pv -> do
  isBool <- toBool <$> [C.exp| int { JS_IsBool(*$(JSValue* pv)) } |]
  if isBool
    then do
      ret <- [C.exp| int { JS_ToBool($(JSContext* ctx), *$(JSValue* pv)) } |]
      if ret < 0
        then fail "jsValueToBool: bool tag but got error return code"
        else return $ Just $ toBool ret
    else return Nothing

js_IsNull :: JSValue -> Bool
js_IsNull v = withJSValuePure v \pv -> toBool <$> [C.exp| int { JS_IsNull(*$(JSValue* pv)) } |]

js_IsUndefined :: JSValue -> Bool
js_IsUndefined v = withJSValuePure v \pv -> toBool <$> [C.exp| int { JS_IsUndefined(*$(JSValue* pv)) } |]

js_IsException :: JSValue -> Bool
js_IsException v = withJSValuePure v \pv -> toBool <$> [C.exp| int { JS_IsException(*$(JSValue* pv)) } |]

jsValueToArray :: Ptr JSContext -> JSValue -> IO (Maybe [JSValue])
jsValueToArray ctx v = withJSValue v \pv -> do
  isArray <- toBool <$> [C.exp| int { JS_IsArray($(JSContext* ctx), *$(JSValue* pv)) } |]
  if isArray
    then do
      vlen <- makeJSValue \plen -> [C.exp| void {
        *$(JSValue* plen) = JS_GetPropertyStr($(JSContext* ctx), *$(JSValue* pv), "length");
      } |]
      jsValueToDouble ctx vlen >>= \case
        Nothing  -> fail "jsValueToArray: length isn't a number"
        Just len -> Just <$> forM [0 .. round len - 1] \i -> makeJSValue \px -> do
          let ui = fromIntegral (i :: Int64) :: Word32
          [C.exp| void {
            *$(JSValue* px) = JS_GetPropertyUint32
              ( $(JSContext* ctx)
              , *$(JSValue* pv)
              , $(uint32_t ui)
              );
          } |]
    else return Nothing

-- Gets the enumerable string-keyed own properties, same as Object.keys().
-- Each value in the returned map holds a new reference; caller must free them.
jsValueToObject :: Ptr JSContext -> JSValue -> IO (Maybe (HM.HashMap T.Text JSValue))
jsValueToObject ctx v = withJSValue v \pv -> do
  isObj <- toBool <$> [C.exp| int { JS_IsObject(*$(JSValue* pv)) } |]
  if isObj
    then alloca \pptab -> alloca \plen -> do
      ret <- [C.exp| int {
        JS_GetOwnPropertyNames
          ( $(JSContext* ctx)
          , $(JSPropertyEnum** pptab)
          , $(uint32_t* plen)
          , *$(JSValue* pv)
          , JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY
          )
      } |]
      if ret /= 0
        then fail "jsValueToObject: JS_GetOwnPropertyNames returned an error"
        else do
          tab <- peek pptab
          len <- peek plen
          -- this frees each atom in the table, then the table itself
          flip finally [C.block| void {
            JS_FreePropertyEnum($(JSContext* ctx), $(JSPropertyEnum* tab), $(uint32_t len));
          } |] $ fmap (Just . HM.fromList) $ forM [0 .. fromIntegral len - 1] \i -> do
            let ui = fromIntegral (i :: Int) :: Word32
            atom <- [C.exp| uint32_t { $(JSPropertyEnum* tab)[$(uint32_t ui)].atom } |]
            vkey <- makeJSValue \pkey -> [C.block| void {
              *$(JSValue* pkey) = JS_AtomToString($(JSContext* ctx), $(uint32_t atom));
            } |]
            key <- useValue ctx vkey $ jsValueToText ctx vkey >>= \case
              Nothing  -> fail "jsValueToObject: property name isn't a string"
              Just txt -> return txt
            value <- makeJSValue \pvalue -> [C.block| void {
              *$(JSValue* pvalue) = JS_GetProperty
                ( $(JSContext* ctx)
                , *$(JSValue* pv)
                , $(uint32_t atom)
                );
            } |]
            return (key, value)
    else return Nothing

data JSException = JSException
  { message :: T.Text
  , stack   :: T.Text
  } deriving (Show)

getJSException :: Ptr JSContext -> JSValue -> IO JSException
getJSException ctx v = withJSValue v \pv -> do
  message <- makeJSValue (\plen -> [C.exp| void {
    *$(JSValue* plen) = JS_GetPropertyStr($(JSContext* ctx), *$(JSValue* pv), "message");
  } |]) >>= jsValueToText ctx >>= \case
    Nothing -> return "<none>"
    Just x  -> return x
  stack <- makeJSValue (\plen -> [C.exp| void {
    *$(JSValue* plen) = JS_GetPropertyStr($(JSContext* ctx), *$(JSValue* pv), "stack");
  } |]) >>= jsValueToText ctx >>= \case
    Nothing -> return "<none>"
    Just x  -> return x
  return JSException{..}

-- Anything with no JSON equivalent (undefined, null, functions, symbols,
-- NaN/Infinity) becomes Null, at any depth. The subvalues we retrieve from
-- arrays and objects are freed here, so the caller's reference counts are
-- unchanged.
jsValueToJSON :: Ptr JSContext -> JSValue -> IO A.Value
jsValueToJSON ctx v = jsValueToBool ctx v >>= \case
  Just b  -> return $ A.Bool b
  Nothing -> jsValueToDouble ctx v >>= \case
    -- NaN and Infinity aren't representable in JSON
    Just d | isNaN d || isInfinite d -> return A.Null
           | otherwise               -> return $ A.Number $ S.fromFloatDigits d
    Nothing -> jsValueToText ctx v >>= \case
      Just t  -> return $ A.String t
      -- arrays are objects too, so this order matters
      Nothing -> jsValueToArray ctx v >>= \case
        Just xs -> useValues ctx xs
          $ A.Array . V.fromList <$> mapM (jsValueToJSON ctx) xs
        Nothing -> jsValueToObject ctx v >>= \case
          Just hm -> useValues ctx (HM.elems hm)
            $ A.Object . KM.fromHashMapText <$> traverse (jsValueToJSON ctx) hm
          Nothing -> return A.Null

-- The returned value has one new reference, which the caller must free (or
-- hand to something like JS_SetPropertyStr that consumes it).
jsonToJSValue :: Ptr JSContext -> A.Value -> IO JSValue
jsonToJSValue ctx = \case
  A.Null -> makeJSValue \p -> [C.block| void {
    *$(JSValue* p) = JS_NULL;
  } |]
  A.Bool b -> do
    let cb = if b then 1 else 0 :: CInt
    makeJSValue \p -> [C.block| void {
      *$(JSValue* p) = JS_NewBool($(JSContext* ctx), $(int cb));
    } |]
  A.Number n -> do
    let cd = coerce (S.toRealFloat n :: Double) :: CDouble
    makeJSValue \p -> [C.block| void {
      *$(JSValue* p) = JS_NewFloat64($(JSContext* ctx), $(double cd));
    } |]
  -- withCStringLen gives us UTF-8 without a terminator, which is what
  -- JS_NewStringLen wants
  A.String t -> TF.withCStringLen t \(pstr, len) -> do
    let clen = fromIntegral len :: CSize
    makeJSValue \p -> [C.block| void {
      *$(JSValue* p) = JS_NewStringLen
        ( $(JSContext* ctx)
        , $(const char* pstr)
        , $(size_t clen)
        );
    } |]
  A.Array xs -> do
    arr <- makeJSValue \p -> [C.block| void {
      *$(JSValue* p) = JS_NewArray($(JSContext* ctx));
    } |]
    forM_ (zip [0 :: Word32 ..] $ V.toList xs) \(i, x) -> do
      v <- jsonToJSValue ctx x
      withJSValue arr \parr -> withJSValue v \pv ->
        -- consumes our reference to v
        [C.block| void {
          JS_SetPropertyUint32
            ( $(JSContext* ctx)
            , *$(JSValue* parr)
            , $(uint32_t i)
            , *$(JSValue* pv)
            );
        } |]
    return arr
  A.Object o -> do
    obj <- makeJSValue \p -> [C.block| void {
      *$(JSValue* p) = JS_NewObject($(JSContext* ctx));
    } |]
    forM_ (KM.toList o) \(k, x) -> do
      v <- jsonToJSValue ctx x
      TF.withCString (K.toText k) \pkey -> withJSValue obj \pobj -> withJSValue v \pv ->
        -- consumes our reference to v
        [C.block| void {
          JS_SetPropertyStr
            ( $(JSContext* ctx)
            , *$(JSValue* pobj)
            , $(const char* pkey)
            , *$(JSValue* pv)
            );
        } |]
    return obj

-- Defines a global variable in the context, replacing any existing one.
setGlobalJSON :: Ptr JSContext -> T.Text -> A.Value -> IO ()
setGlobalJSON ctx key json = do
  glob <- makeJSValue \p -> [C.block| void {
    *$(JSValue* p) = JS_GetGlobalObject($(JSContext* ctx));
  } |]
  useValue ctx glob do
    v <- jsonToJSValue ctx json
    ret <- TF.withCString key \pkey -> withJSValue glob \pglob -> withJSValue v \pv ->
      -- consumes our reference to v, even if it returns an error
      [C.exp| int {
        JS_SetPropertyStr
          ( $(JSContext* ctx)
          , *$(JSValue* pglob)
          , $(const char* pkey)
          , *$(JSValue* pv)
          )
      } |]
    if ret < 0
      then fail "setGlobalJSON: JS_SetPropertyStr returned an error"
      else return ()

--------------------------------------------------------------------------------

js_NewRuntime :: IO (Ptr JSRuntime)
js_NewRuntime = [C.exp| JSRuntime* { JS_NewRuntime() } |]

js_FreeRuntime :: Ptr JSRuntime -> IO ()
js_FreeRuntime rt = [C.block| void { JS_FreeRuntime($(JSRuntime* rt)); } |]

js_NewContext :: Ptr JSRuntime -> IO (Ptr JSContext)
js_NewContext rt = [C.exp| JSContext* { JS_NewContext($(JSRuntime* rt)) } |]

js_FreeContext :: Ptr JSContext -> IO ()
js_FreeContext ctx = [C.block| void { JS_FreeContext($(JSContext* ctx)); } |]

-- The standard library (quickjs-libc) is linked in, but a plain JS_NewContext
-- doesn't install any of it; the command line interpreter opts in by calling
-- these. This defines the global objects: print, console.log, performance, and
-- __loadScript. We pass argc < 0 to skip scriptArgs, having no arguments.
js_std_add_helpers :: Ptr JSContext -> IO ()
js_std_add_helpers ctx = [C.block| void {
  js_std_add_helpers($(JSContext* ctx), -1, NULL);
} |]

-- Registers the 'std' and 'os' modules, so scripts can import them. They still
-- need to be imported; see js_std_import_modules to bind them as globals.
-- Requires js_std_init_handlers on the runtime.
js_std_add_modules :: Ptr JSContext -> IO ()
js_std_add_modules ctx = [C.block| void {
  js_init_module_std($(JSContext* ctx), "std");
  js_init_module_os($(JSContext* ctx), "os");
} |]

-- Needed by the 'os' and 'std' modules (timers, workers). Must be paired with
-- js_std_free_handlers before freeing the runtime.
js_std_init_handlers :: Ptr JSRuntime -> IO ()
js_std_init_handlers rt = [C.block| void { js_std_init_handlers($(JSRuntime* rt)); } |]

js_std_free_handlers :: Ptr JSRuntime -> IO ()
js_std_free_handlers rt = [C.block| void { js_std_free_handlers($(JSRuntime* rt)); } |]

-- Makes 'std' and 'os' visible to non-module code, the way the command line
-- interpreter does it. Requires js_std_add_modules first.
js_std_import_modules :: Ptr JSContext -> IO ()
js_std_import_modules ctx = do
  v <- js_Eval_Text ctx
    "import * as std from 'std';\nimport * as os from 'os';\nglobalThis.std = std;\nglobalThis.os = os;\n"
    "<input>"
    js_EVAL_TYPE_MODULE
  js_FreeValue ctx v

js_EVAL_TYPE_GLOBAL, js_EVAL_TYPE_MODULE :: CInt
js_EVAL_TYPE_GLOBAL = [C.pure| int { JS_EVAL_TYPE_GLOBAL } |]
js_EVAL_TYPE_MODULE = [C.pure| int { JS_EVAL_TYPE_MODULE } |]

setModuleLoader :: Ptr JSRuntime -> IO ()
setModuleLoader rt =
  [C.block| void {
    JS_SetModuleLoaderFunc2($(JSRuntime* rt), NULL, js_module_loader, js_module_check_attributes, NULL);
  } |]

-- 'input' must be zero terminated i.e. input[input_len] = '\0'.
js_Eval :: Ptr JSContext -> CString -> CSize -> CString -> CInt -> IO JSValue
js_Eval ctx input input_len filename eval_flags = makeJSValue \p ->
  [C.block| void {
    *$(JSValue* p) = JS_Eval
      ( $(JSContext* ctx)
      , $(const char* input)
      , $(size_t input_len)
      , $(const char* filename)
      , $(int eval_flags)
      );
  } |]

js_Eval_Text :: Ptr JSContext -> T.Text -> T.Text -> CInt -> IO JSValue
js_Eval_Text ctx src filename eval_flags =
  -- these use UTF-8, I'm assuming that's what quickjs wants
  TF.withCStringLen (src <> "\0") \(psrc, len1) ->
    TF.withCString filename \pfilename ->
      js_Eval ctx psrc (fromIntegral len1 - 1) pfilename eval_flags

js_GetException :: Ptr JSContext -> IO JSValue
js_GetException ctx = makeJSValue \p ->
  [C.block| void {
    *$(JSValue* p) = JS_GetException($(JSContext* ctx));
  } |]

js_Eval_Catch :: Ptr JSContext -> T.Text -> T.Text -> CInt -> IO (Either JSValue JSValue)
js_Eval_Catch ctx src filename eval_flags = do
  ret <- js_Eval_Text ctx src filename eval_flags
  if js_IsException ret
    then Left <$> js_GetException ctx
    else return $ Right ret

withJSContext :: (Ptr JSContext -> IO a) -> IO a
withJSContext f = bracket js_NewRuntime js_FreeRuntime \rt -> do
  setModuleLoader rt
  bracket_ (js_std_init_handlers rt) (js_std_free_handlers rt) do
    bracket (js_NewContext rt) js_FreeContext \ctx -> do
      js_std_add_helpers ctx
      js_std_add_modules ctx
      js_std_import_modules ctx
      f ctx
