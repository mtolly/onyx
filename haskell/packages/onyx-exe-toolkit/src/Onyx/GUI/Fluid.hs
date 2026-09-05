{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Support for windows laid out in FLUID (.fl files under @fluid/@, compiled
-- to C++ by @Setup.hs@).
--
-- Layouts are authored at natural English width with no menu bar. Two things
-- FLUID can't express are applied afterwards by 'adaptWindow': the per-language
-- horizontal stretch, and the vertical space a menu bar needs on Windows and
-- Linux.
--
-- Text comes from the localization files rather than the layout. FLUID's
-- gettext-style i18n mode is pointed at @onyx_l10n@ (see @cbits/onyx_l10n.h@),
-- which routes @{message-key}@ labels, tooltips and menu items back here.
module Onyx.GUI.Fluid
( setGlobalL
, adaptWindow
, lockMinimumSize
, inGroup
, menuBarHeight
) where

import           Control.Exception        (bracket_)
import qualified Data.ByteString          as B
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Foreign
import           Foreign.C
import           Onyx.GUI.Binding         (Fl_Group, Fl_Window)
import           Onyx.Localization        (L (..), Language (..), getAttribute,
                                           l10n, l10nFull)
import           System.Info              (os)
import           System.IO.Unsafe         (unsafePerformIO)

macOS :: Bool
macOS = os == "darwin"

-- | Vertical space a window has to reserve for its menu bar. Zero on macOS,
-- where the menu goes in the system-wide bar instead.
menuBarHeight :: CInt
menuBarHeight = if macOS then 0 else 30

--------------------------------------------------------------------------------
-- Localization hook

-- | FLUID-generated constructors localize their text as they run, from C++,
-- so the loaded localization has to be reachable without an implicit
-- parameter. 'Main' publishes it here at startup.
{-# NOINLINE globalL #-}
globalL :: IORef (Maybe L)
globalL = unsafePerformIO $ newIORef Nothing

setGlobalL :: L -> IO ()
setGlobalL = writeIORef globalL . Just

-- | Called from @onyx_l10n@ for each @{message-key}@ or @{message-key.attr}@
-- found in a layout. Returns a malloc'd UTF-8 string that the C++ side interns
-- and never frees, or null if localization isn't loaded yet.
onyx_l10n_hs :: CString -> IO CString
onyx_l10n_hs ckey = readIORef globalL >>= \case
  Nothing  -> return nullPtr
  Just loc -> do
    key <- peekUTF8 ckey
    let ?loc = loc
    -- Fluent identifiers can't contain a dot, so the first one separates a
    -- message from one of its attributes.
    newUTF8 case T.breakOn "." key of
      (msg, dotAttr) | not (T.null dotAttr) ->
        getAttribute (T.drop 1 dotAttr) (l10nFull msg [])
      _ -> l10n key

foreign export ccall onyx_l10n_hs :: CString -> IO CString

peekUTF8 :: CString -> IO T.Text
peekUTF8 = fmap (TE.decodeUtf8With lenientDecode) . B.packCString

-- | Like 'newCString' but UTF-8, and malloc'd so the C++ side owns it.
newUTF8 :: T.Text -> IO CString
newUTF8 t = B.useAsCStringLen (TE.encodeUtf8 t) \(src, len) -> do
  dst <- mallocBytes $ len + 1
  copyBytes dst src len
  pokeByteOff dst len (0 :: CChar)
  return dst

--------------------------------------------------------------------------------
-- Layout adaptation

foreign import ccall "onyx_adapt_window"
  c_onyx_adapt_window :: Ptr Fl_Window -> CDouble -> CInt -> IO ()

foreign import ccall "onyx_lock_minimum_size"
  lockMinimumSize :: Ptr Fl_Window -> IO ()

foreign import ccall "onyx_group_begin"
  groupBegin :: Ptr Fl_Group -> IO ()

foreign import ccall "onyx_group_end"
  groupEnd :: Ptr Fl_Group -> IO ()

-- | Build widgets into a group whose FLUID constructor already called @end()@.
-- Used for the parts of a layout that stay hand-written: menu bars, and tab
-- or panel contents that are generated from data.
inGroup :: Ptr Fl_Group -> IO a -> IO a
inGroup group = bracket_ (groupBegin group) (groupEnd group)

-- | Apply the language's horizontal stretch and reserve menu bar space. Call
-- once, after the FLUID constructor and before adding the menu bar.
--
-- Returns the height that was reserved at the top, which is where the caller
-- should put the menu bar (and zero on macOS, meaning don't make one).
adaptWindow :: (?loc :: L) => Ptr Fl_Window -> IO CInt
adaptWindow window = do
  let stretch = realToFrac ?loc.loadedLanguage.stretchWidth
  c_onyx_adapt_window window stretch menuBarHeight
  return menuBarHeight
