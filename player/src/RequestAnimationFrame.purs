-- | This module exposes a polyfilled `requestAnimationFrame` function.
module RequestAnimationFrame
  ( requestAnimationFrame
  , requestAnimationFrame_
  ) where

import           Prelude         (Unit, bind)

import           Effect          (Effect)

import           Web.HTML        (window)
import           Web.HTML.Window (Window)

-- | Request the specified action be called on the next animation frame, specifying the `Window` object.
foreign import requestAnimationFrame_ :: forall a. Window -> Effect a -> Effect Unit

-- | Request the specified action be called on the next animation frame.
requestAnimationFrame :: forall a. Effect a -> Effect Unit
requestAnimationFrame action = do
  w <- window
  requestAnimationFrame_ w action
