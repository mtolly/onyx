-- | This module exposes a polyfilled `requestAnimationFrame` function.
module RequestAnimationFrame
  ( requestAnimationFrame
  , requestAnimationFrame_
  ) where

import Prelude

import Control.Monad.Eff

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (Window())

-- | Request the specified action be called on the next animation frame, specifying the `Window` object.
foreign import requestAnimationFrame_ :: forall a eff. Window -> Eff (dom :: DOM | eff) a -> Eff (dom :: DOM | eff) Unit

-- | Request the specified action be called on the next animation frame.
requestAnimationFrame :: forall a eff. Eff (dom :: DOM | eff) a -> Eff (dom :: DOM | eff) Unit
requestAnimationFrame action = do
  w <- window
  requestAnimationFrame_ w action
