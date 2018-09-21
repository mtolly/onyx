module Audio where

import           Data.Time.Duration (Seconds)
import           Effect             (Effect)
import           Prelude

foreign import data Audio :: Type

foreign import loadAudio :: (Audio -> Effect Unit) -> Effect Unit

foreign import playFrom :: Audio -> Seconds -> Effect Unit -> Effect Unit

foreign import stop :: Audio -> Effect Unit
