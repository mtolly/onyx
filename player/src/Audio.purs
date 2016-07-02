module Audio where

import Prelude
import Control.Monad.Eff (Eff())
import Data.Time.Duration (Seconds())

foreign import data AUDIO :: !

foreign import data Audio :: *

foreign import loadAudio :: forall e. (Audio -> Eff (audio :: AUDIO | e) Unit) -> Eff (audio :: AUDIO | e) Unit

foreign import playFrom :: forall e. Audio -> Seconds -> Eff (audio :: AUDIO | e) Unit -> Eff (audio :: AUDIO | e) Unit

foreign import stop :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit

foreign import getPosition :: forall e. Audio -> Eff (audio :: AUDIO | e) Seconds
