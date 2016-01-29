module Audio where

import Prelude
import Control.Monad.Eff (Eff())
import DOM (DOM())
import Data.Time (Seconds())

foreign import data AUDIO :: !

foreign import data Audio :: *

foreign import getTheAudio :: forall e. Eff (dom :: DOM | e) Audio

foreign import onLoad :: forall e.
  Audio -> Eff (audio :: AUDIO | e) Unit -> Eff (audio :: AUDIO | e) Unit

foreign import play :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit

foreign import pause :: forall e. Audio -> Eff (audio :: AUDIO | e) Unit

foreign import playFrom :: forall e. Seconds -> Audio -> Eff (audio :: AUDIO | e) Unit
