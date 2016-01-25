module Main where

import Prelude
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Traversable
import Control.Monad.Eff
import Graphics.Canvas
import DOM
import Data.DOM.Simple.Window
import Control.Monad.Eff.Console
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import Data.Either
import qualified Data.Map as Map
import Data.Tuple
import Control.Monad.Eff.Ref
import Data.Array
import Data.DOM.Simple.Types (DOMEvent())
import Data.DOM.Simple.Events (addMouseEventListener, MouseEventType(..))

foreign import onyxSong :: Foreign

newtype Song = Song
  { end   :: Number
  , drums :: Maybe Drums
  }

newtype Drums = Drums
  { notes  :: Map.Map Number (Array Gem)
  , solo   :: Map.Map Number Boolean
  , energy :: Map.Map Number Boolean
  }

instance isForeignSong :: IsForeign Song where
  read f = do
    end <- readProp "end" f
    NullOrUndefined drums <- readProp "drums" f
    return $ Song { end: end, drums: drums }

readMap :: forall a b. (Ord a, IsForeign a, IsForeign b) => Foreign -> F (Map.Map a b)
readMap f = Map.fromFoldable <$> (readArray f >>= traverse readPair)

readPair :: forall a b. (IsForeign a, IsForeign b) => Foreign -> F (Tuple a b)
readPair pair = Tuple <$> readProp 0 pair <*> readProp 1 pair

instance isForeignDrums :: IsForeign Drums where
  read f = do
    notes  <- readProp "notes"  f >>= readMap
    solo   <- readProp "solo"   f >>= readMap
    energy <- readProp "energy" f >>= readMap
    return $ Drums { notes: notes, solo: solo, energy: energy }

data Gem = Kick | Red | YCym | YTom | BCym | BTom | GCym | GTom

instance isForeignGem :: IsForeign Gem where
  read f = read f >>= \s -> case s of
    "kick"  -> return Kick
    "red"   -> return Red
    "y-cym" -> return YCym
    "y-tom" -> return YTom
    "b-cym" -> return BCym
    "b-tom" -> return BTom
    "g-cym" -> return GCym
    "g-tom" -> return GTom
    _ -> Left $ TypeMismatch "drum gem name" $ show s

instance showGem :: Show Gem where
  show g = case g of
    Kick -> "Kick"
    Red  -> "Red"
    YCym -> "YCym"
    YTom -> "YTom"
    BCym -> "BCym"
    BTom -> "BTom"
    GCym -> "GCym"
    GTom -> "GTom"

draw :: forall e. Song -> CanvasElement -> Eff (dom :: DOM, canvas :: Canvas | e) Unit
draw _ c = do
  ctx <- getContext2D c
  w <- innerWidth globalWindow
  h <- innerHeight globalWindow
  setCanvasWidth  w c
  setCanvasHeight h c
  void $ setFillStyle "rgb(54,59,123)" ctx
  void $ fillRect ctx { x: 0.0, y: 0.0, w: w, h: h }

main :: Eff (canvas :: Canvas, dom :: DOM, console :: CONSOLE, ref :: REF) Unit
main = do
  c <- fromJust <$> getCanvasElementById "the-canvas"
  clicks <- newRef []
  let click e = modifyRef clicks ((e :: DOMEvent) :)
  addMouseEventListener MouseClickEvent click globalWindow
  case read onyxSong of
    Left e -> log $ show e
    Right song -> let
      loop = do
        evts <- modifyRef' clicks $ \evts -> {state: [], value: evts}
        draw song c
      in loop
