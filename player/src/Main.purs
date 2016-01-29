module Main where

import Audio
import Images

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
import Data.DOM.Simple.Events (addMouseEventListener, MouseEventType(..), clientX, clientY)
import DOM.RequestAnimationFrame
import Data.Date
import Data.Time
import Data.Int (round, toNumber)
import Data.Ord (min)

foreign import onyxSong :: Foreign

newtype Song = Song
  { end   :: Seconds
  , drums :: Maybe Drums
  }

newtype Drums = Drums
  { notes  :: Map.Map Seconds (Array Gem)
  , solo   :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  }

instance isForeignSong :: IsForeign Song where
  read f = do
    end <- readProp "end" f
    NullOrUndefined drums <- readProp "drums" f
    return $ Song { end: Seconds end, drums: drums }

readTimedMap :: forall a. (IsForeign a) => Foreign -> F (Map.Map Seconds a)
readTimedMap f = Map.fromFoldable <$> (readArray f >>= traverse readTimedPair)

readTimedPair :: forall a. (IsForeign a) => Foreign -> F (Tuple Seconds a)
readTimedPair pair = Tuple <$> (Seconds <$> readProp 0 pair) <*> readProp 1 pair

instance isForeignDrums :: IsForeign Drums where
  read f = do
    notes  <- readProp "notes"  f >>= readTimedMap
    solo   <- readProp "solo"   f >>= readTimedMap
    energy <- readProp "energy" f >>= readTimedMap
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

type Settings =
  { seeGuitar  :: Boolean
  , seeBass    :: Boolean
  , seeKeys    :: Boolean
  , seeProKeys :: Boolean
  , seeDrums   :: Boolean
  }

data App
  = Paused
    { pausedSongTime :: Seconds
    , settings :: Settings
    }
  | Playing
    { startedPageTime :: Seconds
    , startedSongTime :: Seconds
    , settings :: Settings
    }

draw :: forall e. Seconds -> App -> Song -> (ImageID -> CanvasImageSource) ->
  CanvasElement -> Eff (dom :: DOM, canvas :: Canvas, console :: CONSOLE | e) Unit
draw t app (Song song) getImage canvas = do
  ctx <- getContext2D canvas
  windowW <- innerWidth globalWindow
  windowH <- innerHeight globalWindow
  setCanvasWidth  windowW canvas
  setCanvasHeight windowH canvas
  void $ setFillStyle "rgb(54,59,123)" ctx
  void $ fillRect ctx { x: 0.0, y: 0.0, w: windowW, h: windowH }
  let playPause = case app of
        Paused  _ -> Image_button_play
        Playing _ -> Image_button_pause
  void $ drawImage ctx (getImage playPause) (toNumber _M) (windowH - toNumber _M - toNumber _B)
  let timelineH = windowH - 3.0 * toNumber _M - toNumber _B - 2.0
      filled = case t / song.end of
        Seconds n -> n
  void $ setFillStyle "black" ctx
  void $ fillRect ctx { x: toNumber _M, y: toNumber _M, w: toNumber _B, h: timelineH + 2.0 }
  void $ setFillStyle "white" ctx
  void $ fillRect ctx { x: toNumber _M + 1.0, y: toNumber _M + 1.0, w: toNumber _B - 2.0, h: timelineH }
  void $ setFillStyle "rgb(100,130,255)" ctx
  void $ fillRect ctx
    { x: toNumber _M + 1.0
    , y: toNumber _M + 1.0 + timelineH * (1.0 - filled)
    , w: toNumber _B - 2.0
    , h: timelineH * filled
    }

_M :: Int
_M = 20 -- margin

_B :: Int
_B = 41 -- height/width of buttons

main :: Eff (canvas :: Canvas, dom :: DOM, console :: CONSOLE, ref :: REF, now :: Now) Unit
main = do
  canvas <- fromJust <$> getCanvasElementById "the-canvas"
  clicks <- newRef []
  let click e = modifyRef clicks ((e :: DOMEvent) :)
  addMouseEventListener MouseClickEvent click globalWindow
  withImages $ \imageGetter -> do
    case read onyxSong of
      Left  e    -> log $ show e
      Right song -> do
        audio <- getTheAudio
        onLoad audio $ let
          loop app = do
            ms <- nowEpochMilliseconds
            let nowSeconds = min (case song of Song o -> o.end) $ case app of
                  Paused o -> o.pausedSongTime
                  Playing o -> o.startedSongTime + toSeconds ms - o.startedPageTime
            draw nowSeconds app song imageGetter canvas
            windowH <- round <$> innerHeight globalWindow
            evts <- modifyRef' clicks $ \evts -> {state: [], value: evts}
            let handle es app_ = case uncons es of
                  Nothing -> requestAnimationFrame $ loop app_
                  Just {head: e, tail: et} -> do
                    x <- clientX e
                    y <- clientY e
                    if _M <= x && x <= _M + _B
                      then if windowH - _M - _B <= y && y <= windowH - _M
                        then case app_ of -- play/pause button
                          Paused o -> do
                            ms <- nowEpochMilliseconds
                            playFrom nowSeconds audio
                            handle et $ Playing
                              { startedPageTime: toSeconds ms
                              , startedSongTime: nowSeconds
                              , settings: o.settings
                              }
                          Playing o -> do
                            pause audio
                            handle et $ Paused
                              { pausedSongTime: nowSeconds
                              , settings: o.settings
                              }
                        else if _M <= y && y <= windowH - 2*_M - _B
                          then let -- progress bar
                            frac = 1.0 - toNumber (y - _M) / toNumber (windowH - 3*_M - _B)
                            t = Seconds frac * case song of Song o -> o.end
                            in case app_ of
                              Paused o -> handle et $ Paused $ o
                                { pausedSongTime = t
                                }
                              Playing o -> do
                                ms <- nowEpochMilliseconds
                                playFrom t audio
                                handle et $ Playing $ o
                                  { startedPageTime = toSeconds ms
                                  , startedSongTime = t
                                  }
                          else handle et app_
                      else handle et app_ -- TODO: handle buttons
            handle evts $ case app of
              Paused _ -> app
              Playing o -> if nowSeconds == case song of Song o -> o.end
                then Paused
                  { pausedSongTime: nowSeconds
                  , settings: o.settings
                  }
                else Playing o
          in loop $ Paused
            { pausedSongTime: Seconds 0.0
            , settings:
              { seeGuitar:  true
              , seeBass:    true
              , seeKeys:    true
              , seeProKeys: true
              , seeDrums:   true
              }
            }
