module Main where

import Prelude
import Data.Maybe
import Data.Maybe.Unsafe
import Control.Monad.Eff
import qualified Graphics.Canvas as C
import DOM
import Data.DOM.Simple.Window
import Control.Monad.Eff.Console
import Data.Foreign
import Data.Foreign.Class
import Data.Either
import Control.Monad.Eff.Ref
import Data.Array
import Data.DOM.Simple.Types (DOMEvent())
import Data.DOM.Simple.Events (addMouseEventListener, MouseEventType(..), clientX, clientY)
import DOM.RequestAnimationFrame
import Data.Date
import Data.Time
import Data.Int (round, toNumber)
import Data.Ord (min)
import Control.Monad.Reader.Trans (runReaderT)
import qualified Data.List as L
import Control.Apply ((*>))
import Control.MonadPlus (guard)

import Audio
import Images
import Draw
import Song

foreign import onyxSong :: Foreign

main :: Eff
  ( canvas  :: C.Canvas
  , dom     :: DOM
  , console :: CONSOLE
  , ref     :: REF
  , now     :: Now
  , audio   :: AUDIO
  ) Unit
main = do
  canvas <- fromJust <$> C.getCanvasElementById "the-canvas"
  ctx <- C.getContext2D canvas
  clicks <- newRef []
  let click e = modifyRef clicks ((e :: DOMEvent) :)
  addMouseEventListener MouseClickEvent click globalWindow
  withImages $ \imageGetter -> do
    case read onyxSong of
      Left  e    -> log $ show e
      Right song -> loadAudio $ \audio -> do
        let loop app = do
              ms <- nowEpochMilliseconds
              let nowSeconds = min (case song of Song o -> o.end) $ case app of
                    Paused o -> o.pausedSongTime
                    Playing o -> o.startedSongTime + toSeconds ms - o.startedPageTime
              runReaderT draw
                { time: nowSeconds
                , app: app
                , song: song
                , getImage: imageGetter
                , canvas: canvas
                , context: ctx
                , pxToSecsVert: \px -> Seconds $ toNumber (px - 50) * 0.003
                , secsToPxVert: \(Seconds secs) -> round (secs / 0.003) + 50
                , pxToSecsHoriz: \px -> Seconds $ toNumber (px - 225) * 0.003
                , secsToPxHoriz: \(Seconds secs) -> round (secs / 0.003) + 225
                }
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
                              playFrom audio nowSeconds
                              handle et $ Playing
                                { startedPageTime: toSeconds ms
                                , startedSongTime: nowSeconds
                                , settings: o.settings
                                }
                            Playing o -> do
                              stop audio
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
                                  stop audio
                                  playFrom audio t
                                  handle et $ Playing $ o
                                    { startedPageTime = toSeconds ms
                                    , startedSongTime = t
                                    }
                            else handle et app_
                        else if 2*_M + _B <= x && x <= 2*_M + 2*_B
                          then let
                            go _ L.Nil                = handle et app_
                            go i (L.Cons action rest) = do
                              let ystart = windowH - i * (_M + _B)
                                  yend   = ystart + _B
                              if ystart <= y && y <= yend
                                then handle et $ case app_ of
                                  Paused  o -> Paused  o { settings = action o.settings }
                                  Playing o -> Playing o { settings = action o.settings }
                                else go (i + 1) rest
                            s = case song of Song o -> o
                            in go 1 $ L.fromFoldable $ concat
                              [ guard (isJust s.prokeys) *> [ (\sets -> sets { seeProKeys = not sets.seeProKeys }) ]
                              , guard (isJust s.keys   ) *> [ (\sets -> sets { seeKeys    = not sets.seeKeys    }) ]
                              , guard (isJust s.vocal  ) *> [ (\sets -> sets { seeVocal   = not sets.seeVocal   }) ]
                              , guard (isJust s.drums  ) *> [ (\sets -> sets { seeDrums   = not sets.seeDrums   }) ]
                              , guard (isJust s.bass   ) *> [ (\sets -> sets { seeBass    = not sets.seeBass    }) ]
                              , guard (isJust s.guitar ) *> [ (\sets -> sets { seeGuitar  = not sets.seeGuitar  }) ]
                              ]
                          else handle et app_
              handle evts $ case app of
                Paused _ -> app
                Playing o -> if nowSeconds == case song of Song o -> o.end
                  then Paused
                    { pausedSongTime: nowSeconds
                    , settings: o.settings
                    }
                  else Playing o
        loop $ Paused
          { pausedSongTime: Seconds 0.0
          , settings:
            { seeGuitar:  true
            , seeBass:    true
            , seeKeys:    true
            , seeProKeys: true
            , seeDrums:   true
            , seeVocal:   true
            }
          }
