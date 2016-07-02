module Main where

import Prelude
import Data.Maybe
import Control.Monad.Eff
import Graphics.Canvas as C
import DOM
import Control.Monad.Eff.Console
import Data.Foreign
import Data.Foreign.Class
import Data.Either
import Control.Monad.Eff.Ref
import Data.Array
import DOM.RequestAnimationFrame
import Data.Date
import Data.Time.Duration
import Data.Int (round, toNumber)
import Data.Ord (min, max)
import Control.Monad.Reader.Trans (runReaderT)
import Data.List as L
import Control.Apply ((*>))
import Control.MonadPlus (guard)
import Math (abs)
import Control.Monad.Eff.Now
import Data.DateTime.Instant
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Audio
import Images
import Draw
import Song

foreign import onyxSong :: Foreign

foreign import onPoint
  :: forall e
  .  ({x :: Int, y :: Int} -> Eff (dom :: DOM | e) Unit)
  -> Eff (dom :: DOM | e) Unit

main :: Eff
  ( canvas  :: C.CANVAS
  , dom     :: DOM
  , console :: CONSOLE
  , ref     :: REF
  , now     :: NOW
  , audio   :: AUDIO
  ) Unit
main = do
  canvas <- C.getCanvasElementById "the-canvas" >>= \mc -> case mc of
    Just canvas -> pure canvas
    Nothing -> unsafeThrow "No canvas element found"
  ctx <- C.getContext2D canvas
  clicks <- newRef []
  onPoint $ \e -> modifyRef clicks (e : _)
  withImages $ \imageGetter -> do
    case read onyxSong of
      Left  e    -> log $ show e
      Right song -> loadAudio $ \audio -> do
        let loop app = do
              ms <- unInstant <$> now
              -- This is what we think the time should be, it moves nice and smooth
              let nowTheory = case app of
                    Paused o -> o.pausedSongTime
                    Playing o -> o.startedSongTime + convertDuration ms - o.startedPageTime
              -- This is what Howler says the audio position really is,
              -- it accounts for all sorts of audio hiccups but it's a bit choppy
              nowHowler <- case app of
                Paused o -> pure o.pausedSongTime
                Playing o -> getPosition audio
              -- So, we slightly correct nowTheory towards nowHowler when needed
              let isClose = abs (case nowHowler - nowTheory of Seconds n -> n) < 0.08
                  nowSeconds = if isClose then nowTheory else
                    nowTheory + if nowTheory < nowHowler
                      then min (Seconds 0.01) $ nowHowler - nowTheory
                      else max (Seconds (-0.01)) $ nowHowler - nowTheory
                  app' = if isClose then app else case app of
                    Paused o -> Paused o -- this should never happen
                    Playing o -> Playing
                      { startedPageTime: convertDuration ms
                      , startedSongTime: nowSeconds
                      , settings: o.settings
                      }
              runReaderT draw
                { time: nowSeconds
                , app: app'
                , song: song
                , getImage: imageGetter
                , canvas: canvas
                , context: ctx
                , pxToSecsVert: \px -> Seconds $ toNumber (px - 50) * 0.003
                , secsToPxVert: \(Seconds secs) -> round (secs / 0.003) + 50
                , pxToSecsHoriz: \px -> Seconds $ toNumber (px - 225) * 0.003
                , secsToPxHoriz: \(Seconds secs) -> round (secs / 0.003) + 225
                }
              {h: windowH'} <- getWindowDims
              let windowH = round windowH'
              evts <- modifyRef' clicks $ \evts -> {state: [], value: evts}
              let handle es app_ = case uncons es of
                    Nothing -> requestAnimationFrame $ loop app_
                    Just {head: {x: x, y: y}, tail: et} -> do
                      if _M <= x && x <= _M + _B
                        then if windowH - _M - _B <= y && y <= windowH - _M
                          then case app_ of -- play/pause button
                            Paused o -> do
                              ms <- unInstant <$> now
                              playFrom audio nowSeconds do
                                handle et $ Playing
                                  { startedPageTime: convertDuration ms
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
                                  ms <- unInstant <$> now
                                  stop audio
                                  playFrom audio t do
                                    handle et $ Playing $ o
                                      { startedPageTime = convertDuration ms
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
              case app of
                Playing o | nowSeconds >= (case song of Song o -> o.end) -> do
                  stop audio
                  handle evts $ Paused
                    { pausedSongTime: nowSeconds
                    , settings: o.settings
                    }
                _ -> handle evts app'
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
