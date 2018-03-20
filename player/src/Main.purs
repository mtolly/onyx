module Main where

import           Control.Monad.Eff           (Eff)
import           Control.Monad.Eff.Exception (EXCEPTION, error, throwException, catchException)
import           Control.Monad.Eff.Now       (NOW, now)
import           Control.Monad.Eff.Ref       (REF, modifyRef, modifyRef',
                                              newRef, writeRef, readRef)
import           Control.Monad.Except        (runExcept)
import           Control.MonadPlus           (guard)
import           Data.Array                  (concat, uncons, (:), reverse, concatMap)
import           Data.DateTime.Instant       (unInstant)
import           Data.Either                 (Either (..))
import           Data.Foreign                (Foreign, isUndefined)
import           Data.Int                    (round, toNumber)
import           Data.List                   as L
import           Data.Maybe                  (Maybe (..), isJust)
import           Data.Time.Duration          (Seconds (..), convertDuration, Milliseconds (..))
import           DOM                         (DOM)
import           Graphics.Canvas             as C
import           Prelude
import           RequestAnimationFrame       (requestAnimationFrame)
import Data.Set as Set
import Data.Tuple (Tuple(..))

import           Audio                       (AUDIO, loadAudio, playFrom, stop)
import           Draw                        (App (..), draw, getWindowDims, _B,
                                              _M)
import           Images                      (withImages)
import           Song                        (Song (..), FlexPart(..), Flex(..), isForeignSong)
import Style (customize)

foreign import onyxSong :: Foreign

foreign import onPoint
  :: forall e
  .  ({x :: Int, y :: Int} -> Eff (dom :: DOM | e) Unit)
  -> Eff (dom :: DOM | e) Unit

foreign import numMod :: Number -> Number -> Number

foreign import displayError :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import setTitle :: forall e. String -> Eff (dom :: DOM | e) Unit

drawLoading :: forall e. C.CanvasElement -> Eff (dom :: DOM, canvas :: C.CANVAS, now :: NOW | e) Unit
drawLoading canvas = do
  ctx <- C.getContext2D canvas
  {w: windowW, h: windowH} <- getWindowDims
  void $ C.setCanvasWidth  windowW canvas
  void $ C.setCanvasHeight windowH canvas

  void $ C.setFillStyle customize.loadingBackground ctx
  void $ C.fillRect ctx { x: 0.0, y: 0.0, w: windowW, h: windowH }

  Milliseconds ms <- unInstant <$> now
  let loopTime = customize.loadingLoopTime_ms
      loopTime8 = customize.loadingLoopTime_ms / 8.0
      t = numMod ms loopTime
      smallSide = customize.loadingSmallSquareSize
      bigX = windowW / 2.0 - smallSide
      bigY = windowH / 2.0 - smallSide
      bigSide = smallSide * 2.0
      smallDraw x y = void $
        C.fillRect ctx { x: x, y: y, w: smallSide, h: smallSide }
  void $ C.setFillStyle customize.loadingBigSquare ctx
  void $ C.fillRect ctx { x: bigX, y: bigY, w: bigSide, h: bigSide }
  void $ C.setFillStyle customize.loadingSmallSquares ctx
  let smalls
        | t < loopTime8 = do
          smallDraw bigX bigY
          smallDraw (bigX + smallSide) (bigY + smallSide)
        | t < loopTime8 * 4.0 = do
          let moved = ((t - loopTime8) / (loopTime8 * 3.0)) * smallSide
          smallDraw (bigX + moved) bigY
          smallDraw (bigX + smallSide - moved) (bigY + smallSide)
        | t < loopTime8 * 5.0 = do
          smallDraw (bigX + smallSide) bigY
          smallDraw bigX (bigY + smallSide)
        | otherwise = do
          let moved = ((t - loopTime8 * 5.0) / (loopTime8 * 3.0)) * smallSide
          smallDraw bigX (bigY + smallSide - moved)
          smallDraw (bigX + smallSide) (bigY + moved)
  smalls

main :: Eff
  ( canvas    :: C.CANVAS
  , dom       :: DOM
  , ref       :: REF
  , now       :: NOW
  , audio     :: AUDIO
  , exception :: EXCEPTION
  ) Unit
main = catchException (\e -> displayError (show e) *> throwException e) do
  canvas <- C.getCanvasElementById "the-canvas" >>= \mc -> case mc of
    Just canvas -> pure canvas
    Nothing     -> throwException $ error "Canvas element not found"
  ctx <- C.getContext2D canvas
  clicks <- newRef []
  onPoint $ \e -> modifyRef clicks (e : _)
  song <- case runExcept $ isForeignSong onyxSong of
    Left  e    -> throwException $ error $ if isUndefined onyxSong
      then "No song data was found. Is there a song.js present?"
      else show e
    Right song -> pure song
  setTitle $ case song of Song o -> o.title <> " (" <> o.artist <> ") Onyx Web Player"
  imageGetterRef <- newRef Nothing
  withImages $ writeRef imageGetterRef <<< Just
  audioRef <- newRef Nothing
  loadAudio $ writeRef audioRef <<< Just
  let continueLoading = do
        drawLoading canvas
        imageGetterMaybe <- readRef imageGetterRef
        audioMaybe <- readRef audioRef
        case imageGetterMaybe of
          Just imageGetter -> case audioMaybe of
            Just audio -> requestAnimationFrame $ makeLoop imageGetter audio
            Nothing -> requestAnimationFrame continueLoading
          Nothing -> requestAnimationFrame continueLoading
      makeLoop imageGetter audio = let
        loop app = do
          ms <- unInstant <$> now
          let nowTheory = case app of
                Paused  o -> o.pausedSongTime
                -- Calculate what time should be so it moves nice and smooth
                Playing o -> o.startedSongTime + convertDuration ms - o.startedPageTime
              continue app' = do
                draw
                  { time: nowTheory
                  , app: app'
                  , song: song
                  , getImage: imageGetter
                  , canvas: canvas
                  , context: ctx
                  , pxToSecsVert: \px -> Seconds $ toNumber (px - customize.targetPositionVert) / customize.trackSpeed
                  , secsToPxVert: \(Seconds secs) -> round (secs * customize.trackSpeed) + customize.targetPositionVert
                  , pxToSecsHoriz: \px -> Seconds $ toNumber (px - customize.targetPositionHoriz) / customize.trackSpeed
                  , secsToPxHoriz: \(Seconds secs) -> round (secs * customize.trackSpeed) + customize.targetPositionHoriz
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
                                ms' <- unInstant <$> now
                                playFrom audio nowTheory do
                                  handle et $ Playing
                                    { startedPageTime: convertDuration ms'
                                    , startedSongTime: nowTheory
                                    , settings: o.settings
                                    }
                              Playing o -> do
                                stop audio
                                handle et $ Paused
                                  { pausedSongTime: nowTheory
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
                                    ms' <- unInstant <$> now
                                    stop audio
                                    playFrom audio t do
                                      handle et $ Playing $ o
                                        { startedPageTime = convertDuration ms'
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
                              toggle tup set = if Set.member tup set then Set.delete tup set else Set.insert tup set
                              toggleVocal flex set = let
                                vox = Tuple flex FlexVocal
                                in if Set.member vox set
                                  then Set.delete vox set
                                  else flip Set.map set \tup@(Tuple _ inst) -> case inst of
                                    FlexVocal -> vox -- replace any existing vocal selection with the new one
                                    _         -> tup
                              in go 1 $ L.fromFoldable $ reverse $ concat $ flip map s.parts \(Tuple part (Flex flex)) -> concat
                                [ guard (isJust flex.five   ) *> [toggle $ Tuple part FlexFive   ]
                                , guard (isJust flex.six    ) *> [toggle $ Tuple part FlexSix    ]
                                , guard (isJust flex.drums  ) *> [toggle $ Tuple part FlexDrums  ]
                                , guard (isJust flex.prokeys) *> [toggle $ Tuple part FlexProKeys]
                                , guard (isJust flex.protar ) *> [toggle $ Tuple part FlexProtar ]
                                , guard (isJust flex.vocal  ) *> [toggleVocal part               ]
                                ]
                            else handle et app_
                case app' of
                  Playing o | nowTheory >= (case song of Song obj -> obj.end) -> do
                    stop audio
                    handle evts $ Paused
                      { pausedSongTime: nowTheory
                      , settings: o.settings
                      }
                  _ -> handle evts app'
          continue app
        in loop $ Paused
          { pausedSongTime: Seconds 0.0
          , settings: let
            initialSelect = \(Tuple name (Flex flex)) ->
              if isJust flex.five then [Tuple name FlexFive] else
              if isJust flex.six then [Tuple name FlexSix] else
              if isJust flex.drums then [Tuple name FlexDrums] else
              if isJust flex.prokeys then [Tuple name FlexProKeys] else
              if isJust flex.protar then [Tuple name FlexProtar] else
              if isJust flex.vocal then [Tuple name FlexVocal] else []
            in case song of Song obj -> Set.fromFoldable $ concatMap initialSelect obj.parts
          }
  continueLoading
