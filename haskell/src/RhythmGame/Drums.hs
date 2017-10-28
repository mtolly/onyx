{-# LANGUAGE LambdaCase #-}
module RhythmGame.Drums where

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forM_, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State
import           Data.List                 (partition)
import qualified Data.Map.Strict           as Map
import           Data.Map.Strict.Internal  (Map (..))
import           Data.Maybe                (fromMaybe)
import           Foreign.C                 (CInt)
import qualified RockBand.Drums            as D
import           SDL                       (($=))
import qualified SDL

data Note t a
  = Upcoming a
  | Hit t a
  | Missed a
  deriving (Eq, Ord, Show, Read)

data Track t a = Track
  { trackNotes    :: Map t [Note t a] -- ^ lists must be non-empty
  , trackOverhits :: Map t [a] -- ^ lists must be non-empty
  , trackTime     :: t -- ^ the latest timestamp we have reached
  , trackWindow   :: t -- ^ the half-window of time on each side of a note
  } deriving (Eq, Ord, Show, Read)

-- | Efficiently updates the range @(k1, k2)@ of values.
updateRange :: (Ord k) => (k -> a -> a) -> k -> k -> Map k a -> Map k a
updateRange f k1 k2 = let
  go = \case
    Tip -> Tip
    Bin size k v mL mR -> if k <= k1
      then Bin size k v mL (go mR)
      else if k2 <= k
        then Bin size k v (go mL) mR
        else let
          v' = f k v
          in v' `seq` Bin size k v' (go mL) (go mR)
  in go

traverseRange_ :: (Applicative f, Ord k) => (k -> a -> f ()) -> Bool -> k -> k -> Map k a -> f ()
traverseRange_ f asc k1 k2 = let
  go = \case
    Tip -> pure ()
    Bin _ k v mL mR -> if k <= k1
      then go mR
      else if k2 <= k
        then go mL
        else if asc
          then go mL *> f k v *> go mR
          else go mR *> f k v *> go mL
  in go

updateTime :: (Num t, Ord t) => t -> Track t a -> Track t a
updateTime t trk = let
  f _ = map $ \case
    Upcoming x -> Missed x
    evt        -> evt
  in trk
    { trackTime = t
    , trackNotes = updateRange f (trackTime trk - trackWindow trk) (t - trackWindow trk) $ trackNotes trk
    }

hitPad :: (Num t, Ord t, Eq a) => t -> a -> Track t a -> Track t a
hitPad t x trk = let
  p1 = Map.lookupLE t $ trackNotes trk
  p2 = Map.lookupGE t $ trackNotes trk
  closestNotes = case p1 of
    Nothing -> case p2 of
      Nothing      -> Nothing
      Just (k2, _) -> if k2 - t < trackWindow trk then p2 else Nothing
    Just (k1, _) -> case p2 of
      Nothing -> if t - k1 < trackWindow trk then p1 else Nothing
      Just (k2, _) -> let
        distance1 = t - k1
        distance2 = k2 - t
        in if distance1 < distance2
          then if distance1 < trackWindow trk then p1 else Nothing
          else if distance2 < trackWindow trk then p2 else Nothing
  overhit = trk { trackOverhits = Map.alter newOverhit t $ trackOverhits trk }
  newOverhit = Just . (x :) . fromMaybe []
  in case closestNotes of
    Nothing -> overhit
    Just (k, notes) -> case partition (== Upcoming x) notes of
      ([], _) -> overhit
      (_ : _, notes') ->
        trk { trackNotes = Map.insert k (Hit t x : notes') $ trackNotes trk }

drawDrums :: SDL.Renderer -> SDL.Rectangle CInt -> Track Double (D.Gem ()) -> IO ()
drawDrums rend rect@(SDL.Rectangle (SDL.P (SDL.V2 rectX rectY)) (SDL.V2 rectW rectH)) trk = do
  SDL.rendererDrawColor rend $= SDL.V4 100 100 100 0xFF
  SDL.fillRect rend $ Just rect
  let nowLineFrac = 0.8
      nowLineY = rectY + floor (fromIntegral rectH * nowLineFrac)
      futureSight = 0.7
      drawnSpan = futureSight / nowLineFrac -- time in seconds from top to bottom
      yToTime y = (trackTime trk + futureSight) - (fromIntegral (y - rectY) / fromIntegral rectH) * drawnSpan
      timeToY t = rectY + floor (((trackTime trk + futureSight - t) / drawnSpan) * fromIntegral rectH)
      timeAppear = yToTime $ rectY - 50
      timeDisappear = yToTime $ rectY + rectH + 50
      drawNotes t notes = let
        yCenter = timeToY t
        drawGem gem = let
          floor' :: Double -> CInt
          floor' = floor
          h = case gem of
            D.Kick -> 10
            _      -> 20
          color = case gem of
            D.Kick            -> SDL.V4 153 106 6 0xFF
            D.Red             -> SDL.V4 202 25 7 0xFF
            D.Pro D.Yellow () -> SDL.V4 207 180 57 0xFF
            D.Pro D.Blue ()   -> SDL.V4 71 110 222 0xFF
            D.Pro D.Green ()  -> SDL.V4 58 207 68 0xFF
          w = case gem of
            D.Kick -> rectW
            _      -> floor' $ fromIntegral rectW * 0.25
          x = case gem of
            D.Kick            -> rectX
            D.Red             -> rectX
            D.Pro D.Yellow () -> rectX + floor' (fromIntegral rectW * 0.25)
            D.Pro D.Blue ()   -> rectX + floor' (fromIntegral rectW * 0.5)
            D.Pro D.Green ()  -> rectX + floor' (fromIntegral rectW * 0.75)
          y = yCenter - quot h 2
          in do
            SDL.rendererDrawColor rend $= color
            SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 x y) $ SDL.V2 w h
        in forM_ notes $ \case
          Upcoming gem -> drawGem gem
          Hit _ _ -> return ()
          Missed gem -> drawGem gem
  SDL.rendererDrawColor rend $= SDL.V4 0 0 0 0xFF
  SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 rectX nowLineY) $ SDL.V2 rectW 10
  traverseRange_ drawNotes False timeDisappear timeAppear $ trackNotes trk

playDrums :: SDL.Window -> SDL.Renderer -> Track Double (D.Gem ()) -> IO ()
playDrums window rend trk = flip evalStateT trk $ do
  initTime <- SDL.ticks
  let loop = SDL.pollEvents >>= processEvents >>= \b -> when b $ do
        timestamp <- SDL.ticks
        modify $ updateTime $ fromIntegral (timestamp - initTime) / 1000
        draw
        liftIO $ threadDelay 5000
        loop
      processEvents [] = return True
      processEvents (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return False
        SDL.KeyboardEvent SDL.KeyboardEventData
          { SDL.keyboardEventKeyMotion = SDL.Pressed
          , SDL.keyboardEventKeysym = ksym
          , SDL.keyboardEventRepeat = False
          } -> do
            let hit gem = modify $ hitPad t gem
                t = fromIntegral (SDL.eventTimestamp e - initTime) / 1000
            case SDL.keysymScancode ksym of
              SDL.ScancodeV     -> hit D.Red
              SDL.ScancodeB     -> hit $ D.Pro D.Yellow ()
              SDL.ScancodeN     -> hit $ D.Pro D.Blue ()
              SDL.ScancodeM     -> hit $ D.Pro D.Green ()
              SDL.ScancodeSpace -> hit D.Kick
              _                 -> return ()
            processEvents es
        _ -> processEvents es
      draw = do
        SDL.V2 w h <- SDL.get $ SDL.windowSize window
        SDL.rendererDrawColor rend $= SDL.V4 0 0 0 0xFF
        SDL.clear rend
        get >>= liftIO . drawDrums rend (SDL.Rectangle (SDL.P (SDL.V2 (quot w 4) 0)) (SDL.V2 (quot w 2) h))
        SDL.present rend
  loop
