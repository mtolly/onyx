{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Game.Graphics.Config where

import           Control.Monad.Codec
import           Data.Char           (toUpper)
import           Data.List           (elemIndex)
import           Linear              (V3 (..), V4 (..))
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.StackTrace     (SendMessage)

type Color = V4 Float

hexDigit :: Char -> Maybe Int
hexDigit c = fromIntegral <$> elemIndex (toUpper c) "0123456789ABCDEF"

stackColor :: (SendMessage m) => JSONCodec m Color
stackColor = Codec
  { codecOut = undefined -- TODO
  , codecIn = fromJSON >>= \case
    '#' : hexes -> case mapM hexDigit hexes of
      Just [r1,r2,g1,g2,b1,b2] -> let
        byteToFloat :: Int -> Float
        byteToFloat b = fromIntegral b / 255
        in return $ byteToFloat <$> V4
          (r1 * 16 + r2)
          (g1 * 16 + g2)
          (b1 * 16 + b2)
          255
      _ -> expected "6 hex digits after the hash"
    _ -> expected "string starting with a hash"
  }

stackXYZ :: (SendMessage m) => JSONCodec m (V3 Float)
stackXYZ = asObject "(x,y,z) triple" $ do
  x <- (\(V3 x _ _) -> x) =. req "x" stackJSON
  y <- (\(V3 _ y _) -> y) =. req "y" stackJSON
  z <- (\(V3 _ _ z) -> z) =. req "z" stackJSON
  return $ V3 x y z

data Config = Config
  { view    :: View
  , track   :: Track
  , objects :: Objects
  } deriving (Show)

instance StackJSON Config where
  stackJSON = asObject "Config" $ do
    view    <- (.view   ) =. req "view"    stackJSON
    track   <- (.track  ) =. req "track"   stackJSON
    objects <- (.objects) =. req "objects" stackJSON
    return Config{..}

data View = View
  { background         :: Color
  , track_fade         :: TrackFade
  , height_width_ratio :: Float
  , camera             :: Camera
  } deriving (Show)

instance StackJSON View where
  stackJSON = asObject "View" $ do
    background         <- (.background        ) =. req "background"         stackColor
    track_fade         <- (.track_fade        ) =. req "track-fade"         stackJSON
    height_width_ratio <- (.height_width_ratio) =. req "height-width-ratio" stackJSON
    camera             <- (.camera            ) =. req "camera"             stackJSON
    return View{..}

data TrackFade = TrackFade
  { bottom :: Float
  , top    :: Float
  } deriving (Show)

instance StackJSON TrackFade where
  stackJSON = asObject "TrackFade" $ do
    bottom <- (.bottom) =. req "bottom" stackJSON
    top    <- (.top   ) =. req "top"    stackJSON
    return TrackFade{..}

data Camera = Camera
  { position :: V3 Float
  , rotate   :: Float
  , fov      :: Float
  , near     :: Float
  , far      :: Float
  } deriving (Show)

instance StackJSON Camera where
  stackJSON = asObject "Camera" $ do
    position <- (.position) =. req "position" stackXYZ
    rotate   <- (.rotate  ) =. req "rotate"   stackJSON
    fov      <- (.fov     ) =. req "fov"      stackJSON
    near     <- (.near    ) =. req "near"     stackJSON
    far      <- (.far     ) =. req "far"      stackJSON
    return Camera{..}

data Track = Track
  { y         :: Float
  , note_area :: NoteArea
  , time      :: TrackTime
  , color     :: TrackColor
  , railings  :: Railings
  , beats     :: Beats
  , targets   :: Targets
  , light     :: Light
  } deriving (Show)

instance StackJSON Track where
  stackJSON = asObject "Track" $ do
    y         <- (.y        ) =. req "y"         stackJSON
    note_area <- (.note_area) =. req "note-area" stackJSON
    time      <- (.time     ) =. req "time"      stackJSON
    color     <- (.color    ) =. req "color"     stackJSON
    railings  <- (.railings ) =. req "railings"  stackJSON
    beats     <- (.beats    ) =. req "beats"     stackJSON
    targets   <- (.targets  ) =. req "targets"   stackJSON
    light     <- (.light    ) =. req "light"     stackJSON
    return Track{..}

data NoteArea = NoteArea
  { x_left  :: Float
  , x_right :: Float
  } deriving (Show)

instance StackJSON NoteArea where
  stackJSON = asObject "NoteArea" $ do
    x_left  <- (.x_left ) =. req "x-left"  stackJSON
    x_right <- (.x_right) =. req "x-right" stackJSON
    return NoteArea{..}

data TrackTime = TrackTime
  { z_past      :: Float
  , z_now       :: Float
  , z_future    :: Float
  , secs_future :: Float
  } deriving (Show)

instance StackJSON TrackTime where
  stackJSON = asObject "TrackTime" $ do
    z_past      <- (.z_past     ) =. req "z-past"      stackJSON
    z_now       <- (.z_now      ) =. req "z-now"       stackJSON
    z_future    <- (.z_future   ) =. req "z-future"    stackJSON
    secs_future <- (.secs_future) =. req "secs-future" stackJSON
    return TrackTime{..}

data TrackColor = TrackColor
  { normal :: Color
  , solo   :: Color
  } deriving (Show)

instance StackJSON TrackColor where
  stackJSON = asObject "TrackColor" $ do
    normal <- (.normal) =. req "normal" stackColor
    solo   <- (.solo  ) =. req "solo"   stackColor
    return TrackColor{..}

data Railings = Railings
  { color    :: Color
  , x_width  :: Float
  , y_top    :: Float
  , y_bottom :: Float
  } deriving (Show)

instance StackJSON Railings where
  stackJSON = asObject "Railings" $ do
    color    <- (.color   ) =. req "color"    stackColor
    x_width  <- (.x_width ) =. req "x-width"  stackJSON
    y_top    <- (.y_top   ) =. req "y-top"    stackJSON
    y_bottom <- (.y_bottom) =. req "y-bottom" stackJSON
    return Railings{..}

data Beats = Beats
  { z_past   :: Float
  , z_future :: Float
  } deriving (Show)

instance StackJSON Beats where
  stackJSON = asObject "Beats" $ do
    z_past   <- (.z_past  ) =. req "z-past"   stackJSON
    z_future <- (.z_future) =. req "z-future" stackJSON
    return Beats{..}

data Targets = Targets
  { z_past     :: Float
  , z_future   :: Float
  , secs_light :: Float
  } deriving (Show)

instance StackJSON Targets where
  stackJSON = asObject "Targets" $ do
    z_past     <- (.z_past    ) =. req "z-past"     stackJSON
    z_future   <- (.z_future  ) =. req "z-future"   stackJSON
    secs_light <- (.secs_light) =. req "secs-light" stackJSON
    return Targets{..}

data Light = Light
  { position :: V3 Float
  , ambient  :: Color
  , diffuse  :: Color
  , specular :: Color
  } deriving (Show)

instance StackJSON Light where
  stackJSON = asObject "Light" $ do
    position <- (.position) =. req "position" stackXYZ
    ambient  <- (.ambient ) =. req "ambient"  stackColor
    diffuse  <- (.diffuse ) =. req "diffuse"  stackColor
    specular <- (.specular) =. req "specular" stackColor
    return Light{..}

data Objects = Objects
  { gems     :: Gems
  , sustains :: Sustains
  } deriving (Show)

instance StackJSON Objects where
  stackJSON = asObject "Objects" $ do
    gems     <- (.gems    ) =. req "gems"     stackJSON
    sustains <- (.sustains) =. req "sustains" stackJSON
    return Objects{..}

data Gems = Gems
  { color_hit :: Color
  , secs_fade :: Float
  , light     :: Light
  } deriving (Show)

instance StackJSON Gems where
  stackJSON = asObject "Gems" $ do
    color_hit <- (.color_hit) =. req "color-hit" stackColor
    secs_fade <- (.secs_fade) =. req "secs-fade" stackJSON
    light     <- (.light    ) =. req "light"     stackJSON
    return Gems{..}

data Sustains = Sustains
  { colors :: SustainColors
  , width  :: SustainWidth
  , height :: Float
  } deriving (Show)

instance StackJSON Sustains where
  stackJSON = asObject "Sustains" $ do
    colors <- (.colors) =. req "colors" stackJSON
    width  <- (.width ) =. req "width"  stackJSON
    height <- (.height) =. req "height" stackJSON
    return Sustains{..}

data SustainColors = SustainColors
  { open   :: Color
  , green  :: Color
  , red    :: Color
  , yellow :: Color
  , blue   :: Color
  , orange :: Color
  , purple :: Color
  , energy :: Color
  } deriving (Show)

instance StackJSON SustainColors where
  stackJSON = asObject "SustainColors" $ do
    open   <- (.open  ) =. req "open"   stackColor
    green  <- (.green ) =. req "green"  stackColor
    red    <- (.red   ) =. req "red"    stackColor
    yellow <- (.yellow) =. req "yellow" stackColor
    blue   <- (.blue  ) =. req "blue"   stackColor
    orange <- (.orange) =. req "orange" stackColor
    purple <- (.purple) =. req "purple" stackColor
    energy <- (.energy) =. req "energy" stackColor
    return SustainColors{..}

data SustainWidth = SustainWidth
  { open :: Float
  , fret :: Float
  } deriving (Show)

instance StackJSON SustainWidth where
  stackJSON = asObject "SustainWidth" $ do
    open <- (.open) =. req "open" stackJSON
    fret <- (.fret) =. req "fret" stackJSON
    return SustainWidth{..}
