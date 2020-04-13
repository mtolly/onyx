{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RhythmGame.Graphics.Config where

import           Control.Monad.Codec
import           Control.Monad.Trans.StackTrace (SendMessage)
import           Data.Char                      (toUpper)
import           Data.List                      (elemIndex)
import           JSONData
import           Linear                         (V3 (..), V4 (..))

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
  { cfg_view    :: View
  , cfg_track   :: Track
  , cfg_objects :: Objects
  } deriving (Show)

instance StackJSON Config where
  stackJSON = asObject "Config" $ do
    cfg_view    <- cfg_view    =. req "view"    stackJSON
    cfg_track   <- cfg_track   =. req "track"   stackJSON
    cfg_objects <- cfg_objects =. req "objects" stackJSON
    return Config{..}

data View = View
  { view_background         :: Color
  , view_track_fade         :: TrackFade
  , view_height_width_ratio :: Float
  , view_camera             :: Camera
  } deriving (Show)

instance StackJSON View where
  stackJSON = asObject "View" $ do
    view_background         <- view_background         =. req "background"         stackColor
    view_track_fade         <- view_track_fade         =. req "track-fade"         stackJSON
    view_height_width_ratio <- view_height_width_ratio =. req "height-width-ratio" stackJSON
    view_camera             <- view_camera             =. req "camera"             stackJSON
    return View{..}

data TrackFade = TrackFade
  { tf_bottom :: Float
  , tf_top    :: Float
  } deriving (Show)

instance StackJSON TrackFade where
  stackJSON = asObject "TrackFade" $ do
    tf_bottom <- tf_bottom =. req "bottom" stackJSON
    tf_top    <- tf_top    =. req "top"    stackJSON
    return TrackFade{..}

data Camera = Camera
  { cam_position :: V3 Float
  , cam_rotate   :: Float
  , cam_fov      :: Float
  , cam_near     :: Float
  , cam_far      :: Float
  } deriving (Show)

instance StackJSON Camera where
  stackJSON = asObject "Camera" $ do
    cam_position <- cam_position =. req "position" stackXYZ
    cam_rotate   <- cam_rotate   =. req "rotate"   stackJSON
    cam_fov      <- cam_fov      =. req "fov"      stackJSON
    cam_near     <- cam_near     =. req "near"     stackJSON
    cam_far      <- cam_far      =. req "far"      stackJSON
    return Camera{..}

data Track = Track
  { trk_y         :: Float
  , trk_note_area :: NoteArea
  , trk_time      :: TrackTime
  , trk_color     :: TrackColor
  , trk_railings  :: Railings
  , trk_beats     :: Beats
  , trk_targets   :: Targets
  , trk_light     :: Light
  } deriving (Show)

instance StackJSON Track where
  stackJSON = asObject "Track" $ do
    trk_y         <- trk_y         =. req "y"         stackJSON
    trk_note_area <- trk_note_area =. req "note-area" stackJSON
    trk_time      <- trk_time      =. req "time"      stackJSON
    trk_color     <- trk_color     =. req "color"     stackJSON
    trk_railings  <- trk_railings  =. req "railings"  stackJSON
    trk_beats     <- trk_beats     =. req "beats"     stackJSON
    trk_targets   <- trk_targets   =. req "targets"   stackJSON
    trk_light     <- trk_light     =. req "light"     stackJSON
    return Track{..}

data NoteArea = NoteArea
  { na_x_left  :: Float
  , na_x_right :: Float
  } deriving (Show)

instance StackJSON NoteArea where
  stackJSON = asObject "NoteArea" $ do
    na_x_left  <- na_x_left  =. req "x-left"  stackJSON
    na_x_right <- na_x_right =. req "x-right" stackJSON
    return NoteArea{..}

data TrackTime = TrackTime
  { tt_z_past      :: Float
  , tt_z_now       :: Float
  , tt_z_future    :: Float
  , tt_secs_future :: Float
  } deriving (Show)

instance StackJSON TrackTime where
  stackJSON = asObject "TrackTime" $ do
    tt_z_past      <- tt_z_past      =. req "z-past"      stackJSON
    tt_z_now       <- tt_z_now       =. req "z-now"       stackJSON
    tt_z_future    <- tt_z_future    =. req "z-future"    stackJSON
    tt_secs_future <- tt_secs_future =. req "secs-future" stackJSON
    return TrackTime{..}

data TrackColor = TrackColor
  { tc_normal :: Color
  , tc_solo   :: Color
  } deriving (Show)

instance StackJSON TrackColor where
  stackJSON = asObject "TrackColor" $ do
    tc_normal <- tc_normal =. req "normal" stackColor
    tc_solo   <- tc_solo   =. req "solo"   stackColor
    return TrackColor{..}

data Railings = Railings
  { rail_color    :: Color
  , rail_x_width  :: Float
  , rail_y_top    :: Float
  , rail_y_bottom :: Float
  } deriving (Show)

instance StackJSON Railings where
  stackJSON = asObject "Railings" $ do
    rail_color    <- rail_color    =. req "color"    stackColor
    rail_x_width  <- rail_x_width  =. req "x-width"  stackJSON
    rail_y_top    <- rail_y_top    =. req "y-top"    stackJSON
    rail_y_bottom <- rail_y_bottom =. req "y-bottom" stackJSON
    return Railings{..}

data Beats = Beats
  { beats_z_past   :: Float
  , beats_z_future :: Float
  } deriving (Show)

instance StackJSON Beats where
  stackJSON = asObject "Beats" $ do
    beats_z_past   <- beats_z_past   =. req "z-past"   stackJSON
    beats_z_future <- beats_z_future =. req "z-future" stackJSON
    return Beats{..}

data Targets = Targets
  { tgt_z_past     :: Float
  , tgt_z_future   :: Float
  , tgt_secs_light :: Float
  } deriving (Show)

instance StackJSON Targets where
  stackJSON = asObject "Targets" $ do
    tgt_z_past     <- tgt_z_past     =. req "z-past"     stackJSON
    tgt_z_future   <- tgt_z_future   =. req "z-future"   stackJSON
    tgt_secs_light <- tgt_secs_light =. req "secs-light" stackJSON
    return Targets{..}

data Light = Light
  { light_position :: V3 Float
  , light_ambient  :: Color
  , light_diffuse  :: Color
  , light_specular :: Color
  } deriving (Show)

instance StackJSON Light where
  stackJSON = asObject "Light" $ do
    light_position <- light_position =. req "position" stackXYZ
    light_ambient  <- light_ambient  =. req "ambient"  stackColor
    light_diffuse  <- light_diffuse  =. req "diffuse"  stackColor
    light_specular <- light_specular =. req "specular" stackColor
    return Light{..}

data Objects = Objects
  { obj_gems     :: Gems
  , obj_sustains :: Sustains
  } deriving (Show)

instance StackJSON Objects where
  stackJSON = asObject "Objects" $ do
    obj_gems     <- obj_gems     =. req "gems"     stackJSON
    obj_sustains <- obj_sustains =. req "sustains" stackJSON
    return Objects{..}

data Gems = Gems
  { gems_color_hit :: Color
  , gems_secs_fade :: Float
  , gems_light     :: Light
  } deriving (Show)

instance StackJSON Gems where
  stackJSON = asObject "Gems" $ do
    gems_color_hit <- gems_color_hit =. req "color-hit" stackColor
    gems_secs_fade <- gems_secs_fade =. req "secs-fade" stackJSON
    gems_light     <- gems_light     =. req "light"     stackJSON
    return Gems{..}

data Sustains = Sustains
  { sust_colors :: SustainColors
  , sust_width  :: SustainWidth
  , sust_height :: Float
  } deriving (Show)

instance StackJSON Sustains where
  stackJSON = asObject "Sustains" $ do
    sust_colors <- sust_colors =. req "colors" stackJSON
    sust_width  <- sust_width  =. req "width"  stackJSON
    sust_height <- sust_height =. req "height" stackJSON
    return Sustains{..}

data SustainColors = SustainColors
  { sc_open   :: Color
  , sc_green  :: Color
  , sc_red    :: Color
  , sc_yellow :: Color
  , sc_blue   :: Color
  , sc_orange :: Color
  , sc_energy :: Color
  } deriving (Show)

instance StackJSON SustainColors where
  stackJSON = asObject "SustainColors" $ do
    sc_open   <- sc_open   =. req "open"   stackColor
    sc_green  <- sc_green  =. req "green"  stackColor
    sc_red    <- sc_red    =. req "red"    stackColor
    sc_yellow <- sc_yellow =. req "yellow" stackColor
    sc_blue   <- sc_blue   =. req "blue"   stackColor
    sc_orange <- sc_orange =. req "orange" stackColor
    sc_energy <- sc_energy =. req "energy" stackColor
    return SustainColors{..}

data SustainWidth = SustainWidth
  { sw_open :: Float
  , sw_fret :: Float
  } deriving (Show)

instance StackJSON SustainWidth where
  stackJSON = asObject "SustainWidth" $ do
    sw_open <- sw_open =. req "open" stackJSON
    sw_fret <- sw_fret =. req "fret" stackJSON
    return SustainWidth{..}
