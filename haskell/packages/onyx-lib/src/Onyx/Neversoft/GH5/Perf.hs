{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Neversoft.GH5.Perf where

import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (sort)
import           Data.Maybe                       (mapMaybe)
import           Data.Word
import           Onyx.MIDI.Common                 (pattern ANil, pattern At)
import           Onyx.MIDI.Track.Venue            (Camera3 (..))
import           Onyx.Neversoft.CRC
import           Onyx.Util.Binary                 (runGetM)
import qualified Sound.MIDI.Util                  as U

data Perf = Perf
  { perfDLCKey   :: QBKey
  , perfUnknown1 :: Word32
  , perfUnknown2 :: Word32
  , perfUnknown3 :: Word32
  , perfEntries  :: [(QBKey, PerfEntry)]
  } deriving (Show)

data PerfEntry
  = PerfGH5CameraNote [CameraNote]
  | PerfGH5ActorLoops [B.ByteString]
  | PerfGH6ActorLoops [B.ByteString]
  deriving (Show)

data CameraNote = CameraNote
  { camTime   :: Word32
  , camLength :: Word16
  , camAngle  :: Word8
  } deriving (Show)

getPerfEntry :: Get PerfEntry
getPerfEntry = do
  len <- fromIntegral <$> getWord32be
  qb <- getQBKeyBE
  case qb of
    "gh5_camera_note" -> fmap PerfGH5CameraNote $ replicateM len $ do
      camTime   <- getWord32be
      camLength <- getWord16be
      camAngle  <- getWord8
      return CameraNote{..}
    "gh6_actor_loops" -> fmap PerfGH6ActorLoops $ replicateM len $ do
      getByteString 1000
    "gh5_actor_loops" -> fmap PerfGH5ActorLoops $ replicateM len $ do
      getByteString 0x6C
    _ -> fail $ "Unknown .perf entry type: " <> show qb

getPerf :: Get Perf
getPerf = do
  0x40A001A3 <- getWord32be
  perfDLCKey <- getQBKeyBE
  numEntries <- getWord32be
  "perf" <- getQBKeyBE
  perfUnknown1 <- getWord32be -- always 0x36? nope, seen qb("gh5_actor_loops") in gh5
  perfUnknown2 <- getWord32be
  perfUnknown3 <- getWord32be
  perfEntries <- replicateM (fromIntegral numEntries) $ do
    entryKey <- getQBKeyBE
    -- observed key "autocutcameras" and "momentcameras" both with type "gh5_camera_note"
    entry <- getPerfEntry
    return (entryKey, entry)
  isEmpty >>= \b -> unless b $ fail "Expected EOF at end of .perf"
  return Perf{..}

loadPerf :: FilePath -> IO Perf
loadPerf f = B.readFile f >>= runGetM getPerf . BL.fromStrict

makePerf :: Perf -> BL.ByteString
makePerf p = runPut $ do
  putWord32be 0x40A001A3
  putQBKeyBE p.perfDLCKey
  putWord32be $ fromIntegral $ length p.perfEntries
  putQBKeyBE "perf"
  putWord32be p.perfUnknown1
  putWord32be p.perfUnknown2
  putWord32be p.perfUnknown3
  forM_ p.perfEntries $ \(entryKey, entry) -> do
    putQBKeyBE entryKey
    case entry of
      PerfGH5CameraNote notes -> do
        putWord32be $ fromIntegral $ length notes
        putQBKeyBE "gh5_camera_note"
        forM_ notes $ \cam -> do
          putWord32be cam.camTime
          putWord16be cam.camLength
          putWord8    cam.camAngle
      PerfGH5ActorLoops loops -> do
        putWord32be $ fromIntegral $ length loops
        putQBKeyBE "gh5_actor_loops"
        mapM_ putByteString loops
      PerfGH6ActorLoops loops -> do
        putWord32be $ fromIntegral $ length loops
        putQBKeyBE "gh6_actor_loops"
        mapM_ putByteString loops

rbCameraToPerf :: QBKey -> U.TempoMap -> RTB.T U.Beats Camera3 -> U.Beats -> Perf
rbCameraToPerf key tmap rb endTime = let
  angles = RTB.mapMaybe eachInstant $ RTB.collectCoincident rb
  eachInstant cuts = case mapMaybe mapCut $ reverse $ sort cuts of
    [] -> Nothing -- only directed cuts here
    ns -> Just $ case filter (/= 255) ns of
      n : _ -> n
      []    -> 30 -- only keys cuts here
  mapCut = \case
    V3_coop_all_behind -> Just 31
    V3_coop_all_far -> Just 28
    V3_coop_all_near -> Just 30
    V3_coop_front_behind -> Just 31
    V3_coop_front_near -> Just 30
    V3_coop_d_behind -> Just 26
    V3_coop_d_near -> Just 25
    V3_coop_v_behind -> Just 16
    V3_coop_v_near -> Just 15
    V3_coop_b_behind -> Just 21
    V3_coop_b_near -> Just 20
    V3_coop_g_behind -> Just 11
    V3_coop_g_near -> Just 10
    V3_coop_k_behind -> Just 255
    V3_coop_k_near -> Just 255
    V3_coop_d_closeup_hand -> Just 35
    V3_coop_d_closeup_head -> Just 23
    V3_coop_v_closeup -> Just 13
    V3_coop_b_closeup_hand -> Just 36
    V3_coop_b_closeup_head -> Just 18
    V3_coop_g_closeup_hand -> Just 33
    V3_coop_g_closeup_head -> Just 8
    V3_coop_k_closeup_hand -> Just 255
    V3_coop_k_closeup_head -> Just 255
    -- just picking arbitrarily from duos
    V3_coop_dv_near -> Just 25
    V3_coop_bd_near -> Just 21
    V3_coop_dg_near -> Just 10
    V3_coop_bv_behind -> Just 21
    V3_coop_bv_near -> Just 15
    V3_coop_gv_behind -> Just 11
    V3_coop_gv_near -> Just 15
    V3_coop_kv_behind -> Just 16
    V3_coop_kv_near -> Just 15
    V3_coop_bg_behind -> Just 11
    V3_coop_bg_near -> Just 20
    V3_coop_bk_behind -> Just 21
    V3_coop_bk_near -> Just 20
    V3_coop_gk_behind -> Just 11
    V3_coop_gk_near -> Just 10
    _ -> Nothing -- directed cut
  toMS :: U.Seconds -> Word32
  toMS s = round (s * 1000)
  toCamLength :: Word32 -> Word32 -> Word16
  toCamLength t1 t2 = let
    len = t2 - t1
    in if len > 0xFFFF
      then 0xFFFF
      else fromIntegral len
  autocut = toCameraNotes $ ATB.mapTime toMS $ RTB.toAbsoluteEventList 0 $ U.applyTempoTrack tmap angles
  toCameraNotes = \case
    At t1 angle rest@(At t2 _ _) -> CameraNote
      { camTime = t1
      , camLength = toCamLength t1 t2
      , camAngle = angle
      } : toCameraNotes rest
    At t angle ANil -> [CameraNote
      { camTime = t
      , camLength = toCamLength t $ toMS $ U.applyTempoMap tmap endTime
      , camAngle = angle
      }]
    ANil -> []
  in Perf
    { perfDLCKey = key
    , perfUnknown1 = 0
    , perfUnknown2 = 0
    , perfUnknown3 = 0
    , perfEntries =
      [ ("autocutcameras", PerfGH5CameraNote autocut)
      , ("momentcameras" , PerfGH5CameraNote []     )
      ]
    }

{-

rough descriptions of (some) possible results from camera angles,
names from qb keys in a file from WoR QB.PAK

3-6 moment cameras

8 "guitarist_closeup" guitar close front
9 "guitarist_stage" guitar close back
10 "Guitarist_Mocap01" guitar front
11 "Guitarist_Mocap02" guitar back
13 "singer_closeup" vox close front
14 "singer_stage" vox close back
15 "Singer_Mocap01" vox front
16 "Singer_Mocap02" vox back
18 "bassist_closeup" bass close front
19 "bassist_stage" bass close back
20 "Bassist_Mocap01" bass front
21 "Bassist_Mocap02" bass back
23 "drummer_closeup" drums close front
24 "drummer_stage" drums close back
25 "Drummer_Mocap01" drums front
26 "Drummer_Mocap02" drums back

(assuming mocap ones use ska files?)

28 "longshot" way far, high up above crowd
29 "midshot" frontline, from guitar side
30 "venue_midshot" just above crowd looking at front
31 "generic_stage_shot" back behind band (a ways behind drums)
33 "solo_guitar" guitar fretboard from headstock
34 "solo_vocal" guitar closeup from near bridge? weird
35 "solo_drum" drums (toms) closeup
36 "solo_bass" bass closeup from near bridge
38 "single" vox front?
40 "special01"
41 "special02"
43 "audience" in crowd, to side a bit
45 "zoom_in" zoom in from high above crowd towards stage
46 "zoom_out" zoom out from stage back to just above back crowd
48 "spotlight" above frontline
57 "Verts_Front" rise up from right in front of stage, viewing frontline
58 "Verts_Rear" above drummer, looking down from back of band
60 "G_across_stage" super closeup of frontline from bass side
61 "S_across_stage" close frontline from guitar side

62 "Singer_vert" rise up close back shot of vox
63 "Guitarist_vert" rise up close side shot of guitar
64 "Drummer_vert" rise up close side shot of drums
65 "bassist_vert" rise up close back shot of bass

66 "D_across_stage_S" drums closeup
67 "D_across_stage_G"
68 "Drummer_motion" rotate around back of drums
69 "Guitar_Face_Pan" rise up front guitar
74 "Orbit_Low" fast pan from back of crowd
75 "Orbit_High" fast pan from higher back of crowd

94-98 moment cameras

110 "debug_face_vocal" vox face lock
111 "debug_face_guitar" guitar face lock
112 "debug_face_bass" bass face lock
113 "debug_face_drum" drums face lock

-}

{-
actor loops:
0x40 bytes of stuff
0x88 bytes of zero (0x40 + 0x88 = 200)
0x40 bytes of stuff
0x88 bytes of zero (0x40 + 0x88 = 200)
0x3c bytes of stuff
0x21c bytes of zero (0x3c + 0x21c = 600)
-}
