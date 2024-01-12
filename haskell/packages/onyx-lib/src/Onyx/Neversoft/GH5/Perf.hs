{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Neversoft.GH5.Perf where

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word            (Word32)
import           Onyx.Neversoft.CRC
import           Onyx.Xbox.STFS       (runGetM)

data Perf = Perf
  { perfDLCKey   :: Word32
  , perfUnknown1 :: Word32
  , perfUnknown2 :: Word32
  , perfUnknown3 :: Word32
  , perfEntries  :: [PerfEntry]
  } deriving (Show)

data PerfEntry
  = PerfGH5CameraNote Word32 [(Word32, B.ByteString)]
  | PerfGH6ActorLoops Word32 [B.ByteString]
  deriving (Show)

getPerfEntry :: Get PerfEntry
getPerfEntry = do
  entryID <- getWord32be
  -- entryID for gh5_camera_note is either "autocutcameras" or "momentcameras".
  -- for gh6_actor_loops, entryID appears to be a unique ID
  len <- fromIntegral <$> getWord32be
  qb <- getWord32be
  if qb == qbKeyCRC "gh5_camera_note"
    then fmap (PerfGH5CameraNote entryID) $ replicateM len $ do
      t <- getWord32be
      x <- getByteString 3
      return (t, x)
    else if qb == qbKeyCRC "gh6_actor_loops"
      then fmap (PerfGH6ActorLoops entryID) $ replicateM len $ do
        getByteString 1000
      else fail "Unknown .perf entry type"

getPerf :: Get Perf
getPerf = do
  0x40A001A3 <- getWord32be
  perfDLCKey <- getWord32be
  numEntries <- getWord32be
  0x424056AD <- getWord32be -- qb "perf"
  perfUnknown1 <- getWord32be -- always 0x36?
  perfUnknown2 <- getWord32be
  perfUnknown3 <- getWord32be
  perfEntries <- replicateM (fromIntegral numEntries) getPerfEntry
  isEmpty >>= \b -> unless b $ fail "Expected EOF at end of .perf"
  return Perf{..}

loadPerf :: FilePath -> IO Perf
loadPerf f = B.readFile f >>= runGetM getPerf . BL.fromStrict

{-

matching up camera cuts to video of dlc701 (Soundgarden - Hunted Down)
https://www.youtube.com/watch?v=HE18lMbNAm0

(0,[9,165,31]) behind the band, very slow pan left
(2469,[23,18,45]) in crowd way far from band, zoom in slowly
(8375,[10,43,10]) guitarist shot, pan over to vocalist
  moment camera: (8375,[19,141,3])
(10978,[9,98,9]) no cut here!
(13380,[9,198,48]) front of band, up above
(15883,[10,243,24]) drummer, close up from side
(18685,[6,232,30]) far away, in the middle of crowd
(20454,[8,21,23]) drummer, close up from front
(22522,[8,154,18]) bassist, front
(24725,[16,142,15]) behind vocalist and bassist
(28962,[14,153,23]) behind drummer, rise up from below
  moment camera: (28962,[14,153,68])
(32699,[22,107,43]) guitarist closeup from front
  moment camera: (32699,[22,107,3])
(38438,[14,186,30]) far away, in middle of crowd
(42209,[15,197,74]) in crowd, pan quickly from right to left
(46246,[7,243,8]) guitarist
  moment camera: (46246,[7,243,4])
(48282,[24,196,13]) vocalist first line, dramatic
  moment camera: (48282,[24,196,6])
(54621,[8,121,10]) guitarist
(56790,[17,253,43]) far in crowd, slightly to the left
(61395,[19,8,18]) behind bassist, on floor, rise up slowly
  moment camera: (61395,[19,8,65])
(66266,[21,96,13]) behind vocalist
  moment camera: (66266,[21,96,94])
(71738,[7,210,23]) drummer, close up from front
(73740,[12,131,30]) far away in crowd
(76944,[12,98,11]) behind guitarist, can also see vox/bass
(80113,[11,154,21]) behind bassist, can also see vox
(83083,[8,87,28]) way far away up above crowd
(85218,[7,143,26]) close up behind drummer
(87154,[17,219,58]) behind band up above, slow camera pan down
(91725,[7,177,13]) guitarist from side
  moment camera: (91725,[15,64,5])
(93694,[7,143,8]) no cut here!
(95629,[13,9,43]) far off in crowd
(98966,[8,54,13]) close up of guitarist from front
  moment camera: (98966,[8,54,3])
(101068,[14,186,60]) vocalist from side
  moment camera: (101068,[14,186,5])
(104838,[15,97,30]) guitarist fretboard shot
  moment camera: (104838,[15,97,33])
(108775,[20,152,11]) vocalist from front, slightly below
  moment camera: (108775,[20,152,3])
(114047,[18,230,20]) bassist from front, slightly far away
  moment camera: (114047,[18,230,3])
(118885,[10,209,25]) drummer from front, pan from left to right
(121655,[8,121,10]) guitarist from front/side
(123824,[13,142,28]) far off in crowd, up above
(127294,[17,86,26]) vocalist from front
  moment camera: (127294,[17,86,3])
(131732,[10,43,18]) guitarist fretboard shot
  moment camera: (131732,[10,43,33])
(134334,[16,208,74]) in crowd, pan from left to right
(138638,[12,231,13]) behind vocalist
  moment camera: (138638,[12,231,4])
(141942,[8,254,26]) behind drummer
(144244,[8,54,10]) guitar, camera mounted to guitar body (looking up towards neck)
  moment camera: (144244,[8,54,33])
(146346,[14,53,16]) vocalist from front, can also see guitar
  moment camera: (146346,[14,53,3])
(149983,[9,132,48]) front of band from up above, zooms out quickly and then stops
(152419,[11,20,23]) drummer close up from front
(155255,[11,87,10]) guitar from side, can also see vocalist
(158158,[11,54,26]) drummer from behind, slight pan from right to left
(161028,[14,20,75]) far above crowd, pan from right to left
(164631,[4,110,25]) drummer from side
(165766,[28,39,46]) out above crowd, camera titled

Comparing to https://www.youtube.com/watch?v=AO7LVVaNhvs
Most are more or less the same, but a few angles are different,
and the smaller venue reduces camera movement.

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
