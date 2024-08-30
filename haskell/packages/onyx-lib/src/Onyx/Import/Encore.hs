{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Import.Encore where

import           Control.Monad.Codec
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as B
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.StackTrace            (SendMessage, StackTraceT, fatal,
                                             mapStackTraceT, stackIO)
import           Onyx.Util.Text.Decode      (decodeGeneral)
import qualified Sound.MIDI.File            as F
import qualified Sound.MIDI.Util            as U
import           System.FilePath            (takeDirectory, (</>))

data EncoreInfo f = EncoreInfo
  { title              :: T.Text
  , artist             :: T.Text
  , diff               :: HM.HashMap T.Text Int -- keys: ds/drums ba/bass gr/guitar vl/vocals plastic_drums plastic_bass plastic_guitar; -1 for no part
  , stems              :: HM.HashMap T.Text [f] -- keys: drums bass lead vocals backing (backing required); values can be string or array of strings
  , midi               :: f
  , icon_drums         :: T.Text -- aka sid, usual value Drum
  , icon_bass          :: T.Text -- aka sib, usual value Bass
  , icon_guitar        :: T.Text -- aka sig, usual value Guitar or Keyboard
  , icon_vocals        :: T.Text -- aka siv, usual value Vocals
  -- rest optional
  , length_            :: Maybe Int
  , charters           :: [T.Text]
  , release_year       :: Maybe T.Text
  , album              :: Maybe T.Text
  , art                :: Maybe f
  , preview_start_time :: Maybe Int
  , source             :: Maybe T.Text
  , loading_phrase     :: Maybe T.Text
  , genres             :: [T.Text]
  } deriving (Show, Functor)

-- one string or array of strings
stemJSON :: (SendMessage m) => JSONCodec m [T.Text]
stemJSON = Codec
  { codecIn = lift ask >>= \v -> case A.fromJSON v of
    A.Success x -> return [x]
    A.Error _ -> case A.fromJSON v of
      A.Success xs -> return xs
      A.Error err  -> fatal err
  , codecOut = makeOut $ \case
    [x] -> A.toJSON x
    xs  -> A.toJSON xs
  }

-- required value, prefer first key, fallback to second otherwise
reqSynonym :: (SendMessage m, Eq a) => T.Text -> T.Text -> JSONCodec m a -> ObjectCodec m A.Value a
reqSynonym k1 k2 vc = let
  result = Codec
    { codecIn = codecIn opt1 >>= \case
      Nothing -> codecIn req2
      Just x  -> return x
    , codecOut = codecOut req1
    }
  opt1 = opt Nothing k1 $ maybeCodec vc
  req1 = req k1 vc `asTypeOf` result
  req2 = req k2 vc `asTypeOf` result
  in result

instance StackJSON (EncoreInfo T.Text) where
  stackJSON = asObject "EncoreInfo" $ do
    title              <- (.title)              =. req         "title"              stackJSON
    artist             <- (.artist)             =. req         "artist"             stackJSON
    diff               <- (.diff)               =. req         "diff"               (dict stackJSON)
    stems              <- (.stems)              =. req         "stems"              (dict stemJSON)
    midi               <- (.midi)               =. req         "midi"               stackJSON
    icon_drums         <- (.icon_drums)         =. reqSynonym  "sid" "icon_drums"   stackJSON
    icon_bass          <- (.icon_bass)          =. reqSynonym  "sib" "icon_bass"    stackJSON
    icon_guitar        <- (.icon_guitar)        =. reqSynonym  "sig" "icon_guitar"  stackJSON
    icon_vocals        <- (.icon_vocals)        =. reqSynonym  "siv" "icon_vocals"  stackJSON
    length_            <- (.length_)            =. opt Nothing "length"             stackJSON
    charters           <- (.charters)           =. opt []      "charters"           stackJSON
    release_year       <- (.release_year)       =. opt Nothing "release_year"       stackJSON
    album              <- (.album)              =. opt Nothing "album"              stackJSON
    art                <- (.art)                =. opt Nothing "art"                stackJSON
    preview_start_time <- (.preview_start_time) =. opt Nothing "preview_start_time" stackJSON
    source             <- (.source)             =. opt Nothing "source"             stackJSON
    loading_phrase     <- (.loading_phrase)     =. opt Nothing "loading_phrase"     stackJSON
    genres             <- (.genres)             =. opt []      "genres"             stackJSON
    return EncoreInfo{..}

encoreMidiToFoF :: F.T B.ByteString -> F.T B.ByteString
encoreMidiToFoF (F.Cons typ dvn trks) = let
  renameTrack trk = case U.trackName trk of
    -- making up PAD track names just to keep them around
    Just "PART DRUMS"     -> U.setTrackName "PAD DRUMS"   trk
    Just "PART BASS"      -> U.setTrackName "PAD BASS"    trk
    Just "PART GUITAR"    -> U.setTrackName "PAD GUITAR"  trk
    Just "PART VOCALS"    -> U.setTrackName "PAD VOCALS"  trk
    Just "PLASTIC GUITAR" -> U.setTrackName "PART GUITAR" trk
    Just "PLASTIC BASS"   -> U.setTrackName "PART BASS"   trk
    Just "PLASTIC DRUMS"  -> U.setTrackName "PART DRUMS"  trk
    Just "PITCHED VOCALS" -> U.setTrackName "PART VOCALS" trk
    _                     -> trk
  in F.Cons typ dvn $ map renameTrack trks

loadEncoreInfo :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m (EncoreInfo FilePath)
loadEncoreInfo f = do
  -- should be utf-8 but we'll be safe
  json <- stackIO (B.readFile f) >>= decodeJSONText . decodeGeneral
  info <- mapStackTraceT (`runReaderT` json) fromJSON
  let dir = takeDirectory f
  return $ (dir </>) . T.unpack <$> info

{-
midi tracks:
PART DRUMS, PART BASS, PART GUITAR, PART VOCALS: pad charts
PLASTIC GUITAR, PLASTIC BASS, PLASTIC DRUMS: instrument controller tracks
EVENTS: has [music_start] [music_end] [end]
BEAT: 12/13 like RB

pad charts:

  ### Tap Lanes
  |Difficulty|1|2|3|4|5|
  |:-|:-:|:-:|:-:|:-:|:-:|
  |Expert|96  C6|97  C#6|98  D6|99  D#6|100  E6|
  |Hard|84  C5|85  C#5|86  D5|87  D#5| |
  |Medium|72  C4|73  C#4|74  D4|75  D#4| |
  |Easy|60  C3|61  C#3|62  D3|63  D#3| |

  ### Lift Markers
  |Difficulty|1|2|3|4|5|
  |:-|:-:|:-:|:-:|:-:|:-:|
  |Expert|102  F#6|103  G6|104  G#6|105  A6|106  B6|
  |Hard|90  F#5|91  G5|92 G#5|93  A5| |
  |Medium|78  F#4|79  G4|80  G#4|81  A4| |
  |Easy|66  F#3|67  G3|68  G#3|69  A3| |

  ### Overdrive

  |Difficulty|Lane|
  |:-|:-:|
  |All Diffs|116 G#7|

  ### Solos

  Solos are marked per instrument at pitch 101 (F6), between Expert's lift and tap pitches.

-}
