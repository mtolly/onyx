{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Encore
( EncoreInfo(..)
, encoreMidiToFoF
, loadEncoreInfo
, EncoreFile(..)
, module Onyx.Encore.MIDI
) where

import           Control.Monad.Codec
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as B
import qualified Data.HashMap.Strict        as HM
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.DeriveHelpers
import           Onyx.Encore.MIDI
import           Onyx.MIDI.Track.Beat       (BeatTrack)
import           Onyx.MIDI.Track.Drums      (DrumTrack)
import           Onyx.MIDI.Track.Events     (EventsTrack)
import           Onyx.MIDI.Track.File       (ParseFile (..), fileTrack)
import           Onyx.MIDI.Track.FiveFret   (FiveTrack)
import           Onyx.MIDI.Track.Vocal      (VocalTrack)
import           Onyx.StackTrace            (SendMessage, StackTraceT, fatal,
                                             mapStackTraceT, stackIO)
import           Onyx.Util.Text.Decode      (decodeGeneral)
import qualified Sound.MIDI.File            as F
import qualified Sound.MIDI.Util            as U
import           System.FilePath            (takeDirectory, (</>))

-- encore metadata

data EncoreInfo f = EncoreInfo
  { title              :: T.Text
  , artist             :: T.Text
  , diff               :: HM.HashMap T.Text Int -- keys: ds/drums ba/bass gr/guitar vl/vocals plastic_drums plastic_bass plastic_guitar pitched_vocals; -1 for no part
  , stems              :: HM.HashMap T.Text [f] -- keys: drums bass lead vocals backing (backing required); values can be string or array of strings
  , midi               :: f
  , icon_drums         :: T.Text -- aka sid, usual value Drum
  , icon_bass          :: T.Text -- aka sib, usual value Bass
  , icon_guitar        :: T.Text -- aka sig, usual value Guitar or Keyboard
  , icon_vocals        :: T.Text -- aka siv, usual value Vocals
  -- rest optional
  , length_            :: Maybe Int -- seconds, maybe we should allow non-integer?
  , charters           :: [T.Text]
  , release_year       :: Maybe T.Text
  , album              :: Maybe T.Text
  , art                :: Maybe f
  , preview_start_time :: Maybe Int -- milliseconds
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
  , codecOut = makeOut \case
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
  stackJSON = asObject "EncoreInfo" do
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
    -- PAD names are supported by encore loading from song.ini format
    Just "PART GUITAR"    -> U.setTrackName "PAD GUITAR"  trk
    Just "PART BASS"      -> U.setTrackName "PAD BASS"    trk
    Just "PART DRUMS"     -> U.setTrackName "PAD DRUMS"   trk
    Just "PART VOCALS"    -> U.setTrackName "PAD VOCALS"  trk
    Just "PART KEYS"      -> U.setTrackName "PAD KEYS"    trk
    Just "PLASTIC GUITAR" -> U.setTrackName "PART GUITAR" trk
    Just "PLASTIC BASS"   -> U.setTrackName "PART BASS"   trk
    Just "PLASTIC DRUMS"  -> U.setTrackName "PART DRUMS"  trk
    -- PLASTIC VOCALS is intended for 5-fret vocals track, leave name as-is
    Just "PITCHED VOCALS" -> U.setTrackName "PART VOCALS" trk -- this is RB style vocals
    Just "PRO VOCALS"     -> U.setTrackName "PART VOCALS" trk -- RB style vocals (newer Fortnite name)
    Just "PLASTIC KEYS"   -> U.setTrackName "PART KEYS"   trk
    _                     -> trk
  in F.Cons typ dvn $ map renameTrack trks

loadEncoreInfo :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m (EncoreInfo FilePath)
loadEncoreInfo f = do
  -- should be utf-8 but we'll be safe
  json <- stackIO (B.readFile f) >>= decodeJSONText . decodeGeneral
  info <- mapStackTraceT (`runReaderT` json) fromJSON
  let dir = takeDirectory f
  return $ (dir </>) . T.unpack <$> info

data EncoreFile t = EncoreFile
  { partGuitar    :: EncoreGuitar t
  , partKeys      :: EncoreGuitar t
  , partBass      :: EncoreGuitar t
  , partDrums     :: EncoreDrums  t
  , partVocals    :: EncorePart   t
  , plasticGuitar :: FiveTrack    t
  , plasticKeys   :: FiveTrack    t
  , plasticBass   :: FiveTrack    t
  , plasticDrums  :: DrumTrack    t
  , pitchedVocals :: VocalTrack   t
  , events        :: EventsTrack  t
  , beat          :: BeatTrack    t
  , section       :: SectionTrack t
  } deriving (Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EncoreFile t)

instance ParseFile EncoreFile where
  parseFile = do
    partGuitar    <- (.partGuitar   ) =. fileTrack ("PART GUITAR"    :| [])
    partKeys      <- (.partKeys     ) =. fileTrack ("PART KEYS"      :| [])
    partBass      <- (.partBass     ) =. fileTrack ("PART BASS"      :| [])
    partDrums     <- (.partDrums    ) =. fileTrack ("PART DRUMS"     :| [])
    partVocals    <- (.partVocals   ) =. fileTrack ("PART VOCALS"    :| [])
    plasticGuitar <- (.plasticGuitar) =. fileTrack ("PLASTIC GUITAR" :| [])
    plasticKeys   <- (.plasticKeys  ) =. fileTrack ("PLASTIC KEYS"   :| [])
    plasticBass   <- (.plasticBass  ) =. fileTrack ("PLASTIC BASS"   :| [])
    plasticDrums  <- (.plasticDrums ) =. fileTrack ("PLASTIC DRUMS"  :| [])
    pitchedVocals <- (.pitchedVocals) =. fileTrack ("PITCHED VOCALS" :| ["PRO VOCALS"])
    events        <- (.events       ) =. fileTrack ("EVENTS"         :| [])
    beat          <- (.beat         ) =. fileTrack ("BEAT"           :| [])
    section       <- (.section      ) =. fileTrack ("SECTION"        :| [])
    return EncoreFile{..}
