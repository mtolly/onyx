{-# LANGUAGE OverloadedStrings #-}
module Genre where

import qualified Data.Text as T
import Control.Monad (guard)

data Genre = Genre T.Text T.Text [Subgenre]
  deriving (Eq, Ord, Show, Read)

data Subgenre = Subgenre T.Text T.Text
  deriving (Eq, Ord, Show, Read)

genreDisplay :: T.Text -> T.Text
genreDisplay k = let
  match = do
    Genre key name _ <- allGenres
    guard $ k == key
    return name
  in case match of
    name : _ -> name
    [] -> error $ "genreDisplay: couldn't recognize the genre " ++ show k

allGenres :: [Genre]
allGenres =
  [ Genre "alternative" "Alternative"
    [ Subgenre "alternative" "Alternative"
    , Subgenre "college" "College"
    , Subgenre "other" "Other"
    ]
  , Genre "blues" "Blues"
    [ Subgenre "acoustic" "Acoustic"
    , Subgenre "chicago" "Chicago"
    , Subgenre "classic" "Classic"
    , Subgenre "contemporary" "Contemporary"
    , Subgenre "country" "Country"
    , Subgenre "delta" "Delta"
    , Subgenre "electric" "Electric"
    , Subgenre "other" "Other"
    ]
  , Genre "classical" "Classical"
    [ Subgenre "classical" "Classical"
    ]
  , Genre "classicrock" "Classic Rock"
    [ Subgenre "classicrock" "Classic Rock"
    ]
  , Genre "country" "Country"
    [ Subgenre "alternative" "Alternative"
    , Subgenre "bluegrass" "Bluegrass"
    , Subgenre "contemporary" "Contemporary"
    , Subgenre "honkytonk" "Honky Tonk"
    , Subgenre "outlaw" "Outlaw"
    , Subgenre "traditionalfolk" "Traditional Folk"
    , Subgenre "other" "Other"
    ]
  , Genre "emo" "Emo"
    [ Subgenre "emo" "Emo"
    ]
  , Genre "fusion" "Fusion"
    [ Subgenre "fusion" "Fusion"
    ]
  , Genre "glam" "Glam"
    [ Subgenre "glam" "Glam"
    , Subgenre "goth" "Goth"
    , Subgenre "other" "Other"
    ]
  , Genre "grunge" "Grunge"
    [ Subgenre "grunge" "Grunge"
    ]
  , Genre "hiphoprap" "Hip-Hop/Rap"
    [ Subgenre "alternativerap" "Alternative Rap"
    , Subgenre "gangsta" "Gangsta"
    , Subgenre "hardcorerap" "Hardcore Rap"
    , Subgenre "hiphop" "Hip Hop"
    , Subgenre "oldschoolhiphop" "Old School Hip Hop"
    , Subgenre "rap" "Rap"
    , Subgenre "triphop" "Trip Hop"
    , Subgenre "undergroundrap" "Underground Rap"
    , Subgenre "other" "Other"
    ]
  , Genre "indierock" "Indie Rock"
    [ Subgenre "indierock" "Indie Rock"
    , Subgenre "lofi" "Lo-fi"
    , Subgenre "mathrock" "Math Rock"
    , Subgenre "noise" "Noise"
    , Subgenre "postrock" "Post Rock"
    , Subgenre "shoegazing" "Shoegazing"
    , Subgenre "other" "Other"
    ]
  , Genre "inspirational" "Inspirational"
    [ Subgenre "inspirational" "Inspirational"
    ]
  , Genre "jazz" "Jazz"
    [ Subgenre "acidjazz" "Acid Jazz"
    , Subgenre "contemporary" "Contemporary"
    , Subgenre "experimental" "Experimental"
    , Subgenre "ragtime" "Ragtime"
    , Subgenre "smooth" "Smooth"
    , Subgenre "other" "Other"
    ]
  , Genre "jrock" "J-Rock"
    [ Subgenre "jrock" "J-Rock"
    ]
  , Genre "latin" "Latin"
    [ Subgenre "latin" "Latin"
    ]
  , Genre "metal" "Metal"
    [ Subgenre "alternative" "Alternative"
    , Subgenre "black" "Black"
    , Subgenre "core" "Core"
    , Subgenre "death" "Death"
    , Subgenre "hair" "Hair"
    , Subgenre "industrial" "Industrial"
    , Subgenre "metal" "Metal"
    , Subgenre "power" "Power"
    , Subgenre "prog" "Prog"
    , Subgenre "speed" "Speed"
    , Subgenre "thrash" "Thrash"
    , Subgenre "other" "Other"
    ]
  , Genre "newwave" "New Wave"
    [ Subgenre "darkwave" "Dark Wave"
    , Subgenre "electroclash" "Electroclash"
    , Subgenre "newwave" "New Wave"
    , Subgenre "synthpop" "Synthpop"
    , Subgenre "other" "Other"
    ]
  , Genre "novelty" "Novelty"
    [ Subgenre "novelty" "Novelty"
    ]
  , Genre "numetal" "Nu-Metal"
    [ Subgenre "numetal" "Nu-Metal"
    ]
  , Genre "popdanceelectronic" "Pop/Dance/Electronic"
    [ Subgenre "ambient" "Ambient"
    , Subgenre "breakbeat" "Breakbeat"
    , Subgenre "chiptune" "Chiptune"
    , Subgenre "dance" "Dance"
    , Subgenre "downtempo" "Downtempo"
    , Subgenre "dub" "Dub"
    , Subgenre "drumandbass" "Drum and Bass"
    , Subgenre "electronica" "Electronica"
    , Subgenre "garage" "Garage"
    , Subgenre "hardcoredance" "Hardcore Dance"
    , Subgenre "house" "House"
    , Subgenre "industrial" "Industrial"
    , Subgenre "techno" "Techno"
    , Subgenre "trance" "Trance"
    , Subgenre "other" "Other"
    ]
  , Genre "poprock" "Pop-Rock"
    [ Subgenre "contemporary" "Contemporary"
    , Subgenre "pop" "Pop"
    , Subgenre "softrock" "Soft Rock"
    , Subgenre "teen" "Teen"
    , Subgenre "other" "Other"
    ]
  , Genre "prog" "Prog"
    [ Subgenre "progrock" "Prog Rock"
    ]
  , Genre "punk" "Punk"
    [ Subgenre "alternative" "Alternative"
    , Subgenre "classic" "Classic"
    , Subgenre "dancepunk" "Dance Punk"
    , Subgenre "garage" "Garage"
    , Subgenre "hardcore" "Hardcore"
    , Subgenre "pop" "Pop"
    , Subgenre "other" "Other"
    ]
  , Genre "rbsoulfunk" "R&B/Soul/Funk"
    [ Subgenre "rhythmandblues" "Rhythm and Blues"
    ]
  , Genre "reggaeska" "Reggae/Ska"
    [ Subgenre "reggae" "Reggae"
    , Subgenre "ska" "Ska"
    , Subgenre "other" "Other"
    ]
  , Genre "rock" "Rock"
    [ Subgenre "arena" "Arena"
    , Subgenre "blues" "Blues"
    , Subgenre "folkrock" "Folk Rock"
    , Subgenre "garage" "Garage"
    , Subgenre "hardrock" "Hard Rock"
    , Subgenre "psychadelic" "Psychedelic" -- note: dta name is misspelled
    , Subgenre "rock" "Rock"
    , Subgenre "rockabilly" "Rockabilly"
    , Subgenre "rockandroll" "Rock and Roll"
    , Subgenre "surf" "Surf"
    , Subgenre "other" "Other"
    ]
  , Genre "southernrock" "Southern Rock"
    [ Subgenre "southernrock" "Southern Rock"
    ]
  , Genre "world" "World"
    [ Subgenre "world" "World"
    ]
  , Genre "other" "Other"
    [ Subgenre "acapella" "A capella"
    , Subgenre "acoustic" "Acoustic"
    , Subgenre "contemporaryfolk" "Contemporary Folk"
    , Subgenre "experimental" "Experimental"
    , Subgenre "oldies" "Oldies"
    , Subgenre "other" "Other"
    ]
  ]

defaultSubgenre :: T.Text -> T.Text
defaultSubgenre = \case
  "alternative" -> "alternative"
  "blues" -> "other"
  "classical" -> "classical"
  "classicrock" -> "classicrock"
  "country" -> "other"
  "emo" -> "emo"
  "fusion" -> "fusion"
  "glam" -> "glam"
  "grunge" -> "grunge"
  "hiphoprap" -> "other"
  "indierock" -> "other"
  "inspirational" -> "inspirational"
  "jazz" -> "other"
  "jrock" -> "jrock"
  "latin" -> "latin"
  "metal" -> "metal"
  "newwave" -> "newwave"
  "novelty" -> "novelty"
  "numetal" -> "numetal"
  "popdanceelectronic" -> "other"
  "poprock" -> "other"
  "prog" -> "progrock"
  "punk" -> "other"
  "rbsoulfunk" -> "rhythmandblues"
  "reggaeska" -> "other"
  "rock" -> "rock"
  "southernrock" -> "southernrock"
  "world" -> "world"
  "other" -> "other"
  g -> error $ "defaultSubgenre: couldn't recognize the genre " ++ show g
