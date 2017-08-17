{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Genre (interpretGenre, FullGenre(..)) where

import           Control.Monad (guard)
import           Data.Char     (isAlphaNum)
import           Data.Maybe    (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Text     as T

data Genre = Genre T.Text T.Text [Subgenre]
  deriving (Eq, Ord, Show, Read)

data Subgenre = Subgenre T.Text T.Text
  deriving (Eq, Ord, Show, Read)

data FullGenre = FullGenre
  { fofGenre     :: T.Text
  , rbn2Genre    :: T.Text
  , rbn2Subgenre :: T.Text
  , rbn1Genre    :: T.Text
  , rbn1Subgenre :: T.Text
  } deriving (Eq, Ord, Show, Read)

interpretGenre :: Maybe T.Text -> Maybe T.Text -> FullGenre
interpretGenre mg ms = let
  -- TODO: make sure e.g. prog metal is "Prog Metal" not "Metal" in FoF
  defGenre fof = fill fof "other" "other"
  fill fof gen sub = uncurry (FullGenre fof gen sub) $ magmaV1Genre (gen, sub)
  std = T.filter isAlphaNum . T.toLower
  findRBN2Genre g = [ gen | gen@(Genre key disp _) <- allGenres, elem (std g) [std key, std disp] ]
  findRBN2Subgenre subs s = [ sub | sub@(Subgenre key disp) <- subs, elem (std s) [std key, std disp] ]
  findSingle g = fromMaybe (defGenre g) $ listToMaybe $ catMaybes
    -- first just check if it matches a top genre
    [ case findRBN2Genre g of
      []                   -> Nothing
      Genre key disp _ : _ -> Just $ fill disp key $ defaultSubgenre key
    -- then check if it's something like "Doom Metal" where it's "Subgenre Genre"
    , case T.words g of
      [sub, gen] -> findStrict gen sub
      _          -> Nothing
    -- then, if it's not something super-generic, check if it's a subgenre
    , case std g of
        "college" -> findDoubleMaybe "alternative" "college"
        "chicago" -> findDoubleMaybe "blues" "chicago"
        "delta" -> findDoubleMaybe "blues" "delta"
        "bluegrass" -> findDoubleMaybe "country" "bluegrass"
        "honkytonk" -> findDoubleMaybe "country" "honkytonk"
        "outlaw" -> findDoubleMaybe "country" "outlaw"
        "traditionalfolk" -> findDoubleMaybe "country" "traditionalfolk"
        "goth" -> findDoubleMaybe "glam" "goth"
        "alternativerap" -> findDoubleMaybe "hiphoprap" "alternativerap"
        "gangsta" -> findDoubleMaybe "hiphoprap" "gangsta"
        "gangstarap" -> findDoubleMaybe "hiphoprap" "gangsta"
        "hardcorerap" -> findDoubleMaybe "hiphoprap" "hardcorerap"
        "hiphop" -> findDoubleMaybe "hiphoprap" "hiphop"
        "oldschoolhiphop" -> findDoubleMaybe "hiphoprap" "oldschoolhiphop"
        "rap" -> findDoubleMaybe "hiphoprap" "rap"
        "triphop" -> findDoubleMaybe "hiphoptriphop" "triphop"
        "undergroundrap" -> findDoubleMaybe "hiphoptriphop" "undergroundrap"
        "indie" -> findDoubleMaybe "indierock" "indierock"
        "lofi" -> findDoubleMaybe "indierock" "lofi"
        "mathrock" -> findDoubleMaybe "indierock" "mathrock"
        "noise" -> findDoubleMaybe "indierock" "noise"
        "noiserock" -> findDoubleMaybe "indierock" "noise"
        "postrock" -> findDoubleMaybe "indierock" "postrock"
        "shoegazing" -> findDoubleMaybe "indierock" "shoegazing"
        "shoegaze" -> findDoubleMaybe "indierock" "shoegazing"
        "acidjazz" -> findDoubleMaybe "jazz" "acidjazz"
        "ragtime" -> findDoubleMaybe "jazz" "ragtime"
        "smooth" -> findDoubleMaybe "jazz" "smooth"
        "death" -> findDoubleMaybe "metal" "death"
        "hair" -> findDoubleMaybe "metal" "hair"
        "speed" -> findDoubleMaybe "metal" "speed"
        "thrash" -> findDoubleMaybe "metal" "thrash"
        "darkwave" -> findDoubleMaybe "newwave" "darkwave"
        "electroclash" -> findDoubleMaybe "newwave" "electroclash"
        "synthpop" -> findDoubleMaybe "newwave" "synthpop"
        "ambient" -> findDoubleMaybe "popdanceelectronic" "ambient"
        "breakbeat" -> findDoubleMaybe "popdanceelectronic" "breakbeat"
        "chiptune" -> findDoubleMaybe "popdanceelectronic" "chiptune"
        "dance" -> findDoubleMaybe "popdanceelectronic" "dance"
        "downtempo" -> findDoubleMaybe "popdanceelectronic" "downtempo"
        "dub" -> findDoubleMaybe "popdanceelectronic" "dub"
        "drumandbass" -> findDoubleMaybe "popdanceelectronic" "drumandbass"
        "drumnbass" -> findDoubleMaybe "popdanceelectronic" "drumandbass"
        "electronica" -> findDoubleMaybe "popdanceelectronic" "electronica"
        "garage" -> findDoubleMaybe "popdanceelectronic" "garage"
        "hardcoredance" -> findDoubleMaybe "popdanceelectronic" "hardcoredance"
        "house" -> findDoubleMaybe "popdanceelectronic" "house"
        "industrial" -> findDoubleMaybe "popdanceelectronic" "industrial"
        "techno" -> findDoubleMaybe "popdanceelectronic" "techno"
        "trance" -> findDoubleMaybe "popdanceelectronic" "trance"
        "pop" -> findDoubleMaybe "poprock" "pop"
        "softrock" -> findDoubleMaybe "poprock" "softrock"
        "teen" -> findDoubleMaybe "poprock" "teen"
        "dancepunk" -> findDoubleMaybe "punk" "dancepunk"
        "poppunk" -> findDoubleMaybe "punk" "pop"
        "disco" -> findDoubleMaybe "rbsoulfunk" "disco"
        "funk" -> findDoubleMaybe "rbsoulfunk" "funk"
        "motown" -> findDoubleMaybe "rbsoulfunk" "motown"
        "rhythmandblues" -> findDoubleMaybe "rbsoulfunk" "rhythmandblues"
        "rhythmnblues" -> findDoubleMaybe "rbsoulfunk" "rhythmandblues"
        "reggae" -> findDoubleMaybe "reggaeska" "reggae"
        "ska" -> findDoubleMaybe "reggaeska" "ska"
        "arena" -> findDoubleMaybe "rock" "arena"
        "folkrock" -> findDoubleMaybe "rock" "folkrock"
        "psychedelic" -> findDoubleMaybe "rock" "psychedelic"
        "rockabilly" -> findDoubleMaybe "rock" "rockabilly"
        "rockandroll" -> findDoubleMaybe "rock" "rockandroll"
        "surf" -> findDoubleMaybe "rock" "surf"
        "acapella" -> findDoubleMaybe "other" "acapella"
        "acoustic" -> findDoubleMaybe "other" "acoustic"
        "contemporaryfolk" -> findDoubleMaybe "other" "contemporaryfolk"
        "experimental" -> findDoubleMaybe "other" "experimental"
        "oldies" -> findDoubleMaybe "other" "oldies"
        _ -> Nothing
    -- then, check if a subgenre and genre are both inside it
    , listToMaybe $ do
      Genre key disp subs <- allGenres
      Subgenre skey sdisp <- subs
      guard $ all (`T.isInfixOf` std g) [std disp, std sdisp]
      return $ fill g key skey
    -- finally, check if just a genre is inside it
    , listToMaybe $ do
      Genre key disp _ <- allGenres
      guard $ std disp `T.isInfixOf` std g
      return $ fill g key $ defaultSubgenre key
    ]
  findDoubleMaybe g s = case findRBN2Genre g of
    [] -> Nothing
    Genre key disp subs : _ -> Just $ case findRBN2Subgenre subs s of
      []                  -> fill disp key $ defaultSubgenre key
      Subgenre skey _ : _ -> fill disp key skey
  findStrict g s = case findRBN2Genre g of
    [] -> Nothing
    Genre key disp subs : _ -> case findRBN2Subgenre subs s of
      []                  -> Nothing
      Subgenre skey _ : _ -> Just $ fill disp key skey
  findDouble g s = fromMaybe (defGenre g) $ findDoubleMaybe g s
  in case (mg, ms) of
    (Nothing, Nothing) -> defGenre "Other"
    (Nothing, Just s)  -> findSingle s
    (Just g, Nothing)  -> findSingle g
    (Just g, Just s)   -> findDouble g s

magmaV1Genre :: (T.Text, T.Text) -> (T.Text, T.Text)
magmaV1Genre p@(g, s) = case g of
  "alternative" -> p
  "blues" -> p
  "classical" -> ("other", "other")
  "classicrock" -> p
  "country" -> p
  "emo" -> p
  "fusion" -> p
  "glam" -> p
  "grunge" -> p
  "hiphoprap" -> ("urban", s)
  "indierock" -> p
  "inspirational" -> ("other", "other")
  "jazz" -> p
  "jrock" -> ("rock", "rock")
  "latin" -> ("other", "other")
  "metal" -> p
  "newwave" -> p
  "novelty" -> p
  "numetal" -> p
  "popdanceelectronic" -> case s of
    "ambient"       -> ("other", s)
    "breakbeat"     -> ("other", s)
    "chiptune"      -> ("other", s)
    "dance"         -> ("other", s)
    "downtempo"     -> ("other", "electronica")
    "dub"           -> ("rock", "reggae")
    "drumandbass"   -> ("other", s)
    "electronica"   -> ("other", s)
    "garage"        -> ("urban", s)
    "hardcoredance" -> ("urban", s)
    "house"         -> ("other", s)
    "industrial"    -> ("urban", s)
    "techno"        -> ("other", s)
    "trance"        -> ("other", s)
    "other"         -> ("other", "electronica")
    _               -> ("other", "electronica")
  "poprock" -> p
  "prog" -> p
  "punk" -> p
  "rbsoulfunk" -> case s of
    "disco"          -> ("poprock", s)
    "funk"           -> ("rock", s)
    "motown"         -> ("poprock", s)
    "rhythmandblues" -> ("poprock", s)
    "soul"           -> ("poprock", s)
    "other"          -> ("poprock", "rhythmandblues")
    _                -> ("poprock", "rhythmandblues")
  "reggaeska" -> case s of
    "reggae" -> ("rock", "reggae")
    "ska"    -> ("rock", "ska")
    "other"  -> ("rock", "reggae")
    _        -> ("rock", "reggae")
  "rock" -> p
  "southernrock" -> p
  "world" -> ("other", "other")
  "other" -> p
  _ -> ("other", "other")

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
    -- TODO: saw ('genre' 'new_wave') ('sub_genre' 'subgenre_synth') in Confury's Night of Fate
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
    [ Subgenre "disco" "Disco"
    , Subgenre "funk" "Funk"
    , Subgenre "motown" "Motown"
    , Subgenre "rhythmandblues" "Rhythm and Blues"
    , Subgenre "soul" "Soul"
    , Subgenre "other" "Other"
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
