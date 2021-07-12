{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Genre
( interpretGenre, FullGenre(..)
, GenreWoR(..), displayWoRGenre, qbWoRGenre, rbn2ToWoRGenre
) where

import           Control.Monad      (guard)
import           Data.Char          (isAlphaNum)
import           Data.Maybe         (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Data.Word          (Word32)
import           Neversoft.Checksum (qbKeyCRC)

data Genre = Genre T.Text T.Text [Subgenre]
  deriving (Eq, Ord, Show)

data Subgenre = Subgenre T.Text T.Text
  deriving (Eq, Ord, Show)

data FullGenre = FullGenre
  { fofGenre     :: T.Text
  , rbn2Genre    :: T.Text
  , rbn2Subgenre :: T.Text
  , rbn1Genre    :: T.Text
  , rbn1Subgenre :: T.Text
  , worGenre     :: GenreWoR
  } deriving (Eq, Ord, Show)

interpretGenre :: Maybe T.Text -> Maybe T.Text -> FullGenre
interpretGenre mg ms = let
  -- TODO: make sure e.g. prog metal is "Prog Metal" not "Metal" in FoF
  defGenre fof = fill fof "other" "other"
  fill fof gen sub = let
    (g1, s1) = magmaV1Genre (gen, sub)
    in FullGenre fof gen sub g1 s1 $ rbn2ToWoRGenre (gen, sub)
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
        "darkwave" -> findDoubleMaybe "new_wave" "darkwave"
        "electroclash" -> findDoubleMaybe "new_wave" "electroclash"
        "synth" -> findDoubleMaybe "new_wave" "synthpop"
        "synthpop" -> findDoubleMaybe "new_wave" "synthpop"
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
  "new_wave" -> p
  "novelty" -> p
  "numetal" -> p
  "popdanceelectronic" -> case s of
    "ambient"       -> ("other", s)
    "breakbeat"     -> ("other", s)
    "chiptune"      -> ("other", s)
    "dance"         -> ("other", s)
    "downtempo"     -> ("other", "electronica")
    "dub"           -> ("rock", "reggae")
    "drumandbass"   -> ("urban", s)
    "electronica"   -> ("other", s) -- (urban, electronica) also seems to work
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
  , Genre "new_wave" "New Wave"
    [ Subgenre "darkwave" "Dark Wave"
    , Subgenre "electroclash" "Electroclash"
    , Subgenre "new_wave" "New Wave"
    , Subgenre "synth" "Synthpop"
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
  "new_wave" -> "new_wave"
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

-- List from data/compressed/PAK/qs.pak.xen file "genres.qs" (crc = "genres")
data GenreWoR
  = WoR_Alternative
  | WoR_Big_Band
  | WoR_Black_Metal
  | WoR_Blues
  | WoR_Blues_Rock
  | WoR_Classic_Rock
  | WoR_Country
  | WoR_Dance
  | WoR_Death_Metal
  | WoR_Disco
  | WoR_Electronic
  | WoR_Experimental
  | WoR_Funk
  | WoR_Glam_Rock
  | WoR_Grunge
  | WoR_Hard_Rock
  | WoR_Hardcore_Punk
  | WoR_Heavy_Metal
  | WoR_Hip_Hop
  | WoR_Indie_Rock
  | WoR_Industrial
  | WoR_International
  | WoR_Jazz
  | WoR_Metal
  | WoR_Modern_Rock
  | WoR_New_Wave
  | WoR_Nu_Metal
  | WoR_Other
  | WoR_Pop
  | WoR_Pop_Punk
  | WoR_Pop_Rock
  | WoR_Prog_Rock
  | WoR_Punk
  | WoR_R_n_B
  | WoR_Reggae
  | WoR_Rock
  | WoR_Rockabilly
  | WoR_Ska_Punk
  | WoR_Southern_Rock
  | WoR_Speed_Metal
  | WoR_Surf_Rock
  deriving (Eq, Ord, Show, Enum, Bounded)

displayWoRGenre :: GenreWoR -> T.Text
displayWoRGenre = \case
  WoR_Alternative   -> "Alternative"
  WoR_Big_Band      -> "Big Band"
  WoR_Black_Metal   -> "Black Metal"
  WoR_Blues         -> "Blues"
  WoR_Blues_Rock    -> "Blues Rock"
  WoR_Classic_Rock  -> "Classic Rock"
  WoR_Country       -> "Country"
  WoR_Dance         -> "Dance"
  WoR_Death_Metal   -> "Death Metal"
  WoR_Disco         -> "Disco"
  WoR_Electronic    -> "Electronic"
  WoR_Experimental  -> "Experimental"
  WoR_Funk          -> "Funk"
  WoR_Glam_Rock     -> "Glam Rock"
  WoR_Grunge        -> "Grunge"
  WoR_Hard_Rock     -> "Hard Rock"
  WoR_Hardcore_Punk -> "Hardcore Punk"
  WoR_Heavy_Metal   -> "Heavy Metal"
  WoR_Hip_Hop       -> "Hip Hop"
  WoR_Indie_Rock    -> "Indie Rock"
  WoR_Industrial    -> "Industrial"
  WoR_International -> "International"
  WoR_Jazz          -> "Jazz"
  WoR_Metal         -> "Metal"
  WoR_Modern_Rock   -> "Modern Rock"
  WoR_New_Wave      -> "New Wave"
  WoR_Nu_Metal      -> "Nu Metal"
  WoR_Other         -> "Other"
  WoR_Pop           -> "Pop"
  WoR_Pop_Punk      -> "Pop Punk"
  WoR_Pop_Rock      -> "Pop Rock"
  WoR_Prog_Rock     -> "Prog Rock"
  WoR_Punk          -> "Punk"
  WoR_R_n_B         -> "R & B"
  WoR_Reggae        -> "Reggae"
  WoR_Rock          -> "Rock"
  WoR_Rockabilly    -> "Rockabilly"
  WoR_Ska_Punk      -> "Ska Punk"
  WoR_Southern_Rock -> "Southern Rock"
  WoR_Speed_Metal   -> "Speed Metal"
  WoR_Surf_Rock     -> "Surf Rock"

qbWoRGenre :: GenreWoR -> Word32
qbWoRGenre = qbKeyCRC . TE.encodeUtf8 . T.toLower . displayWoRGenre
-- TODO verify "R & B"

rbn2ToWoRGenre :: (T.Text, T.Text) -> GenreWoR
rbn2ToWoRGenre (g, s) = case g of
  "alternative" -> WoR_Alternative
  "blues" -> WoR_Blues
  "classical" -> WoR_Other
  "classicrock" -> WoR_Classic_Rock
  "country" -> WoR_Country
  "emo" -> WoR_Pop_Punk
  "fusion" -> WoR_Jazz
  "glam" -> WoR_Glam_Rock
  "grunge" -> WoR_Grunge
  "hiphoprap" -> WoR_Hip_Hop
  "indierock" -> WoR_Indie_Rock
  "inspirational" -> WoR_Other
  "jazz" -> WoR_Jazz
  "jrock" -> WoR_International
  "latin" -> WoR_International
  "metal" -> case s of
    "black" -> WoR_Black_Metal
    "death" -> WoR_Death_Metal
    "speed" -> WoR_Speed_Metal
    _       -> WoR_Metal
  "new_wave" -> WoR_New_Wave
  "novelty" -> WoR_Other
  "numetal" -> WoR_Nu_Metal
  "popdanceelectronic" -> case s of
    "dance"         -> WoR_Dance
    "dub"           -> WoR_Reggae
    "hardcoredance" -> WoR_Dance
    "industrial"    -> WoR_Industrial
    _               -> WoR_Electronic
  "poprock" -> WoR_Pop_Rock
  "prog" -> WoR_Prog_Rock
  "punk" -> WoR_Punk
  "rbsoulfunk" -> case s of
    "disco" -> WoR_Disco
    "funk"  -> WoR_Funk
    _       -> WoR_R_n_B
  "reggaeska" -> case s of
    "ska" -> WoR_Ska_Punk
    _     -> WoR_Reggae
  "rock" -> case s of
    "blues"      -> WoR_Blues_Rock
    "hard"       -> WoR_Hard_Rock
    "rockabilly" -> WoR_Rockabilly
    "surf"       -> WoR_Surf_Rock
    _            -> WoR_Rock
  "southernrock" -> WoR_Southern_Rock
  "world" -> WoR_International
  "other" -> WoR_Other
  _ -> WoR_Other
