{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

{-
PS2 Taiko no Tatusjin game formats. There are two generations of disc layout:

Earlier discs
- "Doki! Shinkyoku Darake no Haru Matsuri"
- "Appare Sandaime"
- "Waku Waku Anime Matsuri"
Properties
- IMAGE00.TBL index, IMAGE00.DAT blob
- Charts in SHEET/*.bin (upper case on some discs)
- Song audio is headerless interleaved PS2 ADPCM in BGM(differing number)

Later discs
- "Atsumare! Matsuri da!! Yondaime"
- "Go! Go! Godaime"
- "Taiko Drum Master" and US "Taiko: Drum Master"
- "Tobikkiri! Anime Special"
- "Wai Wai Happy! Rokudaime"
- "Doka! to Oomori Nanadaime"
Properties
- LIST.BIN index, DATA.000 blob, both obfuscated
- Blob groups are zlib streams
- Charts at fumen.<song><1p|2p>_<difficulty>/sht
  (fumen.<song>_<difficulty> in Yondaime)
- Song audio at sound.stream.music_<song>/vag with a simple header
- Song metadata in gamedata/musicinfo.bin

Chart formats are closely related to Donkey Konga 1 sheet format
-}
module Onyx.Namco.Taiko where

import qualified Codec.Compression.Zlib  as Z
import           Control.Monad           (forM, guard, unless)
import           Data.Binary.Get
import           Data.Bits               (complement, xor, (.&.))
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as B8
import qualified Data.ByteString.Lazy    as BL
import           Data.Functor            ((<&>))
import           Data.Int                (Int32, Int64)
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import           Data.Word
import           Onyx.Util.Binary        (splitEvery)
import           Onyx.Util.Handle
import           Onyx.Util.Text.Decode   (decodeGeneral)

-- * Obfuscation

-- obfuscation applied to LIST.BIN and DATA.000
taikoKeyByte :: Int -> Word8
taikoKeyByte i = complement $ fromIntegral i .&. 0xA5

-- in LIST.BIN every byte is obfuscated
descrambleList :: BL.ByteString -> BL.ByteString
descrambleList = BL.packZipWith xor $ BL.cycle $ BL.pack $ map taikoKeyByte [0 .. 0xFF]

-- in DATA.000 only the first byte of each 16-byte block is obfuscated
descrambleData :: BL.ByteString -> BL.ByteString
descrambleData bs = BB.toLazyByteString $ mconcat $ zipWith
  (\line i -> case BL.uncons line of
    Just (h, t) -> BB.word8 (h `xor` taikoKeyByte i) <> BB.lazyByteString t
    Nothing     -> mempty
  )
  (splitEvery 16 bs)
  [0..]

-- * LIST.BIN / DATA.000

data ListGroup = ListGroup
  { name      :: B.ByteString
  , groupType :: Word32 -- 2 = zlib compressed, 6 = stored
  , sector    :: Word32 -- offset into DATA.000, in units of 0x800
  , sizeIn    :: Word32 -- compressed size
  , sizeOut   :: Word32 -- decompressed size
  , files     :: [ListFile]
  } deriving (Show)

data ListFile = ListFile
  { name   :: B.ByteString
  , size   :: Word32
  , offset :: Word32 -- within the decompressed group
  } deriving (Show)

{-
parse descrambled LIST.BIN: two counts, group table, file table, string pool;
from Tobikkiri on there's also a 24-byte ASCII build timestamp after counts,
and trailing hash word on each group record
-}
readListBin :: (MonadFail m) => BL.ByteString -> m [ListGroup]
readListBin bs = do
  let getW32 :: Int64 -> Word32
      getW32 off = runGet getWord32le $ BL.drop off bs
      numGroups = fromIntegral (getW32 0) :: Int
      numFiles  = fromIntegral (getW32 4) :: Int
      -- newer revisions put "YYYY-MM-DD-HH-MM" after the counts
      dated = BL.all (\c -> 0x30 <= c && c <= 0x39) $ BL.take 4 $ BL.drop 8 bs
      (groupStart, groupSize) = if dated then (0x20, 32) else (8, 28) :: (Int64, Int64)
      fileStart = groupStart + groupSize * fromIntegral numGroups
      strStart  = fileStart + 16 * fromIntegral numFiles
      getStr off = BL.toStrict $ BL.takeWhile (/= 0)
        $ BL.drop (strStart + fromIntegral off) bs
  unless (strStart <= BL.length bs) $ fail "LIST.BIN tables extend past end of file"
  forM [0 .. numGroups - 1] \i -> do
    let base = groupStart + groupSize * fromIntegral i
        field j = getW32 $ base + 4 * j
        count     = fromIntegral $ field 1 :: Int
        firstFile = fromIntegral $ field 2 :: Int
    files <- forM [firstFile .. firstFile + count - 1] \j -> do
      let fbase = fileStart + 16 * fromIntegral j
          ffield k = getW32 $ fbase + 4 * k
      return ListFile
        { name   = getStr $ ffield 0
        , size   = ffield 1
        , offset = ffield 2
        }
    return ListGroup
      { name      = getStr $ field 0
      , groupType = field 3
      , sector    = field 4
      , sizeIn    = field 5
      , sizeOut   = field 6
      , files     = files
      }

-- read and decompress one group from DATA.000
loadGroup :: Readable -> ListGroup -> IO BL.ByteString
loadGroup dat grp = do
  raw <- useHandle
    (subHandle id (fromIntegral grp.sector * 0x800) (Just $ fromIntegral grp.sizeIn) dat)
    handleToByteString
  let plain = descrambleData raw
  return $ if grp.groupType == 2 then Z.decompress plain else plain

-- contents of LIST.BIN + DATA.000;
-- groups are decompressed on demand and then cached
listBinFolder :: Readable -> Readable -> IO (Folder B.ByteString Readable)
listBinFolder list dat = do
  groups <- useHandle list handleToByteString >>= readListBin . descrambleList
  subs <- forM groups \grp -> do
    getBlob <- cache $ loadGroup dat grp
    let files = grp.files <&> \f ->
          ( f.name
          , makeHandle (B8.unpack grp.name <> "/" <> B8.unpack f.name) do
            blob <- getBlob
            byteStringSimpleHandle $ BL.take (fromIntegral f.size)
              $ BL.drop (fromIntegral f.offset) blob
          )
    return (grp.name, Folder { folderSubfolders = [], folderFiles = files })
  return Folder { folderSubfolders = subs, folderFiles = [] }

-- * IMAGE00.TBL / IMAGE00.DAT

data ImageEntry = ImageEntry
  { name   :: B.ByteString
  , offset :: Word32
  , size   :: Word32
  } deriving (Show)

-- IMAGE00.TBL is
-- * header giving entry count and table position
-- * (name offset, data offset, size) triples
-- * string pool
-- last entry is empty
readImageTable :: (MonadFail m) => BL.ByteString -> m [ImageEntry]
readImageTable bs = do
  let getW32 :: Int64 -> Word32
      getW32 off = runGet getWord32le $ BL.drop off bs
      count = fromIntegral $ getW32 4 :: Int
      start = fromIntegral $ getW32 8 :: Int64
      getStr off = BL.toStrict $ BL.takeWhile (/= 0) $ BL.drop (fromIntegral off) bs
  unless (start + 12 * fromIntegral count <= BL.length bs)
    $ fail "IMAGE00.TBL entry table extends past end of file"
  return do
    i <- [0 .. count - 1]
    let base = start + 12 * fromIntegral i
        name = getStr $ getW32 base
    guard $ not $ B.null name
    return ImageEntry
      { name   = name
      , offset = getW32 $ base + 4
      , size   = getW32 $ base + 8
      }

-- view contents of IMAGE00.TBL + IMAGE00.DAT
imageFolder :: Readable -> Readable -> IO (Folder B.ByteString Readable)
imageFolder tbl dat = do
  entries <- useHandle tbl handleToByteString >>= readImageTable
  return $ fromFiles $ entries <&> \e ->
    ( fromMaybe (pure e.name) $ NE.nonEmpty $ B8.split '/' e.name
    , subHandle id (fromIntegral e.offset) (Just $ fromIntegral e.size) dat
    )

-- * Notes

data TaikoNoteType
  = NoteDon     -- ^ small red
  | NoteKa      -- ^ small blue
  | NoteBigDon  -- ^ large red
  | NoteBigKa   -- ^ large blue
  | NoteRoll    -- ^ drumroll, length in seconds
  | NoteBigRoll -- ^ large drumroll, length in seconds
  | NoteBalloon -- ^ balloon / kusudama, length in seconds
  deriving (Eq, Ord, Show, Enum, Bounded)

data TaikoMeasure = TaikoMeasure
  { start :: Double
  -- ^ seconds, this is when the bar is actually heard,
  -- one bar later than position stored in the file
  , len   :: Double
  -- ^ seconds, 4 beats of this bar's tempo, spanned by the 48 note slots;
  -- not necessarily the distance to the start of the next bar
  -- (which can be early or late)
  , notes :: [SheetNote]
  } deriving (Show)

-- note together with length in seconds (zero for instantaneous notes)
data SheetNote = SheetNote
  { noteType :: TaikoNoteType
  , position :: Int -- ^ 0-47 within the measure
  , len      :: Double -- ^ seconds, for rolls and balloons
  , hits     :: Maybe Int -- ^ required hits, for balloons
  } deriving (Eq, Show)

-- * Branches

{-
both chart formats store three versions of every measure
for Taiko's chart branching (譜面分岐): normal, advanced (玄人) and master (達人),
game switches mid-song based on conditions.
most charts only fill in the first
-}
data TaikoBranch = BranchNormal | BranchAdvanced | BranchMaster
  deriving (Eq, Ord, Show, Enum, Bounded)

data TaikoChart = TaikoChart
  { branch   :: Maybe TaikoBranch -- ^ which branch was used, if the chart had several
  , measures :: [TaikoMeasure]
  } deriving (Show)

{-
Pick one branch to import based on thresholds and note counts
* If there are conditions, take hardest branch
* If no conditions, and advanced and master exist but are different,
  take master; a few songs in Taiko 5 and Rokudaime are like this
* Otherwise, extra branches aren't difficulty levels;
  in Rokudaime (and some Godaime and Nanadaime songs) branches 2 and 3
  hold one identical alternate chart, branch 1 scales from Easy to Oni as usual,
  so import Branch 1
-}
chooseBranch :: Bool -> [[[SheetNote]]] -> Maybe TaikoBranch
chooseBranch hasConditions perMeasure = let
  inBranch b = map (\branches -> concat $ take 1 $ drop (fromEnum b) branches) perMeasure
  used b = any (not . null) $ inBranch b
  in if not (used BranchAdvanced || used BranchMaster)
    then Nothing
    else Just if
      | hasConditions -> last $ filter used [minBound .. maxBound]
      | used BranchAdvanced && used BranchMaster
        && inBranch BranchAdvanced /= inBranch BranchMaster
        -> BranchMaster
      | otherwise -> BranchNormal

-- * Chart format: @fumen\/sht@ (Yondaime onwards)

{-
sht layout: 16-byte header (measure count, offset of note table, note
count), then measure table, then flat array of 16-byte notes

each measure record starts with a header, then ends with two 9-word blocks,
one per player side; two sides are identical in a 1P chart.
each side block is three (first note, note count, scroll speed) triples,
one per `TaikoBranch'

measure header starts with the measure's position in milliseconds and tempo in
BPM, stored as integers in Yondaime, Godaime, Taiko 5 and the US game, and as
floats in Tobikkiri, Rokudaime and Nanadaime. its size varies by game (20, 24,
40 or 64 bytes), so we derive it from the measure stride, and the number type
is detected from whether the tempo makes sense as an integer. after a flag
word or two come the branch conditions, which are -1 unless the song branches
at that measure; the 64-byte header ends with per-branch scroll speeds
-}
readFumen :: (MonadFail m) => BL.ByteString -> m TaikoChart
readFumen bs = do
  let getW32 :: Int64 -> Word32
      getW32 off = runGet getWord32le $ BL.drop off bs
      getI32 off = fromIntegral (getW32 off) :: Int32
      getF32 off = runGet getFloatle $ BL.drop off bs
      numMeasures = fromIntegral $ getW32 0 :: Int
      noteStart   = fromIntegral $ getW32 4 :: Int64
      numNotes    = fromIntegral $ getW32 8 :: Int
  unless (numMeasures > 0) $ fail "fumen has no measures"
  unless (noteStart + 16 * fromIntegral numNotes <= BL.length bs)
    $ fail "fumen note table extends past end of file"
  let stride = (noteStart - 16) `div` fromIntegral numMeasures
  unless (stride > 72 && (noteStart - 16) `mod` fromIntegral numMeasures == 0)
    $ fail "unrecognized fumen measure size"
  let base i = 16 + stride * fromIntegral i
      -- a float tempo read as an integer is enormous (or negative)
      isFloat = let bpm = getI32 $ base (0 :: Int) + 4 in bpm <= 0 || bpm >= 1000
      number off = if isFloat then realToFrac (getF32 off) else fromIntegral (getI32 off)
      headerWords = fromIntegral $ (stride - 72) `div` 4 :: Int
      -- the 20-byte header has one flag word before the conditions, the others two
      conditionWords = [if headerWords <= 5 then 3 else 4 .. min headerWords 10 - 1]
      hasConditions = or
        [ getI32 (base i + 4 * fromIntegral w) /= -1
        | i <- [0 .. numMeasures - 1]
        , w <- conditionWords
        ]
      starts = [ number (base i) / 1000 | i <- [0 .. numMeasures - 1] ]
      -- bar is 4 beats of its own tempo;
      -- next bar can start earlier or later than where this bar ends
      nominal =
        [ let bpm = number (base i + 4) in if bpm > 0 then 240 / bpm else 2
        | i <- [0 .. numMeasures - 1]
        ]
      -- integer-tempo games round the tempo, so when the gap to the next bar
      -- is a clean number of half-beats, get exact bar length from that instead
      lens = zipWith3
        (\this next nom -> case next of
          Nothing -> nom
          Just (nextStart, nextNom) -> let
            heardGap = (nextStart + nextNom) - (this + nom)
            beats = heardGap / nom * 4
            halves = fromIntegral (round (beats * 2) :: Int) / 2
            in if halves > 0 && abs (beats - halves) <= 0.08
              then heardGap / halves * 4
              else nom
        )
        starts
        (map Just (zip (drop 1 starts) (drop 1 nominal)) <> [Nothing])
        nominal
      notesOf i whichBranch = do
        let side = base i + stride - 72 + 12 * fromIntegral (fromEnum whichBranch)
            first = fromIntegral $ getW32 side :: Int
            count = fromIntegral $ getW32 $ side + 4 :: Int
        j <- [first .. first + count - 1]
        let nbase = noteStart + 16 * fromIntegral j
            extraA = getI32 $ nbase + 8
            extraB = getI32 $ nbase + 12
        noteType <- case getI32 nbase of
          -- higher numbers show up in dense passages;
          -- some of these are not 100% verified
          1  -> [NoteDon]
          2  -> [NoteDon]
          3  -> [NoteDon]
          4  -> [NoteKa]
          5  -> [NoteKa]
          6  -> [NoteRoll]
          7  -> [NoteBigDon]
          8  -> [NoteBigKa]
          9  -> [NoteBigRoll]
          10 -> [NoteBalloon]
          11 -> [NoteBigDon]
          _  -> []
        return SheetNote
          { noteType = noteType
          , position = fromIntegral $ getI32 $ nbase + 4
          , len      = if extraB < 0 then 0 else fromIntegral extraB / 1000
          , hits     = if extraA < 0 then Nothing else Just $ fromIntegral extraA
          }
      branches = [ [ notesOf i b | b <- [minBound .. maxBound :: TaikoBranch] ] | i <- [0 .. numMeasures - 1] ]
      branch = chooseBranch hasConditions branches
  return TaikoChart
    { branch = branch
    , measures =
      -- stored position is one bar early, see 'TaikoMeasure'
      [ TaikoMeasure { start = start + len, len = len, notes = notesOf i $ fromMaybe BranchNormal branch }
      | (i, start, len) <- zip3 [0 .. numMeasures - 1] starts lens
      ]
    }

-- * Chart format: SHEET/*.bin (Doki!, Sandaime, Anime Matsuri)

{-
earlier chart format, this is like a wider Donkey Konga 1 sheet format
* 4-byte file header
* 324-byte measure records
* FE FF FF FF terminator

each record
* 8 unused bytes
* 4-byte header of (position, 1, measure length in 60ths of a second)
* 24 bytes per-side extras
* 2 x 144 bytes note slots

each side's 144 bytes
* measure's 48-slot grid 3 times (one per branch)

second side is dupe of the first in a 1P chart

position is in units of 1/98.44 s just like Donkey Konga
-}
readSheetBin :: (MonadFail m) => BL.ByteString -> m TaikoChart
readSheetBin bs = do
  let recordSize = 0x144 :: Int64
      body = BL.drop 4 bs
      count = fromIntegral $ (BL.length body - 4) `div` recordSize :: Int
  unless (count > 0 && (BL.length body - 4) `mod` recordSize == 0)
    $ fail "unrecognized sheet .bin size"
  let record i = BL.take recordSize $ BL.drop (recordSize * fromIntegral i) body
      branches = [ [ sheetNotes (record i) b | b <- [minBound .. maxBound] ] | i <- [0 .. count - 1] ]
      -- no branch conditions have been found in this format
      branch = chooseBranch False branches
  return TaikoChart
    { branch = branch
    , measures = map (sheetMeasure $ fromMaybe BranchNormal branch) $ map record [0 .. count - 1]
    }

sheetMeasure :: TaikoBranch -> BL.ByteString -> TaikoMeasure
sheetMeasure branch rec' = TaikoMeasure
  -- the stored position is one bar early, see 'TaikoMeasure'
  { start = pos / 98.44 + sheetMeasureLength rec'
  , len   = sheetMeasureLength rec'
  , notes = sheetNotes rec' branch
  } where pos = fromIntegral (runGet getWord16le $ BL.drop 8 rec') :: Double

sheetMeasureLength :: BL.ByteString -> Double
sheetMeasureLength rec' = fromIntegral (BL.index rec' 11) / 60

sheetNotes :: BL.ByteString -> TaikoBranch -> [SheetNote]
sheetNotes rec' branch = do
  let slots = BL.unpack $ BL.take 48 $ BL.drop (0x24 + 48 * fromIntegral (fromEnum branch)) rec'
  (j, v) <- zip [0 ..] slots
  -- rolls are a run of identical bytes rather than a length; they are
  -- emitted here as one note per slot and merged by the caller
  noteType <- case v of
    0x01 -> [NoteDon]
    0x05 -> [NoteDon]
    0x09 -> [NoteDon]
    0x0D -> [NoteKa]
    0x11 -> [NoteKa]
    0x15 -> [NoteBigDon]
    0x16 -> [NoteRoll]
    0x17 -> [NoteBigRoll]
    0x19 -> [NoteBigDon]
    0x1A -> [NoteBigKa]
    0x1B -> [NoteBigDon]
    _    -> []
  return SheetNote
    { noteType = noteType
    , position = j
    , len      = sheetMeasureLength rec' / 48
    , hits     = Nothing
    }

-- * gamedata\/musicinfo.bin

data TaikoSongInfo = TaikoSongInfo
  { songID  :: B.ByteString
  , title   :: T.Text
  , genre   :: Int
  , preview :: Maybe Double -- ^ seconds into the audio
  } deriving (Show)

{-
gamedata/musicinfo.bin: run of counts, then 1 more table offset than count number;
first offset is song table, last is string pool

titles are UTF-8; note, some songs have question marks from misconverted characters:
* "伝説の男　?ビバ・ガッツ?" in Godaime should have wave dashes
* "コイ?クル" in Nanadaime should have an arrow
-}
readMusicInfo :: (MonadFail m) => BL.ByteString -> m [TaikoSongInfo]
readMusicInfo bs = do
  let getW32 :: Int64 -> Word32
      getW32 off = runGet getWord32le $ BL.drop off bs
      getI32 off = fromIntegral (getW32 off) :: Int32
      count = fromIntegral $ getW32 0 :: Int
  -- header is N counts then N+1 offsets, so first offset is 4*(2N+1)
  numCounts <- case [ n | n <- [2, 4, 6, 8 :: Int], getW32 (4 * fromIntegral n) == fromIntegral (4 * (2 * n + 1)) ] of
    n : _ -> return n
    []    -> fail "unrecognized musicinfo.bin header"
  let table = fromIntegral $ getW32 $ 4 * fromIntegral numCounts :: Int64
      strs  = fromIntegral $ getW32 $ 4 * fromIntegral (2 * numCounts) :: Int64
      stride = (fromIntegral (getW32 $ 4 * fromIntegral (numCounts + 1)) - table)
        `div` fromIntegral count
      getStr off = BL.toStrict $ BL.takeWhile (/= 0) $ BL.drop (strs + fromIntegral off) bs
  unless (count > 0 && stride > 8) $ fail "unrecognized musicinfo.bin song table"
  return do
    i <- [0 .. count - 1]
    let base = table + stride * fromIntegral i
        prev = getI32 $ base + 4 * 5
    return TaikoSongInfo
      { songID  = getStr $ getW32 base
      , title   = decodeGeneral $ getStr $ getW32 $ base + 4
      , genre   = fromIntegral $ getI32 $ base + 8
      , preview = do
        guard $ stride >= 24 && prev > 0
        return $ fromIntegral prev / 1000
      }

-- * Song titles for @IMAGE00@ discs

-- song titles for older discs with no text metadata (images used in-game)
taikoImageSongTitles :: Map.Map B.ByteString T.Text
taikoImageSongTitles = Map.fromList
  -- Tatacon de Dodon ga Don
  [ ("CLSN"  , "クラシック　新世界より")
  , ("CLSR"  , "クラシックメドレー (ロック編)")
  , ("CLSU"  , "クラシックメドレー (運動会編)")
  , ("DONDO" , "どんちゃん音頭")
  , ("DORA"  , "ドラえもん音頭")
  , ("DRI"   , "すすめ！ドリラー")
  , ("EKAKI" , "どんちゃんえかき歌")
  , ("FISH"  , "おさかな天国")
  , ("GACHA" , "ガッチャマンの歌")
  , ("HELLO" , "ハロー！どんちゃん")
  , ("KLONO" , "STEPPING WIND 風のクロノア2")
  , ("KOKOO" , "太鼓侍")
  , ("LOVEM" , "LOVEマシーン")
  , ("LUPIN" , "ルパン三世のテーマ '78")
  , ("LVX"   , "ラブリーX")
  , ("MAPPY" , "マッピー音頭")
  , ("MARI"  , "Marionette　-マリオネット-")
  , ("MOJI"  , "もじぴったんメドレー")
  , ("MOON"  , "Mr.Moonlight　～愛のビッグバンド～")
  , ("NAMCO" , "たのしい太鼓道場")
  , ("NAO"   , "風雲！バチお先生")
  , ("NATSU" , "夏祭り")
  , ("OPNG"  , "虹色・夢色・太鼓色")
  , ("POAD"  , "PIECES OF A DREAM")
  , ("SKA"   , "ゴーゴー・キッチン")
  , ("SZE"   , "サザエさん一家")
  , ("TANKO" , "炭坑節")
  , ("TEL"   , "ミニモニ。テレフォン！リンリンリン")
  , ("TRA"   , "traveling")
  , ("TTK"   , "ハム太郎とっとこうた")
  -- Doki! Shinkyoku Darake no Haru Matsuri
  , ("AIAI"  , "アイアイ")
  , ("AMA"   , "亜麻色の髪の乙女")
  , ("ANP"   , "アンパンマンのマーチ")
  , ("BKO"   , "ラブユー☆どんちゃん")
  , ("BUGI"  , "オフ♨ロック")
  , ("CCB"   , "君に、ロマンティック。")
  , ("CLSK"  , "結婚行進曲　「真夏の夜の夢」より")
  , ("CLSW"  , "ウィリアム・テル序曲")
  , ("DAIKU" , "第九交響曲")
  , ("DORA2" , "ドラえもんのうた")
  , ("ENKA"  , "平成太鼓街道　～バチの女～")
  , ("FUNK"  , "サタデー太鼓フィーバー")
  , ("GDM"   , "翔べ！ガンダム")
  , ("GUTS"  , "ガッツだぜ!!")
  , ("HARI"  , "ハリケンジャー参上!")
  , ("HAYA"  , "ドドドドドンだフル！")
  , ("HEAVY" , "地獄の太鼓事典")
  , ("IDOL"  , "エリンギのエクボ")
  , ("JAZZ"  , "Taiko Session　～Live Version～")
  , ("JULY"  , "July 1st")
  , ("MARCH" , "太鼓のマーチ")
  , ("MINI"  , "ミニモニ。ジャンケンぴょん！")
  , ("MOR"   , "ここにいるぜぇ！")
  , ("ODORU" , "おどるポンポコリン")
  , ("OJAMA" , "DANCE!おジャ魔女　おジャ魔女どれみドッカ～ン！")
  , ("SORAN" , "ソーラン節")
  , ("SSG"   , "シーソーゲーム ～勇敢な恋の歌～")
  , ("TAI"   , "大切なもの")
  , ("THEME" , "太鼓ラブ！")
  , ("TOUCH" , "タッチ")
  , ("TRAIN" , "TRAIN-TRAIN")
  , ("XEVI"  , "ゼビウス体操")
  , ("YEAH"  , "Yeah!めっちゃホリディ")
  , ("YPP"   , "太陽もヤッパッパー")
  -- Appare Sandaime
  , ("ABARE" , "爆竜戦隊アバレンジャー")
  , ("AYA1"  , "ね～え？")
  , ("AYA2"  , "GOOD BYE 夏男")
  , ("BLUE"  , "true blue")
  , ("CALIB" , "ソウルキャリバーⅡ　Brave Sword, Braver Soul")
  , ("CHALA" , "CHA-LA HEAD-CHA-LA")
  , ("CLSBU" , "交響曲第5番「運命」")
  , ("CLSCA" , "「カルメン」組曲1番 終曲")
  , ("CLSF"  , "クラシックメドレー ホームパーティー編")
  , ("CLSI"  , "歌劇「アイーダ」より 凱旋行進曲")
  , ("CNA2"  , "MEKADON　～宇宙に消えたメカ太鼓～")
  , ("CS3OP" , "きょうはたいこ曜日")
  , ("DIS2"  , "エレクトリカルパレード　BAROQUE HOEDOWN")
  , ("DONPA" , "ドンパン節")
  , ("DREN"  , "和太鼓戦隊ドンレンジャー")
  , ("GENJI" , "パラダイス銀河")
  , ("GENPE" , "KAGEKIYO　源平討魔伝メドレー")
  , ("GET"   , "Get Wild　シティーハンター")
  , ("INU"   , "いぬのおまわりさん")
  , ("KISHI" , "One Night Carnival")
  , ("KITA"  , "北酒場")
  , ("KUMA"  , "もりのくまさん")
  , ("MANBO" , "おもちでマンボ")
  , ("MDETH" , "メカデス。")
  , ("NANDE" , "なんでだろう　両さんバージョン")
  , ("NEKO"  , "ねこくじら")
  , ("PIECE" , "ヒカリへ")
  , ("PURI2" , "DIAMONDS -ダイアモンド-")
  , ("ROT"   , "さいたま2000")
  , ("SAGA"  , "佐賀県")
  , ("SOUSA" , "Rhythm and Police　踊る大捜査線")
  , ("SQR"   , "スポーツダイジェスドン　～Fill In The Sky～")
  , ("TIGER" , "六甲おろし")
  , ("UFO"   , "UFO")
  , ("VBERA" , "ベラボーマン")
  , ("VGALA" , "Galaga'88")
  , ("VMOMO" , "ワンダーモモ")
  , ("WISH"  , "明日への扉")
  , ("WONDO" , "わっふ～音頭")
  -- Waku Waku Anime Matsuri
  , ("ABA2"  , "We are the ONE～僕らはひとつ～　爆竜戦隊アバレンジャー")
  , ("ANP2"  , "勇気りんりん　それいけ！アンパンマン")
  , ("BAKA"  , "天才バカボン")
  , ("CANDY" , "キャンディ キャンディ")
  , ("CONAN" , "君と約束した優しいあの場所まで　名探偵コナン")
  , ("DORA3" , "ぼくドラえもん　ドラえもん")
  , ("DREN2" , "和太鼓戦隊ドンレンジャー　太鼓の達人 わくわくアニメ祭り")
  , ("FAITH" , "FAITH　ワンピース")
  , ("FUN"   , "Fun!Fun!★ふぁんたじー　わがままフェアリーミルモでポン！ごおるでん")
  , ("GASH"  , "カサブタ　金色のガッシュベル！！")
  , ("GRIP"  , "Grip!　犬夜叉")
  , ("KAME"  , "だまって俺についてこい　こちら葛飾区亀有公園前派出所")
  , ("KINMO" , "さらば　あたしンち")
  , ("NARU"  , "遥か彼方　NARUTO")
  , ("PKMN"  , "アドバンス・アドベンチャー～Advance Adventure～　ポケットモンスターAG")
  , ("SMOON" , "ムーンライト伝説　美少女戦士セーラームーン")
  , ("SZE3"  , "サザエさん")
  , ("UMAN"  , "ウルトラマンの歌　ウルトラマン")
  , ("YAMA"  , "宇宙戦艦ヤマト")
  ]
