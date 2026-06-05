## main console window

console = Onyx Console

homepage = Homepage

version-checking = Checking for updates…
version-new = New version available!
version-latest = You are using the latest version.
version-prerelease = Prerelease version

function-load = Load a Song
function-batch = Batch Recompile
function-quick = Quick Convert/Pack
function-other = Other Tools

explain-header =
  <color=purple>Onyx</color> Music Game Toolkit, version {$version}
  Select an option below to get started.

explain-load =
  bring songs to a new game
  change song speed, reassign instruments
  chart preview tool
explain-batch =
  "Load a song" operations on multiple songs
explain-quick =
  edit songs without changing games
  make packs, or extract from packs
  switch console, e.g. <color=green>360</color> -> <color=cyan>PS3</color>
explain-other =
  midi/audio utilities
  lipsync/dryvox tools

## menu bar (per window on windows/linux, app-wide on mac)

menu-file = &File
menu-file-load = Open Song
menu-file-batch = Batch Recompile
menu-file-quick = Quick Convert
menu-file-other = Tools
menu-file-close = Close Window

menu-edit = &Edit
menu-edit-preferences = Preferences

menu-help = &Help
menu-help-readme = Readme
menu-help-updates = Update history
menu-help-license = License
menu-help-credits = Credits

menu-view = &View
menu-view-console = Show Console

## shared gui widgets

load-files-counter =
  {$count ->
    [one] {$count} file loaded.
   *[other] {$count} files loaded.
  }

instrument-guitar = Guitar
instrument-bass = Bass
instrument-drums = Drums
instrument-keys = Keys
instrument-vocals = Vocals
instrument-other = Other

format-song-artist =
  {$title} ({$artist})

speed-percent = Speed (%)

bass-pedal-1x = 1x Bass Pedal
bass-pedal-2x = 2x Bass Pedal
bass-pedal-both-output = Both
bass-pedal-both-input = 1x+2x Bass Pedal (PS X+ or C3 format)

template = Template

task-cancel = Cancel

song-list-title = Select Songs
song-list-select-all = Select all
song-list-select-none = Select none
song-list-close = Close
song-list-open =
  {$count ->
    [one] Open {$count} song
   *[other] Open {$count} songs
  }

warning-gh2-packs =
  Make sure you combine songs into packs (go to 'Other tools') before playing! Loading more than 16 package files will corrupt your GH2 save.
warning-gh3-cache =
  Make sure you create a GH3 Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs.
warning-ghwor-cache =
  Make sure you create a WoR Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs.
warning-rr-renumber =
  All generated Rock Revolution custom songs have the same ID. To load multiple songs, please use "Quick Convert > RR renumber" to reassign IDs.
warning-rb-quick-convert =
  You appear to be converting {
    $count ->
      [one] a Rock Band song
     *[other] Rock Band songs
  } to RB3 at 100% speed.
  If you don't want to change anything about the {
    $count ->
      [one] song
     *[other] songs
  } except console platform,
  you should use Quick Convert to preserve the {
    $count ->
      [one] song ID
     *[other] song IDs
  } for online compatibility.

game-short-clone-hero = CH
game-short-gh1 = GH1
game-short-gh2 = GH2
game-short-gh3 = GH3
game-short-ghwor = GH:WoR
game-short-rb1 = RB1
game-short-rb2 = RB2
game-short-rb3 = RB3
game-short-rock-revolution = RR

game-clone-hero = Clone Hero
game-gh1 = Guitar Hero 1
game-gh2 = Guitar Hero 2
game-gh3 = Guitar Hero III
game-ghwor = Guitar Hero: Warriors of Rock
game-rb1 = Rock Band 1
game-rb2 = Rock Band 2
game-rb3 = Rock Band 3
game-rock-revolution = Rock Revolution

game-tab-clone-hero = {game-clone-hero}
game-tab-gh1 = {game-short-gh1}
game-tab-gh2 = {game-short-gh2}
game-tab-gh3 = {game-short-gh3}
game-tab-rb1 = {game-short-rb1}
game-tab-rb2 = {game-short-rb2}
game-tab-rb3 = {game-short-rb3}
game-tab-ghwor = {game-short-ghwor}
game-tab-rock-revolution = {game-short-rock-revolution}

label-title = Title
label-artist = Artist
label-album = Album
label-year = Year
label-author = Author
label-genre = Genre
label-subgenre = Subgenre

picker-album-art = Select album art

drums-mode-4lane = 4-Lane Drums
drums-mode-5lane = 5-Lane Drums
drums-mode-pro = Pro Drums
drums-mode-real = Phase Shift Real Drums
drums-mode-elite = Elite Drums

drums-rb3-kit-hard-rock = Hard Rock Kit
drums-rb3-kit-arena = Arena Kit
drums-rb3-kit-vintage = Vintage Kit
drums-rb3-kit-trashy = Trashy Kit
drums-rb3-kit-electronic = Electronic Kit

mode-5fret = 5-Fret
mode-6fret = 6-Fret
mode-pro-keys = Pro Keys
mode-pro-guitar = Pro Guitar
mode-drums = Drums
mode-vocals = Vocals
mode-mania = Mania

gender-female = Female
gender-male = Male
gender-unspecified = Unspecified Gender

vocals-solo = Solo
vocals-harmonies-2 = Harmonies (2)
vocals-harmonies-3 = Harmonies (3)

toggle-ps2-practice-audio = Make practice mode audio for PS2

file-picker-hdr-file = "Select .HDR file"

## batch recompile

batch-tab-songs = Songs
batch-tab-songs-tree-root = Songs
batch-tab-songs-add = Add Song
batch-tab-songs-clear = Clear Songs
batch-tab-songs-import-label = Import:

batch-tab-songs-data-author =
  Author: {$author}
batch-tab-songs-data-format =
  Format: {$format}
batch-tab-songs-data-path =
  Path: {$path}

batch-tab-rb3-all-toms = Convert non-Pro Drums to all toms
  .tooltip = When importing from a FoF/PS/CH chart where no Pro Drums are detected, tom markers will be added over the whole drum chart if this box is checked.
batch-tab-gh-drums-5lane = Pro Drums to 5 lane
batch-tab-gh-drums-4lane = Pro Drums to 4 lane
batch-tab-rr-drums-6pad = Pro Drums to 6 pad
batch-tab-rr-drums-4pad = Pro Drums to 4 pad

batch-configuration-default = Default part configuration
batch-configuration-rb3-fill-guitar-drums =
  Fill empty parts with first guitar, then drums
batch-configuration-rb3-fill-guitar =
  Copy guitar to bass/keys/drums
batch-configuration-rb3-fill-drums =
  Copy drums/dance to guitar/bass/keys
batch-configuration-rb2-guitar-to-bass-empty =
  Copy guitar to bass if empty
batch-configuration-rb2-guitar-to-bass =
  Copy guitar to bass
batch-configuration-rb2-keys-to-guitar-bass-empty =
  Keys on guitar/bass if empty
batch-configuration-rb2-keys-to-guitar =
  Keys on guitar
batch-configuration-rb2-keys-to-bass =
  Keys on bass
batch-configuration-rb2-drums-to-guitar-bass-empty =
  Copy drums/dance to guitar/bass if empty
batch-configuration-rb2-drums-to-guitar-bass =
  Copy drums/dance to guitar/bass
batch-configuration-ch-drums-to-guitar-empty =
  Copy drums/dance to guitar if empty
batch-configuration-ch-drums-to-guitar =
  Copy drums/dance to guitar
batch-configuration-ch-drums-to-rhythm-empty =
  Copy drums/dance to rhythm if empty
batch-configuration-ch-drums-to-rhythm =
  Copy drums/dance to rhythm
batch-configuration-ch-guitar-drums-fill =
  Copy guitar and drums to each other if one is missing

batch-task-360-con = Create Xbox 360 CON files
batch-task-ps3-pkg = Create PS3 PKG files
batch-task-rpcs3-pkg = Create RPCS3 PKG files
batch-task-ps3-folder = Create PS3/RPCS3 folders
batch-task-magma = Create Magma projects
batch-task-ch-folder = Create CH folders
batch-task-ch-sng = Create CH .sng files
batch-task-ch-zip = Create CH zips
batch-task-ps2-diy = Create PS2 DIY folders
batch-task-ps2-add = Add to PS2 ARK as Bonus Songs
batch-task-360-live = Create Xbox 360 LIVE files
batch-task-preview = Build web previews

## quick convert

quick-convert-tab-rb = RB quick convert + pack creator
quick-convert-tab-ch = CH quick convert
quick-convert-tab-rr-renumber = RR renumber
quick-convert-tab-gh2-pack = GH2 pack creator
quick-convert-tab-rb-legacy-pkg = RB legacy CON->PKG

quick-convert-ch-chart-to-midi = .chart to MIDI
  .tooltip = Converts all .chart files to .mid.
quick-convert-ch-ps-tap-open = PS tap/open format
  .tooltip = Encodes five-fret tap and open notes using the older Phase Shift format (System Exclusive messages). This will pull back open extended sustains to not overlap other notes, and remove open notes in chords.
quick-convert-ch-audio-ogg = Audio to .ogg
  .tooltip = Reencode audio files as Ogg Vorbis if they are not already.
quick-convert-ch-audio-opus = Audio to .opus
  .tooltip = Reencode audio file as Opus if they are not already.
quick-convert-ch-real-drums-to-elite = Real Drums to Elite
  .tooltip = If a song has Phase Shift Real Drums charted, adds a conversion to Elite Drums.

## misc tools

misc-tab-midi-stuff = MIDI stuff
misc-tab-mogg-vgs-fsb = MOGG/VGS/FSB
misc-tab-lipsync = Lipsync
misc-tab-dry-vox = Dry vox
misc-tab-milo = .milo
misc-tab-rb3-cache = RB3 cache
misc-tab-gh3-cache = GH3 cache
misc-tab-ghwor-cache = GH:WoR cache
misc-tab-con-to-pkg = CON to PKG

misc-button-extract-milo = Extract .milo to folder
misc-button-repack-milo = Repack .milo from folder
misc-button-gh3-cache-360 = Create GH3 cache (360)
misc-button-gh3-cache-ps3 = Create GH3 cache (PS3)
misc-button-gh3-cache-install-rpcs3 = Install GH3 cache (RPCS3)
misc-button-ghwor-cache-360 = Create GH:WoR cache (360)
misc-button-ghwor-cache-ps3 = Create GH:WoR cache (PS3)
misc-button-ghwor-cache-install-rpcs3 = Install GH:WoR cache (RPCS3)
misc-button-con-to-pkg-rb2 = Convert to PKG (RB2)
misc-button-con-to-pkg-rb3 = Convert to PKG (RB3)

misc-task-make-gh3-cache = Make GH3 cache file
misc-task-install-gh3-cache = Install GH3 cache file
misc-task-make-ghwor-cache = Make GH:WoR cache file
misc-task-install-ghwor-cache = Install GH:WoR cache file
