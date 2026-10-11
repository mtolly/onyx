## numeric formatting (temporary, will be replaced with locale-based system)

# used in e.g. ogg vorbis quality
decimal-separator = .
# used in e.g. score cutoffs
thousands-separator = ,

## main console window

console = Onyx Console

homepage = Homepage

version-checking = Checking for updates…
version-new = New version available!
version-latest = You are using the latest version.
version-prerelease = Prerelease version

function-load = Load a song
function-batch = Batch recompile
function-quick = Quick convert/pack
function-other = Other tools

welcome-header =
  <color=magenta>Onyx</color> Music Game Toolkit, version {$version}
  Select an option below to get started.

welcome-load =
  bring songs to a new game
  change song speed, reassign instruments
  chart preview tool
welcome-batch =
  "Load a song" operations on multiple songs
welcome-quick =
  edit songs without changing games
  make packs, or extract from packs
  switch console, e.g. <color=green>360</color> -> <color=cyan>PS3</color>
welcome-other =
  midi/audio utilities
  lipsync/dryvox tools

language-restart = Restart Onyx to load the new selected language.

## menu bar

menu-file = &File
menu-file-load = {function-load}
menu-file-batch = {function-batch}
menu-file-quick = {function-quick}
menu-file-other = {function-other}
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

## windows and tabs

window-quick-convert = Quick Convert
window-tools = Tools
window-preferences = Onyx Preferences

tab-metadata = Metadata
tab-instruments = Instruments
tab-preview = Preview
tab-stars = Stars
tab-utilities = Utilities
tab-task = Task
song-tab-ch-ps = CH/PS

pref-tab-general = General
pref-tab-rock-band = Rock Band
pref-tab-guitar-hero = Guitar Hero
pref-tab-clone-hero = Clone Hero
pref-tab-360 = 360
pref-tab-ps3 = PS3
pref-tab-wii = Wii
pref-tab-3d-preview = 3D Preview
pref-tab-audio = Audio

## shared gui widgets

load-files-counter =
  {$count ->
    [one] {$count} file loaded.
   *[other] {$count} files loaded.
  }

task-show-files = Show created files
task-cancelled = Cancelled tasks.
task-status = {$done}/{$total} tasks complete: {$succeeded} succeeded, {$failed} failed
task-number = Task {$number}
task-created-files = Created files:

instrument-guitar = Guitar
instrument-bass = Bass
instrument-drums = Drums
instrument-keys = Keys
instrument-vocals = Vocals
instrument-other = Other

speed-percent = Speed (%)
speed-percent-tooltip =
  Change the speed of the chart and its audio (without changing pitch).

bass-pedal-1x = 1x Bass Pedal
bass-pedal-2x = 2x Bass Pedal
bass-pedal-both-output = Both
bass-pedal-both-input = 1x+2x Bass Pedal (PS X+ or C3 format)

template = Template

task-cancel = Cancel

custom-title-suffix = Custom Title Suffix
specific-song-id = Specific Song ID
part-none = (none)

gh2-deluxe-features = GH2 Deluxe features
gh2-deluxe-1x-drums = 1x drums
gh2-deluxe-2x-drums = 2x drums

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
warning-gh2-packs-long =
  Note! When loading songs into Guitar Hero II for Xbox 360, you *must* combine them into packs (go to "Quick convert").
  Loading more than 16 packages will fail to load some songs, and will corrupt your save!
warning-gh3-cache =
  Make sure you create a GH3 Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs.
warning-ghwor-cache =
  Make sure you create a WoR Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs.
warning-ghwor-long =
  Please use the "WoR Song Cache" tab in Other Tools after conversion,
  to produce the extra file needed to load your songs.

  IMPORTANT: There may be an issue with loading too many custom songs
  into Guitar Hero: Warriors of Rock, that could corrupt your save.
  Please back up any save data you care about before loading custom songs!
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
warning-confirm = OK

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

file-picker-create-diy-folder = Create DIY folder
file-picker-create-folder = Create folder
file-picker-load-album-art = Select album art
file-picker-load-dryvox-source-audio = Load song or vocals audio
file-picker-load-hdr = Select .HDR file
file-picker-load-midi = Load MIDI file
file-picker-load-milo = Load .milo
file-picker-load-song = Load song
file-picker-save-ch-folder = Save CH/PS song folder
file-picker-save-ch-sng = Save CH/PS .sng file
file-picker-save-ch-zip = Save CH/PS zip file
file-picker-save-gh2-live = Save GH2 LIVE file
file-picker-save-gh3-cache-360 = Save GH3 cache file (360)
file-picker-save-gh3-cache-ps3 = Save GH3 cache file (PS3)
file-picker-save-gh3-live = Save GH3 LIVE file
file-picker-save-gh3-pkg = Save GH3 PKG file
file-picker-save-ghwor-live = Save GH:WoR LIVE file
file-picker-save-ghwor-pkg = Save GH:WoR PKG file
file-picker-save-lipsync-audio = Save lipsync audio
file-picker-save-magma-v2 = Save Magma v2 project
file-picker-save-midi = Save MIDI file
file-picker-save-milo = Save .milo
file-picker-save-multitrack-audio = Save .{$extension} file
file-picker-save-rb2-con = Save RB2 CON file
file-picker-save-rb2-pkg = Save RB2 PKG file
file-picker-save-rb3-con = Save RB3 CON file
file-picker-save-rb3-pkg = Save RB3 PKG file
file-picker-save-reaper = Save REAPER project
file-picker-save-reduced-midi = Save reduced MIDI file
file-picker-save-rr-live = Save RR LIVE file
file-picker-save-rr-pkg = Save RR PKG file
file-picker-save-voc = Save .voc file
file-picker-save-web-preview = Save web preview folder
file-picker-save-wor-cache-360 = Save WoR cache file (360)
file-picker-save-wor-cache-ps3 = Save WoR cache file (PS3)
file-picker-select-folder = Select folder

file-type-package =
  .tree-root = Packages
  .add = Add package
  .load = Load package
  .clear = Clear packages
file-type-rb-con-live =
  .tree-root = Rock Band CON/LIVE
  .add = Add Rock Band CON/LIVE
  .load = Load Rock Band CON/LIVE
  .clear = Clear Rock Band CON/LIVE
file-type-con-live =
  .tree-root = CON/LIVE
  .add = Add CON/LIVE
  .load = Load CON/LIVE
  .clear = Clear CON/LIVE
file-type-rb-song =
  .tree-root = Rock Band songs
  .add = Add Rock Band song
  .load = Load Rock Band song
  .clear = Clear Rock Band songs
file-type-rr-song =
  .tree-root = RR songs
  .add = Add RR song
  .load = Load RR song
  .clear = Clear RR songs
file-type-ch-song =
  .tree-root = CH songs
  .add = Add CH song
  .load = Load CH song
  .clear = Clear CH songs
file-type-audio =
  .tree-root = Audio
  .add = Add audio
  .load = Load audio
  .clear = Clear audio
file-type-song =
  .tree-root = Songs
  .add = Add song
  .load = Load song
  .clear = Clear songs

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

language-english = English
language-french = French
language-italian = Italian
language-spanish = Spanish
language-german = German
language-japanese = Japanese

vocals-solo = Solo
vocals-harmonies-2 = Harmonies (2)
vocals-harmonies-3 = Harmonies (3)

toggle-ps2-practice-audio = Make practice mode audio for PS2

template-runner-tooltip =
  Template for where to create new files.
    %input_dir% - folder containing the input
    %input_base% - input filename by itself, extension removed
    %modifiers% - added distinguishing features e.g. speed modifier
    %title% - title from song's metadata (including modifiers)
    %artist% - artist from song's metadata
    %album% - album from song's metadata
    %author% - author from song's metadata
    %song_id% - unique song ID

terminal-task-start = <color=cyan>{$task-name}</color>: {$task-path}
terminal-task-warning = <color=yellow>Warning</color>: {$message}
terminal-task-error = <color=red>ERROR!</color> {$message}
terminal-task-success = <color=green>Success!</color> {$message}

## batch recompile

batch-tab-songs = Songs
batch-tab-songs-import-label = Import:

batch-tab-song-entry =
  {$has-title ->
    [false] Untitled
   *[true] {$title}
  }{$has-artist ->
    [false] {""}
   *[true] {" "}({$artist})
  }{$is-2x ->
    [true] {" (2x)"}
   *[false] {""}
  }
batch-tab-songs-data-author =
  Author: {$author}
batch-tab-songs-data-format =
  Format: {$format}
batch-tab-songs-data-path =
  Path: {$path}{
    $index ->
      [none] {""}
     *[some] {" "}(#{$index})
  }

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

## single song view

song-toggle-2x-bass-pedal = 2x Bass Pedal drums

part-guitar = Guitar
part-bass = Bass
part-keys = Keys
part-drums = Drums
part-vocal = Vocal
part-rhythm = Rhythm
part-guitar-coop = Guitar Coop
part-vocal-animation = Vocal (animation)

coop-bass = Coop: Bass
coop-rhythm = Coop: Rhythm

numberbox-gh5-song-id = Custom Song ID (dlc)
numberbox-gh5-cdl = Custom Package ID (cdl)
numberbox-rr-song-id = Custom Song ID
numberbox-rr-leaderboard-id = Custom Leaderboard ID

create-360-con = Create Xbox 360 CON file
create-ps3-pkg = Create PS3 PKG file
create-magma = Create Magma project
create-ps3-folder = Create PS3/RPCS3 folder
create-360-live = Create Xbox 360 LIVE file
create-rpcs3-pkg = Create RPCS3 PKG file
create-ch-folder = Create CH/PS song folder
create-ch-sng = Create CH .sng file
create-ch-zip = Create CH/PS zip file
create-ps2-add-bonus = Add to PS2 ARK as Bonus Song
create-ps2-diy = Create PS2 DIY folder

stars-rb3-title = Rock Band 3 Star Cutoffs
stars-gh-title = Guitar Hero 1/2 Star Cutoffs
stars-lead = Lead
stars-coop = Coop
stars-1 = 1*: {$score}
stars-2 = 2*: {$score}
stars-3 = 3*: {$score}
stars-4 = 4*: {$score}
stars-5 = 5*: {$score}
stars-gold = G*: {$score}
stars-gh-base = Base: {$score}
stars-gh-4 = 4*: {$score}
stars-gh2-5 = 5* (GH2): {$score}
stars-gh1-5 = 5* (GH1): {$score}

diff-easy = (E)
diff-medium = (M)
diff-hard = (H)
diff-expert = (X)

meta-convert = Convert
meta-rhythm-keys = Rhythm Keys
meta-rhythm-bass = Rhythm Bass
meta-auto-emh = Auto EMH
meta-expert-only = Expert Only
meta-cover = Cover
label-track-number = #
default-song-title = Song

tier = Tier {$number}
rank = Rank

audio-plan-select = Select an audio plan
loading-image = Loading image: {$path}

preview-view-tracks = View Tracks
preview-audio = Audio
preview-background = Background
preview-bg-none = None

build-rb3-con = Building RB3 CON (360)
build-rb3-pkg = Building RB3 PKG (PS3)
build-magma = Building Magma project
build-rb3-ps3 = Building RB3 PS3 folders
build-rb2-con = Building RB2 CON (360)
build-rb2-pkg = Building RB2 PKG (PS3)
build-rb2-ps3 = Building RB2 PS3 folders
build-ch-folder = Building CH/PS song folder
build-ch-zip = Building CH/PS zip file
build-ch-sng = Building CH/PS .sng file
build-gh1-ark = Adding to GH1 ARK file
build-gh1-diy = Creating GH1 DIY folder (PS2)
build-gh2-live = Building GH2 LIVE file
build-gh2-ark = Adding to GH2 ARK file
build-gh2-diy = Creating GH2 DIY folder (PS2)
build-gh3-live = Building GH3 LIVE file
build-gh3-pkg = Building GH3 PKG file
build-gh3-ps3 = Building GH3 PS3 folders
build-ghwor-live = Building GH:WoR LIVE file
build-ghwor-pkg = Building GH:WoR PKG file
build-ghwor-ps3 = Building GH:WoR PS3 folders
build-rr-live = Building RR LIVE file
build-rr-pkg = Building RR PKG file

## quick convert

quick-convert-tab-rb = RB quick convert + pack creator
quick-convert-tab-ch = CH quick convert
quick-convert-tab-rr-renumber = RR renumber
quick-convert-tab-gh2-pack = GH2 pack creator
quick-convert-tab-rb-legacy-pkg = RB legacy CON->PKG

quick-convert-limit-easy-medium-frets = Limit Easy/Medium frets
  .tooltip = For all five-fret tracks, ensures Easy and Medium difficulties only use 3 and 4 frets respectively.

quick-convert-rb-black-venue = Black VENUE
  .tooltip = Replaces the VENUE track with one that tries to provide as close to a black background as possible. Currently works on pre-RB3 songs, and songs in RBN2 format; does not work with RB3 songs using .milo venue format.
quick-convert-rb-no-overdrive = No Overdrive
  .tooltip = Removes overdrive phrases from all instruments.
quick-convert-rb-no-lanes-gbk = No lanes (G/B/K)
  .tooltip = Removes trill and tremolo lanes from Guitar/Bass/Keys and their Pro versions. Does not remove glissandos from Pro Keys.
quick-convert-rb-no-lanes-drums = No lanes (drums)
  .tooltip = Removes one-pad and two-pad rolls/swells from Drums.
quick-convert-rb-no-drum-fills = No drum fills
  .tooltip = Removes all activation fills from Drums. Does not remove a Big Rock Ending. This function is mostly useful for recording videos.
quick-convert-rb-force-22-protar = Force 22-fret protar
  .tooltip = For Pro Guitar/Bass with separate Mustang and Squier versions, removes the Mustang version, so all controllers will play the Squier version. This function is mostly useful for recording videos.
quick-convert-rb-unmute-22-protar = Unmute >22-fret protar
  .tooltip = Any muted notes over fret 22 on Pro Guitar/Bass are unmuted. This allows you to pass them through Magma by muting, and then unmute them to record videos. Note, such notes cannot be hit by normal input.
quick-convert-rb-decompress-milo = Decompress .milo_*
  .tooltip = Converts any .milo_xbox, etc. files to uncompressed form. ForgeTool may require this step to read the files for RB4 conversion.
quick-convert-rb-author-to-tag = Author to DTA tag
  .tooltip = Copies author information from the C3 Magma comment format to the RB2/RB3 Deluxe game-readable format.
quick-convert-rb-strip-title-tags = Strip title tags
  .tooltip = Removes the following tags from song titles: (2x Bass Pedal), and author signatures such as (Z) (X) (O) etc.
quick-convert-rb-note-shuffle = Note shuffle
  .tooltip = Shuffles notes on 5-fret and drum parts.
quick-convert-rb-decrypt-moggs = Decrypt MOGGs
  .tooltip = Decrypt all MOGG files. Note that unencrypted MOGGs will not work on PS3 RB3 without RB3Enhanced.

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

quick-convert-mode-in-place = Transform in place: replace input files with new versions
quick-convert-mode-one-to-one = One to one: each input file produces a new output file
quick-convert-mode-make-packs = Make packs: combine songs up to a maximum file size
quick-convert-mode-make-songs = Make songs: produce a single file for each song
quick-convert-mode-make-dolphin = Make Dolphin (Wii) pack

quick-convert-format-con = CON (360)
quick-convert-format-live = LIVE (360)
quick-convert-format-pkg = PKG (PS3)
quick-convert-format-folders = Folders (PS3)

quick-convert-encrypt-edat = Encrypt .mid.edat
quick-convert-for-ps3-files = For PS3 files:

quick-convert-folder-keep = Keep original USRDIR subfolders
quick-convert-folder-keep-any = Keep original USRDIR subfolders (if any)
quick-convert-folder-combine = Combine into one new USRDIR subfolder
quick-convert-folder-combine-per-file = Combine into one new USRDIR subfolder per file
quick-convert-folder-combine-per-pack = Combine into one new USRDIR subfolder per pack
quick-convert-folder-each = Each song gets a new USRDIR subfolder
quick-convert-folder-custom-initial = Custom USRDIR subfolder...
quick-convert-folder-custom = Custom folder: {$folder}
quick-convert-folder-custom-prompt = Enter custom USRDIR subfolder for your pack. (A number will be appended if creating more than one pack.)

quick-convert-max-pack-size = Max Pack Size (MiB)
quick-convert-invalid-pack-size = Invalid pack size
quick-convert-no-songs-added = No songs added
quick-convert-make-packs-count =
  {$count ->
    [one] Make {$count} pack
   *[other] Make {$count} packs
  }
quick-convert-unsupported-format = Unsupported format: transform in place supports CON/LIVE/PKG
quick-convert-empty = (empty)
quick-convert-songs-incl = {$count} songs incl. {$title}
quick-convert-save-pack-file = Save pack file
quick-convert-save-pack-template = Select template for packs (a number will be added for each pack)
quick-convert-pack-number = Pack #{$number}

quick-convert-fof-folder = folder format
quick-convert-fof-sng = .sng format
quick-convert-fof-encore = Encore format
quick-convert-mode-to-folders = To folders: save each song as a folder
quick-convert-mode-to-sng = To SNG: save each song as an .sng file

quick-convert-ch-warning-base = This will modify the selected songs, and the old versions will be deleted.
quick-convert-ch-warning-encore =
  WARNING: This will convert Encore-format songs into CH-compatible ones, removing the original files.
  Select a different mode if you want to keep them.
quick-convert-ch-ops-header = Based on the filename template, these operations will be performed:
quick-convert-ch-op-overwrite =
  {$songs ->
    [one] - {$songs} song will be overwritten with a new version
   *[other] - {$songs} songs will be overwritten with new versions
  }
quick-convert-ch-op-convert =
  {$songs ->
    [one] - {$songs} song will be converted to a new version
   *[other] - {$songs} songs will be converted to new versions
  }
quick-convert-ch-op-convert-delete =
  {$songs ->
    [one] - {$songs} song will be converted to a new version, and the old version will be deleted
   *[other] - {$songs} songs will be converted to new versions, and the old versions will be deleted
  }
quick-convert-ch-ops-encore = WARNING: Some Encore-format songs will be removed/replaced by CH-compatible ones

quick-convert-delete-originals = Delete original files
quick-convert-dolphin-preview-audio = Try to generate preview audio
quick-convert-task-make-dolphin = Make Dolphin pack

task-legacy-con-to-pkg-rb2 = Convert to PKG (RB2)
task-legacy-con-to-pkg-rb3 = Convert to PKG (RB3)

pack-game-from-first = Game (from first in list):
pack-game = Game:
pack-save-stfs = Save STFS file
pack-name = Pack name
pack-description = Pack description
pack-make-con = Make CON pack (RB3/RB2)
pack-make-live = Make LIVE pack (all other games)
pack-make-extracted = Make extracted folder
pack-save-as = Save pack as…
pack-task-extracted = Save pack as extracted contents: {$path}
pack-task-con = Save pack as CON: {$path}
pack-task-live = Save pack as LIVE: {$path}

button-select-folder = Select folder…

song-count =
  {$count ->
    [one] {$count} song
   *[other] {$count} songs
  }

## misc tools

misc-tab-midi-stuff = MIDI stuff
misc-tab-mogg-vgs-fsb = MOGG/VGS/FSB
misc-tab-lipsync = Lipsync
misc-tab-dry-vox = Dry vox
misc-tab-milo = .milo
misc-tab-rb3-cache = RB3 cache
misc-tab-gh3-cache = GH3 cache
misc-tab-ghwor-cache = GH:WoR cache

misc-button-extract-milo = Extract .milo to folder
misc-button-repack-milo = Repack .milo from folder
misc-button-gh3-cache-360 = Create GH3 cache (360)
misc-button-gh3-cache-ps3 = Create GH3 cache (PS3)
misc-button-gh3-cache-install-rpcs3 = Install GH3 cache (RPCS3)
misc-button-ghwor-cache-360 = Create GH:WoR cache (360)
misc-button-ghwor-cache-ps3 = Create GH:WoR cache (PS3)
misc-button-ghwor-cache-install-rpcs3 = Install GH:WoR cache (RPCS3)

misc-task-make-gh3-cache = Make GH3 cache file
misc-task-install-gh3-cache = Install GH3 cache file
misc-task-make-ghwor-cache = Make GH:WoR cache file
misc-task-install-ghwor-cache = Install GH:WoR cache file

label-midi-or-chart = MIDI or .chart
midi-load-dialog = Load MIDI or .chart file
midi-button-reduce = Fill in lower difficulties and drum animations
midi-task-reduce = Reduce MIDI: {$path}
midi-button-prokeys = Find hanging Pro Keys notes
midi-task-prokeys = Pro Keys range check: {$path}
midi-button-reaper = Make REAPER project with RB template
midi-task-reaper = Make REAPER project: {$path}
midi-button-chart-to-midi = Clean convert .chart to MIDI
midi-task-chart-to-midi = Convert to MIDI: {$path}
midi-button-batch = Batch
midi-task-chart-batch = Convert .chart to MIDI (batch): {$path}

label-con-folder = CON folder
label-song-cache = Song cache
cache-load-dialog = Load RB3 song cache
cache-button-apply = Apply numeric IDs from song cache
cache-task = Hardcode song cache: {$path}

util-button-preview = Build web preview
util-button-reduce = Produce MIDI with automatic reductions
util-save-midi-dialog = Save MIDI file
util-task-prokeys = Pro Keys range check

mogg-button-ogg = Ogg to MOGG, no re-encode ({
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  })
mogg-button-combine = Combine into MOGG ({
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  })
mogg-button-vgs = Combine into VGS ({
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  })
mogg-button-fsb3 = Make FSB3-XMA1 ({
  $streams ->
    [one] {$streams} stream
   *[other] {$streams} streams
  }, {
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  } total)
mogg-button-fsb4 = Make FSB4-XMA2 ({
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  })
mogg-task-ogg = Ogg to MOGG
mogg-task-mogg = MOGG file creation
mogg-task-vgs = VGS file creation
mogg-task-fsb3 = FSB3 file creation
mogg-task-fsb4 = FSB4 file creation
audio-format-channels =
  {$format}, {$channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  }
audio-duration =
  {$duration} ({$frames ->
    [one] {$frames} frame
   *[other] {$frames} frames
  } @ {$rate} Hz)

label-midi-file = MIDI file
label-audio-file = Audio file
label-transition-ms = Transition (ms)

dryvox-blank = (blank)

misc-button-make-voc = Make GH2 .voc from PART VOCALS
misc-button-make-milo = Make RB3 .milo_*
  .tooltip = Create a RB3 .milo_* file from the first present out of: LIPSYNC* tracks, HARM* tracks, or PART VOCALS.
misc-button-update-milo = Update RB3/TBRB .milo_*
  .tooltip = Update a RB3 .milo_* file from LIPSYNC* tracks, HARM* tracks, or PART VOCALS. Or, update a TBRB .milo_* file from LIPSYNC_[beatle] tracks.
misc-button-create-lipsync = Create *.lipsync
  .tooltip = Export all LIPSYNC* and LIPSYNC_[beatle] tracks to .lipsync files, ready to be inserted into .milo_* files.
misc-button-vocal-lipsync-rb3 = Turn vocal tracks into LIPSYNC* tracks for RB3
  .tooltip = Uses HARM* or PART VOCALS to produce LIPSYNC{"{"}1-3{"}"} tracks containing viseme information, using the standard RB viseme set.
misc-button-vocal-lipsync-tbrb = Turn vocal tracks into LIPSYNC* tracks for TBRB
  .tooltip = Uses HARM* or PART VOCALS to produce LIPSYNC{"{"}1-3{"}"} tracks containing viseme information, using the Beatles RB viseme set.
misc-button-dryvox-sine = Make sine wave dry vox from MIDI
misc-button-dryvox-clipped = Make clipped dry vox from MIDI and audio

misc-task-extract-milo = Extract {$path}
misc-task-repack-milo = Repack {$path}
misc-task-make-voc = Make GH2 .voc: {$path}
misc-task-make-milo = Create .milo from MIDI lipsync: {$path}
misc-task-update-milo = Update .milo with MIDI lipsync: {$path}
misc-task-export-lipsync = Export .lipsync to folder
misc-task-vocal-to-lipsync = Convert vocal tracks to LIPSYNC tracks: {$path}
misc-task-dryvox = {$name}: {$path}

## preferences

button-save = Save
button-start = Start

pref-unlimited-cores = Unlimited cores
pref-core-limit = Core limit
pref-detect-muted = For non-open-note games, convert "muted strums" to Harmonix style
  .tooltip = When checked, sequences of the form "chord, strummed open notes, strummed chord" are assumed to represent muted strums. Instead of always becoming green, these open notes will become the lowest note of the following chord, or of the preceding chord if it is closer to the open notes.
pref-limit-reduced-frets = When generating lower difficulty 5-fret tracks, limit Medium to 4 frets and Easy to 3 frets

pref-magma-require = Magma required
pref-magma-try = Magma optional
pref-magma-disable = Don't use Magma
pref-black-venue = Always export black VENUE track
pref-label-2x = Label 2x kick charts as (2x Bass Pedal) by default
pref-legal-tempos = Modify tempos in RB export to follow Magma rules
  .tooltip = Magma requires MIDI files to use tempos in the range of 40 to 300 BPM. This is a more restricted range than the games actually require.
pref-rb3-encoding-label = RB3 recompile .dta
pref-pack-encoding-label = RB3 pack .dta
encoding-latin1 = Latin-1
encoding-utf8 = UTF-8
encoding-as-is = As is

pref-gh2-offset = GH2 Audio Offset (ms)
  .tooltip = Adjust audio offset only for Guitar Hero II output, intended to compensate for emulator delay. Positive values mean audio will be pulled earlier, to account for delay. Negative values mean audio will be pushed later.
pref-sort-gh2 = Sort GH2 bonus songs when adding to .ARK
pref-artist-sort = For GH2 and GH3, sort by artist first (instead of title)
  .tooltip = This affects GH2 .ARK bonus songs (when the above box is checked) and GH3 DLC (as sorted in the Song Cache file).

pref-dir-ch = Default CH folder
pref-ch-downmix = Mix down stems in CH output
pref-ch-audio-ogg = Output Ogg Vorbis audio
pref-ch-audio-opus = Output Opus audio

pref-dir-rb = Default CON/LIVE folder
pref-trim-xbox = Ensure valid filenames for Xbox 360 USB or HDD drives
  .tooltip = When checked, the filenames of CON or LIVE files will be trimmed to a max of 42 characters, and will also have plus and comma characters replaced (a restriction for files on a 360 hard drive, but not USB).
pref-rb-number-id = Use true number IDs instead of symbols on Xbox 360 RB files
  .tooltip = When checked, Rock Band files will use a random integer for song_id. Otherwise, an 'o' is prefixed to form a symbol, and the game will generate a random ID. A true integer is required for some contexts such as RGH online play and PS3 conversion, and also maintains scores even if the song cache is regenerated.

pref-ps3-encrypt = Encrypt .mid.edat files for PS3
  .tooltip = When checked, .mid.edat files for PS3 customs will be encrypted. This is required for a real console, but optional (and can be less convenient) when playing on emulator.
pref-dir-ps3 = PS3/RPCS3 install folder

pref-dir-wii = Default Wii .app folder

pref-msaa-none = No MSAA
pref-msaa-2x = MSAA 2x
pref-msaa-4x = MSAA 4x
pref-msaa-8x = MSAA 8x
pref-msaa-16x = MSAA 16x
pref-fxaa = FXAA
pref-fps = Frames per second
pref-elite-hihat-default = Elite drums hihat side: default
pref-elite-hihat-cross = Elite drums hihat side: cross hand
pref-elite-hihat-open = Elite drums hihat side: open hand
pref-elite-ride-default = Elite drums ride side: default
pref-elite-ride-far = Elite drums ride side: far crash
pref-elite-ride-near = Elite drums ride side: near crash
pref-waveform = Audio waveform
pref-lefty-flip = Lefty flip

pref-ogg-quality = OGG Vorbis quality
pref-decrypt-silent = Treat undecryptable MOGGs as silent instead of an error

pref-magma-warning =
  Warning! Disabling Magma can result in Rock Band files that crash the game,
  as Onyx does not yet check all of the error cases that Magma does.
  Please test any charts thoroughly before distributing to others!
