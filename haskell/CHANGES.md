# Version history

## 20230402

* Fix an error when converting StepMania to Rock Band due to difficulty tier
* Import lyrics from Rocksmith files as an unpitched vocal track
* Basic support of Unicode lyrics/sections (#200)
  * Imported MIDI files can have text events in either Latin-1 or UTF-8
  * Exported MIDI files use Latin-1 for Harmonix games, UTF-8 for Clone Hero

## 20230331

* Fix GH:WoR song cache creator overwriting the setlist order of Smash Hits and
  Van Halen exports (#235), thanks to [AddyMills](https://www.addymills.com/)
* Fix audio and chart becoming desynced when converting some songs with XMA
  audio (360 Rock Revolution, Neversoft GH, Power Gig) to Rock Band (#234)
* Fix PS3 .pkg files sometimes breaking game updates by overwriting existing
  game version info and banner images
  * Existing RB packages with the issue can be fixed by running
    through Quick Convert
* Add preliminary support for importing from Clone Hero `.sng` format
* Several new game formats supported for import
  * Rocksmith 2014 (any platform `.psarc`)
    * Converts to RB Pro Guitar, autocharts to 5-fret
  * BMS (`.bms`, `.bme`, `.bml`, `.pms`)
    * Converts to 5-fret, drums, or Pro Keys
  * osu! (`.osz`)
    * Import osu!mania charts, converts to 5-fret, drums, or Pro Keys
    * Import osu!taiko charts, converts to 4-lane drums with audio samples
  * StepMania (`.sm`, `.dwi`)
    * Converts to Phase Shift dance format, or translates to 5-fret or drums
    * Supports stops/freezes (translates to extra MIDI time), but not yet warps

Special thanks to
[EFHIII's .midi to Clone Hero converter](https://github.com/EFHIII/midi-ch)
for the logic used in several new conversion steps.

  * Pro Guitar/Rocksmith and Pro Keys perform pitch-based conversion to 5-fret
  * Also used for lane reduction such as higher-key BMS and osu!mania to 5-fret

## 20230220

* Fix Rock Band 2 output to work around a crash in Lego Rock Band
  due to empty lipsync files
* Fix generating GH2 Deluxe (360) files with no drums,
  where an empty channel list allowed drums to be selected and crash the game
* Fix GH:WoR cache file generation not handling older game data correctly
* When updating lipsync in milo files, an empty or absent track will now replace
  the corresponding lipsync file with an empty one, instead of leaving it as-is
* Added back a button to export lipsync files directly
  instead of into a milo file
* Added a command line build for Windows, which only includes the command line
  interface but is compiled as a console executable, allowing it to be used
  from all shells
  * CLI is usable on the normal Linux and Mac builds
  * CLI has been cleaned up somewhat,
    more improvements/additions coming in later updates

## 20230130

* Additions for Guitar Hero II Deluxe
  * Import and export new supported metadata
    (album, year, source, genre, difficulty, duration)
  * Export album art
    * GH2 Pack Creator then extracts the art into a single folder
      for console installation
  * Drum charts exported for Xbox 360
  * Import drum charts
  * Export solo markers, and (for future support) Strum/HOPO/Tap markers
* Preserve imported HOPO threshold when exporting to GH1/GH2
* Fix loading from GH3/GHA discs (broke in `20221230`) (#225)
* Better implementation of chord snapping, closer to official games
  + should be less likely to apply when it shouldn't
* Preference to disable RB3/RB2 "Magma-legal tempos" hacks
* Import long note markers from DTX format
* New manual in web format

## 20221230

* Improvements to the open note removal algorithm
  * Better preservation of movement for patterns of fretted notes
    with open notes in between
  * Detect open notes used as muted strums in between chords,
    and move them to the low fret of an adjacent chord
    * Can be disabled in Preferences
* Guitar Hero III export fixes
  * Fix some GH3 DLC files not loading into the cache tool,
    particularly for PS3 (#216)
  * Remove default year label of 1960
  * HOPOs on same fret as previous note are turned into strums
    due to not working as HOPOs
  * Delay song start if first note is too early
* Fix several more situations where temporary folders could remain after use
* Fix loading CH `song.ini` with comments, as created by C3 CON Tools'
  Phase Shift converter (#224)
* Fix an issue with importing vocals from Power Gig format
  that affected one song (#220)
* Reorder RB MOGG channels to be more compatible with some
  C3 CON Tools utilities (#217)
* Include Overdrive phrases on lower Pro Keys difficulties
  for Phase Shift compatibility (#211)
* Fix `GUITAR COOP` not being imported from `.chart` files
* DTXMania files now assemble audio samples during playback/conversion
  instead of during import, removing the long processing time on load
* Add exported background images to `song.ini` so Phase Shift loads them
* Tool for doing a straight `.chart` to MIDI conversion with no changes
* Switch Warriors of Rock customs on Xbox 360 to use MP3 audio (like PS3)
  * Uses a bit more space, but is much faster and more reliable to generate
    compared to the previous XMA

## 20221024

* Fix error when generating a GH3 PS3 song cache on Windows

## 20221023

* Export to Guitar Hero III DLC (360, PS3)
  * Supports lead + rhythm/bass charts, automatic HOPO forcing, star power,
    face off sections, practice sections, drum animations
    * Generated files use MP3-FSB audio on both platforms
    * A dummy note on rhythm/bass is used as a stopgap solution to make songs
      end at the intended time, rather than right after the last playable note
  * Customs use a "song cache" file like GH:WoR,
    see "Other tools" window to create
  * Similar to WoR, it seems that it is possible for the save file to get into a
    broken state where further saving does not work, so please back up any save
    data you care about before loading custom songs!
  * Note, currently around 70-80 songs can load at once in the Download setlist;
    however, there does not appear to be a significant limit to unloaded songs
    present in the Song Cache
* Import support from Guitar Hero Aerosmith 360 disc
* Import support from Guitar Hero III PS3 DLC
* Import some missing things from GH3 DLC: face off sections,
  drum animations (as chart)
* Option to limit how many CPU cores are used at once
* Better sorting for GH2 bonus songs and GH3 DLC: case insensitive,
  ignores The/A/An, option to sort first by title or artist
* Vocalist gender control when loading a song (#185)
* Fix a possible crash when saving app preferences on Mac
* Include extra instructions for fixing permissions issues on Mac

## 20220917

* Support for importing from Guitar Hero III (360 disc, 360 DLC, PS2 disc)
* Support for importing from Rock Revolution (360 disc, 360 DLC)
* Support for importing from Freetar (`.sng`) format
* Support reading from disc `.ark` files for Rock Band 1, 2, 3
* Support importing directly from PS2 and Xbox 360 `.iso` files
* Fix a bug with non-100%-speed GH1/GH2 output
  where backing audio speed was double-adjusted
* Fix reading from PAL versions of PS3 Rock Band songs (#208)
* Fix sync of music video songs from Rock Band 2 for PS2
* Support new `.chart` representation of drum lanes, accents, and ghosts
* Add author information readable by Guitar Hero II Deluxe (360)
  and Rock Band 2 Deluxe
* Import Graveyard Shift from GH1 if present
* Audio combine tool now generates working FSB3 with XMA1
  for GH3 and Rock Revolution
* Fix simultaneous `VENUE` lighting commands being converted
  to overlapping notes for RB2
* Fix batch processing sometimes not cleaning up each song's temporary folder

## 20220807

* Support for converting to Guitar Hero (1)
* Fix volume levels when importing from GH1
* Fix preview times when importing from GH1 and GH2
* Support importing Trippolette from GH1 if present
* Support importing from Power Gig PS3 disc and 360 DLC files
* Fix/finish import of vocals from Power Gig
* Fix Linux build which broke Ogg Vorbis encoding in `20220618` (#205)
* Fix song preview times not being moved correctly
  when a song is delayed due to early notes

## 20220618

* Improvements to audio playback, less likely to run out of audio
  when playing multiple stems
* Fix 3D preview on certain Linux systems such as the Steam Deck
* Fix cases where Quick Convert would attempt to produce invalid filenames
* Basic import of songs from Power Gig: Rise of the SixString
  (currently guitar and drums only)
* Add extracted Xbox 360 folder output option to GH2 pack tool
* Fix some preferences not being applied to batch or song windows
  until they were relaunched
* Generated `.RPP` projects show color map in newer REAPER versions
* Fix the conversion of some genre names (#193, #195)
* Fix some conversion errors when calculating song preview times

## 20220319

* Add custom USRDIR subfolder option to Quick Convert PS3 pack creator
* Fix "unmute >22-fret protar notes" Quick Convert toggle (broke in 20220311)
* Fix PS3 GH:WoR output sometimes not being able to generate audio successfully
* Fix 360 pack creator not stripping byte order markers from songs.dta when
  combining, leading to game crashes
* Add back simple CON to PKG tool as an option for certain use cases

## 20220311

* New tool, Quick Convert, which performs operations on Rock Band format songs
  without recompiling
  * Replaces the pack creator (for RB), and can also extract a pack into songs
  * Replaces the black VENUE tool, and can also remove overdrive or lanes,
    and decompress `.milo_*` files
  * Converts between Xbox 360 and PlayStation 3
  * Can resign CON/LIVE, decrypt/encrypt PS3 MIDIs,
    and split/combine package folders within `.pkg`
  * Replaces/includes Dolphin conversion previously in batch mode
* Import/export support for PlayStation 3 format for
  Guitar Hero: Warriors of Rock
  * WoR Song Cache tool can take `.pkg` or `_TEXT.PAK.PS3.EDAT` as input,
    and produce `.pkg`
  * To include data from official DLC, provide `.RAP` files in
    `onyx-resources/raps`
* Tentative import support for Rock Band 4 DLC (extracted contents from PS4)
* Import support for Rock Band 1 for PS2
* Fix preview audio playback continuing for a bit after song window is closed
  on Windows (#183)
* Add an option to treat encrypted .mogg files as silent,
  rather than an error (#184)
* Fix Clone Hero audio output to always end at `[end]`
* Fix Clone Hero output to not include silent audio files other than `song.ogg`
* Improve import of GH1/GH2 (PS2) and GH:WoR (360/PS3) files to not have to do
  lengthy audio conversion up front
* Simpler DTX to RB cymbal mapping (#190)
* Better organized Preferences screen with tabs

## 20211224

* Support for generating PlayStation 3 format for Rock Band 3 and Rock Band 2
  * Generates `.pkg` files, with unique content IDs and folders per song
  * Supports both RPCS3 and jailbroken consoles
  * `.pkg` files can be combined into packs
  * Also supports importing from custom or official `.pkg` files
* Better shortnames for RB2/RB3/GH2 that include some of the title/artist
* Fixes to handling of lipsync files
  * Use lipsync files from CON/RBA files for converted output
  * Fix lipsync files getting out of sync when song speed is modified
    or songs are padded with silence
* Fix CH to GH2 conversion assigning an invalid preview audio time sometimes
* Fix crash when importing from CH folders with non-ASCII characters on Windows,
  on (mostly) external or non-system drives

## 20211108

* Minor fixes to Guitar Hero II Deluxe support
  * Fix transferring 2x Bass Pedal from Clone Hero X+ format
  * Replace ampersand with plus in author name
  * Allow converting drums to guitar for GH2, and use it for drums-only songs

## 20211107

* Fixed length limit on GH:WoR audio generation
  * In very long files, there may be tiny gaps caused by
    stitching encoded audio segments together
  * Stems will be supported in a future release
* Support (on import) new Clone Hero MIDI formats for tap and open notes
* Import and export `album_track` in CH/PS `song.ini`
  (previously only `track` was supported)
* Fix importing from GH1/GH2 ARK files split into multiple pieces
* Allow turning a drums chart into a guitar/bass/keys chart for RB3/RB2/CH
  * Added a few more options for redirecting parts in batch mode
* Additions for Guitar Hero II Deluxe 2.0
  * Option to add drums charts
  * Fix parsing certain DTA scripting constructs in `songs.dtb` when importing
  * Tweaks to generated `songs.dtb` data when installing into a DX 2.0 ARK

## 20210806

* First demo of conversion to Guitar Hero: Warriors of Rock (Xbox 360) DLC
  for RGH/JTAG consoles
  * Convert songs in either single view or batch mode, then generate a
    "song cache" file in Other Tools - this is required to load multiple songs
  * Currently internally uses a separate Windows program to encode audio,
    meaning Wine is required on Linux and Mac
  * Audio has roughly a 13 minute limit at the moment (not a game limit,
    a limitation of the audio encoding tool)
  * Various missing features and game limitations, see `README` for details
  * There appears to be a limit of slightly over 700 songs' metadata in one
    file; any more causes a crash
  * **IMPORTANT**: it also may be possible to corrupt a save by loading too many
    songs over multiple sessions, such that loading any more new songs crashes
    on launch until you delete your save. Until this can be figured out, please
    back up any saves you care about before loading any custom songs!
* Also can import from GH:WoR (360) DLC
* Fix Opus import prematurely cutting off audio tracks sometimes
* Actually include Opus support in Linux build
* Fix GH2 export sometimes specifying characters and venues
  that don't exist in GH:80s
* Fix batch mode temporary folders sometimes not being cleaned up until app exit
* Smarter filename trimming for Xbox filenames (keeps suffixes and game type);
  also remove plus and comma which are disallowed on 360 internal drives
* Fix The Beatles: Rock Band songs being marked as covers
* Fix a hashing bug that could cause song ID conflicts,
  such as between different speedups of the same song
* Fix drum dynamics being applied to RB import even
  without the CH text event to enable them

## 20210702

* Fix converting to CH/PS format on Windows
  (broke due to non-Latin-1 character fix in `20210606`)
* In Xbox 360 pack creator, ignore `spa.bin` file seen in GH2 DLC
* Add support for Opus audio in CH songs
* Fix importing CH/PS songs referencing a `video` in `song.ini`
  which does not exist
* Fix importing `.chart` files which end a solo without starting one
* Fix importing RB songs containing `songs.dta` scripting,
  such as from the TBRB CDLC project

## 20210615

* Support import from GH2 Xbox 360 DLC
* Smaller VGS files for GH2 output
  (silent channel encoded at a lower sample rate)
* Support differing sample rates in VGS creator
* Add GH1 + GH2 star cutoffs in song view
* Fix DTX drums not being converted to RB/CH (broke in `20210522`)
* Minor fix for RB4/ForgeTool compatibility
  when working around the 6-channel MOGG issue
* Fix lead guitar not being imported right from GH2 co-op tracks like Trogdor
  which have separate co-op audio but not a distinct `PART GUITAR COOP` chart

## 20210606

* New tool for combining RB or GH2 Xbox 360 files into packs
* Fix game thumbnail of GH2 Xbox 360 files
* GH2 output picks a random GH2 character/guitar/venue for quickplay
* Fix Rock Band VENUE tracks always being replaced with a black background
* Fix invalid filenames caused by slash or backslash in song metadata
* Mix down drum stem configurations not supported by RB instead of failing
* Fix importing songs containing non-Latin-1 characters on Windows

## 20210530

* Fix Dolphin conversion that broke in `20210522`
* Fix some generated invalid filenames, such as ones that end in a period
* Read cover version tag from FoF/PS/CH songs
* GH2 export changes:
  * Make audio a bit longer to fix some hangs, particularly in practice mode
  * Use `+` instead of `&` in title/artist (no ampersand in font)
  * Add author name that can be read by an upcoming GH2 Deluxe build
  * Allow 5-note chords that were previously being changed to `GRBO`
  * Option (default on) to sort bonus songs alphabetically by title when saving
  * Option to apply a global audio offset only for GH2 export,
    intended to offset emulator delay
  * Add loading phrase from FoF/PS/CH import

## 20210522

* Initial support for converting to Guitar Hero II (PS2 .ARK, Xbox 360 DLC)
* Add support for drum ghosts and accents to 3D player and CH import/export
* Fixes to importing FoF/PS/CH format
  * Fix a bug that could incorrectly move vocal phrases when attempting to
    extend them to at least one quarter note
  * Fix implementation of `delay`/`Offset` sometimes changing time signatures
  * Fix not detecting the MIDI track from songs included with the original
    Frets on Fire, and import "cassette" art into square format
  * Fix `.chart` files with cymbals and no toms being marked as missing
    Pro Drums
* Sort MOGG channel indexes for better compatibility with ForgeTool RB4
  conversion
* Fix time jumps in both 3D and web preview tools
  due to the system clock being changed
* Fix time signatures being discarded due to a `*/1` or `*/2` time signature
* Speed up most song format import code,
  especially DTX and GH1/GH2 audio processing
* New tool to write temporary numeric IDs from an RB3 song cache
  back into song files
* Lipsync additions
  * Simpler interface that converts a MIDI to a .milo in one step,
    or updates an existing one
  * New MIDI track format for lipsync visemes, to support manual edits when
    converting vocal tracks to lipsync
  * Split off RB3 and TBRB viseme mappings to files in the resources folder
* New tool to hardcode the random numeric IDs from a song cache
  to songs without them
* Support including RB `song_id` in output filenames
* Option to not add (2x Bass Pedal) to song titles
* Option to use true numeric IDs for Rock Band files
  (like `1234` instead of `'o1234'`)
* Option to limit Xbox 360 CON/LIVE filenames to 42 characters
  as required by the console

## 20210110

* 3D preview improvements:
  * Support displaying background videos and images from CH/PS format
  * Display MIDI `measures:beats:ticks` timestamp and practice sections
  * Fix some issues with animation smoothness and audio playback
* Add option for OGG Vorbis quality level to Preferences screen,
  which affects both RB `.mogg` and CH `.ogg` output
* Fix a bug that removed or incorrectly translated lanes on gravity blasts
  when importing from CH/PS format
* Fix CH and web player functions to use the Save File dialog with a default
  filename, instead of the less useful Save Folder dialog
* The Beatles: Rock Band custom songs (Xbox 360) import correctly
* Improvements to lipsync files: vowel diphthongs (RB3, TBRB)
  and consonant shapes (RB3)

## 20201121

* Preferences screen in menu:
  * Default folders for each output file type (RB, CH, Wii, web preview),
    used in Save File dialogs and to prefill batch template inputs
  * Antialiasing options for the 3D preview
  * Ability to disable Magma, intended for advanced users or those running
    newer macOS which does not support Wine
  * Toggle to always produce black `VENUE` tracks in RB files
* Read Expert+ extra kicks format in `.chart` files

## 20200822

* Fix some memory leaks caused by the single song window
* Read Pro Drums format from `.chart` files
* Fix a bug where lanes could be redrawn incorrectly when importing from PS/CH
  format
* Be more lenient when reading some spec-incorrect MIDI files
* Significantly faster REAPER project generation
* Draw lanes and BREs in 3D preview
* DTX import: better audio sample mixing, and a smarter algorithm for assigning
  cymbal colors
* Pass tuning cents to REAPER project VSTs

## 20200513

* Fix 3D preview not displaying in AMD graphics cards on Windows
* Fix a crash when auto reductions tried to handle more-than-3-note chords
* Make readme, license, and update history available from menu
* Include `expert+.mid` in CH/PS output

## 20200426

* A new primary way of using Onyx, the "single song view", with several new
  tools and capabilities:
  * A new in-app song preview tool, with 3D graphics
  * A tool to calculate RB3 star cutoffs for any set of instruments
  * Same conversion tools as batch mode (RB3, RB2, CH/PS)
  * Edit any of the song's metadata and turn individual instruments off
    (or redirect) before conversion
* New MIDI file parser, much faster and less memory-hungry
* RB MOGG audio to CH/PS format is significantly faster (previously was
  unnecessarily decoding multiple times)
* Fix "overlap" errors when vocal phrases are less than 1 beat long
* Fix errors with drum swells in Phase Shift songs, such as converts from GH
* Fix some character errors in metadata and lyrics
* Now available for Linux

## 20200223

* Fix invalid filenames being generated by output templates
* Tool to patch CON files to replace the `VENUE` track with a black background
* Tools to make `.lipsync` and `.voc` files from a vocal track
* Tools to extract `.milo_xxx` files to a folder, replace files, and repack them
* Fix parsing lyrics from `.chart` containing a quote character
* Prevent certain kinds of MIDI corruptions from causing errors (issue #137)
* Significantly speed up parsing track data from MIDI files
* Fix some lighting issues when converting RB2 venue tracks to RB3 format

## 20191216

* Fix an issue parsing `*.c3` files when importing Magma projects
* Improved lipsync for English lyrics; now uses a few different vowel shapes by
  looking up lyric words in a pronunciation dictionary
* Add extra options to use a song's title, artist, album, or author in produced
  files/folders (PS/CH export uses "Artist - Title" by default)
* Fix Mac build which didn't bundle a required library correctly

## 20191024

* Fix a bug that caused occasional errors when generating CON files
* Generate MOGGs via a new much faster method, instead of Magma

## 20190918

* Options to prevent importing specific instruments in batch window
* Add automatic drum animations to standalone MIDI reduction generator

## 20190914

* Fix CON names and descriptions getting truncated shorter than necessary
* Fix tap notes being written in a way that confused Clone Hero
* Fix RB2 CONs to use RB2 package thumbnail image
* Support importing 5-lane drums from `.chart`
* Web player: fix 5-fret solos that stopped being shown in `20190823`
* Web player: show HOPOs in Keys track

## 20190823

* Fix an issue with generated CON files that prevented them from being read by
  C3 CON Tools
* Fix a bug that overwrote drum animations and character intensities with
  Magma's default ones
* Fix reading non-ASCII characters in (Latin-1) Magma project files
* Web player: minor optimizations to drawing 5-fret G/B/K and pro keys tracks

## 20190811

* Fix PS/CH speed changes not editing the MIDI tempos
* .NET or Mono no longer required (new library for generating CON files)

## 20190804

* Better conversion of charts with open notes to games that don't support them:
  can now move sequences of adjacent notes to preserve movement
  * Removed the "drop open HOPOs" conversion option; no longer necessary
* Fix `.chart` files with a solo with no end event
* Support for STFS files containing more than one song
* Support for importing from GH1/GH2/GH80s (PS2) `.ark`/`.hdr` files
* Support for importing from DTXMania song format
  * Automatic romanization of Japanese metadata for RB output
* Strip Clone Hero formatting tags from metadata, sections, and lyrics
* Fix a bug with importing Pro Bass that uses more than 4 strings
* Fix STFS imports to always use in-game title instead of C3 comment title

## 20190707

* Fix the HOPO threshold used for `.chart` files to match Clone Hero and
  Moonscraper
* Much better automatic vocal animations
* Mac version probably requires Mojave now, though I haven't confirmed this

## 20190702

* New interface!
* Add "swap guitar/keys" and "swap bass/keys" to RB3 conversion
* Convert to Phase Shift/Clone Hero format
* Import from Magma project format
* Function: MOGG file generator
* Function: Pro Keys hanging notes finder
* Function: Standalone MIDI to REAPER project converter
* Function: Lipsync dry vocals generator
* Match Magma format for printed MIDI timestamps
* Import basic drums and vocal lyrics from `.chart`
* Proof-of-concept generation of lipsync animations (will be improved)
* Fixes to importing Phase Shift/Clone Hero charts:
  * Remove overdrive phrases that have no notes in any difficulty,
    or that have no notes between it and the previous phrase
  * Better detection of dummy tracks and lower difficulties
  * Make short songs at least 30 seconds for Magma
  * Support `hopo_frequency` in `song.ini`
* Web player:
  * Fix longstanding bug when drawing sustains with no gap before the next note
  * Add PS Real Drums display with hihat and rimshot notes
  * Separate Pro Drums and Basic Drums display
  * Better cymbal gem image
  * Compute and show (not pretty) slides in Pro Guitar/Bass

## 20190209

* Tool to convert songs to Dolphin `.app` format for recording videos
* Fixed Force HOPO to override Force Strum when used at the same time
* Fix `.chart` and `.ini` files starting with a byte order mark
* Fix `.chart` files with two copies of the same note
* Import `bass.ogg` (in addition to `rhythm.ogg`) from PS/CH songs
* Fix em dashes and en dashes used in title/artist/album
* Fix Pro Guitar chord name override events with parentheses
* Support lanes on Hard
* Much faster file extraction from CON files
* Web player improvements:
  * New audio format, fixes MP3s not seeking correctly in Safari/Edge
  * Labels and icons on each track
  * Include solo vocals in addition to harmonies
  * Much smaller `song.js` file
  * Practice sections shown on progress bar and above timestamp
  * Fixed display of lanes on lower difficulties
  * Open notes: wide sustains, lane support
  * Show only 4 (or 5) lanes on Pro Bass if higher strings aren't used
  * Pro guitar arpeggios
  * Fixed E/M/H Pro Keys not showing solos

## 20180717

* Fix bug introduced in `20180513` with reading Pro Keys BREs
* As a last-ditch effort, can now wipe a tempo map entirely and reposition all
  events off the grid
* Apply "chord notes slightly off" fix to all instruments
* Web player improvements:
  * New menu for toggling tracks and view options
  * Chord names for Pro Guitar/Bass
  * Hard/Medium/Easy difficulties
  * Fix display flickering on slower systems
* Fix some RBN songs where `[events like this]` were in lyric events
* Fix some rare remaining issues where a non-standard `BEAT` track caused issues
  with early mood events

## 20180513

* Basic support for converting from Phase Shift songs with only "Real Drums"
  charted (removes hihat pedal notes for RB)
* Fix songs with slightly misaligned notes intended to be simultaneous
* Web player upgrades and fixes
  * Pro guitar: tap notes, fixed muted sustain color, fixed HOPO algorithm
  * More customization options: track speed, lefty flip mode,
    experimental "static tracks" mode
  * Fixed drawing GH-style sustains that go all the way to the next note
  * Fixed kick notes being drawn over hand notes on drums
  * Fixed bug where vocals couldn't be re-enabled after being disabled
* Better handling of `VENUE`; can convert any input `VENUE` track
  to RBN2 or RBN1 format as needed
* Fix parsing `[lighting(foo)]` with no space after `lighting`
* Fix when mood events like `[idle_realtime]` are placed too early
* Pass along `song_tonality` when converting
* Most of the guts of MIDI parsing and transformation have been replaced.
  In theory nothing should be different but please report new bugs!

## 20180308

* Support MP3 audio for Clone Hero format
* Fix invalid unison phrases (that "don't quite coincide")
* Basic 5-lane drum support (converts to 4-lane; shows in web player)
* Option to remove open HOPO notes when importing from PS/CH
* Fix importing a `.chart` with two solo start events in a row
* Change all tap notes to be HOPO for RB
* Fix some CH songs being imported with no tracks
* Fix some CH songs using the wrong audio delay value
* Fix vocal charts imported from some GH to PS conversions
* Fix some missing album art from PS imports
* Show song title and artist in page title of web player
* Show `mm:ss.sss` timestamp in web player
* Web player colors and fonts separated out into editable file
  (more customization options coming)

## 20180211

* Better support for fixing `BEAT` track errors, should enable fixing much
  higher tempos
* Support FoF `eighthnote_hopo` field
* Support FoF MIDIs that use old GH star power notes on pitch 103, either
  detected automatically or via `star_power_note`/`multiplier_note`
* Fix sped up FoF songs not taking speed into account when placing `[end]` so
  they got long gaps before the song ended
* Much faster FoF to RB MIDI processing, so Magma will succeed or fail earlier
* Various other fixes for certain FoF MIDIs

## 20180129

* Bug fix: sections were being placed in wrong places when the tempo map had to
  be modified due to too-fast or too-slow tempos, or delayed due to early notes
* Fix for old GH1 MIDIs (`T1 GEMS` track name)
* Fix for very old "trackless" FoF MIDIs
* Replace 5-note chords with GRBO chords
* Reaper projects include "woodblock" sound for guitar/bass/drums/keys
* Reaper projects now use [Seil's Pro Guitar preview plugin][1], with tuning
  configured automatically from `songs.dta`
* Fix leading/trailing space in metadata fields (seen in some `.chart` files)
* New icon

  [1]: http://customscreators.com/index.php?/topic/15811-pro-guitar-preview-for-reaper-v10/

## 20180111

* Fix importing PS songs with 2 drums audio files
  (corresponding to `[mix _ drums4]`)
* Fix incorrect `.dta`/`.rbproj` values for the New Wave genre and its subgenres
* Remove extra tempos and time signatures after `[end]` event
  to prevent Magma errors
* Prevent encoding errors from reading `song.ini`, and assume UTF-8
* Fix rare corruption of certain album art JPEG files
* Remove Easy/Medium basic keys force notes (for TrojanNemo's MIDI validator)
* First Mac app release; see README for requirements

## 20171220

* Web player displays lanes, glissando, and BRE
* Toggle for whether `(RB2 version)` is added to title of RB2 converts

## 20171126

* Better file adder screen: parses and displays songs as they're added
* Browser preview supports Clone Hero's 6-fret (GH Live) mode, open notes,
  and tap notes
* Imports .chart files. Must be in Clone Hero format (in its own folder,
  following the Phase Shift audio naming convention, with optional `song.ini`)
  and `.ogg` audio (not `.mp3`)
* Reaper projects now come with pitch colormaps for 5-fret, 6-fret, and drums
* Various import/convert fixes
* Checks for new version on launch

## 20171011

* A real Windows installer
* Shows a display of the log while running
* Faster speedups of custom song CONs (no longer gives silent MOGG channels
  to the audio stretch library)

## 20170917

* Remove drum fills that are too close for Magma, to allow higher speedups
* Switch to 64-bit for Windows build

## 20170908

* Fix different speed audio sometimes getting desynced from the MIDI

## 20170903

* Fix different speed versions of the same song sharing a song ID

## 20170902

* Fix guitar-on-keys freezing the game: guitar and keys will share the guitar
  audio channels (audio cuts out if either player misses)
* Also fixes guitar tier when copying to keys
* Selects result files in explorer window upon completion

## 20170826

* Revamped the menu organization (convert to RB3/RB2/Magma now a single menu)
* Function: change song speed
* Function: copy Guitar chart to Keys
* Fixed too slow/fast tempo workaround, should now work for more songs
* Detects silent MOGG channels and emits zero-length files
  when exporting to Magma project
* Make sure preview start time is no later than 9:30,
  because C3 forgot to patch that limit in MagmaCompilerC3.exe

## 20170806

* Function: import CON/PS to Magma project
* Add (RB2 version) to title of RB3 to RB2 converts
* If input file ends in \_rb3con,
  output ends in \_rb2con instead of \_rb3con\_rb2con
* In some cases (WIP), can now fix tempos below 40bpm or above 300bpm
* Improvements to RB3 to RB2 venue converter

## 20170701

* Moved back to X360 (.NET library) for STFS (CON) generation for now.
  XboxInternals is having segfaults on some systems,
  and sometimes produces broken files.

## 20170624

* Fixed a bug in extracting CON files with block separation
* Output CON files without block separation for smaller files

## 20170619

* Replaced X360 with XboxInternals for STFS (CON) creation,
  so .NET/Mono is no longer needed
* Converting a FoF song with 1x and 2x kicks
  will automatically create 2 CON files
* On Windows, fix handling of filenames with non-ASCII characters
* FoF conversion: option for whether or not to mark the whole song
  as toms if no Pro Drums detected
* Ensure generated CON files have names no more than 42 characters
* Include (2x Bass Pedal) in CON package title
* Nicer loading animation for web player

## 20170604

Initial public release of the GUI tool, featuring:

* PS to RB3 converter
* (Updated) RB3 to RB2 converter
* Browser preview app
* Song/MIDI to REAPER project converter
* Auto reduction tool
