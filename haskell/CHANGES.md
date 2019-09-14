# Version history

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
