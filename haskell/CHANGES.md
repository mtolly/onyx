# Version history

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
