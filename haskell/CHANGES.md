# Version history

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
