This is my collection of songs I have authored for use in Rock Band 3 and other
similar rhythm games. The charts are provided in a "source" format, where you
must supply your own audio. You can either:

* Manually piece together the MIDI and audio, and then use something like
  Harmonix's Magma, or C3's Magma, to create the song package. You can take a
  peek at the song's Makefile to get the exact audio offset needed.

* Use my toolchain, detailed below, to automatically build everything into a
  finished Xbox 360 CON package, using standard Unix command-line tools.

Most songs are not buildable yet -- I am in the process of setting up the build
process for each one, so check back for updates. These instructions are also
continually being fleshed out.

## Song format

* `notes.mid` is the authored chart. I have added some events which Magma may
  complain about if you do not move or remove them:

  * For 2x Bass Pedal drums, I use notes on pitch 95 for left kick drum notes
    which only show up on 2x. (96 is the normal Expert kick drum pitch.) When
    you use my scripts to create a 2x package, it then moves these notes up to
    96.

  * Countin audio is generated with a track named "countin" and text events
    named "countin_here".

* `tempo-{name of audio source}.mid` is a MIDI file with only a tempo track.
  For songs that support Jammit audio, if the Jammit audio requires a different
  tempo track from the original CD audio (usually for older songs recorded on
  tape), then `notes.mid` will have the Jammit tempos, and `tempo-album.mid`
  will have the original CD audio. You can manually replace the tempo track by
  first importing `notes.mid` into Reaper, then dragging in `tempo-album.mid`
  and checking only the "merge tempo track" box, not the "make new tracks" box.

* `Makefile` details the commands needed to build the complete song package.

* `audio-{name of audio source}.{ext}` is the filename under which you should
  supply your copy of an song's audio.

* `songs.dta` is the RB3 metadata file for the song.

## Requirements

If you are on Windows, install:

* MinGW and MSYS
* .NET Framework 2.0
* Ruby: any of 1.8.x, 1.9.x, or 2.0.x should work
* GNU Make
* Binaries of my tools: `ogg2mogg`, `rb3albumart`, `rb3pkg`, `midiscript`,
  and `jammittools` (if you want to use Jammit audio)
* ImageMagick: you need a recent version with DirectDraw Surface write support
* SoX, and possibly a separate MP3 support package if you want to use MP3 audio

If you are not on Windows, you can either:

* Install Wine, and set up "wine foo.exe" helper scripts for my tools

Or, if you feel like building the tools yourself:

* Wine (one of the tools uses `MagmaCompiler.exe`)
* The Haskell Platform (or, GHC, `cabal-install`, Alex, Happy)
* A .NET compiler: I use Mono but Visual Studio should work with tweaks

## Building a package

In the song directory, first create `audio-album.ogg` or something similar if
you need to supply audio. Then run:

    make gen/{audio source}/{1p or 2p}/package.con

to build your Xbox 360 CON file.

If you need to tell `jammittools` where your Jammit files are located, you can
do that with the `JAMMIT` environment variable. Place this before `make`:

    JAMMIT=/path/to/directory
