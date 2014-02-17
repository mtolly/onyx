This is my collection of songs I have authored for use in Rock Band 3 and other
similar rhythm games. The charts are provided in a "source" format, where you
must supply your own audio. You can either:

* Manually piece together the MIDI and audio, and then use something like
  Harmonix's Magma, or C3's Magma, to create the song package. You can take a
  peek at the song's Makefile to get the exact audio offset needed.

* Use my toolchain, detailed below, to automatically build everything into a
  finished Xbox 360 CON package, using standard Unix command-line tools.

Some songs are not buildable yet -- I am in the process of setting up the build
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

* `songs.dta` is a template for the RB3 metadata file. It has a few missing
  pieces which are filled in during the build process:

  * `<PACKAGE>` comes from a variable in the Makefile.

  * `<LENGTH>` is calculated from the location of the `[end]` event in the MIDI.

## Requirements

* .NET Framework or [Mono](http://www.mono-project.com):
  .NET v2.0, I think?
* [Ruby](https://www.ruby-lang.org):
  I've tested 1.9.3 and 2.0 but other versions might work
* [GNU Make](http://www.gnu.org/software/make/):
  Windows binaries [here](http://gnuwin32.sourceforge.net/packages/make.htm)
* [ImageMagick](http://www.imagemagick.org):
  you need a recent version with DirectDraw Surface write support
* [SoX](http://sox.sourceforge.net/):
  if you want MP3 support you may need a separate decoder package

For Windows:

* [MinGW and MSYS](http://www.mingw.org/):
  [Cygwin](http://www.cygwin.com/) may also work, but is untested

For Linux and Mac:

* [Wine](http://www.winehq.org/)

On Windows, just download binaries of my tools:

* [`ogg2mogg`](https://github.com/mtolly/rb3tools/releases/download/v0.1/ogg2mogg_standalone_v0.1_win32.zip)
* [`rb3albumart`](https://github.com/mtolly/rb3tools/raw/master/rb3albumart/rb3albumart)
* [`rb3pkg`](https://github.com/mtolly/rb3tools/releases/download/v0.1/rb3pkg_v0.1_dotnet.zip)
* `midiscript` (New version required! Coming soon)
* [`jammittools`](https://github.com/mtolly/jammittools/releases/download/v0.1/jammittools_v0.1_win32.zip)
  if you want to use audio from
  [Jammit](http://www.jammit.com/)

On Linux/Mac, a simple option is to set up Wine/Mono helper scripts for the
above Windows binaries. `ogg2mogg.exe`, `midiscript.exe`, and `jammittools.exe`
are Win32 binaries, so you might have an `ogg2mogg` script which simply has:

    #!/bin/bash
    wine /path/to/ogg2mogg.exe

`rb3pkg` is a .NET binary, so simply replace `wine` with `mono` above.
`rb3albumart` is a Ruby script, so it can be run unmodified.

The other option for the Win32 binaries is to compile them yourself. This is
easy to do -- see the `rb3tools` and `jammittools` pages for build information.

## Building a package

In the song directory, first create `audio-album.ogg` or something similar if
you need to supply audio. Then run:

    make gen/{audio source}/{1p or 2p}/package.con

to build your Xbox 360 CON file.

If you need to tell `jammittools` where your Jammit files are located, you can
do that with the `JAMMIT` environment variable. Place this before `make`:

    JAMMIT=/path/to/directory
