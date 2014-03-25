This is a collection of songs I have authored for use in Rock Band 3 and other
similar rhythm games, mostly for drums. The charts are provided in a "source"
format, where you must supply your own audio. It is recommended that you use a
pre-built release package, but instructions are also included to use a more
complex build process.

## Easy method: supply album audio, compile with Magma

  1. Download the [latest release][releases] archive.

[releases]: https://github.com/mtolly/onyxite-customs/releases

  2. In the extracted files, find the folder for the song you want.

  3. The file `Makefile` contains numbers for how to modify your audio.
    Use [Audacity][] to pad, fade, or trim the audio as necessary, and also mix
    it with the file `gen/album/Xp/countin.wav` from the song folder, where
    `X` is the number of kick pedals you'd like, 1 or 2.

[Audacity]: http://audacity.sourceforge.net/

  4. Save the audio to the path `gen/album/Xp/magma/song-countin.wav`.

  5. Compile the Magma project `gen/album/Xp/magma/magma.rbproj` using either
    Harmonix's Magma, or [C3's Magma][c3magma]. If the song is over 10 minutes,
    C3's is required. Note that you may have to click once in the
    "destination" box (the `.rba` path) before compiling.

[c3magma]: http://www.pksage.com/ccc/forums/viewtopic.php?f=12&t=381

  6. Optionally, convert your RBA file to an Xbox 360 CON package with
    [RB3Maker][].

[RB3Maker]: http://rockband.scorehero.com/forum/viewtopic.php?t=34542

## Full build process

Required:

* .NET Framework or [Mono](http://www.mono-project.com):
  .NET v2.0, I think?
* [Ruby](https://www.ruby-lang.org):
  I've tested 1.9.3 and 2.0 but other versions might work
* [GNU Make](http://www.gnu.org/software/make/):
  Windows binaries [here](http://gnuwin32.sourceforge.net/packages/make.htm)
* [ImageMagick](http://www.imagemagick.org):
  you need a recent version with DirectDraw Surface write support
* [SoX](http://sox.sourceforge.net/)
* [LAME](http://lame.sourceforge.net/) if you want to supply MP3 audio

For Windows:

* [MinGW and MSYS](http://www.mingw.org/):
  [Cygwin](http://www.cygwin.com/) may also work, but is untested

For Linux and Mac:

* [Wine](http://www.winehq.org/)

On Windows, just download binaries of my tools:

* [`ogg2mogg`](https://github.com/mtolly/rb3tools/releases/download/v0.1/ogg2mogg_standalone_v0.1_win32.zip)
* [`rb3albumart`](https://github.com/mtolly/rb3tools/raw/master/rb3albumart/rb3albumart)
* [`rb3pkg`](https://github.com/mtolly/rb3tools/releases/download/v0.1/rb3pkg_v0.1_dotnet.zip)
* [`midiscript`](https://github.com/mtolly/midiscript/releases/download/v0.1.1/midiscript_v0.1.1_win32.zip)
* [`jammittools`](https://github.com/mtolly/jammittools/releases/download/v0.1.1/jammittools_v0.1.1_win32.zip)
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

In the song directory, first create `audio-album.ogg` or something similar if
you need to supply audio. Then run:

    make gen/{audio source}/{1p or 2p}/rb3.con

to build your Xbox 360 CON file.

If you need to tell `jammittools` where your Jammit files are located, you can
do that with the `JAMMIT` environment variable. Place this before `make`:

    JAMMIT=/path/to/directory
