[![Build Status](https://travis-ci.org/mtolly/onyxite-customs.svg?branch=master)](https://travis-ci.org/mtolly/onyxite-customs)

This is a collection of songs I have authored for use in Rock Band 3 and other
similar rhythm games, mostly for drums. The charts are provided in a "source"
format, where you must supply your own audio. It is recommended that you use a
pre-built release package, but instructions are also included to use a more
complex build process.

## Easy method: supply album audio, compile with Magma

  1. Download the [latest release][releases] archive.

[releases]: https://github.com/mtolly/onyxite-customs/releases

  2. In the extracted files, find the folder for the song you want.

  3. The file `song.yml` contains numbers for how to modify your audio.
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

As of v0.3, the build system is a Haskell program written with
[Shake](http://community.haskell.org/~ndm/shake/).
You can download a binary from the
[releases](https://github.com/mtolly/onyxite-customs/releases) page,
or compile it yourself with GHC >= 7.8 or the
[Haskell Platform](https://www.haskell.org/platform/) >= 2014.2.0.0.

The following programs must be in your `PATH` (callable by the build program):

  * [ImageMagick](http://www.imagemagick.org):
    you need a recent version with DirectDraw Surface write support
  * [SoX](http://sox.sourceforge.net/)
  * [LAME](http://lame.sourceforge.net/) if you want to supply MP3 audio

Plus the following if you want to compile straight to Xbox 360 CON:

  * [`rb3pkg`](https://github.com/mtolly/rb3tools/releases/download/v0.1/rb3pkg_v0.1_dotnet.zip)

    To place this in your `PATH` on Linux/Mac, make a script with contents:

        #!/bin/sh
        mono /path/to/rb3pkg.exe "$@"

  * .NET Framework or [Mono](http://www.mono-project.com)

Plus this if you want to compile to Magma RBA:

  * [`magmyx`](https://github.com/mtolly/magmyx)

Finally, for Linux and Mac only:

  * [Wine](http://www.winehq.org)

Then, build the `onyxbuild` program in the `build/` directory, or download it
from the releases page. This requires one package not on Hackage,
[ogg2mogg](https://github.com/mtolly/rb3tools/tree/master/ogg2mogg).

`onyxbuild` reads the file `song.yml` to get song information, then builds
whatever files you specify on the command line in a Make-like fashion. Inside
the song folder, to build a song, enter the following command:

    onyxbuild gen/{audio source}/{1p or 2p}/{rb3.con or magma.rba}

Valid audio sources:

  * `album`, for audio from the original CD. You must provide a file named like
    `audio-album.xxx` in the song directory before building.

  * `jammit`, for multitrack audio purchased from [Jammit](http://www.jammit.com/).
    By default your app-created Windows/Mac library will be read from, or you
    can specify the `JAMMIT` environment variable to point to a custom directory.

  * Various other sources as listed in the song's `README.md`.
