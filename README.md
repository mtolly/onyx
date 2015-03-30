# Onyxite's Rock Band Custom Songs

[![Build Status](https://travis-ci.org/mtolly/onyxite-customs.svg?branch=master)](https://travis-ci.org/mtolly/onyxite-customs)

This is a collection of songs I have transcribed for use in Rock Band 3
and other similar rhythm games, primarily for drums.
The charts are provided in a "source" format, where you must supply your own audio.

## Instructions for Magma

  1. Download the [latest Magma projects][releases] archive
    (the file with `magma` in the name, from the latest release).

[releases]: https://github.com/mtolly/onyxite-customs/releases

  2. In the extracted files, find the folder for the song you want to build.
    Inside that, locate the subfolder `gen/album/Xp`,
    where X is 1 or 2 for the number of kick pedals you want.

  3. The file `song.yml` has information on how to modify your audio file so it lines up with the chart.
    Look for a line such as this:

        album:
          pad: [3.205, source]

    This means, take the album audio (`source`)
    and pad the beginning with 3.205 seconds of silence.
    You can do this with [Audacity][] or a similar audio editing tool.
    Other effects (`mix`, `fade`, `trim`, etc.) are also possible.
    Here's a more complicated expression:

        album:
          fade:
            - end
            - 5.200
            - take:
              - begin
              - 598
              - fade:
                - begin
                - 5.673
                - trim: [10.991, source]

    You should perform the effects starting from the innermost one. So, this means:

      1. Remove the first 10.991 seconds.
      2. Fade in the first 5.673 seconds.
      3. Cut off the song after 598 seconds (9 minutes 58 seconds).
      4. Fade out the last 5.200 seconds.

    Also, mix in the file `gen/album/Xp/countin.wav` to add countin sounds.

[Audacity]: http://audacity.sourceforge.net/

  4. Save the audio to the path `gen/album/Xp/magma/song-countin.wav`.

  5. Compile the Magma project `gen/album/Xp/magma/magma.rbproj`
    using either Harmonix's Magma, or [C3's Magma][c3magma].
    If the song is over 10 minutes, C3's is required.
    Note that you may have to click once in the "destination" box (the `.rba` path) before compiling,
    to change the relative path into an absolute one.

[c3magma]: http://www.pksage.com/ccc/forums/viewtopic.php?f=12&t=381

  6. Optionally, convert your RBA file to an Xbox 360 CON package with [RB3Maker][].

[RB3Maker]: http://rockband.scorehero.com/forum/viewtopic.php?t=34542

## Full build system

You don't need to use this if you are just compiling with Magma!
Follow the instructions above instead.

`onyxbuild` is a build tool written with [Shake](http://community.haskell.org/~ndm/shake/),
which automates many steps of building a custom song.
Some current features:

  * transforms audio files from various sources for use in the game,
    according to a simple expression language

  * creates many different metadata formats
    (Magma project, RB3 `songs.dta`, Phase Shift `song.ini`)
    from one input file

  * converts a 2-pedal chart to 1-pedal automatically

  * generates a default `BEAT` track based on the MIDI time signatures

  * adjusts roll lengths to end immediately after the last note-on in the roll
    (as recommended by Harmonix on the RBN forums)

Binaries for Windows/Mac/Linux are available on the
[releases](https://github.com/mtolly/onyxite-customs/releases) page.

If you want to supply MP3 audio, [LAME](http://lame.sourceforge.net/) must be in your PATH.

If you want to compile straight to Xbox 360 CON you need:

  * .NET Framework or [Mono](http://www.mono-project.com)

  * [`rb3pkg`](https://github.com/mtolly/rb3tools/releases/download/v0.1/rb3pkg_v0.1_dotnet.zip)

    To place this in your `PATH` on Linux/Mac, make a script with contents:

        #!/bin/sh
        mono /path/to/rb3pkg.exe "$@"

If you are on very old Windows (XP) you may need this:

  * [Microsoft Visual C++ 2005 SP1](http://www.microsoft.com/en-us/download/details.aspx?displaylang=en&id=5638)

Finally, for Linux and Mac only:

  * [Wine](http://www.winehq.org)

Then, build the `onyxbuild` program in the `build/` directory, or download it
from the releases page.

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
