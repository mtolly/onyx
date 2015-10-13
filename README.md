# Onyxite's Custom Songs & Build Tool for *Rock Band 3*

I transcribe songs for use in Harmonix's *Rock Band 3* and other similar rhythm games, mostly for drums.
Along the way, I've written a program called `onyxbuild`,
a powerful command-line tool for automating the song build process.

In addition to my own charts, this repository also contains work by:
Harmonix,
[Grinnz](https://www.youtube.com/user/SHGrinnz),
Meander,
and [Sideshow](http://pksage.com/ccc/IPS/index.php?/topic/10433-sideshows-customs-10415-a-plethora-of-led-zeppelin/?p=61000).
(Credited in individual song READMEs.)

## Building songs with Magma

  1. Download the [latest Magma projects][releases] archive
    (the file with `magma` in the name, from the latest release).

[releases]: https://github.com/mtolly/onyxite-customs/releases

  2. In the extracted files, find the folder for the song you want to build.
    Inside that, locate the subfolder `gen/plan/album/Xp`,
    where X is 1 or 2 for the number of kick pedals you want.

  3. The file `song.yml` has information on how to modify your audio file so it lines up with the chart.
    Look for a line such as this:

        album:
          song:
            pad: [3.205, album-track]

    This means, take the track from the CD (`album-track`)
    and pad the beginning with 3.205 seconds of silence.
    You can do this with [Audacity][] or a similar audio editing tool.
    Other effects (`mix`, `fade`, `trim`, etc.) are also possible.
    Here's a more complicated expression:

        album:
          song:
            fade:
              - end
              - 5.200
              - take:
                - begin
                - '9:58'
                - fade:
                  - begin
                  - 5.673
                  - trim: [10.991, album-track]

    You should perform the effects starting from the innermost one. So, this means:

      1. Remove the first 10.991 seconds.
      2. Fade in the first 5.673 seconds.
      3. Cut off the song after 9 minutes, 58 seconds.
      4. Fade out the last 5.200 seconds.

    Also, mix in the file `gen/plan/album/countin.wav` to add countin sounds.

[Audacity]: http://audacity.sourceforge.net/

  4. Save the audio to the path `gen/plan/album/Xp/magma/song-countin.wav`.

  5. Compile the Magma project `gen/plan/album/Xp/magma/magma.rbproj`
    using either Harmonix's Magma, or [C3's Magma][c3magma].
    If the song is over 10 minutes, C3's is required.
    Note that you may have to click once in the "destination" box (the `.rba` path) before compiling,
    to change the relative path into an absolute one.

[c3magma]: http://www.pksage.com/ccc/forums/viewtopic.php?f=12&t=381

  6. Optionally, convert your RBA file to an Xbox 360 CON package with [RB3Maker][].

[RB3Maker]: http://rockband.scorehero.com/forum/viewtopic.php?t=34542

## How to use `onyxbuild`

You don't need to do this if you are just compiling with Magma!
Follow the instructions above instead.

`onyxbuild` is a build tool written with [Shake](http://community.haskell.org/~ndm/shake/),
which automates many steps of building a song package.
Some current features:

  * transforms original audio files for use in the game,
    according to a declarative language of audio expressions

  * creates different metadata formats
    (Magma project, RB3 `songs.dta`)
    from one input file

  * thins out a 2-pedal drum chart to 1-pedal automatically

  * generates a default `BEAT` track based on the MIDI time signatures

  * adjusts roll lengths to end immediately after the last note-on in the roll
    (as recommended by Harmonix on the RBN forums)

  * swaps out the tempo track of a MIDI file,
    to easily use the same chart with two differently timed versions of a song

Binaries for Windows and Mac are available on the
[releases](https://github.com/mtolly/onyxite-customs/releases) page.
You'll also need to install:

  * .NET Framework on Windows, or [Mono](http://www.mono-project.com) on Mac/Linux

  * [Microsoft Visual C++ 2008](http://www.microsoft.com/en-us/download/details.aspx?id=29),
    but probably only if you are on an older Windows version

  * [Wine](http://www.winehq.org) on Mac/Linux

If you want to build it yourself,
[`stack`](https://github.com/commercialhaskell/stack) is the preferred way to do so.
You'll also need `libsndfile` (plus OGG/FLAC support) and `libsamplerate`.

`onyxbuild` reads the file `song.yml` to get song information, then builds
whatever files you specify on the command line in a Make-like fashion. Inside
the song folder, to build a song, enter the following command:

    onyxbuild gen/plan/{audio source}/{1p or 2p}/{rb3.con or magma.rba}

Valid audio sources differ by song, but often include:

  * `album`, for audio from the original CD.

    * Either place the audio (FLAC is preferred, OGG or WAV also works) in the song folder,
      or if you have it in another folder, add `--audio your/music/folder/` to the `onyxbuild` command.

  * `jammit`, for multitrack audio purchased from [Jammit](http://www.jammit.com/).

    * By default, the official Jammit app's folder is read from.
      If you have another folder with Jammit data, add `--jammit your/jammit/folder/` to the `onyxbuild` command.

Note that generating `magma.rba` directly currently fails on Mac/Linux if the song has vocals,
but generating `rb3.con` (which still uses Magma to process the MIDI) works fine.

## Licensing

[![Creative Commons License](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)
[![GNU LGPL License](https://www.gnu.org/graphics/lgplv3-88x31.png)](https://www.gnu.org/licenses/lgpl.html)
[![GNU GPL License](https://www.gnu.org/graphics/gplv3-88x31.png)](https://www.gnu.org/licenses/gpl.html)

My transcriptions (*not* the compositions) are freely licensed
under [Creative Commons Attribution-ShareAlike](http://creativecommons.org/licenses/by-sa/4.0/).
All compositions are the property of the original artists.

Most of my code related to this project is licensed under the [GNU LGPL](https://www.gnu.org/licenses/lgpl.html), including any compatible code whose license is not stated.
Some code (including anything linked to [X360](https://github.com/mtolly/X360)) is licensed under the [GNU GPL](https://www.gnu.org/licenses/gpl.html).
