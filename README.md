# Onyxite's Custom Songs & Build Tool for *Rock Band 3*

I transcribe songs for use in Harmonix's *Rock Band 3* and other similar rhythm games, mostly for drums.
Along the way, I've written a powerful command-line tool to streamline the build process.

In addition to my own charts, this repository also contains work by:
Harmonix,
[Grinnz](https://www.youtube.com/user/SHGrinnz),
[mazegeek999](http://pksage.com/ccc/IPS/index.php?/topic/13775-mazegeeks-customs-1117-tarkus-by-emerson-lake-palmer/),
[Meander](http://pksage.com/ccc/IPS/index.php?/topic/11496-meanders-songs-82014-open-for-business/),
and [beard216](http://pksage.com/ccc/IPS/index.php?/topic/12749-beard216s-conversions-silence-in-the-snow-full-album-by-trivium-and-proxy-by-tesseract-1125/).
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

[c3magma]: http://pksage.com/ccc/IPS/index.php?/topic/9257-magma-c3-roks-edition-v332-072815/

  6. Optionally, convert your RBA file to an Xbox 360 CON package with [RB3Maker][].

[RB3Maker]: http://rockband.scorehero.com/forum/viewtopic.php?t=34542

## Build tool

Instructions coming soon!

## Licensing

[![Creative Commons License](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)
[![GNU LGPL License](https://www.gnu.org/graphics/lgplv3-88x31.png)](https://www.gnu.org/licenses/lgpl.html)
[![GNU GPL License](https://www.gnu.org/graphics/gplv3-88x31.png)](https://www.gnu.org/licenses/gpl.html)

My transcriptions (*not* the compositions) are freely licensed
under [Creative Commons Attribution-ShareAlike](http://creativecommons.org/licenses/by-sa/4.0/).
All compositions are the property of the original artists.

Other authors' work hosted here is *not* necessarily licensed as such;
please contact them if you want permission to redistribute or repackage charts.

Most of my code related to this project is licensed under the [GNU LGPL](https://www.gnu.org/licenses/lgpl.html), including any compatible code whose license is not stated.
Some code (including anything linked to [X360](https://github.com/mtolly/X360)) is licensed under the [GNU GPL](https://www.gnu.org/licenses/gpl.html).
