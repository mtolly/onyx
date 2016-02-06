**NOT FINAL, WORK IN PROGRESS**

*Onyx* is a unified tool intended to streamline the process of building custom songs for Rock Band and other similar music games.
It is designed to work with and augment the existing Rock Band Network tools, including REAPER and Harmonix's or C3's Magma.

Some of Onyx's key features:

* *Autocompletion*: if you leave something empty, Onyx can usually fill it in for you. Onyx can produce:

  * a sensible `BEAT` track, built from the MIDI time signatures

  * automatic reduced difficulties for all instruments

  * a placeholder pro keys track from basic keys, using the CDEFG white keys

  * automatic lane ranges for pro keys

  * a 1x Bass Pedal reduced version of a 2x Bass Pedal drums chart

  * drum mix events

  * a `PART VOCALS` track from harmonies

  * keyboardist animations from the expert pro keys chart

* *Integrated audio editing*:
  Onyx lets you write a so-called "audio expression" directly into your project file.
  This expression is a formula for taking the audio from a raw source, such as the original CD, and transforming it to produce the audio used in-game.
  For example, a simple expression might be: "Take the CD track, pad it by 3.5 seconds, and add a custom count-in sample at positions given by the MIDI file".
  This repeatable process means you don't have to carry around the modified audio; you can just keep a FLAC copy of the album, and Onyx will rebuild the game audio whenever needed.

* *Music library integration*:
  Onyx identifies audio files based on a hash of the audio sample contents, and/or their precise length in samples.
  This means you can keep your music library in a totally separate location, organized however you want, and Onyx will find your songs when it needs them.
  Onyx can also extract audio directly from other sources such as Youtube, Jammit, and existing Rock Band MOGG files.

* *Shared metadata*:
  information such as artist, album, year of release, genre, `VENUE` autogen theme, etc. can be shared among all the tracks on an album.
  Onyx uses YAML files for its project format, and a simple convention lets one YAML file include the contents of another.

* *Powerful MIDI editing*:
  Onyx can translate MIDI files to and from a flexible text format, which lets you transform MIDI data with all sorts of existing tools.

    * You can use `diff` and `patch` to see the differences between two MIDI files, and merge work done by two authors to the same starting file. You can also use powerful text editors like `vi`, Emacs, Sublime Text, etc. to make all sorts of tricky edits.

    * You can translate charts between two completely different tempo maps. This is done by writing the event positions in seconds (real time, as opposed to musical beat-based time), swapping out the tempo map, possibly adjusting the positions by some offset, translating back to a MIDI file (reinterpreting those positions with the new tempo map), and finally quantizing the result as needed.

* *Full chart previews*:
  Onyx comes with a high-quality chart playback app, where you can watch and scroll through the song on all instruments in a Beatmania-style display.
  Unlike the Reaper preview plugin, vocals and pro keys are supported, and algorithms such as HOPO and tom/cymbal assignment are properly implemented.

* *Interoperability*:
  Onyx can read and write all sorts of project formats in an intelligent manner,
  including Xbox 360 CONs, Reaper projects, and Phase Shift songs.

My goal in making Onyx was to allow the use of version control software such as Git to help organize the process of making Rock Band customs.
Additionally, I was concerned about the increasing loss of older customs because of a reliance on file hosting sites for distribution.
My hope is that a large body of songs can be archived with Onyx's project format in a very small amount of space, so that the history of the customs community can be preserved.
