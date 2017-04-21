# Onyxite's Rock Band Custom Song Toolkit

Very early alpha release! Many functions are undocumented.
Requires .NET and Visual C++. (If you can run C3 CON Tools, you're good.)

## Converting a Rock Band 3 song to Rock Band 2

Open `rb2-converter.hta`, select the option you want, hit Convert, and select your `rb3con`.
Or, manually open your `rb3con` with one of the following batch files:

  * `convert-rb2.bat` (drops Keys part)
  * `convert-rb2-kg.bat` (moves Keys part to Guitar)
  * `convert-rb2-kb.bat` (moves Keys part to Bass)

The program will RB2-ify your song, and then attempt to run it through Magma v1.
The following conversion steps are performed:

  * removes RB3 added features like pro instruments, harmonies, bass solos,
    trill/tremolo, new drum animations
  * any 2-instrument unisons are made into 1-instrument OD phrases
  * converts your VENUE to RB2 format
  * ensures your lower difficulties have all colors used on Expert

Whether Magma succeeds or not, a `rb2con` will be generated next to the `rb3con`.
Even if Magma fails, the song should (in theory) work; you just won't get lipsync animations.

## Other useful functions

  * `reduce.bat` (takes MIDI file, creates MIDI file)

    Automatic lower difficulty generator. (Quality not guaranteed.)
    To use, remove all MIDI notes/events for a difficulty on drums/guitar/bass/keys.
    For pro keys, remove the difficulty's MIDI track entirely.

  * `ranges.bat` (takes MIDI file, creates MIDI file)

    Generates a legal (not necessarily optimal) set of range shifts for pro keys notes.
    To use, remove all range notes on a pro keys track.

  * `hanging.bat` (takes MIDI file, creates text file)

    Creates a text file of possibly hanging pro keys notes.
    Lists all notes that are less than 1 second after a range shift they require.

  * `player.bat` (takes CON file, creates web app folder)

    SUPER early alpha preview release: generate a chart viewing app for web browsers.
    To use, open CON file with batch file. A player app folder will be placed next to it.
    Open `index.html` to play (can be run locally via `file://`, or hosted on a web server).

  * `mogg.bat` (takes .ogg, creates .mogg)

  * `reap.bat` (takes MIDI file, creates Reaper project)

    Imports a MIDI file into a Reaper project.
    Avoids a few bugs in Reaper's own MIDI import function.
    Also applies most of the C3 template to the MIDI tracks,
    so you get note names and colored tracks.
