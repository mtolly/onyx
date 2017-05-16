# Onyx Music Game Toolkit

Very early alpha release! Many functions are undocumented.
Requires .NET and Visual C++. (If you can run C3 CON Tools, you're good.)

Open `onyx-gui-OPEN-ME.hta` to run.
Or drag a file onto the appropriate `.bat` file if the GUI does not work.

  * `convert-rb3.bat` (takes song.ini, creates _rb3con)

    Attempts to convert a Frets on Fire / Phase Shift song to Rock Band 3.
    Here is a sample of the steps performed:

      * Adds appropriate `[music_start]`, `[music_end]`, and `[end]` events
      * Generates an automatic `BEAT` track from MIDI time signatures
      * Adds auto-generated (roughly CAT-quality) lower difficulties if missing
      * Imports as much metadata as possible from `song.ini`
      * Applies the correct `delay` value from `song.ini` to the audio
      * Delays the song start by a few seconds if notes are present too early
      * Detects double drum roll lanes using the single lane note, and fixes them

  * `convert-rb2.bat` (takes _rb3con, creates _rb2con)

    Converts a Rock Band 3 CON file to Rock Band 2.
    RB2-ification includes the following steps:

      * removes RB3 added features like pro instruments, harmonies, bass solos,
        trill/tremolo, new drum animations
      * any 2-instrument unisons are made into 1-instrument OD phrases
      * converts your VENUE to RB2 format
      * ensures your lower difficulties have all colors used on Expert

    Then it will attempt to validate the song through Magma v1.
    If this succeeds, you're good to go.
    If it fails, it will still continue and simply copy the MIDI as-is.
    The result will still probably work in-game; you just won't get lipsync animations.

    This option will simply drop the Keys part if present.

  * `convert-rb2-kg.bat` (takes _rb3con, creates _rb2con)

    As above, but moves the Keys part to Guitar, and drops Guitar if present.
    The RB3 "keytar" algorithm is more-or-less applied,
    so fast chords will become HOPOs, and overlapping sustains will be shortened.

  * `convert-rb2-kb.bat` (takes _rb3con, creates _rb2con)

    As above, but moves the Keys part to Bass, and drops Bass if present.

  * `player.bat` (takes _rb3con, creates web app folder)

    Generates a JavaScript chart preview app for web browsers,
    which plays back the audio and displays all gameplay tracks.

    To use, open CON file with batch file. A player app folder will be placed next to it.
    Open `index.html` to play (can be run locally via `file://`, or hosted on a web server).

  * `reap.bat` (takes MIDI file, creates Reaper project)

    Imports a MIDI file into a Reaper project.
    Avoids a few bugs in Reaper's own MIDI import function.
    Also applies most of the C3 template to the MIDI tracks,
    so you get note names, colored tracks, and RBN preview windows.
