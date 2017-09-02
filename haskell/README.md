# Onyx Music Game Toolkit (version _ONYXVERSION_)

By Michael Tolly <onyxite@gmail.com>

Built on work and research by:

  * Harmonix
  * TrojanNemo, emist (C3 CON Tools, Magma v2 mod)
  * No1mann (Magma v1 mod)
  * xorloser (ArkTool/DtbCrypt)
  * deimos (dtb2dta)
  * qwertymodo (Magma MOGG redirect)
  * Hetelek, Experiment5X (XboxInternals/Velocity)
  * DJ Shepherd (X360)
  * arkem (py360)
  * and many others!

Onyx is free software via the GNU General Public License v3; see LICENSE.txt.

New versions and source at: https://github.com/mtolly/onyxite-customs/releases

## Requirements

Windows version requires .NET and (I think) the Visual C++ runtime.
(If you can run Magma, you're good.)

Mac version requires Wine and Mono.
For both, you should be able to use one of the official Mac installers
or install via Homebrew.

## Instructions

Open `onyx.exe`/`Onyx.app` to run.

  * Click on menu options to select them.
  * Click on previous pages on the left to go back.
  * Keyboard controls also work (arrow keys, enter, backspace).

## Functions

  * Convert to RB3 (takes CON/RBA/song.ini, creates _rb3con)

    Imports from a Rock Band 3, Frets on Fire, or Phase Shift song,
    and creates either a Rock Band 3 CON file or a Magma v2 project.

    Here is a sample of the steps performed when importing from FoF/PS:

      * Adds appropriate `[music_start]`, `[music_end]`, and `[end]` events
      * Generates an automatic `BEAT` track from MIDI time signatures
      * Adds auto-generated (roughly CAT-quality) lower difficulties if missing
      * Imports as much metadata as possible from `song.ini`
      * Applies the correct `delay` value from `song.ini` to the audio
      * Delays the song start by a few seconds if notes are present too early
      * Detects double drum roll lanes using the single lane note and fixes them
      * In some cases, can alter the tempo map to fix too-slow/too-fast tempos

    Note: to do a batch process of many songs, drag and drop song folders
    (the folders with song.ini immediately inside) onto the file loading screen.

    If the "automatic tom markers" option is turned on, an FoF song that doesn't
    have any tom markers and that does not have "pro_drums = True" in the
    song.ini will have tom markers added over the whole drum part, under the
    assumption that Pro Drums have not been authored for the chart.

    The song speed can be modified to produce sped up or slowed down songs.
    This will modify both the audio and MIDI, and add (X% Speed) to the title.
    For CON inputs this only works with unencrypted MOGG files.
    Different speed versions may not pass Magma in all cases, for example
    if a roll becomes too slow, or if drum fills become too close together.
    If this happens, you can produce a Magma project and then make the needed
    changes before compiling with Magma.

    Also available is an option to copy the Guitar chart to Keys,
    so that two people can play it. This will add force notes to the chart to
    make the Keys chart have the right strum/HOPO notes when played on a guitar.
    For songs with stems, the guitar audio will be linked to both the guitar
    and keys tracks, so if either instrument misses it will cut out.

    When creating a Magma project, a REAPER project will also be generated
    so any remaining problems can be quickly edited and re-exported to MIDI.
    This avoids a few bugs in REAPER's own MIDI import function.
    Also applies most of the C3 template to the MIDI tracks,
    so you get note names, colored tracks, and RBN preview windows.
    Because this must generate WAV audio files to give to Magma,
    encrypted MOGG files in input rb3cons are unsupported.

    Tip: to process a song with an encrypted MOGG, one option is to use
    the Phase Shift converter in C3 CON Tools which mixes the audio down to a
    single file, and then just supply the Phase Shift song to Onyx.

  * Convert to RB2 (takes _rb3con, creates _rb2con)

    Converts a Rock Band 3 CON file to Rock Band 2.
    RB2-ification includes the following steps:

      * removes RB3 added features like pro instruments, harmonies, bass solos,
        trill/tremolo, new drum animations
      * any 2-instrument unisons are made into 1-instrument OD phrases
      * converts your VENUE to RB2 format
      * ensures your lower difficulties have all colors used on Expert
      * adds (RB2 version) to the title to disambiguate when in RB3

    Then it will attempt to validate the song through Magma v1.
    If this succeeds, you're good to go.
    If it fails, it will still continue and simply copy the MIDI as-is.
    The result will still probably work; you just won't get lipsync animations.

    You can choose to drop the Keys part, or move it to Guitar or Bass.
    In the latter two cases, the RB3 "keytar" algorithm is more-or-less applied,
    so fast chords become HOPOs, and overlapping sustains are shortened.

  * Browser song preview (takes _rb3con, creates web app folder)

    Generates a JavaScript chart preview app for web browsers,
    which plays back the audio and displays all gameplay tracks.

    A player folder will appear next to the CON; open `index.html` to run.
    It can be run locally via `file://`, or hosted on a web server.

  * Auto reductions (takes MIDI files, creates MIDI file)

    Generates CAT-like automatic reductions for empty difficulties in a MIDI.
    Quality is not guaranteed, but they should pass Magma.

    To use, ensure that there are no notes or events authored
    for a difficulty you want to be filled in.
    (For Pro Keys, remove the `PART REAL_KEYS_?` track entirely.)
