# Onyx Music Game Toolkit (version _ONYXVERSION_)

By Michael Tolly <onyxite@gmail.com>

Onyx is a multipurpose build tool for Guitar-Hero/Rock-Band-like music games.
It can import and export a wide variety of formats, move parts around,
change song speed, fill in missing details, and assist with song authoring.

Built on work and research by:

  * Harmonix
  * TrojanNemo, emist (C3 CON Tools, Magma v2 mod)
  * No1mann (Magma v1 mod)
  * xorloser (ArkTool/DtbCrypt)
  * deimos (dtb2dta)
  * qwertymodo (Magma MOGG redirect)
  * Bloodline, Inventor (GHL MIDI format help)
  * DJ Shepherd (X360)
  * arkem (py360)
  * and many others!

Onyx is free software via the GNU General Public License v3; see LICENSE.txt.

New versions and source at: https://github.com/mtolly/onyxite-customs/releases

## Requirements

Windows version requires .NET and (I think) the Visual C++ runtime.
(If you can run Magma, you're good.)

Mac version requires Wine, Mono, and Mono's `libgdiplus` library.
You can use the official Mac installers for Wine and Mono; Mono's will install
`libgdiplus` as well. Or you can use Homebrew, by installing the packages
`wine`, `mono`, and `mono-libgdiplus`. The app is compiled with High Sierra
(10.13.6) so you probably need at least that version.

## Instructions

On Windows, run the installer, then run `onyx.exe` (or the installed shortcut).
On Mac, move `Onyx.app` to your Applications folder, and then run it.

## Supported Formats

Onyx can import the following song formats:

  * Rock Band 1/2/3 STFS (CON or LIVE) files

    * Currently only single-song files (not packs) are supported.

  * Magma (v1 or v2) RBA files

  * Frets on Fire / Phase Shift / Clone Hero formats

    * Both `notes.mid` and `notes.chart` are supported.
    * Both OGG Vorbis and MP3 audio are supported.
    * `song.ini` can be absent if using `notes.chart`.
    * Audio files must be named according to PS/CH conventions.

## Batch Mode

This is currently where most of Onyx's functionality is.

First, load files into the Songs tab.
You can drag and drop files or folders onto the list, or browse with Add Song.
Folders will be searched for any recognized songs inside.
To remove a song, select its row in the tree, and press the Delete key.

  * Rock Band 3 (360)

    Creates either a Rock Band 3 CON file or a Magma v2 project.

    Here is a sample of the steps performed when importing from FoF/PS:

      * Add appropriate `[music_start]`, `[music_end]`, and `[end]` events
      * Generate an automatic `BEAT` track from MIDI time signatures
      * Add auto-generated (roughly CAT-quality) lower difficulties if missing
      * Import as much metadata as possible from `song.ini`
      * Apply the correct `delay` value from `song.ini` to the audio
      * Delay the song start by a few seconds if notes are present too early
      * Detect double drum roll lanes using the single lane note and fix them
      * Alter the tempo map to fix some instances of too-slow/too-fast tempos
      * Convert 5-lane drums to 4-lane using the standard Phase Shift rules
      * Convert Phase Shift "Real Drums" to RB by removing hihat pedal notes
      * Convert tap notes to HOPO notes
      * Remove some overdrive phrases if they produce invalid unison phrases

    If the "automatic tom markers" option is turned on, an FoF song that doesn't
    have any tom markers and that does not have "pro_drums = True" in the
    song.ini will have tom markers added over the whole drum part, under the
    assumption that Pro Drums have not been authored for the chart.

    Open notes from a PS or CH song will be translated to green notes.
    If the "drop open HOPOs" option is turned on, HOPO and tap open notes will
    be removed. This is ideal for songs with tap sections that alternate
    between fretted notes and open notes.

    The song speed can be modified to produce sped up or slowed down songs.
    This will modify both the audio and MIDI, and add (X% Speed) to the title.
    For CON inputs this only works with unencrypted MOGG files.
    Different speed versions may not pass Magma in all cases, if certain events
    become too close together or too far apart.
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
    It also applies most of the C3 template to the MIDI tracks,
    so you get note names, colored tracks, and RBN preview windows.
    Because this must generate WAV audio files to give to Magma,
    encrypted MOGG files in input rb3cons are unsupported.

    Tip: to process a song with C3-encrypted MOGG, one option is to use
    the Phase Shift converter in C3 CON Tools which mixes the audio down to a
    single file, and then just supply the Phase Shift song to Onyx.

  * Rock Band 2 (360)

    Creates a CON file for Rock Band 2, using the following conversions:

      * removes RB3 added features like pro instruments, harmonies, bass solos,
        trill/tremolo, new drum animations
      * any 2-instrument unisons are made into 1-instrument OD phrases
      * converts VENUE to RB2 format
      * ensures lower difficulties have all colors used on Expert
      * adds (RB2 version) to the title to disambiguate when in RB3

    Then it will attempt to validate the song through Magma v1.
    If this succeeds, you're good to go.
    If it fails, it will still continue and simply copy the MIDI as-is.
    The result will still probably work; you just won't get lipsync animations.

    You can choose to drop the Keys part, or move it to Guitar or Bass.
    In the latter two cases, the RB3 "keytar" algorithm is more-or-less applied,
    so fast chords become HOPOs, and overlapping sustains are shortened.

  * Clone Hero/Phase Shift

  * Rock Band 3 (Wii)

    Converts a collection of songs to a single RB3 pack in the format used by
    the Dolphin Wii emulator. This is primarily intended for people using
    Dolphin to record videos of songs.

    Options are available for certain MIDI transformations useful for videos:

      * Remove activation drum fills
      * Remove the Mustang version of a Pro Guitar/Bass track, ensuring the
        Squier version (supporting more than 17 frets) will always be played
      * Unmute any muted Pro Guitar/Bass notes over fret 22. This allows you
        to include such notes while compiling with Magma, and then unmute
        them for recording the video

  * Preview

    Generates a JavaScript (Canvas) chart preview app for web browsers,
    which plays back the audio and displays all tracks in 2D "Beatmania" style.

    Supports all Rock Band 3 instrument tracks, including Pro Guitar/Bass/Keys,
    as well as the Clone Hero 6-fret (GHL) mode, and 5-lane drums.

    A player folder will appear next to the CON; open `index.html` to run.
    It can be run locally via `file://`, or hosted on a web server.

    Colors used for drawing the display can be tweaked by editing the file
    `customize.js`. In the future this will be improved to allow overriding
    all pixel sizes as well.

## Song View

In future releases, this will be the primary method of interacting with Onyx.

  * Auto reductions

    Generates CAT-like automatic reductions for empty difficulties in a MIDI.
    Quality is not guaranteed, but they should pass Magma.

    To use, ensure that there are no notes or events authored
    for a difficulty you want to be filled in.
    (For Pro Keys, remove the `PART REAL_KEYS_?` track entirely.)
