# Onyx Music Game Toolkit (version _ONYXVERSION_)

By Michael Tolly <onyxite@gmail.com>

Onyx is a multipurpose build tool for Guitar-Hero/Rock-Band-like music games.
It can import and export a wide variety of formats, move parts around,
change song speed, fill in missing details, and assist with song authoring.

Many people have contributed code, research, and more to Onyx; see CREDITS.txt.

New versions and source at: <https://github.com/mtolly/onyxite-customs/releases>

Onyx is free software via the GNU General Public License v3;
see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.en.html

All original non-code assets such as graphics are licensed under
Creative Commons Attribution-ShareAlike:
https://creativecommons.org/licenses/by-sa/4.0/

## Requirements

Onyx is currently compiled on:

  * Windows 10
  * macOS Monterey
  * Linux kernel 4.4 (Ubuntu 16.04)

It may work on previous versions (such as Windows 7) but this is not certain.

By default, an embedded copy of Magma v2 is used to generate RB3 or RB2 files.
This requires the Visual C++ runtime, and if on Mac or Linux, Wine.

  * On Mac, you can use the official installer, or you can use Homebrew
    (`brew install wine`).

    However, macOS Catalina (10.15) does not support running Magma in Wine due
    to its lack of support for 32-bit programs. To produce Rock Band files on
    this OS you'll need to turn Magma compilation off.

  * To bypass Magma compilation, go to `Edit > Preferences` in the top menu, and
    change the Magma selection to an option other than "Magma required". Please
    make sure and test any resulting files! If you're on Windows I recommend
    leaving it on "Magma optional", so you can see any error messages; they'll
    be converted to warnings and you'll still get the resulting song file.

  * Wine is also required to produce Guitar Hero: Warriors of Rock files on Mac
    and Linux at the moment, due to the audio encoding process. This may be
    fixed in the future.

The Linux build is produced via an Ubuntu 16.04 Docker container, and should
work on most any modern distribution. Older distributions might require
additional installations:

  * On CentOS 7, `yum install mesa-libGL fontconfig` is required.

## Instructions

  * On Windows, run the installer, then run `onyx.exe`
    (or the installed shortcut).

  * On Mac, move `Onyx.app` to your Applications folder, and then run it.

  * On Linux, make the `.AppImage` executable, place it wherever you like,
    and run it. Your system may also offer to install desktop integration.

  * Both Windows and macOS may label the program as malware or untrustworthy.
    Hopefully there should be ways of working around this.
    On Mac, right click `Onyx.app` and select Open, to add a security exception.

## Supported Input Formats

Onyx can import the following song formats:

  * Rock Band 1, 2, or 3 Xbox 360 STFS (CON or LIVE) files

    * Both single-song files and packs are supported.

  * Rock Band 1, 2, or 3 PlayStation 3 .pkg files

    * Official DLC whose .mid.edat are encrypted with RAP files are supported.
      Copy RAP files to `onyx-resources/raps/CONTENTID.RAP`.

  * Rock Band 1 and 2 PlayStation 2 extracted .ARK contents

    * First extract the ISO. Use something like Apache3 to get the 2nd layer
      of the dual layer discs, as most programs do not support this.
    * Then extract the ARK contents using something like arkhelper.
    * Finally drag and drop the extracted folder directly onto the "Load a song"
      button to import, or into the Batch process window.

  * Rock Band 4 PlayStation 4 extracted files
    (tentative support, may not work for everything)

  * Magma (v1 or v2) RBA files

  * Frets on Fire / Phase Shift / Clone Hero formats

    * Both `notes.mid` and `notes.chart` are supported.
    * OGG Vorbis, MP3, Opus, and WAV audio are supported.
    * `song.ini` can be absent if using `notes.chart`.
    * Audio files must be named according to PS/CH conventions.

  * Magma (v1 or v2) projects (.rbproj)

  * Guitar Hero, Guitar Hero II, or Guitar Hero Encore: Rocks the 80s (PS2)

    * Extract the `.iso` contents, and then import either `GEN` or `MAIN.HDR`.
    * The hidden songs Trippolette and Graveyard Shift can be imported from GH1.

  * Guitar Hero II (Xbox 360) DLC (LIVE files)

  * Guitar Hero 5 and Guitar Hero: Warriors of Rock DLC
    (Xbox 360 LIVE files or PlayStation 3 .pkg files)

  * Guitar Hero III disc (Xbox 360, PS2) and DLC (Xbox 360)

    * For Xbox 360 disc: extract ISO, then import from `default.xex`.
    * For PS2 disc: extract ISO, then import from `DATAP.HED`.

  * DTXMania (DrumMania / GuitarFreaks) simfiles (.dtx/.gda)

    * XG features (5-fret guitar/bass, extra drum pads) are supported.
    * Drums are translated to Pro Drums as well as Phase Shift Real Drums.
      (Real Drums display in the web player but aren't yet in PS output.)
    * Keysounded audio is rendered to stem files.
    * Currently only each instrument's highest difficulty from a `set.def`
      is imported as Expert, and difficulties below this are autogenerated.
      To import a specific difficulty directly, import its `.dtx` file.

  * Power Gig: Rise of the SixString (Xbox 360, PS3)

    * Import from `Data.hdr.e.2`.
    * Xbox 360 DLC is also supported.

## Single Song Mode

Click "Load a song", and browse to a supported song file.
You can also drag and drop a file onto the button, or onto the app icon on Mac.

  * The *Metadata* tab displays loaded metadata from the source song.
    You can edit data here to have it reflected in any converted output.

  * The *Instruments* tab shows all the gameplay modes Onyx found in the song.
    Modes are sorted first by logical instrument, and then the specific
    controllers that instrument supports.

    Deselecting an instrument mode checkbox will hide it from any output files.

    In addition to difficulty selections, some controller modes have other
    specific options, such as drum freestyle sounds or number of vocal parts.

  * The *Preview* tab lets you view and listen to supported gameplay tracks
    in a full 3D graphical display. Currently 5-fret guitar/bass/keys (including
    tap/open notes) and 4-lane drums (including cymbals) are supported, with
    more to come.

    You can configure MSAA and FXAA antialiasing in `Edit > Preferences`.

    Gem models and textures created by Inventor211. Most textures, models, and
    shaders are customizable, inside the `onyx-resources` folder.

      * Windows: `C:\Program Files\OnyxToolkit\onyx-resources` or similar
      * Mac: View contents of `Onyx.app`, then `Contents/MacOS/onyx-resources`
      * Linux: Extract `.AppImage`, then `usr/bin/onyx-resources`

    The file `3d-config.yml` contains settings you may also want to edit.

  * The *Stars* tab contains score cutoff calculators for Rock Band 3,
    Guitar Hero (1), and Guitar Hero 2 / 80s, with more games planned to come.
    Select the tracks you want to include, and star cutoffs will be computed.

  * The *RB3*, *RB2*, *CH/PS*, *GH1*, *GH2*, and *GH:WoR* tabs convert your song
    to the given game's format. All tabs let you change the song speed (if
    supported), and can also redirect any instrument parts to new "slots". For
    example, in a Rock Band 3 file, the 5-fret tracks for guitar, bass, and keys
    can be moved around, or even applied to more than 1 slot.

    Note that if your song is a Rock Band file with an encrypted MOGG, you can
    only convert it to a Rock Band output format at 100% speed; Clone Hero
    format and/or modifying the speed are not supported.

    See the "Batch Mode" section for more details on each output format.

  * The *Utilities* tab contains functions other than song conversion. See
    their descriptions in the "Other Tools" section below for more info.

## Batch Mode

First, load files into the Songs tab.
You can drag and drop files or folders onto the list, or browse with Add Song.
Folders will be searched for any recognized songs inside.
To remove a song, select its row in the tree, and press the Delete key.
To change the order of songs, select its row and press Ctrl+Up or Ctrl+Down
(Windows/Linux), or Cmd+Up or Cmd+Down (Mac).

At the bottom of the tab are toggles for whether specific instruments should
be imported. These can be helpful if unnecessary instrument parts are causing
compilation errors.

  * Rock Band 3 (360, PS3)

    Creates either Rock Band 3 CON (360), PKG (PS3), or Magma v2 project.

    Here is a sample of the steps performed when importing from FoF/PS/CH:

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
      * Remove some overdrive phrases if they produce invalid unison phrases,
        have no notes under them on any difficulty, or have no notes between
        two adjacent phrases on any difficulty

    If the "automatic tom markers" option is turned on, an FoF song that doesn't
    have any tom markers and that does not have "pro_drums = True" in the
    song.ini will have tom markers added over the whole drum part, under the
    assumption that Pro Drums have not been authored for the chart.

    Open notes from a PS or CH song will be translated to green notes. Sequences
    of notes adjacent to open notes will be moved up one fret if necessary to
    preserve motion.

    The song speed can be modified to produce sped up or slowed down songs.
    This will modify both the audio and MIDI, and add (X% Speed) to the title.
    For CON inputs this only works with unencrypted MOGG files.
    Different speed versions may not pass Magma in all cases, if certain events
    become too close together or too far apart.
    If this happens, you can produce a Magma project and then make the needed
    changes before compiling with Magma.

    A few options are available for moving/copying Guitar/Bass/Keys parts,
    as well as converting drums parts into guitar for an interesting challenge.
    When moving Keys parts to Guitar/Bass, force notes will be applied so charts
    keep their correct strum/HOPO notes according to the RB3 keytar algorithm.
    Note that in some cases (when the input and output both use MOGG audio)
    the output may assign multiple instruments to the same audio channel
    indexes, which works in game but will cut out the audio if either player
    misses.

    By default, an input format like Phase Shift that can contain both 1x and
    2x Bass Pedal drums charts will generate two separate songs. If you are
    interested in only 1x or only 2x, select the appropriate Bass Pedal option.
    Songs that are detected as 2x Bass Pedal versions can also produce an
    automatic 1x version, using an algorithm to remove certain kick notes.

    When creating a Magma project, a REAPER project will also be generated
    so any remaining problems can be quickly edited and re-exported to MIDI.
    This avoids a few bugs in REAPER's own MIDI import function.
    It also applies most of the C3 template to the MIDI tracks,
    so you get note names, colored tracks, and RBN preview windows.
    Because this must generate WAV audio files to give to Magma,
    encrypted MOGG files in input rb3cons are unsupported.

    Tip: to process a song with a C3-encrypted MOGG, one option is to use
    the Phase Shift converter in C3 CON Tools which mixes the audio down to a
    single file, and then just supply the Phase Shift song to Onyx.

  * Rock Band 2 (360, PS3)

    Creates a CON file for Rock Band 2, using the following conversions:

      * removes RB3 added features like pro instruments, harmonies, bass solos,
        trill/tremolo, new drum animations
      * any 2-instrument unisons are made into 1-instrument OD phrases
      * converts VENUE to RB2 format
      * ensures lower difficulties have all colors used on Expert

    Then it will attempt to validate the song through Magma v1.
    If this succeeds, you're good to go.
    If it fails, it will still continue and simply copy the MIDI as-is.
    The result will still probably work; you just won't get lipsync animations.

    You can choose to drop the Keys part, or move it to Guitar or Bass.
    In the latter two cases, the RB3 "keytar" algorithm is more-or-less applied,
    so fast chords become HOPOs, and overlapping sustains are shortened.

  * Clone Hero/Phase Shift

    Generates a song folder (or zip file containing one) in Phase Shift format.
    These should play correctly in the latest free version of Phase Shift,
    the Steam version of Phase Shift, and the latest version of Clone Hero.
    Songs may also work in Frets on Fire X, but this is not guaranteed.

    Due to how the conversion is implemented, some non-RB features such as Dance
    mode, Real Drums, and Real Keys will not survive a PS-to-PS conversion.
    However other 5-fret charts such as `PART RHYTHM`, as well as Clone Hero
    6-fret tracks, should remain.

  * Guitar Hero, Guitar Hero II

    Includes a variety of formats for the Harmonix GH games on PS2 and Xbox 360:

      * Add a new bonus song to a PS2 GH1/GH2/GH80s ARK file.
        GH2 360 ARKs are not supported currently.

      * Create a folder of files ready to add manually to an ARK.
        This also does not yet support GH2 360 format.

      * Create a DLC LIVE file for GH2 for Xbox 360.
        This requires an RGH or JTAG console to load.

        NOTE: GH2 is only able to load 16 individual DLC package files. Trying
        to load more than 16 will not only fail to load the ones past 16, but
        the game will corrupt your save data when saving. To avoid this, please
        use the "Make a pack (360)" tool in the Quick Convert window to combine
        songs before playing.

    PS2 songs have been tested with standard GH1/GH2, as well as Guitar Hero II
    Deluxe. For Guitar Hero II Deluxe 2.0, you can also include drum charts.

    For PS2 GH2 output, practice mode audio generation can be toggled on/off.
    Disabling it saves some space, and a significant amount of conversion time.
    If disabled, practice mode will still work, just with no audio. If enabled,
    songs with no instrument stems will use the full track as their practice
    audio.

    In batch mode, lead and coop parts are assigned automatically based on
    what parts are present.

      * Lead will be assigned to the first of [guitar, rhythm, keys, bass]
      * Coop will be assigned to the first of [rhythm, bass, keys]
        which is not already used for Lead

    To customize part assignment, use the "Load a song" mode.

  * Guitar Hero: Warriors of Rock

    Creates DLC files for Guitar Hero: Warriors of Rock, for use on a jailbroken
    Xbox 360 (RGH or JTAG) or PlayStation 3.

    * There appears to be a limit of slightly over 700 songs' metadata in one
      file; any more causes a crash on game launch.

      **IMPORTANT**: In addition, it may be possible to corrupt a save somewhat
      by loading too many new songs over multiple sessions. When this happens,
      loading any more new songs will crash the game. So, it is recommended you
      back up any save data you care about before loading custom songs, in case
      this happens, until the issue can be researched in more detail.

    * After creating DLC files, you *must* take your files, plus any WoR DLC you
      want, and create a "WoR Song Cache" from them. See the Other Tools section
      for information. This is required to load your songs!

    * Guitar and bass support some of the Clone Hero feature set, with caveats.

      * Extended sustains can only move upward (hold a lower note and play
        higher notes). Ones that move downward will be trimmed back.
      * Tap notes only support single fretted notes, likely for compatibility
        with the slider control. Open notes and chords will become HOPOs.
      * Open notes work on both guitar and bass, but HOPO open notes only work
        on bass for some reason. On guitar, they become strums.

    * Drums can be created from 4-lane, 5-lane, Pro Drums, or Real Drums inputs.

      * Pro/Real can be converted to 5-lane, or simply copied as 4-lane.
        Select the appropriate option on the WoR conversion tabs.
      * Pro to 5-lane conversion is simple, but will be improved in the future:
        * Yellow cymbal becomes yellow
        * Green cymbal becomes orange
        * Blue cymbal becomes orange, or yellow if there's already an orange
        * Toms work the same, targeting blue and green
      * Drum sustains are not supported yet. In the future, these should be
        generated from Rock Band style drum lanes.
      * Freestyle sections during chart gaps are not added yet
        due to some technical difficulties.
      * Velocity (accent and ghost) is supported if the Clone Hero dynamics
        event is present, and 2x Bass Pedal charts are translated to Expert+.
        A future version will add control over whether to restrict the ghost
        notes to Expert+, or discard the ghost information and include them on
        Expert.

    * Vocals work basically the same as Rock Band, translating phrases, talkies,
      and slides. Phrases are encoded in a slightly different way;
      longer stretches of empty space are broken up into multiple empty phrases,
      and short gaps between phrases are appended to the following phrase.

    * Band animations and lighting/camera are not controlled yet, so a generic
      camera cut rotation is used, and the drummer and singer do not have
      animations.

    * In batch mode, guitar and bass will be selected automatically from the
      available tracks. If there is no suitable track for bass, guitar will be
      copied to bass due to the lack of open pulloff support on guitar.

    * Stems are not yet supported; all audio goes to the backing track. This
      will be fixed in a future release.

      In very long audio tracks, there may be tiny gaps in the audio, almost
      unnoticeable, due to stitching encoded segments back together.

      The stemless audio may behave oddly in multiplayer. It appears the backing
      track is moved towards the center (mono), while the instrument tracks are
      positioned in space to match their highway position. Currently researching
      whether this behavior can be disabled.

    * Song titles and artists may sometimes get a prefix added in front;
      this is to prevent a game crash when sorting by title or artist caused by
      non-ASCII characters (even those that are supported by the game fonts).

  * Preview

    Generates a JavaScript (Canvas) chart preview app for web browsers,
    which plays back the audio and displays all tracks in 2D "Beatmania" style.

    Supports all Rock Band 3 instrument tracks, including Pro Guitar/Bass/Keys,
    as well as Clone Hero's 6-fret (GHL) mode, 5-lane Drums, and PS Real Drums.

    A player folder will appear next to each song; open `index.html` to run.
    It can be run locally via `file://`, or hosted on a web server.

    Colors used for drawing the display can be tweaked by editing the file
    `customize.js`. In the future this will be improved to allow overriding
    all pixel sizes as well, so images will be further customizable.

## Quick Convert

This tool can perform several different kinds of processing/conversion on
existing Rock Band format songs, without doing a full import/export process.
Some examples:

  * Convert between Xbox 360 and PS3 formats, as well as to Dolphin/Wii
  * Encrypt or decrypt `.mid.edat` files in PS3 format
  * Combine songs into packs, including combined songs.dta
  * Extract individual songs from packs
  * Various MIDI transformations useful for gameplay or preview recording:
    * Replace `VENUE` track with a mostly blacked-out background
    * Remove overdrive, trill/tremolo lanes, drum activation fills
    * Remove the Mustang version of Pro Guitar/Bass tracks, ensuring the Squier
      version will always be played
    * Unmute muted Pro Guitar/Bass notes over fret 22, which allows you to
      include such notes when compiling under Magma, and then unmute for
      recording a preview video

The following formats are supported as inputs:

  * Xbox 360 STFS (CON or LIVE)
  * PS3 .pkg
  * Extracted contents of 360 or PS3 format songs
  * Magma (v1 or v2) RBA

And these are supported as outputs (depending on mode selection):

  * Xbox 360 STFS (CON or LIVE)
  * PS3 .pkg
  * Dolphin/Wii .app files

Drag supported input files in, and they should show up if songs are properly
recognized. Then select an output mode, MIDI/milo transformation settings on the
right-hand side, and use the output controls to begin processing.

Other tabs under the Quick Convert window:

  * 360 Pack Creator

    Combines Xbox 360 CON/LIVE files into packages containing multiple songs.
    Works with both Rock Band, and Guitar Hero II. (Rock Band songs can also
    be combined with the main Quick Convert system, which can enforce a
    maximum size per pack.)

    Packs are useful for RB songs because they can load faster on game launch.
    For GH2, they are required due to a hard limit of 16 total package files.

    To use, drag in the existing files to be combined, give the package a name
    to identify it, and click the appropriate button below: CON packs for
    RB2 or RB3 (TU4 or RB3 Enhanced), LIVE packs for GH2.

    All files from the inputs are combined into one tree for the package.
    If more than one input contains the same file:
      * If the file ends in `.dta`, the contents are combined into one file.
      * If the file is `spa.bin` (seen in GH2 360 files), it is ignored.
      * Otherwise, an error is raised.

    The game information (name, title ID, thumbnail) are taken from the first
    input package in the list.

  * Direct CON->PKG

    This is an older method of converting Xbox 360 (CON/LIVE) files directly to
    PS3 .pkg files. This can usually be done by the main Quick Convert window
    with more options, however this tool can also work on things that are not
    valid standalone songs, such as Pro Guitar/Bass upgrades.

## Other Tools

  * Fill in automatic lower difficulties (MIDI function)

    Generates CAT-like automatic reductions for empty difficulties in a MIDI.
    Quality is not guaranteed, but they should pass Magma.

    To use, ensure that there are no notes or events authored
    for a difficulty you want to be filled in.
    (For Pro Keys, remove the `PART REAL_KEYS_?` track entirely.)

    If a drum track with no animations is present, drum animations will also
    be generated. These are of somewhat higher quality than the ones Magma can
    generate, as they will always handle tom markers correctly, and have more
    of a preference for double strokes rather than crossing hands.

  * Find hanging Pro Keys notes (MIDI function)

    Looks for cases in a Pro Keys track where a range shift occurs less than
    1 second before a note that is not visible in the previous range.

  * Make Reaper project with RB template (MIDI function)

    Imports a MIDI file into a REAPER project. It avoids a few bugs in REAPER's
    own MIDI import function, and applies track names and note colors similar to
    the RBN or C3 project templates.

  * MOGG/VGS creator

    Generates an unencrypted MOGG or VGS file from a set of input audio files.
    Created MOGG files should work on Rock Band games, Guitar Hero II for Xbox
    360, and newer games such as Audica, with a proper seek header. Created VGS
    files should work on the PS2 games GH1, GH2, and GH80s; I have not tested
    with PS2 Rock Band games.

    The channels of all the input files, which can be WAV, OGG, MP3, or FLAC,
    will be merged into one multichannel file. As with the batch load screen,
    you can select a song row, and press Delete to remove it, Ctrl+Up to move it
    up in the list, or Ctrl+Down to move it down.

    VGS files can be created with inputs of differing sample rates, but it is
    recommended that all your rates be multiples of a large common factor so
    that channels are interleaved in a regular pattern. MOGG files must have the
    same sample rate among all input files.

    As a special case, if you provide a single OGG file and convert to MOGG, it
    will not be reencoded, but will go straight into the MOGG.

  * Lipsync file generation

    Use simple phonetic analysis to convert from a vocals chart to a lipsync
    file. Uses a dictionary to look up English lyrics, or spelling-based rules
    for German or Spanish lyrics. English supports these phonetic features at
    the moment:

      * RB3: simple vowels, vowel diphthongs, consonants
      * TBRB: simple vowels, vowel diphthongs
      * GH2: simple vowels

    Supports generating `.voc` files (used in GH2), as well as `.milo_*` files
    for later RB games.

    There is also a MIDI track format for encoding lipsync information, which
    lets you make manual edits before converting the rest of the way to the
    animation files. The workflow is as follows:

      1. Turn vocal tracks into LIPSYNC* tracks for either RB3 or TBRB.

      2. Edit the text events to modify the animations. The meaning of text
        events is as follows:

        * `[viseme_name N]` moves linearly from weight N to this viseme's next
          event

        * `[viseme_name N hold]` continues weight N until this viseme's next
          event

        * `[viseme_name N ease]` moves from weight N to this viseme's next
          event, using an "easeInExpo" transition function

      3. For TBRB, rename the tracks from LIPSYNC1, LIPSYNC2, etc. to one of
        LIPSYNC_JOHN, LIPSYNC_PAUL, LIPSYNC_GEORGE, or LIPSYNC_RINGO.

      4. Make or update a .milo_* file, and it will use the LIPSYNC tracks
        as input.

  * Lipsync dry vocals audio creation for Magma

    Two options for generating audio files to give to Magma's vocal animation
    generator.

      * Sine wave dryvox: the pitches from the vocal track become simple pitch
        tones. This is used by Onyx for Magma v1 (RB2) which is picky about
        matching the charted vocal pitches to the dry vocal file, and can error
        if it thinks they do not match.

        Currently slides are not yet supported.

      * Clipped dryvox: an input audio file, either a mixed song file or an
        isolated vocal file, is clipped so audio is only present during the
        vocal notes. This is used by Onyx when exporting Magma v2 projects.

  * `.milo` unpack and repack

    Allows editing files inside the `.milo` archive format seen in more recent
    RB song files, which can contain lipsync, venue information (RB3 onward),
    and tweaks to character animation + vocal assignment.

    Only the `ObjectDir` format (used in song files) is supported; other milos
    found on game discs will likely not work.

  * RB3 song cache ID application

    When a Rock Band custom song contains a symbol rather than a number in the
    `song_id` field, a random number is generated by the game, and saved in the
    song cache file. This ID is used for associating songs with scores. If you
    regenerate the song cache file, new IDs will be assigned, and the scores
    will be disconnected.

    This tool allows you to hardcode the random IDs back into the song files,
    replacing the symbol IDs that they originally had. Then, even if the song
    cache is regenerated from scratch, all scores will still be associated with
    their songs.

  * GH:WoR song cache generation

    This is a required step to load more than one custom song into Warriors of
    Rock. The game loads song metadata by finding the highest ID DLC package,
    and getting all metadata from that. Neversoft used this to avoid a lengthy
    song scan process that some of the Rock Band games had, by including all
    previous DLC information in each new DLC package that was released.

    So, to load customs, we create a "cache" file which is just a DLC package
    containing only metadata and no songs. This cache package will have a higher
    ID than any custom or DLC (the cache ID is set to 2 billion, and customs
    will be between 1 and 2 billion). You must provide all custom songs as well
    as any WoR-format DLC (GH5 DLC is not necessary). Then, simply place this
    cache package next to your DLC on your storage device.

    You can also use previous cache files as an input to create new ones, so you
    don't have to keep around already-processed customs on your computer.

    To create a cache using official DLC for PlayStation 3, you'll need to
    place the appropriate `.RAP` files in the `onyx-resources/raps` folder so
    the contents can be decrypted.
