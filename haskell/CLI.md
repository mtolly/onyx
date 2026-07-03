# Onyx command line

Onyx can be used without the GUI. The full toolkit binary (`onyx`) defaults to
the command line; launch the graphical app with `onyx gui` or `onyx open`.

For a headless build that does not depend on FLTK, build the standalone
`onyx-cli` executable instead (see [BUILD.md](BUILD.md)).

## Building `onyx-cli`

After the normal dependency setup in [BUILD.md](BUILD.md):

```bash
cd haskell
./stack-local build onyx-cli
```

The binary is installed under `.stack-work/install/.../bin/onyx-cli`.

On Windows, `./package-cli` packages a standalone `onyx.exe` with bundled DLLs.

## Usage

Run with no arguments to print help and a list of commands:

```bash
onyx-cli
# or, if you built the full toolkit:
onyx
```

Common options (available on most commands):

| Option | Description |
|--------|-------------|
| `--to PATH` | Output file or directory |
| `--target NAME` | Build target (e.g. `ps`, `gh5`, `rb3`) |
| `--force` | Overwrite existing output |

Run `onyx-cli COMMAND` with no file arguments for command-specific help.

## `from-audio`

Create a Guitar Hero / Clone Hero song package from a single audio file. This
wraps the normal Onyx project and build pipeline: it copies the audio into a
project folder, writes `song.yml`, and compiles the requested target.

```bash
onyx-cli from-audio song.wav [--to output.zip] [--target ps|gh5]
```

| Target | Default output | Game format |
|--------|----------------|-------------|
| `ps` (default) | `.zip` or folder | Clone Hero / Phase Shift |
| `gh5` | `.pkg` or folder | Guitar Hero Warriors of Rock |

Supported input formats: WAV, FLAC, OGG, MP3.

Example — Clone Hero zip:

```bash
onyx-cli from-audio galileu.wav --to galileu_gh.zip
```

Example — GH Warriors of Rock package:

```bash
onyx-cli from-audio galileu.wav --to galileu_gh5.pkg --target gh5
```

A convenience wrapper is provided at the repo root:

```bash
./scripts/gh-from-audio.sh galileu.wav --to galileu_gh.zip
```

`from-audio` packages the audio and an empty chart. To add playable notes,
import MIDI or chart data into the generated project (or use `onyx import` on
existing chart files) and run `onyx-cli build` on the project's `song.yml`.

## Other commands

`onyx-cli` exposes the same command set as the GUI toolkit, including:

- `import` — convert external formats into an Onyx project
- `build` — compile `song.yml` for a target
- `import-audio` — create a project folder from one or more audio files
- `reaper` — generate a REAPER project from a song

See `onyx-cli` with no arguments for the full list.

## GUI vs CLI

| Invocation | Behavior |
|------------|----------|
| `onyx` | Command line (prints help if no args) |
| `onyx gui` | Launch graphical toolkit |
| `onyx open [files...]` | Launch GUI and open files |
| `onyx-cli` | Headless CLI only (no GUI code linked) |
