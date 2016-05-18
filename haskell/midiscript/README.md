# MIDIScript

`midiscript` is a program to translate MIDI files to/from a readable, editable
plain text format. Goals include:

- An easy way to make precise edits to existing MIDI files. For example, taking
  a chunk of events and modifying their positions by the same amount, or editing
  the time signatures of a file without changing the position of the events.

- A way to verify how quantized MIDI events are. Every position in the file is
  written as a precise fraction, not as a rounded decimal like many sequencers
  do, so events that are one tick off from a beat line become very obvious.

- Making it much easier to programatically generate MIDI files; such a program
  can generate text, without having to understand the MIDI binary format.

- The ability to use standard text-processing tools on MIDI files. For example,
  `diff` can show the differences between MIDI files, and this in turn can
  be used by version control systems like Git.

## Syntax

MIDIScript features a lightweight, flexible syntax inspired by that of CSS and
[sng](http://sng.sourceforge.net/).

Each track starts with a string literal (its name), or the keyword `tempo`:

    "foo" { ... events ... }
    tempo { ... events ... }

`midiscript` uniquely identifies tracks based on their names. Every track (other
than the first track, containing tempo and time signature events) must
have a name, and if two tracks have the same name they get merged into one.

Each line of a track is of the form:

    position: event1, event2, event3;

An event can take many forms. Some examples:

    ch 0 on 60 v 50    (channel 0, note on for pitch 60, velocity 50)
    off 60             (use default channel, and default off-velocity of 0)
    text "Yeah!"       (a text event, taking a string literal)
    time (5 : 4)       (set time signature to 5/4)
    time (6, 2, 24, 8) (set time signature to 6/4, using the long MIDI form)

A position (in fact, any number) can be entered in multiple formats:

    5       (either a simple number, or a position expressed in beats)
    2+(1/2) (arbitrary arithmetic expressions are allowed)
    2|1.5   (first skip two measures from start of song, then add 1.5 beats)
    30s     (seconds, converted to a position in beats)

The `measure | beats` format uses the time signature events in the tempo track,
and the `seconds s` format uses tempo change events. Note that both measures and
beats are 0-based, not 1-based. So `0|0` is the very beginning of the song.

When interpreting positions that use a measure number, any time signature events
that aren't on a measure boundary are ignored. If there is no time signature
event at position 0, a default of `4/4` is used. Similarly, if there is no tempo
change event at position 0, the default is 120 beats per minute.

To avoid circular logic, time signature events in the tempo track must be
positioned using only numbers of the forms `x` or `x|y`, and the values inside
must be of the form `x`, where x and y are non-negative numbers (or expressions
involving such). Otherwise the following could result:

    tempo {
      0: time ((1|0) - (0|0) : 4)
      # circular reference!
    }

Similarly, tempo change events must have positions and values that either 1) do
not contain a value in seconds (`seconds s`), or 2) are of the exact form `x s`.

A particularly powerful kind of event is the "subtrack":

    "foo" {
      4: {
        0: on 60;
        1: off 60;
      };
    }

This inserts a timeline of events into the master timeline at a given point. So
this is equivalent to:

    "foo" {
      4: on 60;
      5: off 60;
    }

The events in a track don't have to be in ascending order -- they are sorted
before assembling the MIDI file, so they can be in any order. You can also use
negative numbers as long as the final position of a MIDI event is non-negative:

    "foo" {
      4: {
        0: off 60;
        -1: on 60;
      }
    }

Events that take a channel can have the channel elided. When this is done, their
channel can be given by the following form in any surrounding track braces:

    "Drums" ch 9 { ... events ... }
    # channels start from 0

This same syntax can be used for subtracks. If no such default channel is set in
an enclosing track it is set to 0.
