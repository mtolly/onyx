# FLUID GUI layouts

Onyx's GUI is FLTK, driven from Haskell via `inline-c-cpp`. Historically every
widget was constructed by hand in Haskell, with positions computed by rectangle
arithmetic (`chopTop`, `splitHorizN`, `padded`, …). We are migrating that to
**FLUID**, FLTK's graphical layout editor: `.fl` files describe the layout, a
build step turns them into C++, and Haskell attaches behaviour to the widgets
they create.

The point is to be able to edit layouts visually instead of by arithmetic.
Behaviour stays in Haskell; only geometry, colours and static structure move.

---

## Where things live

```
haskell/packages/onyx-exe-toolkit/
  fluid/*.fl              layouts, edited with dependencies/root/bin/fluid
  gen/*.cxx, *.h          generated from fluid/ at build time (gitignored)
  Setup.hs                runs fluid, adds gen/ to the build
  cbits/onyx_fluid.*      runtime adaptation of generated windows
  cbits/onyx_l10n.*       localization hook the generated code calls
  src/Onyx/GUI/Fluid.hs   Haskell side of the above
scripts/fluid-dump        read real widget geometry off a running app
scripts/fluid-probe       screenshot a window's tabs
scripts/fluid-compare     diff two screenshots
scripts/fluid_patch.py    the GUI.hs patching the first two share
scripts/fluid-build.sh    the build step the first two share
```

---

## Build integration

`Setup.hs` runs `fluid -c` over every `fluid/*.fl` into `gen/`, and adds the
resulting `.cxx` to `cxxSources` plus `gen/` to `includeDirs` for the `onyx`
executable.

Two details worth knowing:

* Output goes to a staging directory first and is only copied over when the
  bytes actually differ, so an unchanged layout doesn't retrigger a C++ (and
  therefore Haskell) recompile on every build.
* fluid copies its `-h` argument verbatim into the generated `#include`, so it
  has to run with its working directory set to the output directory and be
  given bare filenames.

Generation happens in both `confHook` and `buildHook`: the former so the `.cxx`
exist before Cabal inspects the package, the latter so editing a `.fl` takes
effect on a plain build. The *list* of files is fixed at configure time, so
adding a brand new `.fl` needs a reconfigure; editing an existing one doesn't.

`gen/` is gitignored. The build always regenerates it.

---

## How a FLUID window is used from Haskell

FLUID generates a class with public member pointers, one per named widget.
Haskell constructs it and reaches in through `inline-c`:

```haskell
ui <- [C.exp| void* { new MiscWindow() } |]
let window = [C.pure| Fl_Window* { ((MiscWindow*)$(void* ui))->window } |]
    tabs   = [C.pure| Fl_Tabs*   { ((MiscWindow*)$(void* ui))->tabs } |]
```

The handle is kept as `void*` so no Haskell type is needed for the window class
itself. Nesting, where a layout does have it, is reached through in one
expression:

```haskell
[C.pure| Fl_Input* { ((MiscWindow*)$(void* ui))->midiFile->input } |]
```

Reusable panels (`FileLoadPanel`, `FilePickerRow`) *do* have `newtype`s in
`Onyx.GUI.Binding` with `cppTypePairs` entries, because they get passed around
as arguments.

Each migrated page takes a small record of its widgets instead of a `Rectangle`
and a `Ptr Fl_Group` — `MidiTabWidgets`, `MoggTabWidgets`, `LipsyncTabWidgets`,
`DryVoxTabWidgets`, `MiloTabWidgets`, `RB3CacheTabWidgets`,
`GH3CacheTabWidgets`, `WoRCacheTabWidgets`, `RenumberRRTabWidgets`,
`PackTabWidgets`, `CONtoPKGTabWidgets`.

---

## The three things FLUID can't express

### 1. Per-language horizontal stretch

Layouts are authored at natural English width. Each language has a
`stretch-width` factor (`Onyx.Localization`, `Language.stretchWidth`) that
scales every X position and width.

### 2. Menu bar space

On Windows and Linux a window needs ~30px at the top for its menu bar; on macOS
the menu lives in the system bar and takes no window space. Layouts are authored
*without* it.

Both are handled by one pass, `onyx_adapt_window` in `cbits/onyx_fluid.cpp`,
called via `Onyx.GUI.Fluid.adaptWindow` after the FLUID constructor and before
`show()`:

```haskell
menuHeight <- adaptWindow window
when (menuHeight /= 0) do
  inGroup (windowGroup window) $ void $ makeMenuBar windowWidth True
lockMinimumSize window
```

Implementation notes:

* Every target rectangle is computed **before** any resize is applied, then
  applied in **pre-order**. A group's `resize` drags its children along, so
  parents must be resized before children, and each child's own entry then
  overrides whatever the parent did to it.
* Widths come from scaled *edges* (`scale_x(x+w) - scale_x(x)`), so widgets that
  were adjacent or exactly filled their parent still do afterwards. This matches
  `stretchX`/`stretchW` in `Onyx.GUI.Core`.
* Rounding uses `nearbyint` (round-half-to-even) to match Haskell's `round`, so
  FLUID-built and hand-built widgets land on the same pixels.
* The menu offset stops at subwindow boundaries — a child window opens a new
  coordinate space. The stretch still applies inside it.
* `Fl_Center` positions its child itself, so it is re-run after the tree settles.
* Every group gets `init_sizes()` afterwards, since its cached bounds describe
  the pre-stretch layout.

**Note on window height.** The old code kept the window a fixed height and let
the menu bar eat into the content area. The new convention *grows* the window by
`menuBarHeight` instead, so macOS and Windows/Linux get an identical content
area. Migrated windows are therefore 30px taller on PC than they used to be, and
the reclaimed space goes to whatever was resizable.

### 3. Localization

FLUID has a gettext-style i18n mode where you choose the function name. Every
layout sets:

```
i18n_type 1
i18n_include {"onyx_l10n.h"}
i18n_gnu_function {onyx_l10n}
i18n_gnu_static_function {onyx_l10n_noop}
```

which makes the generated code wrap every label, tooltip and menu item:

```cpp
window = new Fl_Double_Window(500, 400, onyx_l10n("{song-list-title}"));
btnOpen->tooltip(onyx_l10n("{song-list-open.tooltip}"));
```

`onyx_l10n` (in `cbits/onyx_l10n.cpp`) translates only strings of the form
`{key}` or `{key.attribute}`, and passes everything else through unchanged —
so FLTK symbol labels like `@fileopen`, plain placeholder text and `""` are
safe. Unknown keys fall back to the braced text, which is also what the Fluent
implementation renders for a missing key.

It calls into Haskell (`onyx_l10n_hs`, a `foreign export ccall` in
`Onyx.GUI.Fluid`) and **interns** the result. Interning is required because
FLUID uses `label()`/`tooltip()`, not `copy_label()`, so the pointer must stay
alive; it's safe because the language is loaded once in `Main.hs` and changing
it only writes preferences (restart required).

`Main.hs` publishes the loaded localization via `setGlobalL`, because the
generated constructors run from C++ and can't see the `?loc` implicit parameter.

A fixed label or tooltip therefore belongs in the `.fl` as `{key}` and
`{key.tooltip}`, not in a Haskell `copy_label` / `setLabelTooltip` call after
construction. Both render the same text, but the layout version shows the real
wording in the editor, and it is one fewer line per widget. Only add
`tooltip {{key.tooltip}}` when the Fluent message actually has a `.tooltip`
attribute — an unknown key falls back to its own braced text, which as a label
looks like a bug and as a tooltip is one.

Text that varies per instance (counts, filenames, or the same panel used with
different wording in different tabs) is still set from Haskell with
`copy_label`. In those cases the `.fl` carries a plain English placeholder so
the editor stays readable.

---

## Custom widgets

Our own FLTK subclasses in `cbits/` are placed in FLUID by setting a widget's
**class** field and adding a global declaration:

```
decl {\#include "hs_dnd_button.h"} {public global
}
...
Fl_Button buttonOpen {
  label {{function-load}}
  xywh {10 400 310 30} color 3135616256 labelsize 13
  class HsDndButton
}
```

FLUID itself needs no rebuild — the editor shows the base widget, the generated
code instantiates the subclass. Requirements and gotchas:

* The subclass constructor must accept `(X, Y, W, H, const char* L = 0)`.
  `HsDndBox`, `HsTree`, `HsScrubber` and `HsGlWindow` were given the label
  parameter for this.
* **For any widget with a subclass set, FLUID unconditionally emits the full
  property set** — `box`, `color`, `selection_color`, `labeltype`, `labelfont`,
  `labelsize`, `labelcolor`, `align`, `when` — because it can't know the
  subclass's defaults. Anything the constructor sets gets overwritten. This bit
  us with `Fl_Simple_Terminal`, whose constructor sets `color(FL_BLACK)`; the
  layout has to restate it. (`Widget_Node.cxx`, all guarded by
  `|| !subclass().empty()`.)

Two widgets have no FLUID node of their own:

* **`Fl_Gl_Window`** doesn't exist in FLUID 1.5's palette at all. Use an
  **`Fl_Group`** node with `class HsGlWindow` — `Fl_Gl_Window` descends from
  `Fl_Group`, so FLUID emits the `end()` that a plain `Fl_Box` placeholder would
  omit. That matters: `Fl_Window` inherits `Fl_Group`'s self-`begin()`, so
  without it every subsequent widget silently becomes a child of the GL
  subwindow. Verified working, but not yet used in anger — the GL preview is in
  the not-yet-migrated song window.
* **`Fl_Simple_Terminal`** is only an `alt_type_name` alias; FLUID reads it as
  an `Fl_Terminal`. Use an **`Fl_Text_Display`** node with
  `class Fl_Simple_Terminal` — the actual base class, so only `Fl_Widget`-level
  properties are emitted and no text buffer is constructed to clash with the one
  the terminal makes itself.

---

## Reusable panels (`widget_class`)

FLUID's `widget_class` generates a real `Fl_Group` subclass with public members,
which can be referenced **from other `.fl` files** exactly like any other custom
widget: an `Fl_Group` node with the class field set, plus a `decl` including the
generated header. Nesting one inside another works too.

Current library:

| Class | File | Used by |
| --- | --- | --- |
| `FileLoadPanel` | `file_load_panel.fl` | 6 instances: MOGG, GH3 cache, GH:WoR cache, RR renumber, GH2 packs, CON→PKG |
| `FilePickerRow` | `file_picker_row.fl` | 6 instances: MIDI ×1, RB3 cache ×2, Lipsync ×1, Dry vox ×2 |

A `widget_class` earns its place when the same *contents* recur. The GH3 and
GH:WoR cache tabs once shared a `CacheTab` class, since they have the same
shape — a `FileLoadPanel` over three task buttons — but every button's wording
differs, so the class could carry no labels of its own and Haskell had to set
all six. Writing the two tabs out directly in `misc.fl` costs a dozen lines and
lets each button keep its `{key}` label in the layout. Shape alone is not a
reason to factor a panel out; shared *content* is.

### The positioning trap

FLUID emits `Fl_Group(X, Y, W, H)` and then builds the children at their
**design** coordinates. Place an instance at (460, 50) and its children stay at
(10, 10). There is no automatic offset. Every `widget_class` here ends with a
`code` node that fixes it:

```cpp
// FLUID builds the children at their design coordinates, which assume this
// group sits at the origin at its design size. Restate that, then resize
// into the frame we were actually constructed with.
Fl_Widget::resize(0, 0, 900, 525);   // the design size, kept in step with xywh
resize(X, Y, W, H);
```

`Fl_Widget::resize` moves only the group's own rectangle; the second call is the
group-aware one that carries the children. This handles rescaling as well as
translation.

### Design at the size you actually use

A `widget_class` is pixel-exact only at its design size; elsewhere
`Fl_Group::resize` scales children **proportionally**, including the gaps
between them. `FileLoadPanel` designed at 400×300 but placed at 900×525 put its
two buttons 7px narrower with the 10px gap stretched to 23px. Redesigning it at
900×525 — the size a Misc tab actually places it at — made the common case a
pure translation.

Mark a sensible `resizable` child (the tree in `FileLoadPanel`, the input in
`FilePickerRow`) so that hosts using a different size keep fixed-height rows and
give the slack to the right widget.

---

## Splitting behaviour from construction

Where a hand-written helper both built widgets and wired them up, it was split
so both paths share one implementation. This lets call sites migrate one at a
time instead of in a big bang.

```haskell
data FileLoadWidgets = FileLoadWidgets { flwCountLabel :: Ptr Fl_Box, … }

fileLoadBehavior :: FileLoadWidgets -> …   -- tree callbacks, DnD, search, labels
fileLoadPanel    :: Ptr FileLoadPanel -> … -- pulls them out of the FLUID class
fileLoadWindow'  :: Rectangle -> …         -- creates them by hand, then delegates
```

Same shape for radio groups: `horizRadioWidgets` takes buttons a layout already
placed; `horizRadio'` creates them and delegates. It errors if the button count
doesn't match the option list, so a `.fl` edit that drops a button fails loudly.

Other helpers: `filePickerRow` (sets the label, hooks the browse button, returns
a read of the field), `filePickerInput` (the raw `Fl_Input`, for callers that
need their own callback on it), `inGroup` (bracketed `begin`/`end`, for building
into a group whose FLUID constructor already called `end()`).

---

## Mixing FLUID chrome with hand-written contents

A partly-migrated window has FLUID owning the frame and Haskell filling the
inside. **The ordering matters**: hand-written page builders use `withRect`,
which applies the stretch *itself*. If `adaptWindow` ran after them, everything
inside would be stretched twice.

So `launchMisc` runs `adaptWindow` **before** filling any tab, and hands each
page the tab's *pre-stretch* rectangle, read off the group before the transform
touches it:

```haskell
Rectangle (Position (X tabX) (Y tabY)) tabSize <- layoutRect tabTask
menuHeight <- adaptWindow window
let tabRect = Rectangle (Position (X tabX) (Y $ tabY + menuHeight)) tabSize
```

Reading the geometry back rather than restating it keeps the `.fl` as the single
source of truth. Same for the window width passed to `makeMenuBar`.

---

## Colours

Colours for FLUID-laid-out widgets live in the `.fl` as packed `fl_rgb_color`
values, so the editor preview matches the running app. Each layout's header
comment maps them back to the names in `Onyx.GUI.Core`:

```
1583242752 = behindTabsColor   rgb(94,94,94)
3771904256 = functionTabColor  rgb(224,210,177)
3017652992 = taskColor         rgb(179,221,187)
1917484032 = globalLogColor    rgb(114,74,124)
3135616256 = loadSongColor     rgb(186,229,181)
3987587328 = batchProcessColor rgb(237,173,193)
3906311680 = quickConvertColor rgb(232,213,150)
2980967424 = miscColor         rgb(177,173,244)
```

FLUID writes these back as negative `int`s when they exceed 2^31
(`(Fl_Color)-1159351040`); that round-trips correctly through the unsigned cast.

The corresponding constants in `Onyx.GUI.Core` are still used by the windows
that haven't migrated yet. They can be swept once everything is in FLUID.

---

## Tooling

Three scripts in `scripts/`. The first two temporarily patch `Onyx.GUI`, build,
and restore it via an `EXIT` trap. They fail loudly if a patch anchor no longer
matches, and abort if the build fails.

| Script | Purpose |
| --- | --- |
| `fluid-dump [-w window] <tab>…` | Print real, laid-out widget geometry |
| `fluid-probe [-w window] <prefix> <tab>…` | Screenshot tabs (one build covers all, via `$ONYX_TAB`) |
| `fluid-compare <a.png> <b.png>` | Align two shots on window content and diff them |

`-w` picks which window to open at startup — `misc` (the default) or `quick`.
Adding another means one more entry in `fluid_patch.py`'s `WINDOWS`: the call
that opens it, and the text following its `show()`, which is where the
tab-selecting code gets spliced in.

### Why `fluid-dump` exists

The coordinates you need when transcribing a layout are the ones on screen, not
the ones in the source. Anything inside an `Fl_Pack` has had its rows resized,
and children of a `padded` group are scaled against that group's invisible
resizable box. Example: a picker row whose source says `chopLeft 100` /
`chopRight 50` actually renders its input at `x=120 w=715` and its browse button
at `x=846 w=44`.

The dump runs from an `Fl::add_timeout`, **not** straight after `show()` —
`Fl_Pack` lays out in `draw()`, so dumping earlier reports pre-layout
coordinates and quietly tells you the wrong thing. `Fl::flush()` before the
event loop starts is not enough.

### Verification method

For each migrated tab: screenshot it before and after, and diff. Nearly every
migration so far has come out at **0 differing pixels**.

A window whose content is all top-anchored can be diffed in one go. Where a tab
has content pinned to its *bottom*, the extra 30px of height (see *Menu bar
space*) moves that content down, and the two halves have to be compared
separately: the top of the tab at a shared offset, and the bottom of the tab
with `FLUID_SKIP_1`/`FLUID_SKIP_2` set 30px (60 screenshot px on a 2× display)
apart so the same band lines up. Overlap the two bands and every row is covered.

Three traps, all of which produced alarming false positives:

* Comparing at fixed offsets. The two runs are separate processes and the window
  manager may place the window elsewhere; the diff then reports thousands of
  differing pixels that are really the desktop behind the window.
  `fluid-compare` anchors each shot on its own window content.
* Cropping one row or column too large, which picks up background outside the
  window frame.
* Comparing top-anchored content inside a bottom-aligned crop. The Quick Convert
  processor checkboxes are top-anchored and 40px apart, so in a band shifted by
  30px each row very nearly lines up with the *next* one — the diff looks like a
  layout bug rather than the crop it is.

And two traps that produced false *negatives*: a `grep`-based build check that
missed a compile error, so the probe screenshotted a stale binary and reported
success; and the app dying at startup for want of an `onyx-resources` directory
next to the executable, so the probe photographed the bare desktop. The scripts
now check the build's exit status, symlink the resources into place, and check
the app is still alive at the moment the screenshot is taken.

`fluid-compare`'s crop sizes are in screenshot pixels, so they depend on the
display's scale factor. `FLUID_CROP_W`/`FLUID_CROP_H` override them.

---

## Migration status

**Done:**

| Window | Layout | Notes |
| --- | --- | --- |
| Song list (multi-song import) | `multiple_songs.fl` | Rows still built in Haskell into an `Fl_Pack` |
| Main console | `console.fl` | `Fl_Simple_Terminal`, real `makeMenuBar` path |
| Tools / Misc | `misc.fl` | All 8 function tabs; Task tab still hand-built |
| Quick Convert | `quick_convert.fl` | RR renumber, GH2 pack and CON→PKG tabs; RB, CH and Task still hand-built |

**Not yet migrated:** `launchWindow` (the song window, including the GL
preview), `launchBatch`, `launchPreferences`, the Quick Convert RB and CH tabs,
and the shared `taskOutputPage`.

The RB and CH tabs are being left in Haskell on purpose for now, since they are
still being changed by hand. RB is also the most awkward one to move: its five
mode groups (`inPlace`, `oneToOne`, `makePacks`, `makeSongs`, `makeDolphin`) all
occupy the same rectangle and are shown and hidden by the mode dropdown, and its
processor column is an `Fl_Pack` generated from a list of twelve MIDI transforms
that belongs in code.

`taskOutputPage` is deliberately last: it's shared between the Misc, song, batch
and quick-convert windows, so migrating it before those would need the same
dual-path treatment `fileLoadWindow` got.

### Known deviations from the old layout

Everything is pixel-identical except:

* Migrated windows are 30px taller on Windows/Linux (see *Menu bar space*).
* The three migrated Quick Convert tabs place a `FileLoadPanel` at 1000px wide,
  100px over its design width, and its Add button comes out 484px instead of
  485 — the gap between Add and Clear is 11px rather than 10. `Fl_Group::resize`
  maps an edge inside the resizable box with `L += dw * (L-RL) / (RR-RL)`,
  truncating; 100 × 435 / 880 is 49.43, so the button's right edge lands one
  pixel short. Nothing else in those three tabs moved — every other widget is at
  the same coordinates as before, confirmed with `fluid-dump`. Redesigning the
  panel at 1000 would only move the rounding error onto the three Misc tabs that
  place it at 900, and no single design width is exact for both.
* The MIDI tab's picker label column narrowed from 132px to the shared 110px, so
  its label and the input's left edge moved 22px left and the input got 22px
  wider. It was the only picker row using `chopLeft 120` instead of `100`; the
  label renders 85px wide, so it still fits comfortably. If the original spacing
  is wanted back, give that tab its own `Fl_Input` + button instead of the
  shared `FilePickerRow`.

---

## Adding a tab or window

1. `dependencies/root/bin/fluid haskell/packages/onyx-exe-toolkit/fluid/x.fl`
   to edit, or write the `.fl` by hand — the format is readable and diffable.
2. If transcribing an existing hand-built layout, get the real coordinates with
   `scripts/fluid-dump`.
3. Author at natural English width, with no menu bar space.
4. Give each widget you need from Haskell a name; leave the rest unnamed.
5. Add a widget record and change the page function to take it instead of a
   `Rectangle`.
6. Build, then verify with `fluid-probe` before and after plus `fluid-compare`.

One non-obvious hazard: `Onyx.GUI` has `CPP` enabled, so `/*` anywhere in a
Haskell comment — for instance writing `fluid/*.fl` — starts a C comment that
never closes, and the build fails at an unrelated line.
