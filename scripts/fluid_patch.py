#!/usr/bin/env python3
#
# Temporarily patch Onyx.GUI so that fluid-dump / fluid-probe can drive one
# window straight from startup. Shared by both scripts; they each save and
# restore GUI.hs around the call.
#
#   python3 fluid_patch.py <dump|probe> <misc|quick> <path/to/GUI.hs>
#
# Patching rather than adding a permanent debug flag keeps the hook out of the
# shipped binary. Every anchor below is asserted, so if GUI.hs moves on the
# scripts stop with an error instead of quietly building something that no
# longer does what the caller thinks.

import sys

# What each window needs: the call that opens it at startup, and the tail of
# its launch function, which is where the tab-selecting code gets spliced in.
# Both launch functions bind `window` and `tabs`, which is what the splice uses.
WINDOWS = {
    'misc': {
        'launch': 'launchMisc sink makeMenuBar',
        'after_show': '\n\n-- _watchSong',
    },
    'quick': {
        'launch': "launchQuickConvert' sink makeMenuBar",
        'after_show': '\n\nlaunchMisc ::',
    },
}

SHOW = '  [C.block| void { $(Fl_Window* window)->show(); } |]'

# Selects the requested tab, for screenshots.
SELECT = '''  __probeTab <- System.Environment.lookupEnv "ONYX_TAB"
  forM_ __probeTab \\__s -> do
    let __n = read __s :: CInt
    [C.block| void {
      Fl_Tabs* t = (Fl_Tabs*)$(Fl_Tabs* tabs);
      t->value(t->child($(int __n)));
    } |]'''

# Selects the requested tab and prints its widget tree. The dump runs from an
# Fl::add_timeout rather than straight after show(): Fl_Pack does its layout in
# draw(), so dumping any earlier reports pre-layout coordinates.
DUMP = '''  __dumpTab <- System.Environment.lookupEnv "ONYX_DUMP"
  forM_ __dumpTab \\__s -> do
    let __n = read __s :: CInt
    [C.block| void {
      Fl_Tabs* t = (Fl_Tabs*)$(Fl_Tabs* tabs);
      t->value(t->child($(int __n)));
      onyx_dump_target = t->child($(int __n));
      Fl::add_timeout(1.5, onyx_dump_cb, 0);
    } |]'''

DUMP_VERBATIM = '''C.include "<typeinfo>"
C.include "<cstdio>"

C.verbatim "\\n\\
static void onyx_dump(Fl_Widget* w, int d) {\\n\\
  for (int i = 0; i < d; i++) fprintf(stderr, \\"  \\");\\n\\
  fprintf(stderr, \\"%-20s x=%4d y=%4d w=%4d h=%4d  label=%s\\\\n\\",\\n\\
    typeid(*w).name(), w->x(), w->y(), w->w(), w->h(), w->label() ? w->label() : \\"\\");\\n\\
  Fl_Group* g = w->as_group();\\n\\
  if (g) for (int i = 0; i < g->children(); i++) onyx_dump(g->child(i), d + 1);\\n\\
}\\n\\
static Fl_Widget* onyx_dump_target = 0;\\n\\
static void onyx_dump_cb(void*) {\\n\\
  if (onyx_dump_target) onyx_dump(onyx_dump_target, 0);\\n\\
  fflush(stderr);\\n\\
  exit(0);\\n\\
}\\n"
'''


def main():
    if len(sys.argv) != 4 or sys.argv[1] not in ('dump', 'probe'):
        sys.exit('usage: fluid_patch.py <dump|probe> <%s> <GUI.hs>'
                 % '|'.join(WINDOWS))
    mode, window, path = sys.argv[1:]
    if window not in WINDOWS:
        sys.exit('fluid_patch.py: unknown window %r, expected one of %s'
                 % (window, ', '.join(WINDOWS)))
    win = WINDOWS[window]

    s = open(path).read()
    edits = [
        (SHOW + win['after_show'],
         SHOW + '\n' + (DUMP if mode == 'dump' else SELECT) + win['after_show']),
        ('\nimport           System.Info',
         '\nimport qualified System.Environment\nimport           System.Info'),
        ('\n  -- Support drag and drop onto mac app icon',
         '\n  %s\n\n  -- Support drag and drop onto mac app icon' % win['launch']),
    ]
    if mode == 'dump':
        edits.insert(0, ('C.include "misc.h"',
                         'C.include "misc.h"\n' + DUMP_VERBATIM))
    for old, new in edits:
        if old not in s:
            sys.exit('fluid_patch.py: patch point not found, GUI.hs has moved '
                     'on:\n  %s' % old.strip()[:70])
        s = s.replace(old, new, 1)
    open(path, 'w').write(s)


main()
