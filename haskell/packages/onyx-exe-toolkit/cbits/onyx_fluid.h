#ifndef ONYX_FLUID_H
#define ONYX_FLUID_H

#include <FL/Fl_Window.H>

// Post-processing for FLUID-generated windows.
//
// .fl layouts are authored at natural English width with no space reserved for
// a menu bar. This applies the two adjustments that FLUID can't express:
//
//   * `stretch` scales every X position and width, matching the per-language
//     stretch-width parameter (see Onyx.GUI.Core stretchX / stretchW). Widths
//     are derived from scaled edges so widgets that were adjacent or exactly
//     filled their parent still do afterwards.
//
//   * `menu_height` grows the window and pushes every widget down, making room
//     for a menu bar that the caller then adds. Pass 0 on macOS, where the
//     menu lives in the global menu bar and takes no window space.
//
// Call this after the FLUID constructor and before show().

extern "C" void onyx_adapt_window(Fl_Window* win, double stretch, int menu_height);

// Stop the window shrinking below its designed size. Call after
// onyx_adapt_window, so the minimum covers the stretch and the menu bar
// without either being spelled out a second time.
extern "C" void onyx_lock_minimum_size(Fl_Window* win);

// Reopen a group that FLUID's constructor already closed, so that widgets
// built the hand-written way can be added to it.
extern "C" void onyx_group_begin(Fl_Group* group);
extern "C" void onyx_group_end(Fl_Group* group);

#endif
