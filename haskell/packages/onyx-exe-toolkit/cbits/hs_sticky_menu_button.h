// MT: this is Claude output to hack Fl_Menu_Button to not close on checkbox select.
// if it causes problems later on we could patch fltk instead

#ifndef HS_STICKY_MENU_BUTTON_H
#define HS_STICKY_MENU_BUTTON_H

#include <FL/Fl_Menu_Button.H>

// A menu button where picking a checkbox (FL_MENU_TOGGLE) item toggles it
// without closing the menu, so several items can be toggled in one go.
// Radio and normal items close the menu as usual. See the .cpp for details.
//
// Note that the menu runs its own nested event loop, so item callbacks should
// act directly (e.g. redraw a widget) rather than queueing work for the main
// loop, which won't run until the menu is closed. They also must not rebuild
// the menu while it is open.
//
// The class itself is kept in the .cpp, because GHC doesn't know to recompile
// inline-c snippets when a header they include changes, so a snippet using
// the class directly could end up with a stale idea of its layout.
Fl_Menu_Button* new_sticky_menu_button(int X, int Y, int W, int H, const char* L = 0);

#endif
