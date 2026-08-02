#include "onyx_fluid.h"
#include "Fl_Center.h"

#include <cmath>
#include <vector>

namespace {

// Matches Haskell's `round` (round half to even) so that layouts built by
// Onyx.GUI.Core's stretchX land on the same pixels as ones built here.
int scale_x(int x, double stretch) {
  return (int)nearbyint(x * stretch);
}

struct Target {
  Fl_Widget* widget;
  int x, y, w, h;
};

void collect(Fl_Widget* widget, double stretch, int dy, std::vector<Target>& out) {
  int nx = scale_x(widget->x(), stretch);
  int nw = scale_x(widget->x() + widget->w(), stretch) - nx;
  Target t = { widget, nx, widget->y() + dy, nw, widget->h() };
  out.push_back(t);

  Fl_Group* group = widget->as_group();
  if (!group) return;
  // A child window opens a new coordinate space. The horizontal stretch still
  // applies inside it, but the menu-bar offset belongs to the top-level
  // window's space only, so it must not be added again.
  int child_dy = widget->as_window() ? 0 : dy;
  for (int i = 0; i < group->children(); i++) {
    collect(group->child(i), stretch, child_dy, out);
  }
}

// Fl_Center positions its child itself, and every resize() of an ancestor
// undoes what we did. Re-run it once the tree has settled.
void recenter(Fl_Widget* widget) {
  Fl_Group* group = widget->as_group();
  if (!group) return;
  group->init_sizes(); // the cached bounds describe the pre-stretch layout
  for (int i = 0; i < group->children(); i++) {
    recenter(group->child(i));
  }
  Fl_Center* center = dynamic_cast<Fl_Center*>(widget);
  if (center) center->resize(center->x(), center->y(), center->w(), center->h());
}

} // namespace

extern "C" void onyx_adapt_window(Fl_Window* win, double stretch, int menu_height) {
  if (!win) return;
  if (stretch == 1.0 && menu_height == 0) return;

  // Compute every target up front: applying a group's resize drags its
  // children along, which would corrupt inputs read later.
  std::vector<Target> targets;
  for (int i = 0; i < win->children(); i++) {
    collect(win->child(i), stretch, menu_height, targets);
  }

  win->size(scale_x(win->w(), stretch), win->h() + menu_height);

  // `targets` is in pre-order, so a group is resized before its children and
  // whatever it did to them is then overridden by their own entries.
  for (size_t i = 0; i < targets.size(); i++) {
    Target& t = targets[i];
    t.widget->resize(t.x, t.y, t.w, t.h);
  }

  recenter(win);
  win->init_sizes();
}

extern "C" void onyx_lock_minimum_size(Fl_Window* win) {
  if (win) win->size_range(win->w(), win->h());
}

extern "C" void onyx_group_begin(Fl_Group* group) {
  if (group) group->begin();
}

extern "C" void onyx_group_end(Fl_Group* group) {
  if (group) group->end();
}
