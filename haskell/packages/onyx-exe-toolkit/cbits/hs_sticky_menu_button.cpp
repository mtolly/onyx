// MT: this is Claude output to hack Fl_Menu_Button to not close on checkbox select.
// if it causes problems later on we could patch fltk instead

#include "hs_sticky_menu_button.h"
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <vector>

// FLTK's menu event loop (Menu_State in Fl_Menu.cxx) is private and always
// ends on a pick, so while our menu is open we:
// - mark every item FL_MENU_CHATTY with a proxy callback, so the menu tells
//   us which item is highlighted (FL_REASON_GOT_FOCUS / FL_REASON_LOST_FOCUS)
// - install an Fl::event_dispatch hook that catches the mouse release or
//   Enter/Space that would pick a highlighted toggle item, toggles it and
//   runs its callback ourselves, and swallows the event
class HsStickyMenuButton : public Fl_Menu_Button {
  struct SavedItem {
    Fl_Callback* callback;
    void* user_data;
    int flags;
  };
  // original callback info for each menu() entry, while the menu is open
  std::vector<SavedItem> saved;

  // the sticky menu currently open, if any (only one menu can be open)
  static HsStickyMenuButton* active;
  static const Fl_Menu_Item* highlighted;
  static bool pushed_in_menu;
  static Fl_Event_Dispatch prev_dispatch;

  void install_proxies();
  void restore_items();
  void toggle_item(const Fl_Menu_Item* m);
  static void proxy_cb(Fl_Widget* w, void* data);
  static int dispatch(int event, Fl_Window* w);

public:
  HsStickyMenuButton(int X, int Y, int W, int H, const char* L = 0)
    : Fl_Menu_Button(X, Y, W, H, L) {}
  ~HsStickyMenuButton();

  const Fl_Menu_Item* popup_sticky();
  int handle(int e) override;
};

Fl_Menu_Button* new_sticky_menu_button(int X, int Y, int W, int H, const char* L) {
  return new HsStickyMenuButton(X, Y, W, H, L);
}

HsStickyMenuButton* HsStickyMenuButton::active = nullptr;
const Fl_Menu_Item* HsStickyMenuButton::highlighted = nullptr;
bool HsStickyMenuButton::pushed_in_menu = false;
Fl_Event_Dispatch HsStickyMenuButton::prev_dispatch = nullptr;

HsStickyMenuButton::~HsStickyMenuButton() {
  if (active == this) {
    active = nullptr;
    highlighted = nullptr;
  }
}

void HsStickyMenuButton::install_proxies() {
  int n = size();
  Fl_Menu_Item* items = (Fl_Menu_Item*) menu();
  saved.resize(n);
  for (int i = 0; i < n; i++) {
    Fl_Menu_Item& it = items[i];
    saved[i] = {it.callback_, it.user_data_, it.flags};
    // leave submenus alone: giving them a callback changes how they open
    if (!it.text || it.submenu()) continue;
    it.callback_ = proxy_cb;
    it.user_data_ = &it;
    it.flags |= FL_MENU_CHATTY;
  }
}

void HsStickyMenuButton::restore_items() {
  // if the menu was rebuilt while open, the saved info doesn't apply to it
  if (!saved.empty() && menu() && size() == (int) saved.size()) {
    Fl_Menu_Item* items = (Fl_Menu_Item*) menu();
    for (size_t i = 0; i < saved.size(); i++) {
      Fl_Menu_Item& it = items[i];
      it.callback_ = saved[i].callback;
      it.user_data_ = saved[i].user_data;
      // keep the current FL_MENU_VALUE, only undo FL_MENU_CHATTY
      it.flags = (it.flags & ~FL_MENU_CHATTY) | (saved[i].flags & FL_MENU_CHATTY);
    }
  }
  saved.clear();
}

// Same as what Fl_Menu_::picked does for a toggle item,
// but calling the item's original callback
void HsStickyMenuButton::toggle_item(const Fl_Menu_Item* m) {
  SavedItem s = saved[m - menu()];
  ((Fl_Menu_Item*) m)->flags ^= FL_MENU_VALUE;
  value(m);
  set_changed();
  redraw();
  if (Fl::grab()) Fl::grab()->redraw(); // the menu window
  if (when() & (FL_WHEN_CHANGED | FL_WHEN_RELEASE)) {
    if (s.callback) {
      Fl::callback_reason_ = FL_REASON_SELECTED;
      s.callback(this, s.user_data);
    } else {
      do_callback(FL_REASON_SELECTED);
    }
  }
}

void HsStickyMenuButton::proxy_cb(Fl_Widget* w, void* data) {
  const Fl_Menu_Item* m = (const Fl_Menu_Item*) data;
  switch (Fl::callback_reason()) {
    case FL_REASON_GOT_FOCUS:
      highlighted = m;
      return;
    case FL_REASON_LOST_FOCUS:
      if (highlighted == m) highlighted = nullptr;
      return;
    default:
      break;
  }
  // A pick that closed the menu, called from Fl_Menu_::picked.
  // Put the original callbacks back and forward to the right one.
  HsStickyMenuButton* self = active;
  if (!self) return;
  self->restore_items();
  if (m->callback_) {
    m->do_callback(self, FL_REASON_SELECTED);
  } else {
    self->do_callback(FL_REASON_SELECTED);
  }
}

int HsStickyMenuButton::dispatch(int event, Fl_Window* w) {
  HsStickyMenuButton* self = active;
  if (self && Fl::grab()) {
    switch (event) {
      case FL_PUSH:
        pushed_in_menu = true;
        break;
      case FL_RELEASE:
        // Same condition as Fl_Menu.cxx for a release to pick an item: the
        // mouse was dragged, or this isn't the click that opened the menu
        if (highlighted && (highlighted->flags & FL_MENU_TOGGLE)
            && (pushed_in_menu || !Fl::event_is_click())) {
          self->toggle_item(highlighted);
          return 1;
        }
        break;
      case FL_KEYBOARD:
        switch (Fl::event_key()) {
          case FL_Enter:
          case FL_KP_Enter:
          case ' ':
            if (highlighted && (highlighted->flags & FL_MENU_TOGGLE)) {
              self->toggle_item(highlighted);
              return 1;
            }
        }
        break;
    }
  }
  return prev_dispatch ? prev_dispatch(event, w) : Fl::handle_(event, w);
}

const Fl_Menu_Item* HsStickyMenuButton::popup_sticky() {
  menu_end(); // popup() does this too, but we need menu() to stay put
  install_proxies();
  active = this;
  highlighted = nullptr;
  pushed_in_menu = false;
  prev_dispatch = Fl::event_dispatch();
  Fl::event_dispatch(dispatch);

  Fl_Widget_Tracker wt(this);
  const Fl_Menu_Item* m = popup();

  Fl::event_dispatch(prev_dispatch);
  prev_dispatch = nullptr;
  if (wt.exists()) {
    active = nullptr;
    highlighted = nullptr;
    restore_items();
  }
  return m;
}

// Same conditions as Fl_Menu_Button::handle for opening the menu,
// but using popup_sticky() instead of popup()
int HsStickyMenuButton::handle(int e) {
  if (!menu() || !menu()->text) return 0;
  switch (e) {
    case FL_PUSH:
      if (!box()) {
        if (Fl::event_button() != 3) return 0;
      } else if (type()) {
        if (!(type() & (1 << (Fl::event_button() - 1)))) return 0;
      }
      if (Fl::visible_focus()) Fl::focus(this);
      popup_sticky();
      return 1;
    case FL_KEYBOARD:
      if (box() && Fl::event_key() == ' ' &&
          !(Fl::event_state() & (FL_SHIFT | FL_CTRL | FL_ALT | FL_META))) {
        popup_sticky();
        return 1;
      }
      break;
    case FL_SHORTCUT:
      if (Fl_Widget::test_shortcut()) {
        popup_sticky();
        return 1;
      }
      break;
  }
  return Fl_Menu_Button::handle(e);
}
