#ifndef HS_DND_BUTTON_H
#define HS_DND_BUTTON_H

#include <FL/Fl_Button.H>
#include <FL/Fl.H>

// Callback receives the event text (file paths separated by \n)
typedef void (*HsDndCallback)(void* data, const char* text);

class HsDndButton : public Fl_Button {
  HsDndCallback dnd_cb;
  void* dnd_data;
public:
  HsDndButton(int X, int Y, int W, int H, const char* L = 0)
    : Fl_Button(X, Y, W, H, L),
      dnd_cb(nullptr), dnd_data(nullptr) {}
  void set_dnd_callback(HsDndCallback cb, void* data) {
    dnd_cb = cb;
    dnd_data = data;
  }
  int handle(int event) override {
    switch (event) {
      case FL_DND_ENTER:
      case FL_DND_DRAG:
      case FL_DND_LEAVE:
      case FL_DND_RELEASE:
        return 1; // accept DnD
      case FL_PASTE:
        if (dnd_cb) {
          dnd_cb(dnd_data, Fl::event_text());
        }
        return 1;
      default:
        return Fl_Button::handle(event);
    }
  }
};

#endif
