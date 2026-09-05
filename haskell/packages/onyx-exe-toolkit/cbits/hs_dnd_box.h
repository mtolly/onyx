#ifndef HS_DND_BOX_H
#define HS_DND_BOX_H

#include <FL/Fl_Box.H>
#include <FL/Fl.H>

typedef void (*HsDndCallback)(void* data, const char* text);

class HsDndBox : public Fl_Box {
  HsDndCallback dnd_cb;
  void* dnd_data;
public:
  HsDndBox(int X, int Y, int W, int H, const char* L = 0)
    : Fl_Box(X, Y, W, H, L),
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
        return 1;
      case FL_PASTE:
        if (dnd_cb) {
          dnd_cb(dnd_data, Fl::event_text());
        }
        return 1;
      default:
        return 0; // don't handle — let events pass through to widgets underneath
    }
  }
};

#endif
