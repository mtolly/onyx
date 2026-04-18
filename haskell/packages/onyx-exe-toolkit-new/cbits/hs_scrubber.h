#ifndef HS_SCRUBBER_H
#define HS_SCRUBBER_H

#include <FL/Fl_Slider.H>
#include <FL/fl_draw.H>

// Callback types for Haskell integration
typedef void (*HsScrubberDrawCallback)(void* data, int x, int y, int w, int h, double value, double maximum);
typedef int (*HsScrubberHandleCallback)(void* data, int event, int x, int y, int w, int h);

class HsScrubber : public Fl_Slider {
  HsScrubberDrawCallback draw_cb;
  void* draw_data;
  HsScrubberHandleCallback handle_cb;
  void* handle_data;
public:
  HsScrubber(int X, int Y, int W, int H)
    : Fl_Slider(X, Y, W, H),
      draw_cb(nullptr), draw_data(nullptr),
      handle_cb(nullptr), handle_data(nullptr) {
    type(FL_HOR_FILL_SLIDER);
  }
  void set_draw_callback(HsScrubberDrawCallback cb, void* data) {
    draw_cb = cb;
    draw_data = data;
  }
  void set_handle_callback(HsScrubberHandleCallback cb, void* data) {
    handle_cb = cb;
    handle_data = data;
  }
  void draw() override {
    if (draw_cb) {
      draw_cb(draw_data, x(), y(), w(), h(), value(), maximum());
    } else {
      Fl_Slider::draw();
    }
  }
  int handle(int event) override {
    if (handle_cb) {
      int result = handle_cb(handle_data, event, x(), y(), w(), h());
      if (result >= 0) return result;
      // negative means fall through to default
    }
    return Fl_Slider::handle(event);
  }
};

#endif
