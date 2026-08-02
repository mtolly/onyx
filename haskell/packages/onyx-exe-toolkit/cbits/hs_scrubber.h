#ifndef HS_SCRUBBER_H
#define HS_SCRUBBER_H

#include <FL/Fl_Slider.H>
#include <FL/fl_draw.H>
#include <vector>

typedef int (*HsScrubberHandleCallback)(void* data, int event, int x, int y, int w, int h);

class HsScrubber : public Fl_Slider {
  Fl_Color filled_color_;
  Fl_Color empty_color_;
  std::vector<double> sections_;
  double song_length_;
  HsScrubberHandleCallback handle_cb;
  void* handle_data;
public:
  HsScrubber(int X, int Y, int W, int H, const char* L = 0)
    : Fl_Slider(X, Y, W, H, L),
      filled_color_(FL_CYAN), empty_color_(FL_DARK3),
      song_length_(0),
      handle_cb(nullptr), handle_data(nullptr) {
    type(FL_HOR_FILL_SLIDER);
  }
  void set_colors(Fl_Color filled, Fl_Color empty) {
    filled_color_ = filled;
    empty_color_ = empty;
  }
  void set_sections(const double* times, int count, double song_length) {
    sections_.assign(times, times + count);
    song_length_ = song_length;
  }
  void set_handle_callback(HsScrubberHandleCallback cb, void* data) {
    handle_cb = cb;
    handle_data = data;
  }
  void draw() override {
    int X = x(), Y = y(), W = w(), H = h();
    double v = value();
    double vmax = maximum();

    // Empty background
    fl_color(empty_color_);
    fl_rectf(X, Y, W, H);
    fl_color(FL_BLACK);
    fl_rect(X, Y, W, H);

    // Progress bar
    if (vmax > 0) {
      int pw = (int)(W * (v / vmax));
      if (pw >= 2) {
        fl_color(filled_color_);
        fl_rectf(X, Y, pw, H);
        fl_color(FL_BLACK);
        fl_rect(X, Y, pw, H);
      }
    }

    // Section markers
    if (!sections_.empty() && song_length_ > 0) {
      fl_line_style(FL_SOLID, 1);
      fl_color(FL_WHITE);
      for (double t : sections_) {
        int sx = X + (int)(W * (t / song_length_)) - 1;
        if (sx > X) {
          fl_line(sx, Y + 3, sx, Y + H - 4);
        }
      }
      fl_line_style(0);
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
