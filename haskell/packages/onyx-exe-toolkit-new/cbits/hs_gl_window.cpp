#include "hs_gl_window.h"

HsGlWindow::HsGlWindow(int x, int y, int w, int h)
  : Fl_Gl_Window(x, y, w, h), draw_cb(nullptr), draw_data(nullptr) {}

void HsGlWindow::set_draw_callback(HsDrawCallback cb, void* data) {
  draw_cb = cb;
  draw_data = data;
}

void HsGlWindow::draw() {
  if (draw_cb) draw_cb(draw_data);
}

int HsGlWindow::pixel_w_public() { return pixel_w(); }
int HsGlWindow::pixel_h_public() { return pixel_h(); }
float HsGlWindow::pixels_per_unit_public() { return pixels_per_unit(); }
