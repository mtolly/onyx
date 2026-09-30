#include "hs_gl_window.h"

HsGlWindow::HsGlWindow(int x, int y, int w, int h, const char* l)
  : Fl_Gl_Window(x, y, w, h, l), draw_cb(nullptr), draw_data(nullptr) {}

void HsGlWindow::set_draw_callback(HsDrawCallback cb, void* data) {
  draw_cb = cb;
  draw_data = data;
}

void HsGlWindow::draw() {
  if (draw_cb) draw_cb(draw_data);
}

// accept keyboard focus (via tab or click)
// so it can send the space shortcut to the play button
int HsGlWindow::handle(int event) {
  switch (event) {
    case FL_FOCUS:
    case FL_UNFOCUS:
      return 1;
    case FL_PUSH:
      take_focus();
      break;
  }
  return Fl_Gl_Window::handle(event);
}
