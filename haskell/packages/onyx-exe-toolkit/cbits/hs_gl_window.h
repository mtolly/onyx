#ifndef HS_GL_WINDOW_H
#define HS_GL_WINDOW_H

#include <FL/Fl_Gl_Window.H>

typedef void (*HsDrawCallback)(void* data);

class HsGlWindow : public Fl_Gl_Window {
  HsDrawCallback draw_cb;
  void* draw_data;
public:
  HsGlWindow(int x, int y, int w, int h, const char* l = 0);
  void set_draw_callback(HsDrawCallback cb, void* data);
  void draw() override;
  int handle(int event) override;
};

#endif
