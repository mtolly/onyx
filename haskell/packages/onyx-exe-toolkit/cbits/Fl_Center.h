// From Greg Ercolano's FLTK Cheat Page
// https://www.seriss.com/people/erco/fltk/#Fl_Center

#ifndef FL_CENTER_H
#define FL_CENTER_H

#include <stdio.h>
#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Box.H>

// A group that can keep a /single child/ centered
class Fl_Center : public Fl_Group {
  int centerx(Fl_Widget *wid) { return x() + (w()/2) - (wid->w()/2); }
  int centery(Fl_Widget *wid) { return y() + (h()/2) - (wid->h()/2); }
public:
  Fl_Center(int X,int Y,int W,int H,const char *L=0) : Fl_Group(X,Y,W,H,L) {
  }
  void resize(int X,int Y,int W,int H) {
    Fl_Widget::resize(X,Y,W,H);                       // dont use Fl_Group::resize()
    if ( children() > 0 ) {                           // assume one child
      Fl_Widget *cw = child(0);
      cw->resize(centerx(cw), centery(cw), cw->w(), cw->h()); // center child
    }
  }
  void end() {
    Fl_Group::end();
    resize(x(), y(), w(), h());                       // force resize of child
  }
};

#endif
