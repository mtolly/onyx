#ifndef HS_VALUE_SLIDER_H
#define HS_VALUE_SLIDER_H

#include <FL/Fl_Value_Slider.H>

// value slider that always shows 2 decimal places,
// using the given string (copied) as the decimal separator
Fl_Value_Slider* new_value_slider(int X, int Y, int W, int H, const char* sep, const char* L = 0);

#endif
