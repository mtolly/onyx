#include "hs_value_slider.h"
#include <stdio.h>
#include <string.h>
#include <string>

class HsValueSlider : public Fl_Value_Slider {
  std::string separator;

public:
  HsValueSlider(int X, int Y, int W, int H, const char* sep, const char* L = 0)
    : Fl_Value_Slider(X, Y, W, H, L), separator(sep ? sep : ".") {}

  int format(char* buffer) override {
    char temp[64];
    int n = snprintf(temp, sizeof(temp), "%.2f", value());
    if (n < 0) n = 0;
    if (n >= (int) sizeof(temp)) n = sizeof(temp) - 1;
    // the separator printf picked depends on the C locale,
    // so find it by skipping the sign and integer digits
    int i = 0;
    if (i < n && temp[i] == '-') i++;
    while (i < n && temp[i] >= '0' && temp[i] <= '9') i++;
    std::string out(temp, i);
    if (i < n) {
      out += separator;
      out.append(temp + i + 1, n - i - 1);
    }
    // buffer must have room for at least 128 bytes (see Fl_Valuator::format)
    size_t len = out.size() < 127 ? out.size() : 127;
    memcpy(buffer, out.data(), len);
    buffer[len] = '\0';
    return (int) len;
  }
};

Fl_Value_Slider* new_value_slider(int X, int Y, int W, int H, const char* sep, const char* L) {
  return new HsValueSlider(X, Y, W, H, sep, L);
}
