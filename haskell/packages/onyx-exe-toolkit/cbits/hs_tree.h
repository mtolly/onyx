#ifndef HS_TREE_H
#define HS_TREE_H

#include <FL/Fl_Tree.H>
#include <FL/Fl.H>

// Callback: (data, event) -> handled (1) or not (0)
typedef int (*HsTreeHandleCallback)(void* data, int event);

class HsTree : public Fl_Tree {
  HsTreeHandleCallback handle_cb;
  void* handle_data;
public:
  HsTree(int X, int Y, int W, int H, const char* L = 0)
    : Fl_Tree(X, Y, W, H, L),
      handle_cb(nullptr), handle_data(nullptr) {}
  void set_handle_callback(HsTreeHandleCallback cb, void* data) {
    handle_cb = cb;
    handle_data = data;
  }
  int handle(int event) override {
    if (handle_cb) {
      int result = handle_cb(handle_data, event);
      if (result == 1) return 1; // handled
    }
    return Fl_Tree::handle(event);
  }
};

#endif
