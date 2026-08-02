#include "onyx_l10n.h"

#include <cstring>
#include <map>
#include <string>

// Implemented in Haskell (Onyx.GUI.Fluid). Takes a message key, returns a
// freshly allocated UTF-8 string, or null if localization isn't loaded yet.
extern "C" char* onyx_l10n_hs(const char* key);

static std::map<std::string, const char*>& l10n_cache() {
  static std::map<std::string, const char*> cache;
  return cache;
}

extern "C" const char* onyx_l10n(const char* text) {
  if (!text) return text;
  size_t len = strlen(text);
  // "{}" is not a key, so require at least one character between the braces
  if (len < 3 || text[0] != '{' || text[len - 1] != '}') return text;

  std::string key(text + 1, len - 2);
  std::map<std::string, const char*>& cache = l10n_cache();
  std::map<std::string, const char*>::iterator it = cache.find(key);
  if (it != cache.end()) return it->second;

  char* translated = onyx_l10n_hs(key.c_str());
  // On failure fall back to the original braced text, which is also what the
  // Fluent implementation renders for an unknown key.
  const char* result = translated ? translated : text;
  cache[key] = result;
  return result;
}
