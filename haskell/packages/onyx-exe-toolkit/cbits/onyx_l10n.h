#ifndef ONYX_L10N_H
#define ONYX_L10N_H

// Localization hook for FLUID-generated code.
//
// The .fl files set FLUID's "GNU gettext" i18n mode with the function name
// onyx_l10n, so every label, tooltip and menu item in generated code comes
// through here at widget construction time.
//
// Only strings of the form "{message-key}" or "{message-key.attribute}" are
// treated as keys; anything else (plain literals, FLTK symbol labels such as
// "@fileopen") is returned unchanged. FLUID uses label()/tooltip() rather than
// the copy_ variants, so the returned pointer must outlive the widget: results
// are interned and never freed. That is safe because the language is chosen
// once at startup (Main.hs) and changing it only writes preferences.

#ifdef __cplusplus
extern "C" {
#endif

const char* onyx_l10n(const char* text);

#ifdef __cplusplus
}
#endif

#endif
