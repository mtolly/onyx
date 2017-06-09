#ifndef XBOXINTERNALS_GLOBAL_H
#define XBOXINTERNALS_GLOBAL_H

#if defined(XBOXINTERNALS_STATIC)
#  define XBOXINTERNALSSHARED_EXPORT
#else
#  if defined(_MSC_VER) // VC++
#    define Q_DECL_EXPORT __declspec(dllexport)
#    define Q_DECL_IMPORT __declspec(dllimport)
#  else
#      include <QtCore/qglobal.h>
#  endif

#  if defined(XBOXINTERNALS_LIBRARY)
#    define XBOXINTERNALSSHARED_EXPORT Q_DECL_EXPORT
#  else
#    define XBOXINTERNALSSHARED_EXPORT Q_DECL_IMPORT
#  endif
#endif

#endif // XBOXINTERNALS_GLOBAL_H
