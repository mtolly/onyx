#ifndef WINNAMES_H
#define WINNAMES_H

#ifdef _WIN32
// Visual C++: Avoid the Windows header to prevent a macro mess.
// NOTES: This is not a complete fix. It's best to avoid Windows headers in our own headers.
#ifdef _MSC_VER
typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned long DWORD;
typedef int INT32;
typedef signed __int64 INT64;
typedef unsigned __int64 UINT64;
#else
#include <windows.h>
#endif
#else
typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned int DWORD;
typedef int INT32;
typedef long long INT64;
typedef unsigned long long UINT64;
#endif

typedef struct _WINFILETIME
{
    DWORD dwHighDateTime;
    DWORD dwLowDateTime;

} WINFILETIME;

typedef short INT16;
typedef signed int INT24;

#endif
