// 
// data type defs for easy cross-processor support
// 

#ifndef _COMMON_TYPES_
#define _COMMON_TYPES_


typedef unsigned char		u8;
typedef unsigned short		u16;
typedef unsigned int		u32;
typedef unsigned long long	u64;

typedef signed char			s8;
typedef signed short		s16;
typedef signed int			s32;
typedef signed long long	s64;

#ifndef NULL
#define NULL	0
#endif // NULL


#ifdef WIN32
	// windows specific stuff
	#define STRICMP		_stricmp
	#define FSEEK64		_fseeki64
	#define FTELL64		_ftelli64
	#define DIRSEPCHAR	'\\'
	#define MKDIR(dir)	_mkdir(dir)
	
	#include <direct.h>
	#include <io.h>
#else
	// "other systems" - yes there probably should be some system specific checks here...
	#define STRICMP		strcasecmp
	#define FSEEK64		fseeko
	#define FTELL64		ftello
	#define DIRSEPCHAR	'/'
	#define MKDIR(dir)	mkdir(dir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH)
	
	#define _FILE_OFFSET_BITS 64
	#define __USE_FILE_OFFSET64
#endif


// this MUST be included after the "_FILE_OFFSET_BITS" define for
// 64bit fileio to work properly in linux etc
#include <stdio.h>


#endif // _COMMON_TYPES_


