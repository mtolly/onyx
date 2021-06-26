#include "fsbanklib.h"

#include <stdio.h>

void __stdcall Update(int index, int memused, void *userdata)
{
    printf("UPDATE : Memory used %d kb\n", memused / 1024);
}

void __stdcall Debug(const char *debugstring, void *userdata)
{
    printf("DEBUG  : %s\n", debugstring);
}

int main(int argc, char **argv)
{
    if (argc != 3)
    {
        printf("Usage: makefsb4 in.wav out.fsb\n");
        return 1;
    }
    char *file_in = argv[1];
    char *file_out = argv[2];

    FSBANK_RESULT result;

    result = FSBank_Init();
    if (result != FSBANK_OK)
    {
        printf("ERROR\n");
        return 1;
    }

    result = FSBank_SetUpdateCallback(Update, 0);
    if (result != FSBANK_OK)
    {
        printf("ERROR\n");
        return 1;
    }

    result = FSBank_SetDebugCallback(Debug, 0);
    if (result != FSBANK_OK)
    {
        printf("ERROR\n");
        return 1;
    }

#if 1
    /*
        This version compiles the wavs into 1 fsb.
    */
    result = FSBank_Build(FSBANK_BUILDMODE_SINGLE, FSBANK_FORMAT_XMA, FSBANK_PLATFORM_XBOX360, FSBANK_BUILD_DEFAULT, 0, file_out, 1, &file_in, 0, 0, 60);
#else
    /*
        This version compiles the wavs into their own fsb.  1 each.
    */
    result = FSBank_Build(FSBANK_BUILDMODE_MULTI, FSBANK_FORMAT_PCM, FSBANK_PLATFORM_PC, FSBANK_BUILD_DEFAULT, 0, ".", NUMFILES, &files[0], 0, 0, 0);
#endif
    if (result != FSBANK_OK)
    {
        printf("ERROR\n");
        return 1;
    }

    result = FSBank_Close();
    if (result != FSBANK_OK)
    {
        printf("ERROR\n");
        return 1;
    }

    return 0;
}
