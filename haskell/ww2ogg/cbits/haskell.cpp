#define __STDC_CONSTANT_MACROS
#include <iostream>
#include <fstream>
#include <cstring>
#include "wwriff.h"
#include "stdint.h"
#include "errors.h"

using namespace std;

extern "C" {

int hs_ww2ogg(char *fin, char *fout, char *codebook) {
    string finStr(fin);
    string foutStr(fout);
    string codebookStr(codebook);

    try
    {
        Wwise_RIFF_Vorbis ww(finStr,
                codebookStr,
                false,
                false,
                kNoForcePacketFormat
                );

        ofstream of(fout, ios::binary);
        if (!of) throw File_open_error(foutStr);

        ww.generate_ogg(of);
    }
    catch (const File_open_error& fe)
    {
        return 1;
    }
    catch (const Parse_error& pe)
    {
        return 1;
    }

    return 0;
}

}
