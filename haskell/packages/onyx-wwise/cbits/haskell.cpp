#define __STDC_CONSTANT_MACROS
#include <iostream>
#include <cstring>
#include <sstream>
#include "wwriff.h"
#include "stdint.h"
#include "errors.h"

using namespace std;

extern "C" {

int hs_ww2ogg(char *in_data, size_t in_len, void **out_stream, size_t *out_len, char *codebook) {

    // TODO improve this to do less copying.
    // probably https://stackoverflow.com/questions/7781898/get-an-istream-from-a-char

    string codebookStr(codebook);
    string input(in_data, in_len);

    try
    {
        Wwise_RIFF_Vorbis ww(input,
                codebookStr,
                false,
                false,
                kNoForcePacketFormat
                );

        ostringstream *of = new ostringstream();
        ww.generate_ogg(*of);
        *out_stream = (void *) of;
        *out_len = of->str().length();
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

void hs_fill_stream(void *stream_void, char *out_data)
{
    ostringstream *stream = (ostringstream *) stream_void;
    memcpy(out_data, stream->str().data(), stream->str().length());
}

void hs_delete_stream(void *stream) {
    delete (ostringstream *) stream;
}

}
