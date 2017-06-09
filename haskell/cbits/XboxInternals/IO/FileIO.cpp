#include "IO/FileIO.h"
#include <vector>

FileIO::FileIO(string path, bool truncate) :
    BaseIO(), filePath(path)
{
    fstr = new fstream(path.c_str(),
            fstream::in | fstream::out | fstream::binary | (truncate ? fstream::trunc :
                    static_cast<std::ios_base::openmode>(0)));
    if (!fstr->is_open())
    {
        std::string ex("FileIO: Error opening the file. ");
        ex += strerror(errno);
        ex += "\n";
        throw ex;
    }

    endian = BigEndian;

    fstr->rdbuf()->pubsetbuf(0, 0);
    fstr->seekp(0, std::ios_base::end);
    length = fstr->tellp();
    fstr->seekp(0);
}

void FileIO::SetPosition(UINT64 pos, ios_base::seek_dir dir)
{
    fstr->seekp(pos, static_cast<std::ios_base::seekdir>(dir));
}

UINT64 FileIO::GetPosition()
{
    return fstr->tellp();
}

UINT64 FileIO::Length()
{
    return length;
}

void FileIO::Close()
{
    fstr->close();
}

void FileIO::Flush()
{
    fstr->flush();
}

void FileIO::ReverseGenericArray(void *arr, int elemSize, int len)
{
    std::vector<char> tempVec;
    tempVec.reserve(elemSize);
    char* temp = tempVec.data();

    for (int i = 0; i < (len / 2); i++)
    {
        memcpy(temp, ((char*)arr) + (i * elemSize), elemSize);
        memcpy(((char*)arr) + (i * elemSize), ((char*)arr) + (((len - 1) * elemSize) - (i * elemSize)),
               elemSize);
        memcpy(((char*)arr) + (((len - 1) * elemSize) - (i * elemSize)), temp, elemSize);
    }
}

string FileIO::GetFilePath()
{
    return filePath;
}

void FileIO::ReadBytes(BYTE *outBuffer, DWORD len)
{
    fstr->read((fstream::char_type*)outBuffer, len);
    if (fstr->fail())
        throw string("FileIO: Error reading from file.\n");
}

void FileIO::WriteBytes(BYTE *buffer, DWORD len)
{
    fstr->write((fstream::char_type*)buffer, len);
    if (fstr->fail())
        throw string("FileIO: Error writing to file.\n");
}

FileIO::~FileIO(void)
{
    if(fstr->is_open())
        fstr->close();
    delete fstr;
}
