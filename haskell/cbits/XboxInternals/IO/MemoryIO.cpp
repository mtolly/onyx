#include "MemoryIO.h"

MemoryIO::MemoryIO(BYTE *data, size_t length) :
    BaseIO(), memory(data), length(length)
{

}

MemoryIO::~MemoryIO()
{

}

void MemoryIO::SetPosition(UINT64 pos, std::ios_base::seek_dir dir)
{
    DWORD newPos;
    switch (dir)
    {
        case std::ios_base::beg:
            newPos = pos;
            break;
        case std::ios_base::cur:
            newPos = this->pos + pos;
            break;
        case std::ios_base::end:
            newPos = length + pos;
            break;
        default:
            throw std::string("MemoryIO: Unsupported seek direction\n");
    }

    if (newPos > length)
        throw std::string("MemoryIO: Cannot seek beyond the end of the stream\n");
    this->pos = pos;
}

UINT64 MemoryIO::GetPosition()
{
    return pos;
}

UINT64 MemoryIO::Length()
{
    return length;
}

void MemoryIO::ReadBytes(BYTE *outBuffer, DWORD len)
{
    memcpy(outBuffer, memory + pos, len);
    pos += len;
}

void MemoryIO::WriteBytes(BYTE *buffer, DWORD len)
{
    memcpy(memory + pos, buffer, len);
    pos += len;
}

void MemoryIO::Close()
{

}

void MemoryIO::Flush()
{

}

