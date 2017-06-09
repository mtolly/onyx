#ifndef BASEIO_H
#define BASEIO_H

#include <iostream>
#include "winnames.h"

#include "XboxInternals_global.h"

using std::string;
using std::wstring;

enum EndianType
{
    BigEndian,
    LittleEndian,
    Default
};

class XBOXINTERNALSSHARED_EXPORT BaseIO
{
public:
    BaseIO();
    virtual ~BaseIO();

    // set the byte order in which to read the bytes
    void SetEndian(EndianType byteOrder);

    // swap the endianess
    void SwapEndian();

    // seek to a position in a file
    virtual void SetPosition(UINT64 position, std::ios_base::seek_dir dir = std::ios_base::beg) = 0;

    // get current address in the file
    virtual UINT64 GetPosition() = 0;

    // get length of the file
    virtual UINT64 Length() = 0;

    // get the byte order in which to read the bytes
    EndianType GetEndian();

    // read len bytes from the current file at the current position into buffer
    virtual void ReadBytes(BYTE *outBuffer, DWORD len) = 0;

    // Write len bytes from the current file at the current position into buffer
    virtual void WriteBytes(BYTE *buffer, DWORD len) = 0;

    // all the read functions
    BYTE ReadByte();
    INT16 ReadInt16();
    WORD ReadWord();
    INT24 ReadInt24(EndianType et = Default);
    INT32 ReadInt32();
    DWORD ReadDword();
    INT64 ReadInt64();
    UINT64 ReadUInt64();
    UINT64 ReadMultiByte(size_t size);
    float ReadFloat();
    double ReadDouble();
    string ReadString(int len = -1, char nullTerminator = 0, bool forceInclude0 = true,
            int maxLength = 0x7FFFFFFF);
    wstring ReadWString(int len = -1);

    // Write functions
    void Write(BYTE b);
    void Write(WORD w);
    void Write(INT24 i24, EndianType et = Default);
    void Write(DWORD dw);
    void Write(UINT64 u64);
    void Write(string s, int forceLen = -1, bool nullTerminating = true, BYTE nullTerminator = 0);
    void Write(wstring ws, bool nullTerminating = true);
    void Write(BYTE *buffer, DWORD len);

    // flush the io's buffer
    virtual void Flush() = 0;

    // close the io
    virtual void Close() = 0;

protected:
    EndianType byteOrder;

private:
    void reverseByteArray(BYTE *array, DWORD len);
};

#endif //BASEIO_H
