#include "BaseIO.h"
#include <vector>

using namespace std;

BaseIO::BaseIO() :
    byteOrder(BigEndian)
{
    // should be implemented by derived class
}

BaseIO::~BaseIO()
{

}

void BaseIO::SetEndian(EndianType byteOrder)
{
    this->byteOrder = byteOrder;
}

EndianType BaseIO::GetEndian()
{
    return this->byteOrder;
}

void BaseIO::reverseByteArray(BYTE *array, DWORD len)
{
    BYTE temp;
    for (DWORD i = 0; i < len / 2; i++)
    {
        temp = array[i];
        array[i] = array[len - i - 1];
        array[len - i - 1] = temp;
    }
}

BYTE BaseIO::ReadByte()
{
    BYTE toReturn;
    ReadBytes(&toReturn, 1);
    return toReturn;
}

INT16 BaseIO::ReadInt16()
{
    return (INT16)ReadWord();
}

WORD BaseIO::ReadWord()
{
    WORD toReturn;
    ReadBytes(reinterpret_cast<BYTE*>(&toReturn), 2);

    if (byteOrder == BigEndian)
        reverseByteArray(reinterpret_cast<BYTE*>(&toReturn), 2);

    return toReturn;
}

INT24 BaseIO::ReadInt24(EndianType et)
{
    EndianType orig = byteOrder;

    if(et != Default)
        byteOrder = et;

    INT24 returnVal = ReadDword();

    if(byteOrder == BigEndian)
        returnVal = (returnVal & 0xFFFFFF00) >> 8;
    else
        returnVal = returnVal & 0x00FFFFFF;

    SetPosition(GetPosition() - 1);
    byteOrder = orig;

    return returnVal;
}

INT32 BaseIO::ReadInt32()
{
    return (INT32)ReadDword();
}

DWORD BaseIO::ReadDword()
{
    DWORD toReturn;
    ReadBytes(reinterpret_cast<BYTE*>(&toReturn), 4);

    if (byteOrder == BigEndian)
        reverseByteArray(reinterpret_cast<BYTE*>(&toReturn), 4);

    return toReturn;
}

INT64 BaseIO::ReadInt64()
{
    return (INT64)ReadUInt64();
}

UINT64 BaseIO::ReadUInt64()
{
    UINT64 toReturn;
    ReadBytes(reinterpret_cast<BYTE*>(&toReturn), 8);

    if (byteOrder == BigEndian)
        reverseByteArray(reinterpret_cast<BYTE*>(&toReturn), 8);

    return toReturn;
}

UINT64 BaseIO::ReadMultiByte(size_t size)
{
    switch (size)
    {
        case 1:
            return ReadByte();
        case 2:
            return ReadWord();
        case 4:
            return ReadDword();
        case 8:
            return ReadUInt64();
        default:
            throw string("BaseIO: Invalid multi-byte size.\n");
    }
}

float BaseIO::ReadFloat()
{
    return (float)ReadDword();
}

double BaseIO::ReadDouble()
{
    return (double)ReadUInt64();
}

string BaseIO::ReadString(int len, char nullTerminator, bool forceInclude0, int maxLength)
{
    string toReturn;

    // assume it's null terminating
    if (len == -1)
    {
        toReturn = "";
        int i = 1;
        char nextChar;
        while ((nextChar = ReadByte()) != nullTerminator && (forceInclude0 && nextChar != 0) &&
                (i++ <= maxLength))
            toReturn += nextChar;
    }
    else
    {
        std::vector<char> strVec;
        strVec.reserve(len + 1);
        char* str = strVec.data();
        str[len] = 0;

        ReadBytes((BYTE*)str, len);
        toReturn = string(str);
    }

    return toReturn;
}

wstring BaseIO::ReadWString(int len)
{
    wstring toReturn;

    // assume it's null terminating
    if (len == -1)
    {
        toReturn = L"";
        wchar_t nextChar;
        while ((nextChar = ReadWord()) != 0)
            toReturn += nextChar;
    }
    else
    {
        for (int i = 0; i < len; i++)
        {
            wchar_t c = static_cast<wchar_t>(ReadWord());
            if (c != 0)
                toReturn += c;
            else
                break;
        }
    }

    return toReturn;
}

void BaseIO::Write(BYTE b)
{
    WriteBytes(&b, 1);
}

void BaseIO::Write(WORD w)
{
    if (byteOrder == BigEndian)
        reverseByteArray(reinterpret_cast<BYTE*>(&w), 2);
    WriteBytes(reinterpret_cast<BYTE*>(&w), 2);
}

void BaseIO::Write(INT24 i24, EndianType et)
{
    EndianType orig = byteOrder;
    if(et != Default)
        byteOrder = et;

    if(byteOrder == BigEndian)
    {
        i24 <<= 8;
        reverseByteArray(reinterpret_cast<BYTE*>(&i24), 4);
    }
    WriteBytes(reinterpret_cast<BYTE*>(&i24), 3);

    byteOrder = orig;
}

void BaseIO::Write(DWORD dw)
{
    if (byteOrder == BigEndian)
        reverseByteArray(reinterpret_cast<BYTE*>(&dw), 4);
    WriteBytes(reinterpret_cast<BYTE*>(&dw), 4);
}

void BaseIO::Write(UINT64 u64)
{
    if (byteOrder == BigEndian)
        reverseByteArray(reinterpret_cast<BYTE*>(&u64), 8);
    WriteBytes(reinterpret_cast<BYTE*>(&u64), 8);
}

void BaseIO::Write(string s, int forceLen, bool nullTerminating, BYTE nullTerminator)
{
    WriteBytes((BYTE*)s.c_str(), s.length() + nullTerminating);

    if (forceLen > 0)
    {
        forceLen -= s.size();

        for (int i = 0; i < forceLen; i++)
            Write(nullTerminator);
    }
}

void BaseIO::Write(wstring ws, bool nullTerminating)
{
    if (byteOrder == LittleEndian)
        WriteBytes((BYTE*)ws.c_str(), (ws.length() + nullTerminating) * 2);
    else
    {
        WORD curChar;
        for (DWORD i = 0; i < ws.length(); i++)
        {
            curChar = ws.at(i);
            Write(curChar);
        }
        if (nullTerminating)
            Write((WORD)0);
    }
}

void BaseIO::Write(BYTE *buffer, DWORD len)
{
    WriteBytes(buffer, len);
}

void BaseIO::SwapEndian()
{
    if (byteOrder == LittleEndian)
        byteOrder = BigEndian;
    else
        byteOrder = LittleEndian;
}
