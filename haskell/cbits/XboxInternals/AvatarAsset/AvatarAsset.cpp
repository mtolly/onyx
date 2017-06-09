#include "AvatarAsset.h"


AvatarAsset::AvatarAsset(string assetPath) : ioPassedIn(false)
{
    metadata.gender = (AssetGender)0;
    customColors.entries = NULL;
    customColors.count = 0;
    animation.frameCount = 0;

    io = new FileIO(assetPath);
    readHeader();
    readBlocks();
}

AvatarAsset::AvatarAsset(FileIO *io) : io(io), ioPassedIn(true)
{
    metadata.gender = (AssetGender)0;
    customColors.entries = NULL;
    customColors.count = 0;

    readHeader();
    readBlocks();
}

void AvatarAsset::readHeader()
{
    // seek to the begining of the file
    io->SetPosition(0);

    // read in the magic to verify the file
    header.magic = io->ReadDword();
    if (header.magic != 0x53545242)
        throw string("STRB: Invalid magic.\n");

    // read in the rest of the header
    header.blockAlignmentStored = (bool)io->ReadByte();
    header.littleEndian = (bool)io->ReadByte();
    io->SetEndian((EndianType)header.littleEndian);

    io->ReadBytes(header.guid, 16);

    // read the rest of the header
    header.blockIDSize = io->ReadByte();
    header.blockSpanSize = io->ReadByte();
    header.unused = io->ReadWord();
    header.blockAlignment = (header.blockAlignmentStored) ? io->ReadByte() : 1;

    // calculate the stuff that needs to be calculated
    header.blockStartAddress = roundUpToBlockAlignment((header.blockAlignmentStored) ? 30 : 0x1A);
    header.blockHeaderSize = roundUpToBlockAlignment((header.blockIDSize + header.blockSpanSize) +
            header.blockSpanSize);
}

void AvatarAsset::readBlocks()
{
    DWORD prevAddress = header.blockStartAddress, prevDataLen = 0;
    io->SetPosition(0, ios_base::end);
    DWORD fileSize = io->GetPosition();

    for (;;)
    {
        // seek to the position of the next block
        DWORD curBlockAddress = prevAddress + roundUpToBlockAlignment(prevDataLen);
        io->SetPosition(curBlockAddress);

        // if the io is at the end of the file, then break out
        if ((curBlockAddress + header.blockHeaderSize) > fileSize)
            break;

        // create a new block
        STRBBlock block;
        block.data = NULL;

        // read the block header
        block.id = (STRRBBlockId)io->ReadMultiByte(header.blockIDSize);
        block.dataLength = prevDataLen = (int)io->ReadMultiByte(header.blockSpanSize);
        block.fieldSize = (int)io->ReadMultiByte(header.blockSpanSize);

        if (block.id == STRBAssetMetadata)
            parseMetadataLegacyV1(curBlockAddress + header.blockHeaderSize);
        else if (block.id == STRBAssetMetadataVersioned)
            parseMetadata(curBlockAddress + header.blockHeaderSize);
        else if (block.id == STRBCustomColorTable)
            parseColorTable(curBlockAddress + header.blockHeaderSize);
        else if (block.id == STRBAnimation)
            readAnimationInfo(curBlockAddress + header.blockHeaderSize);

        // check the values read in
        if (block.dataLength < 0 || block.fieldSize == 0 || (block.dataLength % block.fieldSize) != 0)
            throw string("STRB: Invalid block header value(s).\n");

        prevAddress = curBlockAddress + header.blockHeaderSize;
        block.dataAddress = prevAddress;

        blocks.push_back(block);
    }
}

void AvatarAsset::parseMetadataLegacyV1(DWORD pos)
{
    // seek to the position of the metadata
    io->SetPosition(pos);

    // preserve the endian
    EndianType byteOrder = io->GetEndian();
    io->SetEndian(LittleEndian);

    // read in the metadata
    metadata.metadataVersion = 1;
    metadata.gender = (AssetGender)io->ReadByte();
    metadata.type = (BinaryAssetType)io->ReadDword();
    metadata.assetTypeDetails = io->ReadDword();
    metadata.category = (AssetSubcategory)io->ReadInt32();
    metadata.skeletonVersion = Nxe;

    // set the endian back
    io->SetEndian(byteOrder);
}

void AvatarAsset::parseMetadata(DWORD pos)
{
    // seek to the position of the metadata
    io->SetPosition(pos);

    switch (io->ReadByte())
    {
        case 1:
            parseMetadataLegacyV1(pos + 1);
            return;
        case 2:
            // preserve the endian
            EndianType byteOrder = io->GetEndian();
            io->SetEndian(LittleEndian);

            // read in the metadata
            metadata.gender = (AssetGender)io->ReadByte();
            metadata.type = (BinaryAssetType)io->ReadInt32();
            metadata.assetTypeDetails = io->ReadDword();
            metadata.category = (AssetSubcategory)io->ReadInt32();
            metadata.skeletonVersion = (SkeletonVersion)io->ReadByte();

            // set the endian back
            io->SetEndian(byteOrder);
            return;
    }
}

void AvatarAsset::parseColorTable(DWORD pos)
{
    // seek to the color table position
    io->SetPosition(pos);

    // preserve the endian
    EndianType byteOrder = io->GetEndian();
    io->SetEndian(LittleEndian);

    // read the entry count
    customColors.count = io->ReadDword();

    // allocate enough memory for all the entries
    customColors.entries = new ColorTableEntry[customColors.count];

    // read the entries
    for (DWORD i = 0; i < customColors.count; i++)
    {
        customColors.entries[i].color1.color.blue = io->ReadByte();
        customColors.entries[i].color1.color.green = io->ReadByte();
        customColors.entries[i].color1.color.red = io->ReadByte();
        customColors.entries[i].color1.color.alpha = io->ReadByte();
        customColors.entries[i].color1.unknown = io->ReadDword();

        customColors.entries[i].color2.color.blue = io->ReadByte();
        customColors.entries[i].color2.color.green = io->ReadByte();
        customColors.entries[i].color2.color.red = io->ReadByte();
        customColors.entries[i].color2.color.alpha = io->ReadByte();
        customColors.entries[i].color2.unknown = io->ReadDword();

        customColors.entries[i].color3.color.blue = io->ReadByte();
        customColors.entries[i].color3.color.green = io->ReadByte();
        customColors.entries[i].color3.color.red = io->ReadByte();
        customColors.entries[i].color3.color.alpha = io->ReadByte();
        customColors.entries[i].color3.unknown = io->ReadDword();
    }

    // set the endian back
    io->SetEndian(byteOrder);
}

void AvatarAsset::ReadBlockData(STRBBlock *block)
{
    // allocated enough memory for the data
    block->data = new BYTE[block->dataLength];

    // seek to the positon of the data
    io->SetPosition(block->dataAddress);

    // read in the block data
    io->ReadBytes(block->data, block->dataLength);
}

void AvatarAsset::readAnimationInfo(DWORD pos)
{
    // seek to the position of the animation information
    io->SetPosition(pos);

    // preserve the endian
    EndianType byteOrder = io->GetEndian();
    io->SetEndian(LittleEndian);

    // read the frame count
    animation.frameCount = io->ReadDword();

    // read the fps
    animation.framesPerSecond =  io->ReadFloat();

    // round dat, yo
    animation.framesPerSecond = (float)(((int)((animation.framesPerSecond + .05f) * 100)) / 100);

    // calculate the duration of the animation
    animation.duration = (float)animation.frameCount / animation.framesPerSecond;

    io->SetEndian(byteOrder);
}

DWORD AvatarAsset::roundUpToBlockAlignment(DWORD valueToRound)
{
    if (((header.blockAlignment & (header.blockAlignment - 1)) != 0) || (header.blockAlignment == 0))
        throw string("STRB: Error when rounding.\n");
    int blockAlignment = header.blockAlignment;
    return (((valueToRound + blockAlignment) - 1) & ~(blockAlignment - 1));
}

ColorTable AvatarAsset::GetCustomColorTable()
{
    if (customColors.entries == NULL)
        throw string("Asset: No color table found for asset.\n");

    return customColors;
}

struct Animation AvatarAsset::GetAnimation()
{
    if (animation.frameCount == 0)
        throw string("Asset: No animation found for asset.\n");

    return animation;
}

AssetMetadata AvatarAsset::GetAssetMetadata()
{
    // if the metadata doesn't exist then we need to throw an error
    if (metadata.gender == 0)
        throw string("Asset: No metadata available for asset.\n");

    return metadata;
}

AvatarAsset::~AvatarAsset(void)
{
    // cleanup the io
    if (!ioPassedIn)
    {
        io->Close();
        delete io;
    }

    // cleanup the colors
    if (customColors.entries != NULL)
        delete customColors.entries;

    // cleanup the blocks
    for (DWORD i = 0; i < blocks.size(); i++)
        if (blocks.at(i).data != NULL)
            delete blocks.at(i).data;
}
