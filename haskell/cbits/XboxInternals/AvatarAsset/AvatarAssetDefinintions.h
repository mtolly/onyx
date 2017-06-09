#pragma once

#include "winnames.h"

enum AssetSubcategory
{
    CarryableCarryable = 0x44c,
    CarryableFirst = 0x44c,
    CarryableLast = 0x44c,
    CostumeCasualSuit = 0x68,
    CostumeCostume = 0x69,
    CostumeFirst = 100,
    CostumeFormalSuit = 0x67,
    CostumeLast = 0x6a,
    CostumeLongDress = 0x65,
    CostumeShortDress = 100,
    EarringsDanglers = 0x387,
    EarringsFirst = 900,
    EarringsLargehoops = 0x38b,
    EarringsLast = 0x38b,
    EarringsSingleDangler = 0x386,
    EarringsSingleLargeHoop = 0x38a,
    EarringsSingleSmallHoop = 0x388,
    EarringsSingleStud = 900,
    EarringsSmallHoops = 0x389,
    EarringsStuds = 0x385,
    GlassesCostume = 0x2be,
    GlassesFirst = 700,
    GlassesGlasses = 700,
    GlassesLast = 0x2be,
    GlassesSunglasses = 0x2bd,
    GlovesFingerless = 600,
    GlovesFirst = 600,
    GlovesFullFingered = 0x259,
    GlovesLast = 0x259,
    HatBaseballCap = 0x1f6,
    HatBeanie = 500,
    HatBearskin = 0x1fc,
    HatBrimmed = 0x1f8,
    HatCostume = 0x1fb,
    HatFez = 0x1f9,
    HatFirst = 500,
    HatFlatCap = 0x1f5,
    HatHeadwrap = 0x1fa,
    HatHelmet = 0x1fd,
    HatLast = 0x1fd,
    HatPeakCap = 0x1f7,
    RingFirst = 0x3e8,
    RingLast = 0x3ea,
    RingLeft = 0x3e9,
    RingRight = 0x3e8,
    ShirtCoat = 210,
    ShirtFirst = 200,
    ShirtHoodie = 0xd0,
    ShirtJacket = 0xd1,
    ShirtLast = 210,
    ShirtLongSleeveShirt = 0xce,
    ShirtLongSleeveTee = 0xcc,
    ShirtPolo = 0xcb,
    ShirtShortSleeveShirt = 0xcd,
    ShirtSportsTee = 200,
    ShirtSweater = 0xcf,
    ShirtTee = 0xc9,
    ShirtVest = 0xca,
    ShoesCostume = 0x197,
    ShoesFirst = 400,
    ShoesFormal = 0x193,
    ShoesHeels = 0x191,
    ShoesHighBoots = 0x196,
    ShoesLast = 0x197,
    ShoesPumps = 0x192,
    ShoesSandals = 400,
    ShoesShortBoots = 0x195,
    ShoesTrainers = 0x194,
    TrousersCargo = 0x131,
    TrousersFirst = 300,
    TrousersHotpants = 300,
    TrousersJeans = 0x132,
    TrousersKilt = 0x134,
    TrousersLast = 0x135,
    TrousersLeggings = 0x12f,
    TrousersLongShorts = 0x12e,
    TrousersLongSkirt = 0x135,
    TrousersShorts = 0x12d,
    TrousersShortSkirt = 0x133,
    TrousersTrousers = 0x130,
    WristwearBands = 0x322,
    WristwearBracelet = 800,
    WristwearFirst = 800,
    WristwearLast = 0x323,
    WristwearSweatbands = 0x323,
    WristwearWatch = 0x321
};

enum BinaryAssetType
{
    Component = 1,
    Texture = 2,
    ShapeOverride = 3,
    Animation = 4,
    ShapeOverridePost = 5
};

enum SkeletonVersion
{
    Nxe = 1,
    Natal,
    NxeAndNatal
};

enum AssetGender
{
    Male = 1,
    Female,
    Both
};

struct STRBHeader
{
    // in the file
    DWORD magic;
    bool blockAlignmentStored;
    bool littleEndian;
    BYTE guid[0x10];
    BYTE blockIDSize;
    BYTE blockSpanSize;
    WORD unused;
    BYTE blockAlignment;

    // calculated
    int blockHeaderSize;
    DWORD blockStartAddress;
};

enum STRRBBlockId
{
    STRBAnimation = 1,
    STRBAssetMetadata = 6,
    STRBAssetMetadataVersioned = 8,
    STRBCustomColorTable = 7,
    STRBEof = -1,
    STRBInvalid = 0,
    STRBModel = 3,
    STRBShapeOverrides = 4,
    STRBSkeleton = 5,
    STRBTexture = 2
};

struct AssetMetadata
{
    BYTE metadataVersion;
    AssetGender gender;
    BinaryAssetType type;
    DWORD assetTypeDetails;
    AssetSubcategory category;
    SkeletonVersion skeletonVersion;
};

struct RGBColor
{
    BYTE blue;
    BYTE green;
    BYTE red;
    BYTE alpha;
};

struct ColorGroup
{
    RGBColor color;
    DWORD unknown;
};

struct ColorTableEntry
{
    ColorGroup color1, color2, color3;
};

struct ColorTable
{
    DWORD count;
    ColorTableEntry *entries;
};

struct Animation
{
    DWORD frameCount;
    float duration;
    float framesPerSecond;
};

struct STRBBlock
{
    STRRBBlockId id;
    int dataLength;
    int fieldSize;

    BYTE *data;
    DWORD dataAddress;
};
