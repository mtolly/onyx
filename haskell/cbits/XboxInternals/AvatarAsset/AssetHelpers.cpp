#include "AssetHelpers.h"

string AssetHelpers::AssetSubcategoryToString(AssetSubcategory category)
{
    switch (category)
    {
        case CarryableCarryable:
            return string("Carryable, Carryable");
        case CostumeCasualSuit:
            return string("Costume, Casual Suit");
        case CostumeCostume:
            return string("Costume, Costume");
        case CostumeFormalSuit:
            return string("Costume, Formal Suit");
        case CostumeLast:
            return string("Costume, Last");
        case CostumeLongDress:
            return string("Costume, Long Dress");
        case CostumeShortDress:
            return string("Costume, Short Dress");
        case EarringsDanglers:
            return string("Earrings, Danglers");
        case EarringsLargehoops:
            return string("Earrings, Largehoops");
        case EarringsSingleDangler:
            return string("Earrings, Single Dangler");
        case EarringsSingleLargeHoop:
            return string("Earrings, Single Large Hoop");
        case EarringsSingleSmallHoop:
            return string("Earrings, Single Small Hoop");
        case EarringsSingleStud:
            return string("Earrings, Single Stud");
        case EarringsSmallHoops:
            return string("Earrings, Small Hoops");
        case EarringsStuds:
            return string("Earrings, Studs");
        case GlassesCostume:
            return string("Glasses, Costume");
        case GlassesGlasses:
            return string("Glasses, Glasses");
        case GlassesSunglasses:
            return string("Glasses, Sunglasses");
        case GlovesFingerless:
            return string("Gloves, Fingerless");
        case GlovesFullFingered:
            return string("Gloves, Full Fingered");
        case HatBaseballCap:
            return string("Hat, Baseball Cap");
        case HatBeanie:
            return string("Hat, Beanie");
        case HatBearskin:
            return string("Hat, Bearskin");
        case HatBrimmed:
            return string("Hat, Brimmed");
        case HatCostume:
            return string("Hat, Costume");
        case HatFez:
            return string("Hat, Fez");
        case HatFlatCap:
            return string("Hat, Flat Cap");
        case HatHeadwrap:
            return string("Hat, Headwrap");
        case HatHelmet:
            return string("Hat, Helmet");
        case HatPeakCap:
            return string("Hat, Peak Cap");
        case RingLast:
            return string("Ring, Last");
        case RingLeft:
            return string("Ring, Left");
        case RingRight:
            return string("Ring, Right");
        case ShirtCoat:
            return string("Shirt, Coat");
        case ShirtHoodie:
            return string("Shirt, Hoodie");
        case ShirtJacket:
            return string("Shirt, Jacket");
        case ShirtLongSleeveShirt:
            return string("Shirt, Long Sleeve Shirt");
        case ShirtLongSleeveTee:
            return string("Shirt, Long Sleeve Tee");
        case ShirtPolo:
            return string("Shirt, Polo");
        case ShirtShortSleeveShirt:
            return string("Shirt, Short Sleeve Shirt");
        case ShirtSportsTee:
            return string("Shirt, Sports Tee");
        case ShirtSweater:
            return string("Shirt, Sweater");
        case ShirtTee:
            return string("Shirt, Tee");
        case ShirtVest:
            return string("Shirt, Vest");
        case ShoesCostume:
            return string("Shoes, Costume");
        case ShoesFormal:
            return string("Shoes, Formal");
        case ShoesHeels:
            return string("Shoes, Heels");
        case ShoesHighBoots:
            return string("Shoes, High Boots");
        case ShoesPumps:
            return string("Shoes, Pumps");
        case ShoesSandals:
            return string("Shoes, Sandals");
        case ShoesShortBoots:
            return string("Shoes, Short Boots");
        case ShoesTrainers:
            return string("Shoes, Trainers");
        case TrousersCargo:
            return string("Trousers, Cargo");
        case TrousersHotpants:
            return string("Trousers, Hotpants");
        case TrousersJeans:
            return string("Trousers, Jeans");
        case TrousersKilt:
            return string("Trousers, Kilt");
        case TrousersLeggings:
            return string("Trousers, Leggings");
        case TrousersLongShorts:
            return string("Trousers, Long Shorts");
        case TrousersLongSkirt:
            return string("Trousers, Long Skirt");
        case TrousersShorts:
            return string("Trousers, Shorts");
        case TrousersShortSkirt:
            return string("Trousers, Short Skirt");
        case TrousersTrousers:
            return string("Trousers, Trousers");
        case WristwearBands:
            return string("Wristwear, Bands");
        case WristwearBracelet:
            return string("Wristwear, Bracelet");
        case WristwearSweatbands:
            return string("Wristwear, Sweatbands");
        case WristwearWatch:
            return string("Wristwear, Watch");
        default:
            throw string("Asset: Error converting 'AssetSubcategory' to string, invalid value.\n");
    }
}

string AssetHelpers::AssetGenderToString(AssetGender gender)
{
    switch (gender)
    {
        case Male:
            return string("Male");
        case Female:
            return string("Female");
        case Both:
            return string("Both");
        default:
            throw string("Asset: Error converting 'AssetGender' to string, invalid value.");
    }
}

string AssetHelpers::SkeletonVersionToString(SkeletonVersion version)
{
    switch (version)
    {
        case Nxe:
            return string("Nxe");
        case Natal:
            return string("Natal");
        default:
            throw string("Asset: Invalid 'Skeleton Version' value.\n");
    }
}

string AssetHelpers::BlockIDToString(STRRBBlockId id)
{
    switch (id)
    {
        case STRBAnimation:
            return string("Animation");
        case STRBAssetMetadata:
            return string("Asset Metadata");
        case STRBAssetMetadataVersioned:
            return string("Asset Metadata Versioned");
        case STRBCustomColorTable:
            return string("Custom Color Table");
        case STRBEof:
            return string("Eof");
        case STRBInvalid:
            return string("Invalid");
        case STRBModel:
            return string("Model");
        case STRBShapeOverrides:
            return string("Shape Overrides");
        case STRBSkeleton:
            return string("Skeleton");
        case STRBTexture:
            return string("Texture");
        default:
            throw string("STRB: Invalid 'STRBBlockId' value.\n");
    }
}

string AssetHelpers::BinaryAssetTypeToString(BinaryAssetType type)
{
    switch (type)
    {
        case Component:
            return string("Component");
        case Texture:
            return string("Texture");
        case ShapeOverride:
            return string("ShapeOverride");
        case Animation:
            return string("Animation");
        case ShapeOverridePost:
            return string("ShapeOverridePost");
        default:
            throw string("Asset: Invalid 'BinaryAssetType' value.\n");
    }
}
