#include "AccountHelpers.h"

string AccountHelpers::CountryToString(XboxLiveCountry country)
{
    return XdbfHelpers::RegionIDtoString(country);
}

string AccountHelpers::SubscriptionTeirToString(SubscriptionTeir teir)
{
    switch (teir)
    {
        case NoSubcription:
            return string("No Subcription");
        case Silver:
            return string("Silver");
        case Gold:
            return string("Gold");
        case FamilyGold:
            return string("FamilyGold");
        default:
            throw string("Account: Invalid 'Subscription Teir' value.\n");
    }
}

string AccountHelpers::XboxLiveServiceProviderToString(XboxLiveServiceProvider provider)
{
    switch (provider)
    {
        case LiveDisabled:
            return string("Live Disabled");
        case ProductionNet:
            return string("ProductionNet");
        case PartnerNet:
            return string("PartnerNet");
        default:
            char temp[5] = {0};
            memcpy(temp, &provider, 4);
            FileIO::ReverseGenericArray(temp, 1, 4);
            return string(temp);
    }
}

string AccountHelpers::ConsoleLanguageToString(ConsoleLanguage language)
{
    switch (language)
    {
        case NoLanguage:
            return string("No Language");
        case English:
            return string("English");
        case Japanese:
            return string("Japanese");
        case German:
            return string("German");
        case French:
            return string("French");
        case Spanish:
            return string("Spanish");
        case Italian:
            return string("Italian");
        case Korean:
            return string("Korean");
        case TChinese:
            return string("TChinese");
        case Portuguese:
            return string("Portuguese");
        case SChinese:
            return string("SChinese");
        case Polish:
            return string("Polish");
        case Russian:
            return string("Russian");
        case Norwegian:
            return string("Norwegian");
        default:
            return string("Unknown Language");
    }
}
