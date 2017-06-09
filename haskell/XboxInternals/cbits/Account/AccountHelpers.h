#pragma once

#include "AccountDefinitions.h"
#include "Gpd/XdbfHelpers.h"
#include "Gpd/XdbfDefininitions.h"
#include "IO/FileIO.h"
#include <iostream>
#include <stdlib.h>

#include "XboxInternals_global.h"

using std::string;

class XBOXINTERNALSSHARED_EXPORT AccountHelpers
{
public:
    static string CountryToString(XboxLiveCountry country);

    static string SubscriptionTeirToString(SubscriptionTeir teir);

    static string ConsoleLanguageToString(ConsoleLanguage language);

    static string XboxLiveServiceProviderToString(XboxLiveServiceProvider provider);
};

