#pragma once

#include "winnames.h"
#include "Gpd/XdbfDefininitions.h"
#include <iostream>
#include <time.h>

#include "XboxInternals_global.h"

using std::string;

class XBOXINTERNALSSHARED_EXPORT XdbfHelpers
{
public:

    static string EntryTypeToString(EntryType type);

    static string SettingEntryTypeToString(SettingEntryType type);

    static bool CompareEntries(XdbfEntry a, XdbfEntry b);

    static time_t FILETIMEtoTimeT(WINFILETIME time);

    static unsigned int FILETIMEtoMilliseconds(WINFILETIME time);

    static WINFILETIME TimeTtoFILETIME(time_t time);

    static WINFILETIME TimeTtoFILETIME(time_t time, unsigned int milliseconds);

    static string AchievementTypeToString(AchievementFlags type);

    static string RegionIDtoString(XboxLiveCountry id);

    static string AssetSubcategoryToString(AssetSubcategory category);

    static string AssetGenderToString(AssetGender gender);

    static string IDtoString(UINT64 id);

    static string GetAchievementState(AchievementEntry *entry);
};
