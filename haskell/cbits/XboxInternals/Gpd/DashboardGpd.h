#pragma once

#include "Xdbf.h"
#include "XdbfDefininitions.h"
#include "XdbfHelpers.h"
#include "GpdBase.h"
#include <iostream>

#include "XboxInternals_global.h"

using std::string;

class XBOXINTERNALSSHARED_EXPORT DashboardGpd : public GpdBase
{
public:
    DashboardGpd(string gpdPath);
    DashboardGpd(FileIO *io);

    ~DashboardGpd(void);

    vector<TitleEntry> gamesPlayed;

    SettingEntry gamercardRegion;
    SettingEntry gamerzone;
    SettingEntry gamerscoreUnlocked;
    SettingEntry achievementsUnlocked;
    SettingEntry reputation;
    SettingEntry gamePlayedCount;
    SettingEntry motto;
    SettingEntry gamerPictureKey;
    SettingEntry gamerBio;
    SettingEntry gamerName;
    SettingEntry gamerLocation;
    SettingEntry avatarInformation;
    ImageEntry avatarImage;
    SettingEntry yearsOnLive;

    // Description: get the box art URL hosted on xbox.com for the title passed in
    static string GetSmallBoxArtURL(TitleEntry *entry);
    static string GetLargeBoxArtURL(TitleEntry *entry);

    // Description: Write the title entry back to the file
    void WriteTitleEntry(TitleEntry *entry);

    // Description: create a new title entry, make it look like you played a game you actually haven't
    void CreateTitleEntry(TitleEntry *entry);

    // Description: delete a title entry
    void DeleteTitleEntry(TitleEntry *entry);

    // Description: remove all of the unused memory from the gpd
    void CleanGpd();

private:

    // Description: read the title entry
    TitleEntry readTitleEntry(XdbfEntry entry);

    // read all of the starting stuff
    void init();
};

