#pragma once

#include "Xdbf.h"
#include "XdbfDefininitions.h"
#include "XdbfHelpers.h"
#include "GpdBase.h"
#include <iostream>

#include "XboxInternals_global.h"

using std::string;

class XBOXINTERNALSSHARED_EXPORT GameGpd : public GpdBase
{
public:
    GameGpd(string gpdPath);
    GameGpd(FileIO *io);

    ~GameGpd(void);

    // Description: all of the achievements in this gpd
    vector<AchievementEntry> achievements;

    // Description: the name of the game the achievement is for
    StringEntry gameName;

    // Description: the thumbnail image for the game
    ImageEntry thumbnail;

    // Description: Write an achievement entry back to the gpd
    void WriteAchievementEntry(AchievementEntry *entry);

    // Description: create an achievement
    void CreateAchievement(AchievementEntry *entry, BYTE *thumbnail, DWORD thumbnailLen);

    // Description: delete an achievement
    void DeleteAchievement(AchievementEntry *entry);

    // Description: get the type of an achievement
    static string GetAchievementType(AchievementEntry *entry);

    // Decription: retrieves the thumbnail image for the achievement passed in, returns true if found and false if not
    bool GetAchievementThumbnail(AchievementEntry *entry, ImageEntry *out);

    // Description: unlock all of the achievements in the Gpd offline and Write them to the file
    void UnlockAllAchievementsOffline();

    // Description: remove all of the unused memory from the gpd
    void CleanGpd();

    // Description: call before writing anything to init the io
    void StartWriting();

    // Description: call after writing anything to close the io
    void StopWriting();

private:

    // Descritpion: read the achievement entry passed in
    AchievementEntry readAchievementEntry(XdbfEntry entry);

    // Description: read in all of the default stuff to be read in for a game gpd
    void init();

    string filePath;
};

