#pragma once

#include "Xdbf.h"
#include "XdbfDefininitions.h"
#include "XdbfHelpers.h"
#include "GpdBase.h"
#include <iostream>
#include <stdio.h>

#include "XboxInternals_global.h"

using std::string;

class XBOXINTERNALSSHARED_EXPORT AvatarAwardGpd : public GpdBase
{
public:
    AvatarAwardGpd(string gpdPath);
    AvatarAwardGpd(FileIO *io);
    ~AvatarAwardGpd(void);

    vector<struct AvatarAward> avatarAwards;

    // Description: get the gender of the avatar award passed in
    static AssetGender GetAssetGender(struct AvatarAward *award);

    // Description: unlock all of the avatar awards in this gpd
    void UnlockAllAwards();

    // Description: get the URL of the image of the award passed in, hosted on xbox.com, 64x64
    static string GetLittleAwardImageURL(struct AvatarAward *award);

    // Description: get the URL of the image of the award passed in, hosted on xbox.com, 128x128
    static string GetLargeAwardImageURL(struct AvatarAward *award);

    // Description: get the GUID of the avatar award, used in URLs on xbox.com
    static string GetGUID(struct AvatarAward *award);

    // Description: Write the avatar award back to the gpd
    void WriteAvatarAward(struct AvatarAward *award);

    // Description: create a new avatar award
    void CreateAvatarAward(struct AvatarAward *award);

    // Description: delete an award
    void DeleteAvatarAward(struct AvatarAward *award);

    // Description: remove all of the unused memory from the gpd
    void CleanGpd();

private:

    // Description: read the avatar award entry passed in and return it
    struct AvatarAward readAvatarAwardEntry(XdbfEntry entry);

    // Description: get the URl of the image of the award passed in, hosted on xbox.com
    static string getAwardImageURL(struct AvatarAward *award, bool little);

    // Description: read in all the stuff that needs to be read at the start
    void init();

    // Description: get the next award index, for the id of a new entry
    WORD getNextAwardIndex();
};
