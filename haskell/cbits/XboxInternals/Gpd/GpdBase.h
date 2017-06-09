#pragma once

#include <iostream>
#include "Xdbf.h"
#include "XdbfDefininitions.h"
#include <vector>

#include "XboxInternals_global.h"

using std::string;
using std::vector;

class XBOXINTERNALSSHARED_EXPORT GpdBase
{
public:
    GpdBase(FileIO *io);
    GpdBase(string gpdPath);

    ~GpdBase(void);
    Xdbf *xdbf;

    vector<ImageEntry> images;
    vector<StringEntry> strings;
    vector<SettingEntry> settings;

    // Description: delete the setting entry passed in from the xdbf file
    void DeleteSettingEntry(SettingEntry setting);

    // Description: delete an image entry
    void DeleteImageEntry(ImageEntry image);

    // Description: create a new setting entry, leave the entry field of the setting entry struct blank, this function will set that info
    void CreateSettingEntry(SettingEntry *setting, UINT64 entryID);

    // Description: create a new image entry
    void CreateImageEntry(ImageEntry *image, UINT64 entryID);

    // Description: Write the setting entry to the file
    void WriteSettingEntry(SettingEntry setting);

    // Description: Write the image entry to the file
    void WriteImageEntry(ImageEntry image);

    // Description: close all open resources attatched to the file
    void Close();

    // Description: remove all of the unused memory
    void Clean();

    // Description: get a setting entry from its id
    SettingEntry GetSetting(UINT64 id);

protected:
    bool ioPassedIn;
    FileIO *io;

private:
    // Description: read the string entry passed in
    wstring readStringEntry(XdbfEntry entry);

    // Description: read the setting entry passed in
    SettingEntry readSettingEntry(XdbfEntry entry);

    // read in all of the images, settings, and strings
    void init();
};
