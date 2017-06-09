#pragma once

#include <iostream>
#include "winnames.h"

using std::string;
using std::wstring;

enum PasscodeKeys
{
    DPAD_Up,
    DPAD_Down,
    DPAD_Left,
    DPAD_Right,
    X,
    Y,
    LT,
    RT,
    LB,
    RB
};

enum SubscriptionTeir
{
    NoSubcription = 0,
    Silver = 3,
    Gold = 6,
    FamilyGold = 9
};

enum ReservedFlags
{
    PasswordProtected = 0x10000000,
    LiveEnabled = 0x20000000,
    Recovering = 0x40000000
};

enum ConsoleLanguage
{
    NoLanguage,
    English,
    Japanese,
    German,
    French,
    Spanish,
    Italian,
    Korean,
    TChinese,
    Portuguese,
    SChinese,
    Polish,
    Russian,
    Norwegian = 15
};

enum XboxLiveServiceProvider
{
    LiveDisabled = 0,
    ProductionNet = 0x50524F44,	// PROD
    PartnerNet = 0x50415254		// PART
};

struct AccountInfo
{
    DWORD reservedFlags;
    DWORD liveFlags;
    wstring gamertag;
    UINT64 xuid;
    DWORD cachedUserFlags;
    XboxLiveServiceProvider serviceProvider;
    BYTE passcode[4];
    string onlineDomain;
    string kerbrosRealm;
    BYTE onlineKey[0x10];
};

const BYTE RETAIL_KEY[0x10] = { 0xE1, 0xBC, 0x15, 0x9C, 0x73, 0xB1, 0xEA, 0xE9, 0xAB, 0x31, 0x70, 0xF3, 0xAD, 0x47, 0xEB, 0xF3 };
const BYTE DEVKIT_KEY[0x10] = { 0xDA, 0xB6, 0x9A, 0xD9, 0x8E, 0x28, 0x76, 0x4F, 0x97, 0x7E, 0xE2, 0x48, 0x7E, 0x4F, 0x3F, 0x68 };
const BYTE CONFOUNDER[8] = { 0x56, 0x65, 0x6C, 0x6F, 0x63, 0x69, 0x74, 0x79 };
