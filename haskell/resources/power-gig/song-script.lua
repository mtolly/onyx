
function init(self)
    self.loadLipSync = loadLipSync;
    self.setAnimOverrides = setAnimOverrides;
    self.getAnimPackages = getAnimPackages;
    self.loadAnimPackages = loadAnimPackages;
end

function loadLipSync()
    local sceneObject = PowerChordApp.getSceneObject();
    -- [Removed for Onyx]
    -- sceneObject.venueData.vocalist:LoadLipSyncData("AnimData:LipSync\\TornadoOfSouls.vvf"); 
end

function setAnimOverrides()
    local sceneObject = PowerChordApp.getSceneObject();
    local guitaristEntity = sceneObject.venueData.guitarist;
    local vocalistEntity = sceneObject.venueData.vocalist;
    local drummerEntity = sceneObject.venueData.drummer;
    local guitarPropEntity = VisionGame.GetObject("GuitarProp");
    

    local EighthNoteEase = 0.24;

    local SixteenthNoteEase = 0.12;
    
    
    for i=1, G.SongUtilities.GuitaristPerformanceAnimCount do
        guitaristEntity:SetOverrideEase("GuitaristFullBody", i, EighthNoteEase, EighthNoteEase)
    end
    
    --don't forget the idle animation...which is not next to any of the other performance animations.
    guitaristEntity:SetOverrideEase("GuitaristFullBody", 32, EighthNoteEase, EighthNoteEase)
    
    for i=1, G.SongUtilities.VocalistPerformanceAnimCount do
        vocalistEntity:SetOverrideEase("VocalistFullBody", i, EighthNoteEase, EighthNoteEase)
    end
    
    vocalistEntity:SetOverrideEase("VocalistFullBody", 40, EighthNoteEase, EighthNoteEase)
    
    for i=1, G.SongUtilities.DrummerPerformanceAnimCount do
        drummerEntity:SetOverrideEase("DrummerFill", i, SixteenthNoteEase, SixteenthNoteEase)
    end
    
    for i=1, 5 do
        guitarPropEntity:SetOverrideEase("GuitaristFret", i, SixteenthNoteEase, SixteenthNoteEase);
    end
end

function getAnimPackages()
    
    local guitaristPacks = {
        "Main",
        "Metal",
        "FLVGroove"
    }
    
    local vocalistPacks = {
        "Main",
        "Sing_Metal",
        "Flavor_2_3",
        "FillClap"
        
    }
    
    local drummerPacks = {
        "Main",
        "Base_Hat4_Ride",
        "Fill_SnareRoll_TomSnare",
        "Fill_Tom_HiHat",
        "Hit_CrashRide_SnareTom",
        "Flavor_Smash_Clicks"
    }
    
    local packages = {
        ["guitarist"] = guitaristPacks,
        ["vocalist"] = vocalistPacks,
        ["drummer"] = drummerPacks
    }
    
    return packages;
    
end
