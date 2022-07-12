local packageName = "{{& package_name }}"
local songListManager = PowerChordApp.GetSongListManager();
local lbManager = PowerChordApp.getLeaderboardManager();

local DLCPkgAlias = DLCManager.GetCurDLCPkgDriveName();
local Platform = PowerChordApp.getPlatformType();

local AppVersion = PowerChordApp.getVersion();
if( AppVersion >= 1.00 ) then
  if( Platform == G.XBOX ) then
    if( AppVersion == 1.00 ) then
      DLCManager.SetCurDLCPackageName( "Data" );  --For Xbox version 1.0
    end
  end 
  VisionLog.Print( "[LUA][DLC][PKG::"..packageName.."] AddContent.lua: Got current DLC pkg alias: '"..DLCPkgAlias.."'" );
  --load some new string tables [removed for Onyx]
  --PowerChordApp.getStringTable():loadStringsFromFile( "StringTables\\TornadoOfSouls.english.xml" );

  song = SongUtilities.SongData:new()
  song:SetKey( "{{& song_key }}" )
  song:SetScoreWeight( 1.0 )
  song:SetColor( 6 )
  song:SetAB( false )
  song:SetBC( false )
  song:SetCA( true )
  song:SetUnlockData( SongUtilities.dataUnlockTier1 )
  -- [Onyx TODO: should we set leaderboard IDs here, like disc songs?]
  SongUtilities.SongList[ song:GetKey() ] = song

  songListManager:addSong( song:GetKey(), song:GetXML(), true, song:GetColor(), song:GetAB(), song:GetBC(), song:GetCA(), song:GetScoreWeight() );

  local lbIDs = song:GetLeaderboardIDs();
  lbManager:addSongTableMapping( song:GetKey(), lbIDs.solo, lbIDs.multi );
else
  VisionLog.PrintWarning( "[LUA][DLC][PKG::"..packageName.."] AddContent.lua: Executable version incompatible with this DLC!! Sending event G.DLCVersionIncompatible: '"..G.DLCVersionIncompatible.."'" );
  PowerChordApp.sendEvent( G.DLCVersionIncompatible, packageName );
end
