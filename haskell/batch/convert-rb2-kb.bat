"%~dp0onyx.exe" convert %1 --game rb2 --keys-on-bass
IF %ERRORLEVEL% NEQ 0 (pause)
