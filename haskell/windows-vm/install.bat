@echo off
rem Run by dockur/windows at the end of its unattended install. See setup.ps1.
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0setup.ps1"
