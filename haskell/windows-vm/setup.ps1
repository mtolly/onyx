# One-time setup of the Windows build VM. dockur/windows copies the /oem
# folder to C:\OEM and runs install.bat (which runs this) as the Windows user,
# an administrator, at the end of its unattended install. ../win-vm puts the
# installers and authorized_keys next to this file before Windows installs.
#
# Log: C:\OEM\setup.log. Writes C:\OEM\setup-done when finished.

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
Start-Transcript -Path C:\OEM\setup.log -Append
$oem = 'C:\OEM'

function Set-Reg($path, $name, $value) {
  if (-not (Test-Path $path)) { New-Item -Path $path -Force | Out-Null }
  Set-ItemProperty -Path $path -Name $name -Value $value -Type DWord
}

Write-Host '=== power: never sleep or hibernate'
powercfg /change standby-timeout-ac 0
powercfg /change hibernate-timeout-ac 0
powercfg /change monitor-timeout-ac 0
powercfg /hibernate off

Write-Host '=== Windows Update: no automatic updates or restarts in the middle of builds'
$au = 'HKLM:\SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU'
Set-Reg $au 'NoAutoUpdate' 1
Set-Reg $au 'NoAutoRebootWithLoggedOnUsers' 1

Write-Host '=== trusted root certificates'
# A fresh Windows only fetches root certificates when its own networking
# (Edge, PowerShell, ...) first needs one. stack's TLS reads the store directly
# and never triggers that, so it fails with "certificate has unknown CA".
# Import Microsoft's whole trusted root list up front.
certutil.exe -generateSSTFromWU "$env:TEMP\roots.sst"
Get-ChildItem "$env:TEMP\roots.sst" | Import-Certificate -CertStoreLocation Cert:\LocalMachine\Root | Out-Null
Remove-Item "$env:TEMP\roots.sst"

Write-Host '=== long paths'
Set-Reg 'HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem' 'LongPathsEnabled' 1

Write-Host '=== Defender: skip scanning build folders (big speedup for GHC and make)'
foreach ($p in @('C:\build', 'C:\Tools', "$env:LOCALAPPDATA\Programs\stack", "$env:APPDATA\stack")) {
  try { Add-MpPreference -ExclusionPath $p } catch { Write-Host "could not exclude ${p}: $_" }
}

Write-Host '=== Git for Windows (provides Git Bash, the shell builds run in)'
Start-Process -Wait -FilePath "$oem\git.exe" -ArgumentList '/VERYSILENT', '/NORESTART', '/NOCANCEL', '/SP-', '/SUPPRESSMSGBOXES'
$git = 'C:\Program Files\Git\cmd\git.exe'
# per BUILD.md; matters for the git repos stack clones (stack.yaml extra-deps)
& $git config --system core.autocrlf false
& $git config --system core.eol lf
& $git config --system core.longpaths true

Write-Host '=== NSIS'
Start-Process -Wait -FilePath "$oem\nsis.exe" -ArgumentList '/S'

Write-Host '=== stack'
New-Item -ItemType Directory -Force -Path C:\Tools | Out-Null
Copy-Item "$oem\stack.exe" C:\Tools\stack.exe
$machinePath = [Environment]::GetEnvironmentVariable('Path', 'Machine')
if ($machinePath -notlike '*C:\Tools*') {
  [Environment]::SetEnvironmentVariable('Path', "$machinePath;C:\Tools", 'Machine')
}

Write-Host '=== OpenSSH server, with Git Bash as its shell'
Start-Process -Wait -FilePath msiexec.exe -ArgumentList '/i', "$oem\openssh.msi", '/qn'
if (-not (Test-Path HKLM:\SOFTWARE\OpenSSH)) { New-Item -Path HKLM:\SOFTWARE\OpenSSH -Force | Out-Null }
Set-ItemProperty -Path HKLM:\SOFTWARE\OpenSSH -Name DefaultShell -Value 'C:\Program Files\Git\bin\bash.exe'
Set-Service -Name sshd -StartupType Automatic
# first start creates C:\ProgramData\ssh and the host keys
Start-Service sshd
# admin accounts read keys from here, not ~/.ssh; sshd requires this exact ACL
$keys = 'C:\ProgramData\ssh\administrators_authorized_keys'
Copy-Item "$oem\authorized_keys" $keys
icacls.exe $keys /inheritance:r /grant '*S-1-5-32-544:F' /grant '*S-1-5-18:F'
if (-not (Get-NetFirewallRule -Name onyx-sshd -ErrorAction SilentlyContinue)) {
  New-NetFirewallRule -Name onyx-sshd -DisplayName 'OpenSSH Server (onyx build)' `
    -Direction Inbound -Protocol TCP -LocalPort 22 -Action Allow | Out-Null
}
# restart so new sessions see the PATH change above
Restart-Service sshd

New-Item -ItemType Directory -Force -Path C:\build | Out-Null
Set-Content -Path "$oem\setup-done" -Value 'ok'
Write-Host '=== done'
Stop-Transcript
