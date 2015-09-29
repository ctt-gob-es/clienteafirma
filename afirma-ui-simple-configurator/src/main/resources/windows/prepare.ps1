$ScriptPath = Split-Path $MyInvocation.MyCommand.Path
New-Item "$ScriptPath\prepared" -type file