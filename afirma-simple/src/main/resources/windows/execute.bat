@if (1==1) @if(1==0) @ELSE
@echo off&SETLOCAL ENABLEEXTENSIONS
>nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"||(
    cscript //E:JScript //nologo "%~f0"
    @goto :EOF
)
echo.Performing admin tasks...
call "$$PATH_BAT$$" "\"$$PATH_DIR_EXE$$\""
@goto :EOF
@end @ELSE
ShA=new ActiveXObject("Shell.Application")
ShA.ShellExecute("cmd.exe", "/c $$PATH_BAT$$ $$PATH_DIR_EXE$$","","runas",5);
@end