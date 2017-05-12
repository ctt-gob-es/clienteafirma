@if (1==1) @if(1==0) @ELSE
@echo off&SETLOCAL ENABLEEXTENSIONS
>nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"||(
    cscript //E:JScript //nologo "%~f0"
    @goto :EOF
)
echo.Performing admin tasks...
call "$$PATH_EXE$$" "$$INSTALL_DIR$$"
@goto :EOF
@end @ELSE
ShA=new ActiveXObject("Shell.Application")
ShA.ShellExecute("$$PATH_EXE$$","$$INSTALL_DIR$$","","runas",5);
@end