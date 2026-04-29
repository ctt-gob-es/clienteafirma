@echo off
setlocal EnableExtensions

REM Si no se indica la ruta, salimos directamente
if "%~1"=="" (
    exit /b 0
)

set "TARGET=%~1"

REM Establecemos la ruta eliminandole la barra final si la tiene
if "%TARGET:~-1%"=="\" set "TARGET=%TARGET:~0,-1%"

REM Leemos el PATH del registro
for /f "tokens=2,*" %%A in (
    'reg query "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v Path 2^>nul'
) do set "SYSPATH=%%B"

REM Si no se ha podido leer el PATH, devolvemos un error
if not defined SYSPATH (
    exit /b 1
)

REM Si ya existe en el PATH, no hacemos nada
echo ;%SYSPATH%; | findstr /I /C:";%TARGET%;" >nul
if not errorlevel 1 (
    exit /b 0
)

REM Construir nuevo PATH
set "NEWPATH=%SYSPATH%"

REM Asegurar punto y coma final
if not "%NEWPATH:~-1%"==";" set "NEWPATH=%NEWPATH%;"

set "NEWPATH=%NEWPATH%%TARGET%"

REM Guardamos el nuevo PATH en el registro
reg add "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" ^
 /v Path /t REG_EXPAND_SZ /d "%NEWPATH%" /f >nul 2>&1

if errorlevel 1 (
    exit /b 1
)

exit /b 0
