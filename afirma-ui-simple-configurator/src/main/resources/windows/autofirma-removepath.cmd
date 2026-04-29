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

REM En NEWPATH iremos almacenando el nuevo PATH
set "NEWPATH="

REM En REST almacenaremos el fragmento de PATH que nos queda por procesar
set "REST=%SYSPATH%"

REM Asegurar punto y coma final para que funcione el resto del algoritmo
if not "%REST:~-1%"==";" set "REST=%REST%;"

:loop
if not defined REST goto done

REM Obtenemos el siguiente fragmento de PATH
for /f "delims=;" %%P in ("%REST%") do (
    set "ITEM=%%P"
)

REM Eliminamos el fragmento recogido de lo que queda pendiente de procesar
call set "REST=%%REST:*;=%%"

REM Eliminamos la barra final si la tiene
if "%ITEM:~-1%"=="\" set "ITEM=%ITEM:~0,-1%"

REM Comprobamos que no sea la ruta de la aplicacion y la agregamos en ese caso
if /I not "%ITEM%"=="%TARGET%" (
    if defined NEWPATH (
        set "NEWPATH=%NEWPATH%;%ITEM%"
    ) else (
        set "NEWPATH=%ITEM%"
    )
)

goto loop

:done

REM Guardamos el nuevo PATH en el registro
reg add "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" ^
 /v Path /t REG_EXPAND_SZ /d "%NEWPATH%" /f >nul 2>&1

if errorlevel 1 (
    exit /b 1
)

exit /b 0
