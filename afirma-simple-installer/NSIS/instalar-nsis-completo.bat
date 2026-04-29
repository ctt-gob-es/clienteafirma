@echo off
:: Verifica si se está ejecutando como admin
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo Solicitando privilegios de administrador...
    powershell -Command "Start-Process '%~f0' -Verb runAs"
    exit /b
)

echo ================================
echo Ejecutando como administrador...
echo ================================

:: Rutas relativas
set SCRIPT_DIR=%~dp0
set PLUGINS_SRC=%SCRIPT_DIR%plugins
set LANGUAGES_SRC=%SCRIPT_DIR%Contrib\Language Files

:: Rutas de destino
set NSIS=C:\Program Files (x86)\NSIS
set PLUGIN_DEST=%NSIS%\Plugins\x86-unicode
set INCLUDE_DEST=%NSIS%\Include
set LANG_DEST=%NSIS%\Contrib\Language files

:: Archivos requeridos
set FILE_1=nsProcess.dll
set FILE_2=registry.dll
set FILE_3=nsProcess.nsh
set FILE_4=registry.nsh
set FILE_5=lang_strings.nsh
set FILE_6=NTProfiles.nsh

echo.
echo Verificando archivos necesarios en: %PLUGINS_SRC%

:: Comprobación de existencia
set MISSING=0
for %%F in (%FILE_1% %FILE_2% %FILE_3% %FILE_4% %FILE_5% %FILE_6%) do (
    if not exist "%PLUGINS_SRC%\%%F" (
        echo FALTA: %%F
        set MISSING=1
    )
)

if %MISSING%==1 (
    echo.
    echo ================================
    echo ERROR: Faltan uno o más archivos requeridos.
    echo Operación cancelada.
    echo ================================
    pause
    exit /b
)

echo.
echo Todos los archivos requeridos están presentes.
echo Copiando .dll a: %PLUGIN_DEST%
echo Copiando .nsh a: %INCLUDE_DEST%

:: Copiar .dll
copy "%PLUGINS_SRC%\*.dll" "%PLUGIN_DEST%" /Y

:: Copiar .nsh
copy "%PLUGINS_SRC%\*.nsh" "%INCLUDE_DEST%" /Y

echo.
echo Copiando archivos de idioma desde: %LANGUAGES_SRC%
echo A destino: %LANG_DEST% e %INCLUDE_DEST%

:: Copiar idiomas
for %%f in ("%LANGUAGES_SRC%\*.nlf") do (
    copy "%%f" "%LANG_DEST%" /Y
)

for %%f in ("%LANGUAGES_SRC%\*.nsh") do (
    copy "%%f" "%LANG_DEST%" /Y
    copy "%%f" "%INCLUDE_DEST%" /Y
)

echo.
echo ================================
echo Instalación completada correctamente.
echo ================================
pause