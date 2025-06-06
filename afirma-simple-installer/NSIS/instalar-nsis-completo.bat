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

echo.
echo Copiando plugins desde: %PLUGINS_SRC%
echo A destino: %PLUGIN_DEST% e %INCLUDE_DEST%

:: Copiar plugins
copy "%PLUGINS_SRC%\nsProcess.dll" "%PLUGIN_DEST%" /Y
copy "%PLUGINS_SRC%\registry.dll" "%PLUGIN_DEST%" /Y
copy "%PLUGINS_SRC%\nsProcess.nsh" "%INCLUDE_DEST%" /Y
copy "%PLUGINS_SRC%\Registry.nsh" "%INCLUDE_DEST%" /Y

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
