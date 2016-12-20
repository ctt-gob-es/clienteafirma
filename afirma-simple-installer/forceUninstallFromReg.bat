setlocal enableDelayedExpansion
for /f "delims=" %%i in ('reg query "HKLM\Software\Classes\Installer\Products" /f "AutoFirma" /s /e') do (
	set outputAutoFirma=%%i
	if /i "!outputAutoFirma:~0,4!"=="HKEY" (
		reg delete %%i /f
	)
) 

"C:\Program Files (x86)\AutoFirma\no_ejecutar_x86.exe"
"C:\Program Files\AutoFirma\no_ejecutar_x64.exe"
"C:\Program Files\AutoFirma\no_ejecutar_x86.exe"

@pause