@echo off
setlocal
if "%AFIRMA_SIGN_PFX%"=="" (
  echo ERROR: AFIRMA_SIGN_PFX no definido. No se firma. 1>&2
  exit /b 1
)
if "%AFIRMA_SIGN_PASS%"=="" (
  echo ERROR: AFIRMA_SIGN_PASS no definido. No se firma. 1>&2
  exit /b 1
)
signtool sign /f "%AFIRMA_SIGN_PFX%" /p "%AFIRMA_SIGN_PASS%" Autofirma_64_installer.msi
exit /b %ERRORLEVEL%
