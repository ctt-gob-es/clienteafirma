
REM Registro del esquema afirma
REG QUERY HKCR\afirma || REG ADD HKCR\afirma /ve /d "URL:Afirma Protocol"
REG QUERY HKCR\afirma /v "URL Protocol" || REG ADD HKCR\afirma /v "URL Protocol" /d ""
REG QUERY HKCR\afirma\DefaultIcon || REG ADD HKCR\afirma\DefaultIcon /ve /d "%*\ic_firmar.ico"
REG QUERY HKCR\afirma\shell\open\command || REG ADD HKCR\afirma\shell\open\command /ve /d "%*\Autofirma.exe \"%%1\""
