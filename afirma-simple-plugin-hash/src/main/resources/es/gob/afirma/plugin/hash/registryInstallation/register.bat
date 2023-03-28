
REM Realizar hash de fichero
REG QUERY HKCR\*\shell\afirma.hashFile || REG ADD HKCR\*\shell\afirma.hashFile /ve /d "Generar huella digital"
REG QUERY HKCR\*\shell\afirma.hashFile /v "Icon" || REG ADD HKCR\*\shell\afirma.hashFile /v "Icon" /d "%*"
REG QUERY HKCR\*\shell\afirma.hashFile\command || REG ADD HKCR\*\shell\afirma.hashFile\command /ve /d "%* createdigest \"%%1\" -gui"

REM Realizar hash de directorio
REG QUERY HKCR\Directory\shell\afirma.hashDirectory || REG ADD HKCR\Directory\shell\afirma.hashDirectory /ve /d "Generar huella digital"
REG QUERY HKCR\Directory\shell\afirma.hashDirectory /v "Icon" || REG ADD HKCR\Directory\shell\afirma.hashDirectory /v "Icon" /d "%*"
REG QUERY HKCR\Directory\shell\afirma.hashDirectory\command || REG ADD HKCR\Directory\shell\afirma.hashDirectory\command /ve /d "%* createdigest \"%%1\" -gui"

REM Comprobar hash binario de fichero
REG QUERY HKCR\.hash\shell\afirma.hash || REG ADD HKCR\.hash\shell\afirma.hash /ve /d "Comprobar huella digital"
REG QUERY HKCR\.hash\shell\afirma.hash /v "Icon" || REG ADD HKCR\.hash\shell\afirma.hash /v "Icon" /d "%*"
REG QUERY HKCR\.hash\shell\afirma.hash\command || REG ADD HKCR\.hash\shell\afirma.hash\command /ve /d "%* checkdigest -i \"%%1\" -gui"

REM Comprobar hash base 64 de fichero
REG QUERY HKCR\.hashb64\shell\afirma.hashb64 || REG ADD HKCR\.hashb64\shell\afirma.hashb64 /ve /d "Comprobar huella digital"
REG QUERY HKCR\.hashb64\shell\afirma.hashb64 /v "Icon" || REG ADD HKCR\.hashb64\shell\afirma.hashb64 /v "Icon" /d "%*"
REG QUERY HKCR\.hashb64\shell\afirma.hashb64\command || REG ADD HKCR\.hashb64\shell\afirma.hashb64\command /ve /d "%* checkdigest -i \"%%1\" -gui"

REM Comprobar hash hexadecimal de fichero
REG QUERY HKCR\.hexhash\shell\afirma.hexhash || REG ADD HKCR\.hexhash\shell\afirma.hexhash /ve /d "Comprobar huella digital"
REG QUERY HKCR\.hexhash\shell\afirma.hexhash /v "Icon" || REG ADD HKCR\.hexhash\shell\afirma.hexhash /v "Icon" /d "%*"
REG QUERY HKCR\.hexhash\shell\afirma.hexhash\command || REG ADD HKCR\.hexhash\shell\afirma.hexhash\command /ve /d "%* checkdigest -i \"%%1\" -gui"

REM Comprobar hash hexadecimal de fichero
REG QUERY HKCR\.hashfiles\shell\afirma.hashfiles || REG ADD HKCR\.hashfiles\shell\afirma.hashfiles /ve /d "Comprobar huella digital"
REG QUERY HKCR\.hashfiles\shell\afirma.hashfiles /v "Icon" || REG ADD HKCR\.hashfiles\shell\afirma.hashfiles /v "Icon" /d "%*"
REG QUERY HKCR\.hashfiles\shell\afirma.hashfiles\command || REG ADD HKCR\.hashfiles\shell\afirma.hashfiles\command /ve /d "%* checkdigest -i \"%%1\" -d -gui"
