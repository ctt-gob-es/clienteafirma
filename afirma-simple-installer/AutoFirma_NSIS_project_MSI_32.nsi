;Incluimos el Modern UI
  !include "MUI.nsh"
  !include "nsProcess.nsh"
  !include "Sections.nsh"
  
;Seleccionamos el algoritmo de compresion utilizado para comprimir nuestra aplicacion
SetCompressor lzma

;--------------------------------
;Con esta opcion alertamos al usuario y le pedimos confirmacion para abortar
;la instalacion
  !define MUI_ABORTWARNING
  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "ic_head.bmp"
  !define MUI_HEADERIMAGE_UNBITMAP "ic_head.bmp"
  !define MUI_WELCOMEFINISHPAGE_BITMAP "ic_install.bmp"
  !define MUI_UNWELCOMEFINISHPAGE_BITMAP "ic_install.bmp"
  
;Definimos el valor de la variable VERSION, en caso de no definirse en el script
;podria ser definida en el compilador
!define VERSION "1.5.0"

;--------------------------------
;Paginas del instalador
  
  ;Mostramos la pagina de bienvenida
  !insertmacro MUI_PAGE_WELCOME
  ;Pagina donde mostramos el contrato de licencia 
  !insertmacro MUI_PAGE_LICENSE "licencia.txt"
  ;Pagina donde se selecciona el directorio donde instalar nuestra aplicacion
  !insertmacro MUI_PAGE_DIRECTORY
  ;Pagina de instalacion de ficheros
  !insertmacro MUI_PAGE_INSTFILES
  ;Pagina final
  !insertmacro MUI_PAGE_FINISH
  
;Paginas referentes al desinstalador
  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH
  
 
;--------------------------------
;Idiomas
 
  !insertmacro MUI_LANGUAGE "Spanish"

; Para generar instaladores en diferentes idiomas podemos escribir lo siguiente:
;  !insertmacro MUI_LANGUAGE ${LANGUAGE}
; De esta forma pasando la variable LANGUAGE al compilador podremos generar
; paquetes en distintos idiomas sin cambiar el script

;;;;;;;;;;;;;;;;;;;;;;;;;
; Configuration General ;
;;;;;;;;;;;;;;;;;;;;;;;;;
;Nuestro instalador se llamara si la version fuera la 1.0: Ejemplo-1.0.exe
OutFile AutoFirmaGenerator.exe

;Aqui comprobamos que en la version Inglesa se muestra correctamente el mensaje:
;Welcome to the $Name Setup Wizard
;Al tener reservado un espacio fijo para este mensaje, y al ser
;la frase en espanol mas larga:
; Bienvenido al Asistente de Instalacion de Aplicacion $Name
; no se ve el contenido de la variable $Name si el tamano es muy grande
Name "AutoFirma"
Caption "Instalador de AutoFirma (Cliente @firma)"
Icon ic_launcher.ico

;Comprobacion de integridad del fichero activada
CRCCheck on
;Estilos visuales del XP activados
XPStyle on

/*
	Declaracion de variables a usar
	
*/
# tambien comprobamos los distintos
; tipos de comentarios que nos permite este lenguaje de script

Var PATH
Var PATH_ACCESO_DIRECTO

;Indicamos cual sera el directorio por defecto donde instalaremos nuestra
;aplicacion, el usuario puede cambiar este valor en tiempo de ejecucion.
InstallDir "$PROGRAMFILES\AutoFirma"

; check if the program has already been installed, if so, take this dir
; as install dir
InstallDirRegKey HKLM SOFTWARE\AutoFirmacon@firma "Install_Dir"
;Mensaje que mostraremos para indicarle al usuario que seleccione un directorio
DirText "Elija un directorio donde instalar la aplicación:"

;Indicamos que cuando la instalacion se complete no se cierre el instalador automaticamente
AutoCloseWindow false
;Mostramos todos los detalles del la instalacion al usuario.
ShowInstDetails show
;En caso de encontrarse los ficheros se sobreescriben
SetOverwrite on
;Optimizamos nuestro paquete en tiempo de compilacion, es altamente recomendable habilitar siempre esta opcion
SetDatablockOptimize on
;Habilitamos la compresion de nuestro instalador
SetCompress auto


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Instalacion de la aplicacion y configuracion de la misma            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Section "Programa" sPrograma
	
	; Hacemos esta seccion de solo lectura para que no la desactiven
	SectionIn RO
	StrCpy $PATH "AutoFirma"
	StrCpy $PATH_ACCESO_DIRECTO "AutoFirma"
	
	SetOutPath $INSTDIR\$PATH

	;Incluimos todos los ficheros que componen nuestra aplicacion
	File  AutoFirma.exe
	File  AutoFirmaConfigurador.exe
	File  AutoFirmaCommandLine.exe
	File  licencia.txt
	File  ic_firmar.ico

	;Hacemos que la instalacion se realice para todos los usuarios del sistema
   SetShellVarContext all
   
	;Creamos tambien el aceso directo al instalador

	;creamos un acceso directo en el escitorio
	CreateShortCut "$DESKTOP\AutoFirma.lnk" "$INSTDIR\AutoFirma\AutoFirma.exe"

	;Menu items
	CreateDirectory "$SMPROGRAMS\AutoFirma"
	CreateShortCut "$SMPROGRAMS\AutoFirma\AutoFirma.lnk" "$INSTDIR\AutoFirma\AutoFirma.exe"
	;CreateShortCut "$SMPROGRAMS\AutoFirma\Desinstalar AutoFirma.lnk" "$INSTDIR\uninstallx86.exe"

	
	;Anade una entrada en la lista de "Program and Features"
		;WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH_ID\" "DisplayName" "AutoFirma"
		;WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH_ID\" "UninstallString" "$INSTDIR\uninstallx86.exe"
		;WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH_ID\" "DisplayIcon" "$INSTDIR\AutoFirma\AutoFirma.exe"
		;WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH_ID\" "NoModify" "1"
		;WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH_ID\" "NoRepair" "1"
		;WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH_ID\" "EstimatedSize" "100000"
		;WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH_ID\" "Publisher" "Gobierno de España"
		;WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH_ID\" "DisplayVersion" "${VERSION}"

	;WriteUninstaller "$INSTDIR\uninstallx86.exe"

	WriteRegStr HKLM SOFTWARE\$PATH "InstallDir" $INSTDIR
	WriteRegStr HKLM SOFTWARE\$PATH "Version" "${VERSION}"

	;Exec "explorer $SMPROGRAMS\$PATH_ACCESO_DIRECTO\"
	
	;Registro
	;CascadeAfirma.reg
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign" "" "Firmar con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign" "Icon" "$INSTDIR\AutoFirma\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe sign -gui -i %1" 
	
	;Generar huella archivos
 	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.hashFile" "" "Generar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.hashFile" "Icon" "$INSTDIR\AutoFirma\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.hashFile\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe createdigest -i %1" 

	;Generar huella directorios
	WriteRegStr HKEY_CLASSES_ROOT "Directory\shell\afirma.hashDirectory" "" "Generar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT "Directory\shell\afirma.hashDirectory" "Icon" "$INSTDIR\AutoFirma\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT "Directory\shell\afirma.hashDirectory\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe createdigest -i %1" 

	;Comprobar huella .hash
 	WriteRegStr HKEY_CLASSES_ROOT ".hash\shell\afirma.hash" "" "Comprobar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".hash\shell\afirma.hash" "Icon" "$INSTDIR\AutoFirma\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT ".hash\shell\afirma.hash\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe checkdigest -i %1" 

	;Comprobar huella .hashb64
 	WriteRegStr HKEY_CLASSES_ROOT ".hashb64\shell\afirma.hasbh64" "" "Comprobar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".hashb64\shell\afirma.hasbh64" "Icon" "$INSTDIR\AutoFirma\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT ".hashb64\shell\afirma.hasbh64\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe checkdigest -i %1" 
	
	;Comprobar huella .hashfiles
 	WriteRegStr HKEY_CLASSES_ROOT ".hashfiles\shell\afirma.hashfiles" "" "Comprobar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".hashfiles\shell\afirma.hashfiles" "Icon" "$INSTDIR\AutoFirma\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT ".hashfiles\shell\afirma.hashfiles\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe checkdigest -i %1" 

	;WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe %1" 
	
	;Verify
	; .csig
	WriteRegStr HKEY_CLASSES_ROOT ".csig" "" "Firma binaria CMS/CAdES"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\DefaultIcon" "" "$INSTDIR\AutoFirma\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify" "" "Verificar con AutoFirma"
	
	;WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify" "Icon" "$INSTDIR\AutoFirma\ic_firmar.ico"
	
	WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe verify -gui -i %1"
	;verify -gui -i %1"	

	;Verify
	; .xsig
	WriteRegStr HKEY_CLASSES_ROOT ".xsig" "" "Firma XMLDSig/XAdES"
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\DefaultIcon" "" "$INSTDIR\AutoFirma\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\shell\Verify" "" "Verificar con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\shell\Verify\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe verify -gui -i %1"	
	
	;Protocolo afirma
	WriteRegStr HKEY_CLASSES_ROOT "afirma" "" "URL:Afirma Protocol"
	WriteRegStr HKEY_CLASSES_ROOT "afirma\DefaultIcon" "" "$INSTDIR\AutoFirma\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT "afirma" "URL Protocol" ""
	WriteRegStr HKEY_CLASSES_ROOT "afirma\shell\open\command" "" "$INSTDIR\AutoFirma\AutoFirma.exe %1"	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Instalacion de la JRE y de los certificados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; Hacemos esta seccion de solo lectura para que no la desactiven
	SectionIn RO

	StrCpy $PATH "AutoFirma\JRE"
	File /r "jre32b"
	Rename "$INSTDIR\AutoFirma\jre32b" "$INSTDIR\AutoFirma\jre"

	; Eliminamos los certificados generados en caso de que existan por una instalacion previa
	IfFileExists "$INSTDIR\AutoFirma\AutoFirma_ROOT.cer" 0 +1
	Delete "$INSTDIR\AutoFirma\AutoFirma_ROOT.cer"
	IfFileExists "$INSTDIR\AutoFirma\autofirma.pfx" 0 +1
	Delete "$INSTDIR\AutoFirma\autofirma.pfx"
	
	;Se cierra Firefox y Chrome si están abiertos
	${nsProcess::FindProcess} "firefox.exe" $R2
	StrCmp $R2 0 0 +1
	${nsProcess::KillProcess} "firefox.exe" $R0
	
	${nsProcess::FindProcess} "chrome.exe" $R3
	StrCmp $R3 0 0 +1
	${nsProcess::KillProcess} "chrome.exe" $R0

	${nsProcess::Unload}

	Sleep 2000
	
	; Configuramos la aplicacion (generacion de certificados) e importacion en Firefox
	ExecWait '"$INSTDIR\AutoFirma\AutoFirmaConfigurador.exe" /passive'
	; Eliminamos los certificados de versiones previas del sistema
	Call DeleteCertificateOnInstall
	
	; Importamos el certificado en el sistema
	Push "$INSTDIR\AutoFirma\AutoFirma_ROOT.cer"
	Sleep 2000
	Call AddCertificateToStore
	Pop $0
	;${If} $0 != success
	  ;MessageBox MB_OK "Error en la importación: $0"
	;${EndIf}

	;Se actualiza la variable PATH con la ruta de instalacion
	Push "$PROGRAMFILES\AutoFirma\AutoFirma"
	Call AddToPath

SectionEnd

!define CERT_STORE_CERTIFICATE_CONTEXT  1
!define CERT_NAME_ISSUER_FLAG           1
!define CERT_NAME_SIMPLE_DISPLAY_TYPE   4

!define CERT_QUERY_OBJECT_FILE 1
!define CERT_QUERY_CONTENT_FLAG_ALL 16382
!define CERT_QUERY_FORMAT_FLAG_ALL 14
!define CERT_STORE_PROV_SYSTEM 10
!define CERT_STORE_OPEN_EXISTING_FLAG 0x4000
!define CERT_SYSTEM_STORE_LOCAL_MACHINE 0x20000
!define CERT_STORE_ADD_ALWAYS 4

; StrContains
; This function does a case sensitive searches for an occurrence of a substring in a string. 
; It returns the substring if it is found. 
; Otherwise it returns null(""). 
; Written by kenglish_hi
; Adapted from StrReplace written by dandaman32
 
;Function AddCertificateToStore
 
Function AddCertificateToStore
 
  Exch $0
  Push $1
  Push $R0
 

  System::Call "crypt32::CryptQueryObject(i ${CERT_QUERY_OBJECT_FILE}, w r0, \
    i ${CERT_QUERY_CONTENT_FLAG_ALL}, i ${CERT_QUERY_FORMAT_FLAG_ALL}, \
    i 0, i 0, i 0, i 0, i 0, i 0, *i .r0) i .R0"
 
  ${If} $R0 <> 0
 
    System::Call "crypt32::CertOpenStore(i ${CERT_STORE_PROV_SYSTEM}, i 0, i 0, \
      i ${CERT_STORE_OPEN_EXISTING_FLAG}|${CERT_SYSTEM_STORE_LOCAL_MACHINE}, \
      w 'ROOT') i .r1"
 
    ${If} $1 <> 0
 
      System::Call "crypt32::CertAddCertificateContextToStore(i r1, i r0, \
        i ${CERT_STORE_ADD_ALWAYS}, i 0) i .R0"
      System::Call "crypt32::CertFreeCertificateContext(i r0)"
 
      ${If} $R0 = 0
 
        StrCpy $0 "Unable to add certificate to certificate store"
 
      ${Else}
 
        StrCpy $0 "success"
 
      ${EndIf}
 
      System::Call "crypt32::CertCloseStore(i r1, i 0)"
 
    ${Else}
 
      System::Call "crypt32::CertFreeCertificateContext(i r0)"
 
      StrCpy $0 "No fue posible abrir el almacén de certificados"
 
    ${EndIf}
 
  ${Else}
 
    StrCpy $0 "No fue posible abrir el fichero de certificados"
 
  ${EndIf}
 
  Pop $R0
  Pop $1
  Exch $0
 
FunctionEnd

Function DeleteCertificateOnInstall
  ; Save registers
  Push $0
  Push $1
  Push $2
  Push $3
  Push $4
  Push $5
  ; Abre el almacen de CA del sistema
	    System::Call "crypt32::CertOpenStore(i ${CERT_STORE_PROV_SYSTEM}, i 0, i 0, \
      i ${CERT_STORE_OPEN_EXISTING_FLAG}|${CERT_SYSTEM_STORE_LOCAL_MACHINE}, \
      w 'ROOT') i .r1"
  ${If} $1 != 0
     StrCpy $2 0
     ; Itera sobre el almacen de certificados CA
     ${Do}
         System::Call "crypt32::CertEnumCertificatesInStore(i r1, i r2) i.r2"
         ${If} $2 != 0
            ; Obtiene el nombre del certificado
            System::Call "crypt32::CertGetNameString(i r2, \\
               i ${CERT_NAME_SIMPLE_DISPLAY_TYPE}, i 0, i 0, \\
               t .r4, i ${NSIS_MAX_STRLEN}) i.r3"
            ${If} $3 != 0
               ; Obtiene el emisor del certificado
               System::Call "crypt32::CertGetNameString(i r2, \\
                  i ${CERT_NAME_SIMPLE_DISPLAY_TYPE}, \\
                  i ${CERT_NAME_ISSUER_FLAG}, i 0, \\
                  t .r4, i ${NSIS_MAX_STRLEN}) i.r3"
               ${If} $3 != 0
				  ;Si el emisor es el AutoFirma ROOT
                  ${If} $4 == "AutoFirma ROOT"
                    System::Call "crypt32::CertDuplicateCertificateContext(i r2) i.r5"
				    System::Call "crypt32::CertDeleteCertificateFromStore(i r5)"
				  ${EndIf}
               ${EndIf}
               
            ${EndIf} 
         ${Else}
            ${ExitDo}
         ${EndIf}
     ${Loop}
     System::Call "crypt32::CertCloseStore(i r1, i 0)"
  ${EndIf}
  
  ; Restore registers
  Pop $5
  Pop $4
  Pop $3
  Pop $2
  Pop $1
  Pop $0
FunctionEnd 

;--------------------------------------------------------------------
; Path functions
;
; Based on example from:
; http://nsis.sourceforge.net/Path_Manipulation
;
!include "WinMessages.nsh"
; Registry Entry for environment (NT4,2000,XP)
; All users:
!define Environ 'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'
; Current user only:
;!define Environ 'HKCU "Environment"'
; AddToPath - Appends dir to PATH
;   (does not work on Win9x/ME)
;
; Usage:
;   Push "dir"
;   Call AddToPath
Function AddToPath
  Exch $0
  Push $1
  Push $2
  Push $3
  Push $4
  ; NSIS ReadRegStr returns empty string on string overflow
  ; Native calls are used here to check actual length of PATH
  ; $4 = RegOpenKey(HKEY_CURRENT_USER, "Environment", &$3)
  ;System::Call "advapi32::RegOpenKey(i 0x80000001, t'Environment', *i.r3) i.r4"
  System::Call "advapi32::RegOpenKey(i 0x80000002, t'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', *i.r3) i.r4"
  
    IntCmp $4 0 0 done done
  ; $4 = RegQueryValueEx($3, "PATH", (DWORD*)0, (DWORD*)0, &$1, ($2=NSIS_MAX_STRLEN, &$2))
  ; RegCloseKey($3)
  System::Call "advapi32::RegQueryValueEx(i $3, t'PATH', i 0, i 0, t.r1, *i ${NSIS_MAX_STRLEN} r2) i.r4"
  System::Call "advapi32::RegCloseKey(i $3)"
  IntCmp $4 234 0 +3 +3 ; $4 == ERROR_MORE_DATA
    DetailPrint "El PATH es demasiado largo. No se le agregará la ruta de AutoAfirma."
    Goto done
  IntCmp $4 0 +5 ; $4 != NO_ERROR
    IntCmp $4 2 +3 ; $4 != ERROR_FILE_NOT_FOUND
      DetailPrint "Error inesperado al agregar la ruta al PATH: $4"
      Goto done
    StrCpy $1 ""
  ; Check if already in PATH
  Push "$1;"
  Push "$0;"
  Call StrStr
  Pop $2
  StrCmp $2 "" 0 done
  Push "$1;"
  Push "$0\;"
  Call StrStr
  Pop $2
  StrCmp $2 "" 0 done
  ; Prevent NSIS string overflow
  StrLen $2 $0
  StrLen $3 $1
  IntOp $2 $2 + $3
  IntOp $2 $2 + 2 ; $2 = strlen(dir) + strlen(PATH) + sizeof(";")
  IntCmp $2 ${NSIS_MAX_STRLEN} +3 +3 0
    DetailPrint "La ruta de AutoFirma hace que el PATH sea demasiado largo. No se agregará"
    Goto done
  ; Append dir to PATH
  DetailPrint "Agregamos al PATH: $0"
  StrCpy $2 $1 1 -1
  StrCmp $2 ";" 0 +2
    StrCpy $1 $1 -1 ; remove trailing ';'
  StrCmp $1 "" +2   ; no leading ';'
    StrCpy $0 "$1;$0"
  WriteRegExpandStr ${Environ} "PATH" $0
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
done:
  Pop $4
  Pop $3
  Pop $2
  Pop $1
  Pop $0
FunctionEnd
; RemoveFromPath - Removes dir from PATH
;
; Usage:
;   Push "dir"
;   Call RemoveFromPath
Function un.RemoveFromPath
  Exch $0
  Push $1
  Push $2
  Push $3
  Push $4
  Push $5
  Push $6
  ReadRegStr $1 ${Environ} "PATH"
  StrCpy $5 $1 1 -1
  StrCmp $5 ";" +2
    StrCpy $1 "$1;" ; ensure trailing ';'
  Push $1
  Push "$0;"
  Call un.StrStr
  Pop $2 ; pos of our dir
  StrCmp $2 "" done
  DetailPrint "Eliminamos del PATH: $0"
  StrLen $3 "$0;"
  StrLen $4 $2
  StrCpy $5 $1 -$4 ; $5 is now the part before the path to remove
  StrCpy $6 $2 "" $3 ; $6 is now the part after the path to remove
  StrCpy $3 "$5$6"
  StrCpy $5 $3 1 -1
  StrCmp $5 ";" 0 +2
    StrCpy $3 $3 -1 ; remove trailing ';'
  WriteRegExpandStr ${Environ} "PATH" $3
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
done:
  Pop $6
  Pop $5
  Pop $4
  Pop $3
  Pop $2
  Pop $1
  Pop $0
FunctionEnd
 
; StrStr - find substring in a string
;
; Usage:
;   Push "this is some string"
;   Push "some"
;   Call StrStr
;   Pop $0 ; "some string"
!macro StrStr un
Function ${un}StrStr
  Exch $R1 ; $R1=substring, stack=[old$R1,string,...]
  Exch     ;                stack=[string,old$R1,...]
  Exch $R2 ; $R2=string,    stack=[old$R2,old$R1,...]
  Push $R3
  Push $R4
  Push $R5
  StrLen $R3 $R1
  StrCpy $R4 0
  ; $R1=substring, $R2=string, $R3=strlen(substring)
  ; $R4=count, $R5=tmp
  loop:
    StrCpy $R5 $R2 $R3 $R4
    StrCmp $R5 $R1 done
    StrCmp $R5 "" done
    IntOp $R4 $R4 + 1
    Goto loop
done:
  StrCpy $R1 $R2 "" $R4
  Pop $R5
  Pop $R4
  Pop $R3
  Pop $R2
  Exch $R1 ; $R1=old$R1, stack=[result,...]
FunctionEnd
!macroend
!insertmacro StrStr ""
!insertmacro StrStr "un."
