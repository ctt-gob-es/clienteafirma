;Incluimos el Modern UI
  !include "MUI.nsh"
  !include "nsProcess.nsh"
  !include "Sections.nsh"
  !include "FileFunc.nsh"	 
  
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
!define VERSION "1.8.0"
!define FILE_VERSION "1.8.0.0"

VIProductVersion "${FILE_VERSION}"
VIFileVersion "${FILE_VERSION}"
VIAddVersionKey "ProductName" "AutoFirma"
VIAddVersionKey "ProductVersion" "${VERSION}"
VIAddVersionKey "FileVersion" "${VERSION}"
VIAddVersionKey "LegalCopyright" "(C) Gobierno de España"
VIAddVersionKey "FileDescription" "AutoFirma (32 bits)"

;--------------------------------
;Macros para el tratamiento de los parametros de entrada del instalador

  !insertmacro GetParameters
  !insertmacro GetOptions

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
OutFile AutoFirma32\AutoFirmaGenerator.exe

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
;Ruta relativa interna de instalacion
Var PATH
;Parametro que indica si se debe crear el acceso directo en el escritorio
Var CREATE_ICON
;Parametro que indica si se debe activar el uso del almacen del sistema en Firefox
Var FIREFOX_SECURITY_ROOTS			
;Parametro que indica la ruta del certificado a pasar por el administrador
Var CERTIFICATE_PATH
;Parametro que indica la ruta del almacen de claves a pasar por el administrador
Var KEYSTORE_PATH		  


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

Function .onInit

	${GetParameters} $R0
		ClearErrors
	;Para que el metodo GetOptions no de problemas, se deben proporcionar los parametros con un delimitador inicial como '/'
	;Este delimitador se agrega en el .wxs del instalador MSI
	${GetOptions} $R0 "/CREATE_ICON=" $CREATE_ICON
	${GetOptions} $R0 "/FIREFOX_SECURITY_ROOTS=" $FIREFOX_SECURITY_ROOTS  
	${GetOptions} $R0 "/CERTIFICATE_PATH=" $CERTIFICATE_PATH  
	${GetOptions} $R0 "/KEYSTORE_PATH=" $KEYSTORE_PATH 
	
	; Comprobamos que no solo se informe de un parametro, sino de
	; los dos o de ninguno
	${If} $KEYSTORE_PATH != "false"
	${AndIf} $CERTIFICATE_PATH == "false" 
		Abort
	${EndIf}

	${If} $CERTIFICATE_PATH != "false" 
	${AndIf} $KEYSTORE_PATH == "false" 
		Abort
	${EndIf}

	; En caso de haber indicado los archivos, comprobamos que existan
	${If} $CERTIFICATE_PATH != "false" 
	${AndIf} $KEYSTORE_PATH != "false" 
		IfFileExists "$CERTIFICATE_PATH" 0 +2
		IfFileExists "$KEYSTORE_PATH" +2 0
			Abort
	${EndIf}
	
FunctionEnd

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Instalacion de la aplicacion y configuracion de la misma            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Section "Programa" sPrograma

	; En caso de haber recibido los archivos, comprobamos que existan
	${If} $CERTIFICATE_PATH != "false" 
	${AndIf} $KEYSTORE_PATH != "false" 
		IfFileExists "$CERTIFICATE_PATH" 0 +2
		IfFileExists "$KEYSTORE_PATH" +2 0
			Quit
	${EndIf}
	
	; Hacemos esta seccion de solo lectura para que no la desactiven
	SectionIn RO
	StrCpy $PATH "AutoFirma"

	;Se cierra Firefox y Chrome si están abiertos
	${nsProcess::FindProcess} "firefox.exe" $R2
	StrCmp $R2 0 0 +1
	${nsProcess::KillProcess} "firefox.exe" $R0

	${nsProcess::FindProcess} "chrome.exe" $R3
	StrCmp $R3 0 0 +1
	${nsProcess::KillProcess} "chrome.exe" $R0

	${nsProcess::Unload}

	Sleep 2000

	;Eliminamos posibles versiones antiguas de 64 bits (Solo en Windows 64)
	System::Call 'kernel32::GetCurrentProcess()i.r0'
	System::Call 'kernel32::IsWow64Process(ir0,*i.r1)i.r2?e'
	pop $3
	IntCmp $1 1 0 +3 0
		SetRegView 64
		Call RemoveOldVersions

	;Eliminamos posibles versiones antiguas de 32 bits
	SetRegView 32
	Call RemoveOldVersions

	;Establecemos la vista del registro acorde a la arquitectura del instalador
	SetRegView 32

	;Eliminamos el directorio de instalacion si existia
	RMDir /r '$INSTDIR\$PATH'

	;Iniciamos la instalacion

	;El desinstalador del MSI lo dejamos en el directorio principal
	;Este fichero ya esta incluido en el propio MSI pero cuando se instala el MSI sobre una
	;instalacion con EXE, al ejecutar el proceso de desinstalacion del EXE se elimina este
	;recurso. Por eso lo volvemos a crear una vez terminada la desinstalacion
	SetOutPath $INSTDIR
	File  no_ejecutar_x86.exe
	
	;Dejamos los ficheros de la aplicacion en un subdirectorio
	SetOutPath $INSTDIR\$PATH

	;Copiamos la JRE
	File /r java32\jre
	
	;Copiamos todos los ficheros que componen nuestra aplicacion
	File  AutoFirma32\AutoFirma.exe
	File  AutoFirma32\AutoFirmaConfigurador.exe
	File  AutoFirma32\AutoFirmaCommandLine.exe
	File  licencia.txt
	File  ic_firmar.ico

	;Hacemos que la instalacion se realice para todos los usuarios del sistema
   SetShellVarContext all

	;Creamos un acceso directo en el escitorio salvo que se haya configurado lo contrario
	StrCmp $CREATE_ICON "false" +2
		CreateShortCut "$DESKTOP\AutoFirma.lnk" "$INSTDIR\$PATH\AutoFirma.exe"

	;Menu items
	CreateDirectory "$SMPROGRAMS\AutoFirma"
	CreateShortCut "$SMPROGRAMS\AutoFirma\AutoFirma.lnk" "$INSTDIR\$PATH\AutoFirma.exe"

	WriteRegStr HKLM "SOFTWARE\$PATH" "InstallDir" $INSTDIR
	WriteRegStr HKLM "SOFTWARE\$PATH" "Version" "${VERSION}"

	;Registro
	;CascadeAfirma.reg
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign" "" "Firmar con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign" "Icon" "$INSTDIR\$PATH\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign\command" "" '$INSTDIR\$PATH\AutoFirma.exe sign -gui -i "%1"'
	
	;Generar huella archivos
 	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.hashFile" "" "Generar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.hashFile" "Icon" "$INSTDIR\$PATH\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.hashFile\command" "" '$INSTDIR\$PATH\AutoFirma.exe createdigest -i "%1"'

	;Generar huella directorios
	WriteRegStr HKEY_CLASSES_ROOT "Directory\shell\afirma.hashDirectory" "" "Generar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT "Directory\shell\afirma.hashDirectory" "Icon" "$INSTDIR\$PATH\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT "Directory\shell\afirma.hashDirectory\command" "" '$INSTDIR\$PATH\AutoFirma.exe createdigest -i "%1"'

	;Comprobar huella .hash
 	WriteRegStr HKEY_CLASSES_ROOT ".hash\shell\afirma.hash" "" "Comprobar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".hash\shell\afirma.hash" "Icon" "$INSTDIR\$PATH\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT ".hash\shell\afirma.hash\command" "" '$INSTDIR\$PATH\AutoFirma.exe checkdigest -i "%1"'

	;Comprobar huella .hashb64
 	WriteRegStr HKEY_CLASSES_ROOT ".hashb64\shell\afirma.hasbh64" "" "Comprobar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".hashb64\shell\afirma.hasbh64" "Icon" "$INSTDIR\$PATH\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT ".hashb64\shell\afirma.hasbh64\command" "" '$INSTDIR\$PATH\AutoFirma.exe checkdigest -i "%1"'
	
	;Comprobar huella .hashfiles
 	WriteRegStr HKEY_CLASSES_ROOT ".hashfiles\shell\afirma.hashfiles" "" "Comprobar huella digital con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".hashfiles\shell\afirma.hashfiles" "Icon" "$INSTDIR\$PATH\AutoFirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT ".hashfiles\shell\afirma.hashfiles\command" "" '$INSTDIR\$PATH\AutoFirma.exe checkdigest -i "%1"'
	
	;Verify
	; .csig
	WriteRegStr HKEY_CLASSES_ROOT ".csig" "" "Firma binaria CMS/CAdES"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\DefaultIcon" "" "$INSTDIR\$PATH\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify" "" "Verificar con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify\command" "" '$INSTDIR\$PATH\AutoFirma.exe verify -gui -i "%1"'

	;Verify
	; .xsig
	WriteRegStr HKEY_CLASSES_ROOT ".xsig" "" "Firma XMLDSig/XAdES"
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\DefaultIcon" "" "$INSTDIR\$PATH\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\shell\Verify" "" "Verificar con AutoFirma"
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\shell\Verify\command" "" '$INSTDIR\$PATH\AutoFirma.exe verify -gui -i "%1"'
	
	;Protocolo afirma
	WriteRegStr HKEY_CLASSES_ROOT "afirma" "" "URL:Afirma Protocol"
	WriteRegStr HKEY_CLASSES_ROOT "afirma\DefaultIcon" "" "$INSTDIR\$PATH\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT "afirma" "URL Protocol" ""
	WriteRegStr HKEY_CLASSES_ROOT "afirma\shell\open\command" "" '$INSTDIR\$PATH\AutoFirma.exe "%1"'

	; Eliminamos los certificados generados en caso de que existan por una instalacion previa
	IfFileExists "$INSTDIR\$PATH\AutoFirma_ROOT.cer" 0 +2
		Delete "$INSTDIR\$PATH\AutoFirma_ROOT.cer"
	IfFileExists "$INSTDIR\$PATH\autofirma.pfx" 0 +2
		Delete "$INSTDIR\$PATH\autofirma.pfx"

	; Configuramos la aplicacion (generacion de certificados) e importacion en Firefox
	StrCpy $R4 ""
	StrCmp $FIREFOX_SECURITY_ROOTS "true" 0 +2
		StrCpy $R4 "-firefox_roots"
	
	; Comprobamos si el administrador le ha pasado el parametro con el certificado
	StrCpy $R5 ""
	StrCmp $CERTIFICATE_PATH "false" +2
		StrCpy $R5 '-certificate_path "$CERTIFICATE_PATH"'
	
	; Comprobamos si el administrador le ha pasado el parametro con el almacen
	StrCpy $R6 ""
	StrCmp $KEYSTORE_PATH "false" +2
		StrCpy $R6 '-keystore_path "$KEYSTORE_PATH"'

	ExecWait '"$INSTDIR\$PATH\AutoFirmaConfigurador.exe" $R4 $R5 $R6 /passive'

	; Eliminamos los certificados de versiones previas del sistema
	Call DeleteCertificateOnInstall
	
	; Importamos el certificado en el sistema
	StrCpy $R7 "$INSTDIR\$PATH\AutoFirma_ROOT.cer"
	Push $R7
	
	Sleep 2000
	Call AddCertificateToStore
	Pop $0
	;${If} $0 != success
	  ;MessageBox MB_OK "Error en la importación: $0"
	;${EndIf}

	;Se actualiza la variable PATH con la ruta de instalacion
	Push "$INSTDIR\$PATH"
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

; Funcion para eliminar versiones anteriores de AutoFirma. Las versiones se
; buscan a traves del registro, para lo cual afecta si se tiene configurada la
; vista de 32 o 64 bits
; Uso:
;   Call RemoveOldVersions
Function RemoveOldVersions
  
	; Comprueba que no este ya instalada
	ClearErrors

	;Comprobamos si la aplicacion ya esta registrada
	ReadRegStr $R0 HKLM SOFTWARE\$PATH "InstallDir"
	${If} ${Errors}
		Goto End
	${EndIf}

	; Usamos la ruta de instalacion de la version anterior para eliminarla del PATH. Si el desinstalador
	; de la version anterior funcionase bien, esto no seria necesario
	Push "$R0\AutoFirma"
	Call RemoveFromPath

	;Buscamos el desinstalador de esa verion y lo ejecutamos
	;Esto funciona tambien para las versiones MSI, ya que estas
	;ya se habran desregistrado al iniciarse el proceso de instalacion
	;de este nuevo MSI
	StrCpy $R1 ""
	IfFileExists "$R0\uninstall.exe" 0 +2
		StrCpy $R1 "uninstall.exe"
	IfFileExists "$R0\no_ejecutar_x64.exe" 0 +2
		StrCpy $R1 "no_ejecutar_x64.exe"
	IfFileExists "$R0\no_ejecutar_x86.exe" 0 +2
		StrCpy $R1 "no_ejecutar_x86.exe"

	;Ejecutamos el desinstalador si se ha encontrado y lo eliminamos despues
	StrCmp $R1 "" +3 0
		ExecWait '"$R0\$R1" /S _?=$R0'
		RMDir /r $R1

	;Si el directorio de instalacion nuevo es distinto al anterior,
	;nos aseguramos del borrado eliminandolo
	StrCmp $INSTDIR $R0 +2 0
		RMDir /r /REBOOTOK $R0

	End:

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
Function RemoveFromPath
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
  Call StrStr
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
Function StrStr
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
