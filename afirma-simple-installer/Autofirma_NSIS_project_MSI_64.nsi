;Incluimos el Modern UI
  !include "MUI.nsh"
  !include "nsProcess.nsh"
  !include "Registry.nsh"
  !include "Sections.nsh"
  !include "FileFunc.nsh"
  
;Incluimos la libreria que permite obtener todas las rutas de los usuarios en el sistema
  !include "NTProfiles.nsh"

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
!define VERSION "1.10"
!define FILE_VERSION "1.10.0.0"

VIProductVersion "${FILE_VERSION}"
VIFileVersion "${FILE_VERSION}"
VIAddVersionKey "ProductName" "Autofirma"
VIAddVersionKey "ProductVersion" "${VERSION}"
VIAddVersionKey "FileVersion" "${VERSION}"
VIAddVersionKey "LegalCopyright" "(C) Gobierno de España"
VIAddVersionKey "FileDescription" "Autofirma (64 bits)"

;--------------------------------
;Macros para el tratamiento de los parametros de entrada del instalador

  !insertmacro GetParameters
  !insertmacro GetOptions

;--------------------------------
;Paginas del instalador
  
  ;Mostramos la pagina de bienvenida
  !insertmacro MUI_PAGE_WELCOME
  ;Pagina donde mostramos el contrato de licencia 
  !insertmacro MUI_PAGE_LICENSE $(LICENSE)
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
 !insertmacro MUI_LANGUAGE "English"
 !insertmacro MUI_LANGUAGE "Catalan"
 !insertmacro MUI_LANGUAGE "Basque"
 !insertmacro MUI_LANGUAGE "Galician"
 !insertmacro MUI_LANGUAGE "Valencian"
 
;Incluimos el archivo con los literales multidioma
  !include "lang_strings.nsh"

;Licencias
 LicenseLangString LICENSE ${LANG_SPANISH} "license\licencia_es.txt"
 LicenseLangString LICENSE ${LANG_ENGLISH} "license\licencia_en.txt"
 LicenseLangString LICENSE ${LANG_CATALAN} "license\licencia_ca.txt"
 LicenseLangString LICENSE ${LANG_GALICIAN} "license\licencia_gl.txt"
 LicenseLangString LICENSE ${LANG_BASQUE} "license\licencia_eu.txt"
 LicenseLangString LICENSE ${LANG_VALENCIAN} "license\licencia_va.txt"
 
 LicenseData $(LICENSE)

; Para generar instaladores en diferentes idiomas podemos escribir lo siguiente:
;  !insertmacro MUI_LANGUAGE ${LANGUAGE}
; De esta forma pasando la variable LANGUAGE al compilador podremos generar
; paquetes en distintos idiomas sin cambiar el script


;;;;;;;;;;;;;;;;;;;;;;;;;
; Configuration General ;
;;;;;;;;;;;;;;;;;;;;;;;;;
;Nuestro instalador se llamara si la version fuera la 1.0: Ejemplo-1.0.exe
OutFile Autofirma64\AutofirmaGenerator.exe

;Aqui comprobamos que en la version Inglesa se muestra correctamente el mensaje:
;Welcome to the $Name Setup Wizard
;Al tener reservado un espacio fijo para este mensaje, y al ser
;la frase en espanol mas larga:
; Bienvenido al Asistente de Instalacion de Aplicacion $Name
; no se ve el contenido de la variable $Name si el tamano es muy grande
Name "Autofirma"
Caption $(INST_CAPTION)
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
;Parametro que indica la ruta del fichero de configuracion PList con las preferencias para el sistema.
Var CONFIG_PATH
;Parametro que indica la ruta del fichero de actualizacion de preferencias del sistema.
Var UPDATE_CONFIG
;Parametro que indica si se debe utilizar el JRE instalada junto a Autofirma o no.
Var USE_SYSTEM_JRE
;Parametro que indica la ruta con el archivo de idioma
Var LANGUAGE_PATH
;Parametro que indica el idioma a usar por Autofirma
Var DEFAULT_LANGUAGE
;Variable para el registro
Var SIGN_STRING
;Variable para el registro
Var BINARY_STRING
;Variable para el registro
Var XADES_STRING

;Indicamos cual sera el directorio por defecto donde instalaremos nuestra
;aplicacion, el usuario puede cambiar este valor en tiempo de ejecucion.
InstallDir "$PROGRAMFILES64\Autofirma"

;Mensaje que mostraremos para indicarle al usuario que seleccione un directorio
DirText $(CHOOSE_DIR)

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

Section "Autofirma" sPrograma

	; En caso de haber recibido los archivos, comprobamos que existan
	${If} $CERTIFICATE_PATH != "false" 
	${AndIf} $KEYSTORE_PATH != "false" 
		IfFileExists "$CERTIFICATE_PATH" 0 +2
		IfFileExists "$KEYSTORE_PATH" +2 0
			Quit
	${EndIf}

	; Hacemos esta seccion de solo lectura para que no la desactiven
	SectionIn RO
	StrCpy $PATH "Autofirma"
	
	;Comprobamos que el sistema sea de 64bits y salimos en caso contrario
	System::Call 'kernel32::GetCurrentProcess()i.r0'
	System::Call 'kernel32::IsWow64Process(ir0,*i.r1)i.r2?e'
	pop $3
	IntCmp $1 1 +2 0 0
		Quit
	
	;Si se ha solicitado usar el JRE instalado en el sistema pero no se encuentra instalado, saldremos del instalador
	${If} $USE_SYSTEM_JRE == "true" 
		Call isValidJavaVersionAvailable
		Pop $0
		${If} $0 == "false"
			;Si se ha solicitado usar el JRE instalado en el sistema pero no se encuentra instalado, saldremos del instalador
			Quit
		${EndIf}
    ${EndIf}

	;Se cierra Firefox si esta abierto
	${nsProcess::FindProcess} "firefox.exe" $R2
	StrCmp $R2 0 0 +1
	${nsProcess::KillProcess} "firefox.exe" $R0

	${nsProcess::Unload}

	Sleep 2000

	;Eliminamos posibles versiones antiguas de 64 bits
	SetRegView 64
	Call RemoveOldVersions

	;Eliminamos posibles versiones antiguas de 32 bits
	SetRegView 32
	Call RemoveOldVersions

	;Volvemos a establecer la vista del registro acorde a la arquitectura del instalador
	SetRegView 64

	;Eliminamos el directorio de instalacion si existia
	RMDir /r '$INSTDIR\$PATH'

	;Iniciamos la instalacion

	;El desinstalador del MSI lo dejamos en el directorio principal
	;Este fichero ya esta incluido en el propio MSI pero cuando se instala el MSI sobre una
	;instalacion con EXE, al ejecutar el proceso de desinstalacion del EXE se elimina este
	;recurso. Por eso lo volvemos a crear una vez terminada la desinstalacion
	SetOutPath $INSTDIR
	File  no_ejecutar_x64.exe
	
	;Dejamos los ficheros de la aplicacion en un subdirectorio
	SetOutPath $INSTDIR\$PATH

	;Copiamos todos los ficheros que componen nuestra aplicacion
	File  Autofirma64\Autofirma.exe
	File  Autofirma64\AutofirmaConfigurador.exe
	File  Autofirma64\AutofirmaCommandLine.exe
	File  license\licencia_es.txt
	File  license\licencia_en.txt
	File  license\licencia_ca.txt
	File  license\licencia_va.txt
	File  license\licencia_eu.txt
	File  license\licencia_gl.txt
	File  ic_firmar.ico

	;Si no se ha solicitado usar el JRE instalado en el sistema, copiamos el JRE del instalador
	${If} $USE_SYSTEM_JRE != "true" 
		File /r java64\jre
	${EndIf}
	
	;Hacemos que la instalacion se realice para todos los usuarios del sistema
    SetShellVarContext all
	
	;Creamos un acceso directo en el escitorio salvo que se haya configurado lo contrario
	StrCmp $CREATE_ICON "false" +2
		CreateShortCut "$DESKTOP\Autofirma.lnk" "$INSTDIR\$PATH\Autofirma.exe"

	;Menu items
	CreateDirectory "$SMPROGRAMS\Autofirma"
	CreateShortCut "$SMPROGRAMS\Autofirma\Autofirma.lnk" "$INSTDIR\$PATH\Autofirma.exe"

	WriteRegStr HKLM "SOFTWARE\$PATH" "InstallDir" $INSTDIR
	WriteRegStr HKLM "SOFTWARE\$PATH" "Version" "${VERSION}"

	${If} $DEFAULT_LANGUAGE == "es_ES"
        StrCpy $SIGN_STRING "Firmar con Autofirma"
    ${ElseIf} $DEFAULT_LANGUAGE == "ca_ES"
        StrCpy $SIGN_STRING "Signar amb Autofirma"
    ${ElseIf} $DEFAULT_LANGUAGE == "va_ES"
        StrCpy $SIGN_STRING "Signar amb Autofirma"
    ${ElseIf} $DEFAULT_LANGUAGE == "gl_ES"
        StrCpy $SIGN_STRING "Asinar con Autofirma"
    ${ElseIf} $DEFAULT_LANGUAGE == "eu_ES"
        StrCpy $SIGN_STRING "Autofirmarekin sinatu"
    ${ElseIf} $DEFAULT_LANGUAGE == "en_US"
        StrCpy $SIGN_STRING "Sign with Autofirma"
    ${Else}
        StrCpy $SIGN_STRING "Firmar con Autofirma"
    ${EndIf}

	;Registro
	;CascadeAfirma.reg
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign" "" $SIGN_STRING
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign" "Icon" "$INSTDIR\$PATH\Autofirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign\command" "" '$INSTDIR\$PATH\Autofirma.exe sign -gui -i "%1"'
	
	${If} $DEFAULT_LANGUAGE == "es_ES"
        StrCpy $BINARY_STRING "Firma binaria CMS/CAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "ca_ES"
        StrCpy $BINARY_STRING "Signatura binària CMS/CAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "va_ES"
        StrCpy $BINARY_STRING "Signatura binària CMS/CAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "gl_ES"
        StrCpy $BINARY_STRING "Firma binaria CMS/CAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "eu_ES"
        StrCpy $BINARY_STRING "CMS/CAdES sinadura bitarra"
    ${ElseIf} $DEFAULT_LANGUAGE == "en_US"
        StrCpy $BINARY_STRING "Binary signature CMS/CAdES"
    ${Else}
        StrCpy $BINARY_STRING "Firma binaria CMS/CAdES"
    ${EndIf}
	
	;Verify
	; .csig
	WriteRegStr HKEY_CLASSES_ROOT ".csig" "" $BINARY_STRING
	WriteRegStr HKEY_CLASSES_ROOT ".csig\DefaultIcon" "" "$INSTDIR\$PATH\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify" "" "Verificar con Autofirma"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify\command" "" '$INSTDIR\$PATH\Autofirma.exe verify -gui -i "%1"'
	
	${If} $DEFAULT_LANGUAGE == "es_ES"
        StrCpy $XADES_STRING "Firma XMLDSig/XAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "ca_ES"
        StrCpy $XADES_STRING "Signatura XMLDSig/XAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "va_ES"
        StrCpy $XADES_STRING "Signatura XMLDSig/XAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "gl_ES"
        StrCpy $XADES_STRING "Firma XMLDSig/XAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "eu_ES"
        StrCpy $XADES_STRING "Sinadura: XMLDSig/XAdES"
    ${ElseIf} $DEFAULT_LANGUAGE == "en_US"
        StrCpy $XADES_STRING "Signature XMLDSig/XAdES"
    ${Else}
        StrCpy $XADES_STRING "Firma XMLDSig/XAdES"
    ${EndIf}

	;Verify
	; .xsig
	WriteRegStr HKEY_CLASSES_ROOT ".xsig" "" $XADES_STRING
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\DefaultIcon" "" "$INSTDIR\$PATH\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\shell\Verify" "" "Verificar con Autofirma"
	WriteRegStr HKEY_CLASSES_ROOT ".xsig\shell\Verify\command" "" '$INSTDIR\$PATH\Autofirma.exe verify -gui -i "%1"'
	
	;Protocolo afirma
	WriteRegStr HKEY_CLASSES_ROOT "afirma" "" "URL:Afirma Protocol"
	WriteRegStr HKEY_CLASSES_ROOT "afirma\DefaultIcon" "" "$INSTDIR\$PATH\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT "afirma" "URL Protocol" ""
	WriteRegStr HKEY_CLASSES_ROOT "afirma\shell\open\command" "" '$INSTDIR\$PATH\Autofirma.exe "%1"'
	
	; Eliminamos los certificados generados en caso de que existan por una instalacion previa
	IfFileExists "$INSTDIR\$PATH\Autofirma_ROOT.cer" 0 +2
		Delete "$INSTDIR\$PATH\Autofirma_ROOT.cer"
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
	
	; Comprobamos si el administrador le ha pasado el parametro con el fichero de configuracion PList
	StrCpy $R7 ""
	StrCmp $CONFIG_PATH "false" +2
		StrCpy $R7 '-config_path "$CONFIG_PATH"'
		
	; Comprobamos si el administrador le ha pasado el parametro con el fichero de actualizacion de preferencias
	StrCpy $R8 ""
	StrCmp $UPDATE_CONFIG "true" 0 +2
		StrCpy $R8 "-update_config"
		
	; Comprobamos si el administrador le ha pasado el parametro con el almacen
	StrCpy $R9 ""
	StrCmp $LANGUAGE_PATH "false" +2
		StrCpy $R9 '-language_path "$LANGUAGE_PATH"'
		
	; Comprobamos si el administrador le ha pasado el parametro con el almacen
	StrCpy $1 ""
	StrCmp $DEFAULT_LANGUAGE "false" +2
		StrCpy $1 '-default_language "$DEFAULT_LANGUAGE"'

	ExecWait '"$INSTDIR\$PATH\AutofirmaConfigurador.exe" $R4 $R5 $R6 $R7 $R8 $R9 $1 /passive'

	; Eliminamos los certificados de versiones previas del sistema
	Call DeleteCertificateOnInstall
	
	; Importamos el certificado en el sistema
	StrCpy $R7 "$INSTDIR\$PATH\Autofirma_ROOT.cer"
	Push $R7
	
	Sleep 2000
	Call AddCertificateToStore
	Pop $0

SectionEnd


Function .onInit

	${GetParameters} $R0
	ClearErrors
	;Para que el metodo GetOptions no de problemas, se deben proporcionar los parametros con un delimitador inicial como '/'
	;Este delimitador se agrega en el .wxs del instalador MSI
	${GetOptions} $R0 "/CREATE_ICON=" $CREATE_ICON
	${GetOptions} $R0 "/FIREFOX_SECURITY_ROOTS=" $FIREFOX_SECURITY_ROOTS
	${GetOptions} $R0 "/CERTIFICATE_PATH=" $CERTIFICATE_PATH
	${GetOptions} $R0 "/KEYSTORE_PATH=" $KEYSTORE_PATH
	${GetOptions} $R0 "/CONFIG_PATH=" $CONFIG_PATH
	${GetOptions} $R0 "/UPDATE_CONFIG=" $UPDATE_CONFIG
	${GetOptions} $R0 "/USE_SYSTEM_JRE=" $USE_SYSTEM_JRE
	${GetOptions} $R0 "/LANGUAGE_PATH=" $LANGUAGE_PATH
	${GetOptions} $R0 "/DEFAULT_LANGUAGE=" $DEFAULT_LANGUAGE
	
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
	
	${If} $LANGUAGE_PATH != "false"
		IfFileExists "$LANGUAGE_PATH" +2 0
			Abort
	${EndIf}

FunctionEnd

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

;Function isValidJavaVersionAvailable
; Check if its available by PATH a compatible Java version (8+).
; Usage:
;   Call isValidJavaVersionAvailable
;   Pop $R0
;		 - $R0 is "true" if a valid Java version is found, "false" otherwise.
Function isValidJavaVersionAvailable

	Push $0
	Push $1
	
	StrCpy $0 "true"
	
	; Comprobamos la version de Java y, si no es compatible, devolvemos false
	Call GetJavaVersion
	Pop $1
	IntCmp $1 8 +2 0 +2		; Si la version de Java no es la 8 o superior, se establece $0 a false
	  Goto invalidjava
	
	; Comprobamos que la arquitectura sea de 64 bits y, si lo es, devolvemos true
	Call IsJava64Arch
	Pop $1
	StrCmp $1 "true" return	; Si Java es de 64 bits, vamos a la salida de Java correcto
	
	invalidjava:
	  StrCpy $0 "false"
	
	return:
	  Push $1
	  Exch $0
	
FunctionEnd

;Function GetJavaVersion
; Check Java version avaible by PATH.
; Usage:
;   Call GetJavaVersion
;   Pop $R0
;		 - $R0 is the Java version (without "1." prefix) o -1 is it not available.
Function GetJavaVersion
 
  ; Reservamos variables introduciendo variables en la pila
  Push $0
  Push $1
  Push $2
  Push $3
  Push $4

  ; Ejecutamos "java -version", que devolvera un resultado valido si java esta en el PATH
  nsExec::ExecToStack 'java -version'
  Pop $0 ; Codigo de salida
  StrCmp $0 "error" novalidjava
  StrCmp $0 "timeout" novalidjava
  Pop $0 ; Texto de salida

  StrLen $1 $0	; Por aprovechamiento de variables se usa offset en negativo
  IntOp $1 $1 + 1	; Aumentamos el offset para que encaje bien en la logica del bucle
  
  ; Recorremos la cadena buscando las comillas
  buscarcomillas:
	IntOp $1 $1 - 1	; Reducimos el offset
    StrCmp $1 0 novalidjava  ; Cuando el offset llega a 0, se nos acabo la cadena
	; StrCpy VARIABLE TEXTO NUM_CARACT OFFSET (que al ser negativo cuenta desde atras)
	StrCpy $2 $0 1 -$1	; Tomamos el caracter del offset (contamos desde atras)
    StrCmp $2 "$\"" 0 buscarcomillas ; Comprobamos si ese caracter son las comillas:
									  ; - salimos del bucle si eran comillas
									  ; - seguimos buscando si no

  StrCpy $3 -1	; Numero de caracteres que componen la cadena de numero de version
  
  ; Seguimos recorriendo la cadena buscando un . y almacenando el texto
  buscarprimeraversion:
    IntOp $3 $3 + 1	; Incrementamos el numero de caracteres que componen la version
	IntOp $1 $1 - 1	; Reducimos el offset
    StrCmp $1 0 novalidjava  ; Cuando el offset llega a 0, se nos acabo la cadena
	StrCpy $2 $0 1 -$1	; Tomamos el caracter del offset (contamos desde atras)
	StrCmp $2 "." +2						; Comprobamos si ese caracter era un punto o unas comillas:
	StrCmp $2 "$\"" +1 buscarprimeraversion	; - salimos del bucle si era un punto
											; - seguimos buscando si no

  ; Comprobamos si esa primera version que hemos encontrado es valida
  IntOp $4 $3 + $1	; Contamos desde donde tenemos que tomar el texto
  StrCpy $2 $0 $3 -$4	; Tomamos el texto que va desde las comillas al punto (sin incluir)
  StrCpy $3 -1	; Reiniciamos el contador de version
  StrCmp $2 "1" buscarsegundaversion ; Si la version es "1", es que es una "1.x" y debemos buscar el segundo numero
  StrCpy $0 $2	; Si no, esa version es la salida
  Goto return
  
  ; Seguimos recorriendo la cadena mientras no encontremos un caracter que sea . o "
  buscarsegundaversion:
    IntOp $3 $3 + 1	; Incrementamos el numero de caracteres que componen la version
	IntOp $1 $1 - 1	; Reducimos el offset
    StrCmp $1 0 novalidjava  ; Cuando el offset llega a 0, se nos acabo la cadena
	StrCpy $2 $0 1 -$1	; Tomamos el caracter del offset (contamos desde atras)
    StrCmp $2 "." +3 	; Comprobamos si ese caracter es un punto y salimos del bucle en ese caso
	StrCmp $2 "$\"" +2	; Comprobamos si ese caracter son comillas y salimos del bucle en ese caso
  Goto buscarsegundaversion
  
  ; Esa cadena que hemos identificado, debe ser la version
  IntOp $4 $3 + $1	; Contamos desde donde tenemos que tomar el texto
  StrCpy $0 $0 $3 -$4	; Tomamos el texto desde la posicion calculada a la actual y esa es la salida
  Goto return
  
  novalidjava:
	StrCpy $0 -1
  
  ; Extraemos los valores reservados de la pila, salvo por el ultimo que sera la salida
  return:
    Pop $4
    Pop $3
    Pop $2
    Pop $1
	Exch $0

FunctionEnd

;Function isJava64Arch
; Check that the Java architecture is 64-bit.
; Usage:
;   Call isJava64Arch
;   Pop $R0
;		 - $R0 is "true" if the Java architecture is 64-bit o "false" otherwise.
Function isJava64Arch

  Push $0
  Push $1

  ; Ejecutamos "java -version", que devolvera un resultado valido si java esta en el PATH
  nsExec::ExecToStack 'java -Xinternalversion'
  Pop $0 ; Codigo de salida
  StrCmp $0 "error" novalidjava
  StrCmp $0 "timeout" novalidjava
  Pop $0 ; Texto de salida

  ; Consideraremos que es un Java de 64 bits si la cadena de salida contiene la subcadena "64-Bit"
  Push $0
  Push "64-Bit"
  Call StrContains
  Pop $1
  StrCpy $0 "true"
  StrCmp $1 "" novalidjava return

  novalidjava:
    StrCpy $0 "false"
  
  return:
    Pop $1
    Exch $0

FunctionEnd

; StrContains
;
; This function does a case sensitive searches for an occurrence of a substring in a string. 
; It returns the substring if it is found. 
; Otherwise it returns null(""). 
; Written by kenglish_hi
; Adapted from StrReplace written by dandaman32
Var STR_HAYSTACK
Var STR_NEEDLE
Var STR_CONTAINS_VAR_1
Var STR_CONTAINS_VAR_2
Var STR_CONTAINS_VAR_3
Var STR_CONTAINS_VAR_4
Var STR_RETURN_VAR

Function StrContains
  Exch $STR_NEEDLE
  Exch 1
  Exch $STR_HAYSTACK
    StrCpy $STR_RETURN_VAR ""
    StrCpy $STR_CONTAINS_VAR_1 -1
    StrLen $STR_CONTAINS_VAR_2 $STR_NEEDLE
    StrLen $STR_CONTAINS_VAR_4 $STR_HAYSTACK
    loop:
      IntOp $STR_CONTAINS_VAR_1 $STR_CONTAINS_VAR_1 + 1
      StrCpy $STR_CONTAINS_VAR_3 $STR_HAYSTACK $STR_CONTAINS_VAR_2 $STR_CONTAINS_VAR_1
      StrCmp $STR_CONTAINS_VAR_3 $STR_NEEDLE found
      StrCmp $STR_CONTAINS_VAR_1 $STR_CONTAINS_VAR_4 done
      Goto loop
    found:
      StrCpy $STR_RETURN_VAR $STR_NEEDLE
      Goto done
    done:
   Pop $STR_NEEDLE ;Prevent "invalid opcode" errors and keep the
   Exch $STR_RETURN_VAR  
FunctionEnd

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
        StrCpy $0 $(CANT_OPEN_KEYSTORE)
      ${Else}
        StrCpy $0 "success"
      ${EndIf}
      System::Call "crypt32::CertCloseStore(i r1, i 0)"
    ${Else}
      System::Call "crypt32::CertFreeCertificateContext(i r0)"
      StrCpy $0 $(CANT_OPEN_KEYSTORE)
    ${EndIf}
  ${Else}
    StrCpy $0 $(CANT_OPEN_CERT_FILE)
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

; Funcion para eliminar versiones anteriores de Autofirma. Las versiones se
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

	; Comprobamos si existe configuracion de usuario de Autofirma. Si no existe, vamos directamente a la
	; desinstalacion de la version anterior de Autofirma y, si existe, hacemos copia para restaurarla una
	; vez que desinstalemos esa version (el desinstalar una version elimina la configuracion).
	StrCpy $6 "0"
	ClearErrors
	EnumRegKey $0 HKCU "Software\JavaSoft\Prefs\es\gob\afirma\standalone\ui\preferences" 0
	IfErrors InitUninstall
	
			; Identificamos una clave en la que poder hacer la copia de los datos. Esta queda registrada en $R0
			ClearErrors
			StrCpy $0 0
			loopsearch:
				StrCpy $6 "Software\JavaSoft\Prefs\copia$0"
				EnumRegKey $1 HKCU $6 0
				IfErrors donesearch
				IntOp $0 $0 + 1
				Goto loopsearch
			donesearch:
		
			; Copiamos los valores a la nueva clave
			StrCpy $0 $6
			Push "Software\JavaSoft\Prefs\es\gob\afirma\standalone\ui\preferences"
			Push $0
			Call CopyRegValues

	InitUninstall:

	;Ejecutamos el desinstalador si se ha encontrado y lo eliminamos despues
	StrCmp $R1 "" +3 0
		ExecWait '"$R0\$R1" /S _?=$R0'
		RMDir /r $R1

	;Si el directorio de instalacion nuevo es distinto al anterior,
	;nos aseguramos del borrado eliminandolo
	StrCmp $INSTDIR $R0 +2 0
		RMDir /r /REBOOTOK $R0

	; Si habia una configuracion anterior de Autofirma, la restauramos
	StrCmp $6 "0" End 
		StrCpy $0 $6
		Push $0
		Push "Software\JavaSoft\Prefs\es\gob\afirma\standalone\ui\preferences"
		Call CopyRegValues
		
		; Eliminamos la copia
		DeleteRegKey HKCU $6

	End:

FunctionEnd

; Funcion para copiar los valores de una clave de registro a otra.
; Uso:
;	Push sourceDir
;	Push targetDir
;   Call CopyRegValues sourceKey targetKey
Function CopyRegValues
	; Obtenemos los dos primeros parametros de la pila
	Exch $R0	# Clave destino
	Exch
	Exch $R1	# Clave origen

	; Nos aseguramos de reservar variables para el metodo
	Push $0
	Push $1
	Push $2

	StrCpy $0 0
	looplist:
	  	  
	  ClearErrors
	  EnumRegValue $1 HKCU $R1 $0
	  IfErrors donelist
	  IntOp $0 $0 + 1
	  ReadRegStr $2 HKCU $R1 $1
	  WriteRegStr HKCU $R0 $1 $2
	  Goto looplist
	donelist:

	; Limpiamos las variables
	Pop $2
	Pop $1
	Pop $0

FunctionEnd																  

!include "WinMessages.nsh"
