﻿;Incluimos el Modern UI
  !include "MUI.nsh"
  !include "nsProcess.nsh"
  !include "Registry.nsh"
  !include "Sections.nsh"

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
  !define VersionCheckNew "!insertmacro MVersionCheck"
  
;Definimos el valor de la variable VERSION, en caso de no definirse en el script
;podria ser definida en el compilador
!define VERSION "1.9"
!define FILE_VERSION "1.9.0.0"

VIProductVersion "${FILE_VERSION}"
VIFileVersion "${FILE_VERSION}"
VIAddVersionKey "ProductName" "Autofirma"
VIAddVersionKey "ProductVersion" "${VERSION}"
VIAddVersionKey "FileVersion" "${VERSION}"
VIAddVersionKey "LegalCopyright" "(C) Gobierno de España"
VIAddVersionKey "FileDescription" "Autofirma (64 bits)"

;--------------------------------
;Paginas del instalador
  
  ;Mostramos la pagina de bienvenida
  !insertmacro MUI_PAGE_WELCOME
  ;Pagina donde mostramos el contrato de licencia 
  !insertmacro MUI_PAGE_LICENSE "licencia.txt"
  ;Pagina donde se selecciona el directorio donde instalar nuestra aplicacion
  !insertmacro MUI_PAGE_DIRECTORY
  ;Pagina personalizada con las opciones de configuracion
  Page custom createConfigPage leaveConfigPage
  ;Pagina con las opciones de instalacion
  !insertmacro MUI_PAGE_COMPONENTS
  SpaceTexts none
  ;Pagina de instalacion de ficheros
  !insertmacro MUI_PAGE_INSTFILES
  ;Pagina final
  !insertmacro MUI_PAGE_FINISH
  ;Paginas referentes al desinstalador
  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH


; Creamos la pagina de configuracion personalizada
!include nsDialogs.nsh

; Variables para el guardado de los componentes de la pagina de configuracion
Var StartMenu_Integration_Checkbox
Var StartMenu_Integration_Checkbox_State
Var Shorcut_Integration_Checkbox
Var Shorcut_Integration_Checkbox_State
Var Firefox_Integration_Checkbox
Var Firefox_Integration_Checkbox_State

;Parametro que indica si se encuentra alguna versión de JRE instalada en el sistema.
Var INSTALL_JRE 

!define SECTION_ON ${SF_SELECTED} # 0x1

Function createConfigPage
  !insertmacro MUI_HEADER_TEXT "Opciones de integración avanzadas" "Seleccione las opciones de integración que desee que configure Autofirma"
  
  nsDialogs::Create 1018
  Pop $0

  ${If} $0 == error
    Abort
  ${EndIf}

  ; Creamos los elementos de interfaz
  ${NSD_CreateCheckbox} 0 0 100% 10u "Agregar al menú inicio."
  Pop $StartMenu_Integration_Checkbox
  
  ${NSD_CreateCheckbox} 0 17u 100% 10u "Crear acceso directo en el escritorio."
  Pop $Shorcut_Integration_Checkbox

  ${NSD_CreateCheckbox} 0 34u 100% 10u "Configurar Firefox para que confíe en los certificados raíz del sistema."
  Pop $Firefox_Integration_Checkbox
  
  ; Restablecemos el valor por si hubiese cambio de pantalla
  ${If} $StartMenu_Integration_Checkbox_State == ${BST_CHECKED}
    ${NSD_Check} $StartMenu_Integration_Checkbox
  ${EndIf}
  ${If} $Shorcut_Integration_Checkbox_State == ${BST_CHECKED}
    ${NSD_Check} $Shorcut_Integration_Checkbox
  ${EndIf}
  ${If} $Firefox_Integration_Checkbox_State == ${BST_CHECKED}
    ${NSD_Check} $Firefox_Integration_Checkbox
  ${EndIf}
  
  ; Establecemos el mismo comportamiento al pulsar Atras en la pagina que al continuar
  ${NSD_OnBack} "leaveConfigPage"

nsDialogs::Show
  
FunctionEnd

Function leaveConfigPage

	${NSD_GetState} $StartMenu_Integration_Checkbox $StartMenu_Integration_Checkbox_State
	${NSD_GetState} $Shorcut_Integration_Checkbox $Shorcut_Integration_Checkbox_State
	${NSD_GetState} $Firefox_Integration_Checkbox $Firefox_Integration_Checkbox_State

FunctionEnd

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
OutFile Autofirma64/Autofirma_64_v1_9_installer.exe

;Aqui comprobamos que en la version Inglesa se muestra correctamente el mensaje:
;Welcome to the $Name Setup Wizard
;Al tener reservado un espacio fijo para este mensaje, y al ser
;la frase en espanol mas larga:
; Bienvenido al Asistente de Instalacion de Aplicacion $Name
; no se ve el contenido de la variable $Name si el tamano es muy grande
Name "Autofirma"
Caption "Instalador de Autofirma"
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

;Indicamos cual sera el directorio por defecto donde instalaremos nuestra
;aplicacion, el usuario puede cambiar este valor en tiempo de ejecucion.
InstallDir "$PROGRAMFILES64\Autofirma"

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
;Personalizamos el mensaje de desinstalacion
UninstallText "Desinstalador de Autofirma."

!macro MVersionCheck Ver1 Ver2 OutVar
 Push "${Ver1}"
 Push "${Ver2}"
  Call VersionCheck
 Pop "${OutVar}"
!macroend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Instalacion de la aplicacion y configuracion de la misma            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Section "Autofirma" sPrograma

	; Hacemos esta seccion de solo lectura para que no la desactiven
	SectionIn RO
	StrCpy $PATH "Autofirma"

	;Comprobamos que el sistema sea de 64bits y salimos en caso contrario
	System::Call 'kernel32::GetCurrentProcess()i.r0'
	System::Call 'kernel32::IsWow64Process(ir0,*i.r1)i.r2?e'
	pop $3
	${If} $1 != 1
		MessageBox MB_OK "No se puede instalar Autofirma 64 bits en un entorno 32 bits." 
		Quit
	${EndIf}

	;Comprobamos si ya existe una version de Autofirma instalada. Si existe, se devolvera
	;su numero de version y se dejara configurado el registro a 32 o 64 bits segun corresponda
	Call CheckVersionInstalled
	Pop $R1
	${If} $R1 != ""
		; Si es la misma version o superior, detenemos el proceso. Si no, se elimina.
		${VersionCheckNew} $R1 ${VERSION} "$R2"
		${If} $R2 = 0
		  MessageBox MB_OK "Esta versión de Autofirma ya está instalada." 
		  Quit
		${ElseIf} $R2 <> 2
		  MessageBox MB_OK "La versión actual de Autofirma es más nueva que la que se quiere instalar."
		  Quit
		${EndIf}
		Call RemoveOldVersions
	${EndIf}

	;Volvemos a establecer la vista del registro acorde a la arquitectura del instalador
	SetRegView 64

	;Limpiamos el directorio al que se van a copiar los ficheros y bloqueamos la ejecucion
	;hasta que este listo
	IfFileExists $INSTDIR\$PATH 0 +4
		RMDir /r '$INSTDIR\$PATH'
		Sleep 3000
		Goto -3

	;Dejamos los ficheros de la aplicacion en un subdirectorio
	SetOutPath $INSTDIR\$PATH
	
	;Copiamos todos los ficheros que componen nuestra aplicacion
	File  Autofirma64\Autofirma.exe
	File  Autofirma64\AutofirmaConfigurador.exe
	File  Autofirma64\AutofirmaCommandLine.exe
	File  licencia.txt
	File  ic_firmar.ico

	;Copiamos la JRE en caso de que no se vaya usar el JRE instalado en el sistema
	${If} $INSTALL_JRE == "true" 
		File /r java64\jre
    ${EndIf}

	;Hacemos que la instalacion se realice para todos los usuarios del sistema
    SetShellVarContext all
   
	;Se pide que se cierre Firefox si esta abierto
	
	loopFirefox:
	${nsProcess::FindProcess} "firefox.exe" $R2
	StrCmp $R2 0 0 +2
		MessageBox MB_OK|MB_DEFBUTTON1|MB_ICONEXCLAMATION 'Cierre el navegador Mozilla Firefox para continuar con la instalación de Autofirma.' IDOK loopFirefox

	${nsProcess::Unload}
	
	Sleep 2000

	;Si se ha configurado, creamos un acceso directo en el escritorio
	${If} $Shorcut_Integration_Checkbox_State == 1
		CreateShortCut "$DESKTOP\Autofirma.lnk" "$INSTDIR\$PATH\Autofirma.exe"
	${Endif}

	;Si se ha configurado, creamos el grupo de accesos en el menu inicio
	${If} $StartMenu_Integration_Checkbox_State == 1
		CreateDirectory "$SMPROGRAMS\Autofirma"
		CreateShortCut "$SMPROGRAMS\Autofirma\Autofirma.lnk" "$INSTDIR\$PATH\Autofirma.exe"
		CreateShortCut "$SMPROGRAMS\Autofirma\Desinstalar.lnk" "$INSTDIR\uninstall.exe"
	${Endif}

	;Anade una entrada en la lista de "Program and Features"
	WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" "DisplayName" "Autofirma"
	WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" "UninstallString" "$INSTDIR\uninstall.exe"
	WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" "DisplayIcon" "$INSTDIR\$PATH\Autofirma.exe"
	WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" "NoModify" "1"
	WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" "NoRepair" "1"
	WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" "EstimatedSize" "100000"
	WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" "Publisher" "Gobierno de España"
	WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" "DisplayVersion" "${VERSION}"

	WriteUninstaller "$INSTDIR\uninstall.exe"

	WriteRegStr HKLM "SOFTWARE\$PATH" "InstallDir" $INSTDIR
	WriteRegStr HKLM "SOFTWARE\$PATH" "Version" "${VERSION}"

	;Registro
	;CascadeAfirma.reg
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign" "" "Firmar con Autofirma"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign" "Icon" "$INSTDIR\$PATH\Autofirma.exe"
	WriteRegStr HKEY_CLASSES_ROOT "*\shell\afirma.sign\command" "" '$INSTDIR\$PATH\Autofirma.exe sign -gui -i "%1"'

	;Verify
	; .csig
	WriteRegStr HKEY_CLASSES_ROOT ".csig" "" "Firma binaria CMS/CAdES"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\DefaultIcon" "" "$INSTDIR\$PATH\ic_firmar.ico"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify" "" "Verificar con Autofirma"
	WriteRegStr HKEY_CLASSES_ROOT ".csig\shell\Verify\command" "" '$INSTDIR\$PATH\Autofirma.exe verify -gui -i "%1"'

	;Verify
	; .xsig
	WriteRegStr HKEY_CLASSES_ROOT ".xsig" "" "Firma XMLDSig/XAdES"
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
	StrCpy $R0 ""
	${If} $Firefox_Integration_Checkbox_State == ${BST_CHECKED}
		StrCpy $R0 "-firefox_roots"
	${Endif}
	ExecWait '"$INSTDIR\$PATH\AutofirmaConfigurador.exe" $R0 /passive'
	
	; Eliminamos los certificados de versiones previas del sistema
	Call DeleteCertificateOnInstall

	; Importamos el certificado en el sistema
	Sleep 2000
	Push "$INSTDIR\$PATH\Autofirma_ROOT.cer"
	Call AddCertificateToStore
	Pop $0
	;${If} $0 != success
	  ;MessageBox MB_OK "Error en la importación: $0"
	;${EndIf}

	;Se actualiza la variable PATH con la ruta de instalacion
	Push "$INSTDIR\$PATH"
	Call AddToPath

SectionEnd

Section "Java Runtime Environment" sJRE
	; No se instala aqui la JRE, ya que el propio Autofirma depende de ella. Si se pide instalar,
	; se instalara desde la seccion de Autofirma para asegurar que ya estar disponible cuando sea necesario
SectionEnd

	!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
	!insertmacro MUI_DESCRIPTION_TEXT ${sPrograma} "Archivos necesarios para la ejecución del programa"
	!insertmacro MUI_DESCRIPTION_TEXT ${sJRE} "Entorno de ejecución de Java. Obligatorio cuando no se encuentra Java en el equipo"
	!insertmacro MUI_FUNCTION_DESCRIPTION_END

Function .onInit

	StrCpy $StartMenu_Integration_Checkbox_State ${BST_CHECKED}
	StrCpy $Shorcut_Integration_Checkbox_State ${BST_CHECKED}
	
	; Por defecto, se instalara la JRE
	StrCpy $INSTALL_JRE "true"
	
	; Identificamos si hay una version de Java compatible accesible en el PATH
	Call isValidJavaVersionAvailable
	Pop $0
	
	; Si no hay una version compatible de Java, desactivamos el que se pueda omitir la
	; instalacion de la JRE
	StrCmp $0 "false" 0 +2		
		SectionSetFlags ${sJRE} 17
		
FunctionEnd

; Desactivamos la instalacion de la JRE si el usuario deselecciona
; esta opcion
Function .onSelChange

	Push $0
 
	SectionGetFlags ${sJRE} $0
	IntOp $0 $0 & ${SECTION_ON}
	${If} $0 == 1 
		StrCpy $INSTALL_JRE "true"
    ${Else}
		StrCpy $INSTALL_JRE "false"
    ${EndIf}
	
	Pop $0
	
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

;Identifica la version instalada de Autofirma.
;Devuelve la cadena con el numero de version y deja el registro configurado
;para la arquitectura correspondiente a la version identificada
Function CheckVersionInstalled

  ;Buscamos en 64 bits			  
  SetRegView 64
  ReadRegStr $R0 HKLM "SOFTWARE\$PATH" "Version"

  ;Si lo hemos encontrado ya, salimos 
  IfErrors +2
	Goto End

  ;Buscamos en 32 bits
  SetRegView 32
  ReadRegStr $R0 HKLM "SOFTWARE\$PATH" "Version"
 
  End:
	Push $R0 ; output
FunctionEnd

Function VersionCheck
 Exch $R0 ; second version number
 Exch
 Exch $R1 ; first version number
 Push $R2
 Push $R3
 Push $R4
 Push $R5 ; second version part
 Push $R6 ; first version part
 
  StrCpy $R1 $R1.
  StrCpy $R0 $R0.
  
 Next: StrCmp $R0$R1 "" 0 +3
  StrCpy $R0 0
  Goto Done
 
  StrCmp $R0 "" 0 +2
   StrCpy $R0 0.
  StrCmp $R1 "" 0 +2
   StrCpy $R1 0.
 
 StrCpy $R2 0
  IntOp $R2 $R2 + 1
  StrCpy $R4 $R1 1 $R2
  StrCmp $R4 . 0 -2
    StrCpy $R6 $R1 $R2
    IntOp $R2 $R2 + 1
    StrCpy $R1 $R1 "" $R2
 
 StrCpy $R2 0
  IntOp $R2 $R2 + 1
  StrCpy $R4 $R0 1 $R2
  StrCmp $R4 . 0 -2
    StrCpy $R5 $R0 $R2
    IntOp $R2 $R2 + 1
    StrCpy $R0 $R0 "" $R2
 
 IntCmp $R5 0 Compare
 IntCmp $R6 0 Compare
 
 StrCpy $R3 0
  StrCpy $R4 $R6 1 $R3
  IntOp $R3 $R3 + 1
  StrCmp $R4 0 -2
 
 StrCpy $R2 0
  StrCpy $R4 $R5 1 $R2
  IntOp $R2 $R2 + 1
  StrCmp $R4 0 -2
 
 IntCmp $R3 $R2 0 +2 +4
 Compare: IntCmp 1$R5 1$R6 Next 0 +3
 
  StrCpy $R0 1
  Goto Done
  StrCpy $R0 2
 
 Done:
 Pop $R6
 Pop $R5
 Pop $R4
 Pop $R3
 Pop $R2
 Pop $R1
 Exch $R0 ; output
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

Function un.DeleteCertificate
  Exch $R0
  ; Save registers
  Push $0
  Push $1
  Push $2
  Push $3
  Push $4
  Push $5
  Push $6
  Push $7
  Push $8
  Push $9

  ; Abrimos el certificado que queremos eliminar del almacen
  System::Call "crypt32::CryptQueryObject(i ${CERT_QUERY_OBJECT_FILE}, w R0, \
    i ${CERT_QUERY_CONTENT_FLAG_ALL}, i ${CERT_QUERY_FORMAT_FLAG_ALL}, \
    i 0, i 0, i 0, i 0, i 0, i 0, *i .R0) i .r9"
 
  ; Si hemos podido cargar el certificado, buscaremos su nombre y emisor.
  ; Si no, se usara el nombre y emisor por defecto ("Autofirma ROOT")
  ${If} $9 <> 0
	  ; Leemos el nombre del certificado
	  System::Call "crypt32::CertGetNameString(i R0, \\
		   i ${CERT_NAME_SIMPLE_DISPLAY_TYPE}, \\
		   i 0, i 0, \\
		   t .r7, i ${NSIS_MAX_STRLEN}) i.r6"

	   ; Leemos el emisor del certificado
	   System::Call "crypt32::CertGetNameString(i R0, \\
		  i ${CERT_NAME_SIMPLE_DISPLAY_TYPE}, \\
		  i ${CERT_NAME_ISSUER_FLAG}, i 0, \\
		  t .r8, i ${NSIS_MAX_STRLEN}) i.r6"
  ${Else}
    ; Usamos el nombre y emisor por defecto ("Autofirma ROOT")
	StrCpy $7 "Autofirma ROOT"
	StrCpy $8 "Autofirma ROOT"
  ${EndIf}

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
			   t .r3, i ${NSIS_MAX_STRLEN}) i.r6"
			${If} $6 != 0
			   ; Obtiene el emisor del certificado
			   System::Call "crypt32::CertGetNameString(i r2, \\
				  i ${CERT_NAME_SIMPLE_DISPLAY_TYPE}, \\
				  i ${CERT_NAME_ISSUER_FLAG}, i 0, \\
				  t .r4, i ${NSIS_MAX_STRLEN}) i.r6"
			   ${If} $6 != 0
				  ; Si el subject y el issuer coinciden con los del certificado que
				  ; buscamos, se elimina
				  ${If} $3 == $7
				  ${AndIf} $4 == $8
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
  Pop $9
  Pop $8
  Pop $7
  Pop $6
  Pop $5
  Pop $4
  Pop $3
  Pop $2
  Pop $1
  Pop $0
FunctionEnd 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Configuracion de la desinstalacion ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Section "uninstall"
	StrCpy $PATH "Autofirma"
	SetShellVarContext all

	; ==== Desinstalador EXE - INICIO ====
   
	;Se pide que se cierre Firefox si esta abierto
	loopFirefox:
		${nsProcess::FindProcess} "firefox.exe" $R2
		StrCmp $R2 0 0 +2
			MessageBox MB_OK|MB_DEFBUTTON1|MB_ICONEXCLAMATION 'Cierre el navegador Mozilla Firefox para continuar con la desinstalación de Autofirma.' IDOK loopFirefox
	
	; ==== Desinstalador EXE - FIN ====
	
	; ==== Desinstalador MSI - INICIO ====
   
	; Se fuerza el cierre de Firefox si esta abierto. Los saltos cuentan un paso mas
	; (+4 en lugar de +3) porque si no se queda en un bucle infinito
;	loopFirefox:
;		${nsProcess::FindProcess} "firefox.exe" $R2
;		StrCmp $R2 0 0 +4
;			${nsProcess::KillProcess} "firefox.exe" $R0
;			Goto loopFirefox

	; ==== Desinstalador MSI - FIN ====

	${nsProcess::Unload}
	
	Sleep 2000

	; Eliminamos del almacen el certificado de CA que tenemos en el
	; directorio de instalacion. Si no esta ese, se eliminara el por defecto
	Push "$INSTDIR\$PATH\Autofirma_ROOT.cer"
	Call un.DeleteCertificate
	
	; Ejecutamos el proceso de desinstalacion del Configurador java
	ExecWait '"$INSTDIR\$PATH\AutofirmaConfigurador.exe" -uninstall /passive'

	;Borramos el subdirectorio con todos los recursos salvo el desinstalador
	;y bloqueamos la ejecucion hasta que este listo
	IfFileExists $INSTDIR\$PATH 0 +4
		RMDir /r '$INSTDIR\$PATH'
		Sleep 3000
		Goto -3

	;Borrar directorio de instalacion si es un directorio valido (contiene "Autofirma" o es una subcarpeta de Program Files)
	Push $INSTDIR
	Push "Program Files (x86)\"
	Call un.StrContains
	Pop $0
	StrCmp $0 "Program Files (x86)\" DirectorioValido
	Push $INSTDIR
	Push "Program Files\"
	Call un.StrContains
	Pop $0
	StrCmp $0 "Program Files\" DirectorioValido
	Push $INSTDIR
	Push $PATH
	Call un.StrContains
	Pop $0
	StrCmp $0 "" PostValidacion

	DirectorioValido:
		RMDir /r $INSTDIR

	PostValidacion:
	;Borrar accesos directos del escritorio y menu inicio
	Delete "$DESKTOP\Autofirma.lnk"
	RMDir /r $SMPROGRAMS\$PATH
	
	;Eliminamos las entradas de registro en la vista de 64 bits
	SetRegView 64
	Call un.UninstallFromRegistry

	;Eliminamos las entradas de registro en la vista de 32 bits
	SetRegView 32
	Call un.UninstallFromRegistry

SectionEnd


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

!macro StrContains un
Function ${un}StrContains
  Exch $STR_NEEDLE
  Exch 1
  Exch $STR_HAYSTACK
  ; Uncomment to debug
  ;MessageBox MB_OK 'STR_NEEDLE = $STR_NEEDLE STR_HAYSTACK = $STR_HAYSTACK '
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
!macroend
!insertmacro StrContains ""
!insertmacro StrContains "un."

; Funcion para eliminar versiones anteriores de Autofirma. Las versiones se
; buscan a traves del registro, para lo cual afecta si se tiene configurada la
; vista de 32 o 64 bits
; Uso:
;   Call RemoveOldVersions
Function RemoveOldVersions
  
	; Comprueba que este ya instalada
	ClearErrors
	ReadRegStr $R0 HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH\" "UninstallString"

	${If} ${Errors}
		Goto CheckAutofirmaVersion
	${EndIf}
	
	; Se ha encontrado Autofirma instalado
	ReadRegStr $R1 HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH\" "DisplayVersion"
	${VersionCheckNew} $R1 ${VERSION} "$R2"
	${If} $R2 = 2
		; Informamos de que existe una version anterior, ofrecemos el eliminarla y cerramos el
		; instalador si no se quiere desinstalar
		MessageBox MB_YESNO "La instalación de Autofirma requiere desinstalar la versión anterior encontrada en el equipo. No se realizará la nueva instalación sin desinstalar la anterior. ¿Desea continuar?" /SD IDYES IDNO Exit
			Goto UninstallOlderVersion
	${EndIf}

	; Si no se encuentra o no va a eliminar la version instalada, finalizamos el proceso
	Goto End
	
	; No se encontro Autofirma instalado por el primer metodo, lo comprobamos de otra forma
	CheckAutofirmaVersion:
		${registry::Open} "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall" "/K=0 /V=1 /S=0 /B=1 /N='DisplayName'" $0
		StrCmp $0 0 0 searchAutofirmaLoop
		Goto End

		searchAutofirmaLoop:
		${registry::Find} "$0" $1 $2 $3 $4

		; Si hemos terminado la busqueda, salimos del bucle
		StrCmp $4 '' close

		; Si hemos encontrado el registro, obtenemos la cadena de desinstalacion, preparamos las variables y dejamos de repetir el bucle
		StrCmp $4 "REG_SZ" 0 searchAutofirmaLoop
		StrCmp $3 "AutoFirma" +2 0
		StrCmp $3 "Autofirma" 0 searchAutofirmaLoop
		ReadRegStr $R0 HKLM $1 "UninstallString"
		
		close:
		${registry::Close} "$0"
		${registry::Unload}

		; Si se encontro Autofirma, se pide desinstalar
		StrCmp $3 "AutoFirma" +2 0
		StrCmp $3 "Autofirma" 0 End
		; Informamos de que existe una version anterior, ofrecemos el eliminarla y cerramos el
		; instalador si no se quiere desinstalar
		MessageBox MB_YESNO "La instalación de Autofirma requiere desinstalar la versión anterior encontrada en el equipo. No se realizará la nueva instalación sin desinstalar la anterior. ¿Desea continuar?" /SD IDYES IDNO Exit
			Goto UninstallOlderVersion
	
	; No se encontro Autofirma instalado, asi que finalizamos el proceso
	Goto End

	Exit:
		Quit

	; Iniciamos el proceso de desinstalacion de la version antigua
	UninstallOlderVersion:

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

	; Iniciamos la desinstalacion
	InitUninstall:
		; Tomamos la ruta de instalacion de la version anterior y la eliminamos del PATH. Si el desinstalador
		; de la version 1.6.5 y anteriores funcionasen bien, esto no seria necesario
		ReadRegStr $R1 HKLM "SOFTWARE\$PATH\" "InstallDir"
		StrCmp $R1 "" +3 0
			Push "$R1\AutoFirma"
			Call RemoveFromPath

		; Preparamos una variable para indicar en ella si tras la desinstalacion deberemos borrar el directorio de
		; instalacion anterior
		StrCpy $R3 ""

		; Preparamos la sentencia de desinstalacion interpretando primeramente que se instalo mediante MSI.
		; Almacenamos en $R1 la ruta desde la que ejecutar la desinstalacion (directorio del sistema)
		; Almacenamos en $R2 la sentencia de desinstalacion agregando parametros para que sea silenciosa
		StrCpy $R1 $SYSDIR
		StrCpy $R2 "$R0 /qn"
		
		Push $R0
		Push "msiexec"
		Call StrStr
		Pop $0

		; Si no es una instalacion MSI, pisamos las variables por las apropiadas para la desinstalacion convencional
		StrCmp $0 "" 0 EjecutarDesinstalador
			Push $R0
			Call GetParent
			Pop $R1	
			StrCpy $R2 '"$R0" /S _?=$R1'
			; Si el directorio de instalacion es distinto del anterior, establecemos una variable para senalar que
			; queremos que se elimine ese directorio despues de la desinstalacion, ya que sabemos que quedaran restos
			; del instalador EXE anterior
			StrCmp $R1 $INSTDIR EjecutarDesinstalador 0
				StrCpy $R3 "Uninstall"

		EjecutarDesinstalador:
			ExecWait $R2

		; Si se indico que se eliminase el desinstalador de la version anterior, lo hacemos
		; Si no, terminamos el proceso
		StrCmp $R3 "Uninstall" 0 EndUninstall
			;Borrar directorio de instalacion si es un directorio valido (es una subcarpeta de Program Files o contiene "Autofirma")
			Push $R1
			Push "Program Files (x86)\"
			Call StrContains
			Pop $0
			StrCmp $0 "Program Files (x86)\" EliminarDirectorio
			Push $R1
			Push "Program Files\"
			Call StrContains
			Pop $0
			StrCmp $0 "Program Files\" EliminarDirectorio
			Push $R1
			Push $PATH
			Call StrContains
			Pop $0
			StrCmp $0 "" EndUninstall
			EliminarDirectorio:
				RMDir /r $R1
				
	EndUninstall:

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

; Funcion para eliminar de registro las entradas agregadas por la aplicacion
; Uso:
;   Call un.UninstallFromRegistry
Function un.UninstallFromRegistry

	DeleteRegKey HKLM "SOFTWARE\$PATH"
    DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH" 

	DeleteRegKey HKEY_CLASSES_ROOT "*\shell\afirma.sign"
	DeleteRegKey HKEY_CLASSES_ROOT "*\shell\afirma.verify"

	DeleteRegKey HKEY_CLASSES_ROOT ".csig\shell\Verify"
	DeleteRegKey HKEY_CLASSES_ROOT ".xsig\shell\Verify"

	DeleteRegKey HKEY_CLASSES_ROOT "afirma"
	
	;Borramos las claves de registro en las que se almacenan las preferencias de la aplicacion
	DeleteRegKey HKCU "Software\JavaSoft\Prefs\es\gob\afirma\ui"
	DeleteRegKey HKCU "Software\JavaSoft\Prefs\es\gob\afirma\standalone"
	DeleteRegKey HKCU "Software\JavaSoft\Prefs\es\gob\afirma\core"
	DeleteRegKey HKCU "Software\JavaSoft\Prefs\es\gob\afirma\plugin"
	DeleteRegKey /ifempty HKCU "Software\JavaSoft\Prefs\es\gob\afirma"
	DeleteRegKey /ifempty HKCU "Software\JavaSoft\Prefs\es\gob"
	DeleteRegKey /ifempty HKCU "Software\JavaSoft\Prefs\es"

	;Se elimina la ruta de la variable de entorno Path
	Push "$INSTDIR\$PATH"
	Call un.RemoveFromPath

FunctionEnd

;--------------------------------------------------------------------
; Path functions

; GetParent
; input, top of stack  (e.g. C:\Program Files\Foo)
; output, top of stack (replaces, with e.g. C:\Program Files)
; modifies no other variables.
;
; Usage:
;   Push "C:\Program Files\Directory\Whatever"
;   Call GetParent
;   Pop $R0
;   ; at this point $R0 will equal "C:\Program Files\Directory"
Function GetParent
 
  Exch $R0
  Push $R1
  Push $R2
  Push $R3
 
  StrCpy $R1 0
  StrLen $R2 $R0
 
  loop:
    IntOp $R1 $R1 + 1
    IntCmp $R1 $R2 get 0 get
    StrCpy $R3 $R0 1 -$R1
    StrCmp $R3 "\" get
  Goto loop
 
  get:
    StrCpy $R0 $R0 -$R1
 
    Pop $R3
    Pop $R2
    Pop $R1
    Exch $R0
 
FunctionEnd

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
    DetailPrint "El PATH es demasiado largo. No se le agregará la ruta de Autofirma."
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
    DetailPrint "La ruta de Autofirma hace que el PATH sea demasiado largo. No se agregará"
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
