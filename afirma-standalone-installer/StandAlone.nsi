;Include Modern UI
  !include "MUI.nsh"

;Seleccionamos el algoritmo de compresión utilizado para comprimir nuestra aplicación
SetCompressor lzma


;--------------------------------
;Con esta opcion alertamos al usuario y le pedimos confirmación para abortar
;la instalación
;Esta macro debe colocarse en esta posición del script sino no funcionara
	

  !define MUI_ABORTWARNING
  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "ic_head.bmp"
  !define MUI_HEADERIMAGE_UNBITMAP "ic_head.bmp"
  !define MUI_WELCOMEFINISHPAGE_BITMAP "ic_install.bmp"
  !define MUI_UNWELCOMEFINISHPAGE_BITMAP "ic_install.bmp"
   
;Definimos el valor de la variable VERSION, en caso de no definirse en el script
;podria ser definida en el compilador
!define VERSION "2.0"


;--------------------------------
;Pages
  
  ;Mostramos la página de bienvenida
  !insertmacro MUI_PAGE_WELCOME
  ;Página donde mostramos el contrato de licencia 
  !insertmacro MUI_PAGE_LICENSE "licencia.txt"
  ;página donde se muestran las distintas secciones definidas
  !insertmacro MUI_PAGE_COMPONENTS
  ;página donde se selecciona el directorio donde instalar nuestra aplicacion
  !insertmacro MUI_PAGE_DIRECTORY
  ;página de instalación de ficheros
  !insertmacro MUI_PAGE_INSTFILES
  ;página final
  !insertmacro MUI_PAGE_FINISH
  
;páginas referentes al desinstalador
  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH
  
 
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "Spanish"

; Para generar instaladores en diferentes idiomas podemos escribir lo siguiente:
;  !insertmacro MUI_LANGUAGE ${LANGUAGE}
; De esta forma pasando la variable LANGUAGE al compilador podremos generar
;paquetes en distintos idiomas sin cambiar el script

;;;;;;;;;;;;;;;;;;;;;;;;;
; Configuration General ;
;;;;;;;;;;;;;;;;;;;;;;;;;
;Nuestro instalador se llamara si la version fuera la 1.0: Ejemplo-1.0.exe
OutFile ClienteStandAlone_${VERSION}.exe

;Aqui comprobamos que en la versión Inglesa se muestra correctamente el mensaje:
;Welcome to the $Name Setup Wizard
;Al tener reservado un espacio fijo para este mensaje, y al ser
;la frase en español mas larga:
; Bienvenido al Asistente de Instalación de Aplicación $Name
; no se ve el contenido de la variable $Name si el tamaño es muy grande
Name "Cliente @firma"
Caption "Instalador de Firma @firma StandAlone"
Icon ic_launcher.ico

;Comprobacion de integridad del fichero activada
CRCCheck on
;Estilos visuales del XP activados
XPStyle on

/*
	Declaracion de variables a usar
	
*/
# también comprobamos los distintos
; tipos de comentarios que nos permite este lenguaje de script

Var PATH
Var PATH_ACCESO_DIRECTO
;Indicamos cual sera el directorio por defecto donde instalaremos nuestra
;aplicación, el usuario puede cambiar este valor en tiempo de ejecución.
InstallDir "$PROGRAMFILES\StandAlone"

; check if the program has already been installed, if so, take this dir
; as install dir
InstallDirRegKey HKLM SOFTWARE\StandAlone "Install_Dir"
;Mensaje que mostraremos para indicarle al usuario que seleccione un directorio
DirText "Elija un directorio donde instalar la aplicación:"

;Indicamos que cuando la instalación se complete no se cierre el instalador automáticamente
AutoCloseWindow false
;Mostramos todos los detalles del la instalación al usuario.
ShowInstDetails show
;En caso de encontrarse los ficheros se sobreescriben
SetOverwrite on
;Optimizamos nuestro paquete en tiempo de compilación, es áltamente recomendable habilitar siempre esta opción
SetDatablockOptimize on
;Habilitamos la compresión de nuestro instalador
SetCompress auto
;Personalizamos el mensaje de desinstalación
UninstallText "Desinstalador de Firma con @firma StandAlone."


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Install settings                                                    ;
; En esta sección añadimos los ficheros que forman nuestra aplicación ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Section "Programa"
	StrCpy $PATH "StandAlone"
	StrCpy $PATH_ACCESO_DIRECTO "StandAlone"

	SetOutPath $INSTDIR\$PATH

;Incluimos todos los ficheros que componen nuestra aplicación
	File  StandAlone.exe
	File  licencia.txt

;Hacemos que la instalación se realice para todos los usuarios del sistema
        SetShellVarContext all

;Creamos también el aceso directo al instalador

	;creamos un acceso directo en el escitorio
	CreateShortCut "$DESKTOP\Cliente @firma.lnk" "$INSTDIR\StandAlone\StandAlone.exe"

	;Menu items
	CreateDirectory "$SMPROGRAMS\StandAlone"
	CreateShortCut "$SMPROGRAMS\StandAlone\Cliente @firma.lnk" "$INSTDIR\StandAlone\StandAlone.exe"
	CreateShortCut "$SMPROGRAMS\StandAlone\unistall.lnk" "$INSTDIR\unistall.exe"

	
	;Añade una entrada en la lista de "Program and Features"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH \" "DisplayName" "StandAlone ${VERSION}"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH \" "UninstallString" "$INSTDIR\unistall.exe"
	
	WriteUninstaller "unistall.exe"

	WriteRegStr HKLM SOFTWARE\$PATH "InstallDir" $INSTDIR
	WriteRegStr HKLM SOFTWARE\$PATH "Version" "${VERSION}"

	;Exec "explorer $SMPROGRAMS\$PATH_ACCESO_DIRECTO\"
	
	;Registry
	;CascadeAfirma.reg
	;WriteRegStr HKEY_CLASSES_ROOT "*\shell\standalone.sign" "" "Firmar con 'Cliente @firma'"
	;WriteRegStr HKEY_CLASSES_ROOT "*\shell\standalone.sign" "Icon" "$INSTDIR\StandAlone\StandAlone.exe"
	;WriteRegStr HKEY_CLASSES_ROOT "*\shell\standalone.sign\command" "" "$INSTDIR\StandAlone\StandAlone.exe %1"
		
	

SectionEnd


;;;;;;;;;;;;;;;;;;;;;;
; Uninstall settings ;
;;;;;;;;;;;;;;;;;;;;;;

Section "Uninstall"
	StrCpy $PATH "StandAlone"
	StrCpy $PATH_ACCESO_DIRECTO "StandAlone"
    
    SetShellVarContext all

	RMDir /r $INSTDIR\$PATH
	;remove instalation directory
	RMDir /r $INSTDIR 
	
	;delete start menu shortcuts
	Delete "$DESKTOP\Cliente @firma.lnk"
	RMDir /r $SMPROGRAMS\$PATH_ACCESO_DIRECTO


	DeleteRegKey HKLM "SOFTWARE\$PATH"
    DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$PATH \"
	
	;DeleteRegKey HKEY_CLASSES_ROOT "*\shell\standalone.sign"
	
SectionEnd
