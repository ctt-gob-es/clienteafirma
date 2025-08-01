;Language: Valencian

!insertmacro LANGFILE "Valencian" = "Valencià" "Valencia"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Benvingut a l'Assistent d'Instal·lació de $(^NameDA)"
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TEXT "Este programa instal·larà $(^NameDA) en el seu ordinador.$\r$\n$\r$\nSe recomana que tancament totes les altres aplicacions abans d'iniciar la instal·lació. Això farà possible actualitzar arxius relacionats amb el sistema sense haver de reiniciar el seu ordinador.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TITLE "Benvingut a l'Assistent de Desinstal·lació de $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TEXT "Este assistent li guiarà durant la desinstal·lació de $(^NameDA).$\r$\n$\r$\nAntes de començar la desinstal·lació, assegure's que $(^NameDA) no s'està executant.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Acord de llicència"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Per favor revise els termes de la llicència abans d'instal·lar $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Si accepta tots els termes de l'acord, seleccione Accepte per a continuar. Ha d'acceptar l'acord per a instal·lar $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepta els termes de l'acord, marque a baix la casella. Ha d'acceptar els termes per a instal·lar $(^NameDA). $_CLICK"
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepta els termes de l'acord, seleccione a baix la primera opció. Ha d'acceptar els termes per a instal·lar $(^NameDA). $_CLICK"
!endif

!ifdef MUI_UNLICENSEPAGE
  ${LangFileString} MUI_UNTEXT_LICENSE_TITLE "Acord de llicència"
  ${LangFileString} MUI_UNTEXT_LICENSE_SUBTITLE "Per favor revise els termes de la llicència abans de desinstal·lar $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM "Si accepta tots els termes de l'acord, seleccione Accepte per a continuar. Ha d'acceptar l'acord per a desinstal·lar $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_CASELLA DE SELECCIÓ "Si accepta els termes de l'acord, marque a baix la casella. Ha d'acceptar els termes per a desinstal·lar $(^NameDA). $_CLICK"
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepta els termes de l'acord, seleccione a baix la primera opció. Ha d'acceptar els termes per a desinstal·lar $(^NameDA). $_CLICK"
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Pressione Avançar Pàgina per a veure la resta de l'acord."
!endif

!ifdef MUI_COMPONENTSPAGE
  ${LangFileString} MUI_TEXT_COMPONENTS_TITLE "Selecció de components"
  ${LangFileString} MUI_TEXT_COMPONENTS_SUBTITLE "Seleccione quines característiques de $(^NameDA) desitja instal·lar."
!endif

!ifdef MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_UNTEXT_COMPONENTS_TITLE "Selecció de components"
  ${LangFileString} MUI_UNTEXT_COMPONENTS_SUBTITLE "Seleccione quines característiques de $(^NameDA) desitja desinstal·lar."
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descripció"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Situe el ratolí damunt d'un component per a veure la seua descripció."
  !else
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Seleccione un component per a veure la seua descripció."
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  ${LangFileString} MUI_TEXT_DIRECTORY_TITLE "Triar lloc d'instal·lació"
  ${LangFileString} MUI_TEXT_DIRECTORY_SUBTITLE "Trie el directori per a instal·lar $(^NameDA)."
!endif

!ifdef MUI_UNDIRECTORYPAGE
  ${LangFileString} MUI_UNTEXT_DIRECTORY_TITLE "Triar lloc de desinstal·lació"
  ${LangFileString} MUI_UNTEXT_DIRECTORY_SUBTITLE "Trie el directori des del qual es desinstal·larà $(^NameDA)."
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Instal·lant"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Per favor espere mentre $(^NameDA) s'instal·la."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Instal·lació Completada"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "La instal·lació s'ha completat correctament."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Instal·lació Anul·lada"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "La instal·lació no es va completar correctament."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Desinstal·lant"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Per favor espere mentre $(^NameDA) es desinstal·la."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Desinstal·lació Completada"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "La desinstal·lació s'ha completat correctament."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Desinstal·lació Anul·lada"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "La desinstal·lació no es va completar correctament."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Completant l'Assistent d'Instal·lació de $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) ha sigut instal·lat en el seu sistema.$\r$\n$\r$\nPresione Acabar per a tancar este assistent."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "El seu sistema ha de ser reiniciat perquè puga completar-se la instal·lació de $(^NameDA). Desitja reiniciar ara?"
!endif

!ifdef MUI_UNFINISHPAGE
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TITLE "Completant l'Assistent de Desinstal·lació de $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) ha sigut desinstal·lat del seu sistema.$\r$\n$\r$\nPresione Acabar per a tancar este assistent."
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_REBOOT "El seu ordinador ha de ser reiniciat per a completar la desinstal·lació de $(^NameDA). Desitja reiniciar ara?"
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Reiniciar ara"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Desitge reiniciar manualment més tard"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Executant $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&Veure Llija'm"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Acabar"  
!endif

!ifdef MUI_STARTMENUPAGE
  ${LangFileString} MUI_TEXT_STARTMENU_TITLE "Triar Carpeta del Menú Inici"
  ${LangFileString} MUI_TEXT_STARTMENU_SUBTITLE "Trie una Carpeta del Menú Inici per als accessos directes de $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_TOP "Seleccione una carpeta del Menú Inici en la qual vulga crear els accessos directes del programa. També pot introduir un nom per a crear una nova carpeta."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_CASELLA DE SELECCIÓ "No crear accessos directes"
!endif

!ifdef MUI_UNCONFIRMPAGE
  ${LangFileString} MUI_UNTEXT_CONFIRM_TITLE "Desinstal·lar $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_CONFIRM_SUBTITLE "Elimina $(^NameDA) del seu sistema."
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "Està segur que desitja eixir de la instal·lació de $(^Name)?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Està segur que desitja eixir de la desinstal·lació de $(^Name)?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  ${LangFileString} MULTIUSER_TEXT_INSTALLMODE_TITLE "Triar Usuaris"
  ${LangFileString} MULTIUSER_TEXT_INSTALLMODE_SUBTITLE "Trie els usuaris per als quals Vosté desitja instal·lar $(^NameDA)."
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_TOP "Seleccione si desitja instal·lar $(^NameDA) només per a Vosté o per a tots els usuaris d'este Ordinador.$(^ClickNext)"
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS "Instación para qualsevol usuari d'este ordinador"
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER "Instal·lació només per a mi"
!endif


