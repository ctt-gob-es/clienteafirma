/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.core.ui;

import es.gob.afirma.core.AOCancelledOperationException;

/** Gestor de componentes de interfaz gr&aacute;fico
 * @version 0.4 */
public interface AOUIManager {

    /** Pregunta al usuario por una contrase&ntilde;a.
     * @param text
     *        Texto que se muestra en el di&aacute;logo para pedir la
     *        contrase&ntilde;a
     * @param c
     *        Componente padre (para la modalidad)
     * @return Contrase&ntilde;a introducida por el usuario
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancela el proceso de solicitud de
     *         contrase&ntilde;a */
    char[] getPassword(String text, Object c) throws AOCancelledOperationException;
    
    /** Muestra un di&aacute;logo para pedir una contrase&ntilde;a al usuario.
     * @param text
     *        Texto con el que se solicitar&aacute; la entrada de texto al
     *        usuario (<i>prompt</i>)
     * @param charSet
     *        Juego de caracteres aceptados para la contrase&ntilde;a
     * @param beep
     *        <code>true</code> si se desea un sonido de advertencia al
     *        introducir un caracter no v&aacute;lido, <code>false</code> en
     *        caso contrario
     * @param c
     *        Componente padre (para la modalidad)
     * @return Array de caracteres del texto introducido como contrase&ntilde;a
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancela o cierra el di&aacute;logo */
    char[] getPassword(String text, final String charSet, final boolean beep, final Object c) throws AOCancelledOperationException;
    
    
    Object showInputDialog(Object parentComponent, Object message, String title, int messageType, Object icon, Object[] selectionValues, Object initialSelectionValue);
    
    int showConfirmDialog(Object parentComponent, Object message, String title, int optionType, int messageType);
    
    int getPlainMessageCode();
    
    int getYesNoOptionCode();
    
    int getWarningMessageCode();
    
    int getYesOptionCode();
    
    int getNoOptionCode();
    
    int getOkCancelOptionCode();
    
    int getOkOptionCode();
    
    int getInformationMessageCode();
    
    /** Pregunta al usuario por un nombre de fichero para su carga.
     * @param extensions
     *        Extensiones predeterminadas para el fichero
     * @param description
     *        Descripci&oacute;n del tipo de fichero correspondiente con las
     *        extensiones
     * @param parentComponent
     *        Componente padre (para la modalidad)
     * @return Nombre de fichero (con ruta) seleccionado por el usuario */
    String getLoadFileName(String[] extensions, String description, Object parentComponent);

    /** Pregunta al usuario por un nombre de fichero para su carga.
     * @param dialogTitle
     *        T&iacute;tulo de la ventana de di&aacute;logo.
     * @param extensions
     *        Extensiones predeterminadas para el fichero
     * @param description
     *        Descripci&oacute;n del tipo de fichero correspondiente con las
     *        extensiones
     * @param parentComponent
     *        Componente padre (para la modalidad)
     * @return Nombre de fichero (con ruta) seleccionado por el usuario */
    String getLoadFileName(String dialogTitle, String[] extensions, String description, Object parentComponent);


}
