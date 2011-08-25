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

import java.io.File;

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
    
    /**
     * JOptionPane.showInputDialog().
     * @param parentComponent Componente padre (se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing
     * @param message Mensaje
     * @param title Titulo del cuadro de di&aacute;logo
     * @param messageType Tipo de mensaje
     * @param icon Icono a mostrar en el di&aacute;logo
     * @param selectionValues Valores posibles para seleccionar
     * @param initialSelectionValue Valor seleccionado por defecto
     * @return Valor seleccionado
     */    
    Object showInputDialog(Object parentComponent, Object message, String title, int messageType, Object icon, Object[] selectionValues, Object initialSelectionValue);
    
    /**
     * JOptionPane.showConfirmDialog().
     * @param parentComponent Componente padre (se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing
     * @param message Mensaje
     * @param title Titulo del cuadro de di&aacute;logo
     * @param optionType Tipo de opciones a confirmar
     * @param messageType Tipo de mensaje
     * @return Opci&oacute;n seleccionada
     */
    int showConfirmDialog(Object parentComponent, Object message, String title, int optionType, int messageType);
    
    /** 
     * Obtiene el c&oacute;digo equivalente a JOptionPane.PLAIN_MESSAGE en la implementaci&oacute;n del entorno operativo actual. 
     * @return C&oacute;digo equivalente a JOptionPane.PLAIN_MESSAGE
     */
    int getPlainMessageCode();
    
    /** 
     * Obtiene el c&oacute;digo equivalente a JOptionPane.YES_NO_OPTION en la implementaci&oacute;n del entorno operativo actual. 
     * @return C&oacute;digo equivalente a JOptionPane.YES_NO_OPTION
     */
    int getYesNoOptionCode();
    
    /** 
     * Obtiene el c&oacute;digo equivalente a JOptionPane.WARNING_MESSAGE en la implementaci&oacute;n del entorno operativo actual. 
     * @return C&oacute;digo equivalente a JOptionPane.WARNING_MESSAGE
     */
    int getWarningMessageCode();
    
    /** 
     * Obtiene el c&oacute;digo equivalente a JOptionPane.YES_OPTION en la implementaci&oacute;n del entorno operativo actual. 
     * @return C&oacute;digo equivalente a JOptionPane.YES_OPTION
     */
    int getYesOptionCode();
    
    /** 
     * Obtiene el c&oacute;digo equivalente a JOptionPane.NO_OPTION en la implementaci&oacute;n del entorno operativo actual. 
     * @return C&oacute;digo equivalente a JOptionPane.NO_OPTION
     */
    int getNoOptionCode();
    
    /** 
     * Obtiene el c&oacute;digo equivalente a JOptionPane.OK_CANCEL_OPTION en la implementaci&oacute;n del entorno operativo actual. 
     * @return C&oacute;digo equivalente a JOptionPane.OK_CANCEL_OPTION
     */
    int getOkCancelOptionCode();
    
    /** 
     * Obtiene el c&oacute;digo equivalente a JOptionPane.OK_OPTION en la implementaci&oacute;n del entorno operativo actual. 
     * @return C&oacute;digo equivalente a JOptionPane.OK_OPTION
     */
    int getOkOptionCode();
    
    /** 
     * Obtiene el c&oacute;digo equivalente a JOptionPane.INFORMATION_MESSAGE en la implementaci&oacute;n del entorno operativo actual. 
     * @return C&oacute;digo equivalente a JOptionPane.INFORMATION_MESSAGE
     */
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
    
    /** Pregunta al usuario por la localizaci&oacute;n de un fichero espec&iacute;fico para su carga.
     * @param dialogTitle
     *        T&iacute;tulo de la ventana de di&aacute;logo.
     * @param fileName
     *        Nombre del fichero a localizar
     * @param description
     *        Descripci&oacute;n del tipo de fichero correspondiente con las
     *        extensiones
     * @param parentComponent
     *        Componente padre (para la modalidad)
     * @return Nombre de fichero (con ruta) seleccionado por el usuario */
    File getLoadFile(String dialogTitle, String fileName, String description, Object parentComponent);
    
}
