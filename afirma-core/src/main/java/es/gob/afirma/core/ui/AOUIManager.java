/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.ui;

import java.io.File;
import java.io.IOException;
import java.util.List;

/** Gestor de componentes de interfaz gr&aacute;fico
 * @version 0.4 */
public interface AOUIManager {

    /** Pregunta al usuario por una contrase&ntilde;a.
     * @param text Texto que se muestra en el di&aacute;logo para pedir la contrase&ntilde;a
     * @param c Componente padre (para la modalidad)
     * @return Contrase&ntilde;a introducida por el usuario
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando el usuario cancela el
     *         proceso de solicitud de contrase&ntilde;a */
    char[] getPassword(String text, Object c);

    /** Muestra un di&aacute;logo para pedir una contrase&ntilde;a al usuario.
     * @param text Texto con el que se solicitar&aacute; la entrada de texto al
     *        usuario (<i>prompt</i>)
     * @param icon Objeto de tipo {@code javax.swing.Icon} con el icono del di&aacute;logo
     * 				o {@code null} para no mostrar icono.
     * @param charSet Juego de caracteres aceptados para la contrase&ntilde;a
     * @param beep <code>true</code> si se desea un sonido de advertencia al
     *             introducir un caracter no v&aacute;lido, <code>false</code> en caso contrario
     * @param c Componente padre (para la modalidad)
     * @return Array de caracteres del texto introducido como contrase&ntilde;a
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando el usuario cancela o cierra el di&aacute;logo */
    char[] getPassword(String text, Object icon, String charSet, boolean beep, Object c);

    /** Muestra un di&aacute;logo para pedir dos veces una contrase&ntilde;a al usuario (ambas deben coincidir).
     * Es el procedimiento normal cuando se pide el establecimiento de una nueva contrase&ntilde;a, para evitar errores.
     * @param text Texto con el que se solicitar&aacute; la entrada de texto al
     *             usuario (<i>prompt</i>).
     * @param text2 Texto con el que se solicitar&aacute; al usuario que repita la contrase&ntilde;a.
     * @param imageIcon Objeto de tipo {@code javax.swing.Icon} con el icono del di&aacute;logo o
     * 			   {@code null} para no mostrar icono.
     * @param charSet Juego de caracteres aceptados para la contrase&ntilde;a.
     * @param beep <code>true</code> si se desea un sonido de advertencia al
     *             introducir un caracter no v&aacute;lido, <code>false</code> en
     *             caso contrario.
     * @param c Componente padre (para la modalidad).
     * @return Array de caracteres del texto introducido como contrase&ntilde;a.
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando el usuario cancela o cierra el di&aacute;logo. */
	char[] getDoublePassword(String text, String text2, Object imageIcon, String charSet, boolean beep, Object c);

    /** JOptionPane.showInputDialog().
     * @param parentComponent Componente padre (se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing
     * @param message Mensaje
     * @param title Titulo del cuadro de di&aacute;logo
     * @param messageType Tipo de mensaje
     * @param icon Icono a mostrar en el di&aacute;logo
     * @param selectionValues Valores posibles para seleccionar
     * @param initialSelectionValue Valor seleccionado por defecto
     * @return Valor seleccionado */
    Object showInputDialog(Object parentComponent, Object message, String title, int messageType, Object icon, Object[] selectionValues, Object initialSelectionValue);

    /** Di&aacute;logo de selecci&oacute;n de certificados.
     * @param parentComponent Componente padre. Se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing.
     * @param dialogManager Manejador del di&aacute;logo gr&aacute;fico.
     * @return Alias del certificado seleccionado. */
    String showCertificateSelectionDialog(Object parentComponent, KeyStoreDialogManager dialogManager);

    /** JOptionPane.showConfirmDialog().
     * @param parentComponent Componente padre. Se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing.
     * @param message Mensaje
     * @param title Titulo del cuadro de di&aacute;logo
     * @param optionType Tipo de opciones a confirmar
     * @param messageType Tipo de mensaje
     * @return Opci&oacute;n seleccionada */
    int showConfirmDialog(Object parentComponent, Object message, String title, int optionType, int messageType);

    /** JOptionPane.showMessageDialog().
     * @param parentComponent Componente padre. Se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing.
     * @param message Mensaje
     * @param title Titulo del cuadro de di&aacute;logo
     * @param messageType Tipo de mensaje */
    void showMessageDialog(Object parentComponent, Object message, String title, int messageType);

    /** JOptionPane.showMessageDialog().
     * @param parentComponent Componente padre. Se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing.
     * @param message Mensaje
     * @param title Titulo del cuadro de di&aacute;logo
     * @param messageType Tipo de mensaje
     * @param icon Icono para el di&aacute;logo */
    void showMessageDialog(Object parentComponent, Object message, String title, int messageType, Object icon);

    /** Muestra un di&aacute;logo de error de forma modal.
     * @param message Mensaje de error.
     * @param title Titulo de la ventana de error.
     * @param messageType Tipo de mensaje.
     * @param t Informaci&oacute;n sobre el error */
    void showErrorMessage(final Object message, final String title, final int messageType, Throwable t);

    /** Muestra un di&aacute;logo de error de forma modal.
     * @param parentComponent Componente padre para la modalidad.
     * @param message Mensaje de error.
     * @param title Titulo de la ventana de error.
     * @param messageType Tipo de mensaje.
     * @param t Informaci&oacute;n sobre el error */
	void showErrorMessage(Object parentComponent, Object message, String title, int messageType, Throwable t);

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.PLAIN_MESSAGE en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.PLAIN_MESSAGE */
    int getPlainMessageCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.YES_NO_OPTION en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.YES_NO_OPTION */
    int getYesNoOptionCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.WARNING_MESSAGE en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.WARNING_MESSAGE */
    int getWarningMessageCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.ERROR_MESSAGE en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.ERROR_MESSAGE */
    int getErrorMessageCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.YES_OPTION en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.YES_OPTION */
    int getYesOptionCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.NO_OPTION en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.NO_OPTION */
    int getNoOptionCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.OK_CANCEL_OPTION en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.OK_CANCEL_OPTION */
    int getOkCancelOptionCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.OK_OPTION en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.OK_OPTION */
    int getOkOptionCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.INFORMATION_MESSAGE en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.INFORMATION_MESSAGE */
    int getInformationMessageCode();

    /** Obtiene el c&oacute;digo equivalente a JOptionPane.QUESTION_MESSAGE en la implementaci&oacute;n del entorno operativo actual.
     * @return C&oacute;digo equivalente a JOptionPane.QUESTION_MESSAGE */
    int getQuestionMessageCode();

    /** Pide al usuario que seleccione un fichero.
     * @param dialogTitle
     *        T&iacute;tulo de la ventana de di&aacute;logo.
     * @param currentDir Directorio inicial del di&aacute;logo
     * @param filename Nombre del fichero a localizar
     * @param extensions Extensiones predeterminadas para el fichero
     * @param description Descripci&oacute;n del tipo de fichero correspondiente con las extensiones
     * @param selectDirectory {@code true} para permitir la selecci&oacute;n de directorios, {@code true}
     * 					  para selecci&oacute;n de ficheros. En caso de directorios el par&aacute;metro
     * 					  {@code multiselect} se ignorar&aacute;.
     * @param multiSelect {@code true} para permitir selecci&oacute;n m&uacute;ltiple, {@code false}
     *                    para selecci&oacute;n de un &uacute;nico fichero
     * @param icon Icono del di&aacute;logo de selecci&oacute;n.
     *             Si se especifica <code>null</code> y se indica un <code>Frame</code> como padre se
     *             hereda el icono de este.
     * @param parentComponent
     *        Componente padre (para la modalidad)
     * @return Nombre de fichero (con ruta) seleccionado por el usuario */
    File[] getLoadFiles(String dialogTitle,
    		            String currentDir,
    		            String filename,
    		            String[] extensions,
    		            String description,
    		            boolean selectDirectory,
    		            boolean multiSelect,
    		            Object icon,
    		            Object parentComponent);

    /** Muestra un di&aacute;logo de guardado para almacenar los datos indicados.
     * Los datos ser&aacute;n almacenados en el directorio y con el nombre que
     * indique el usuario. Si el fichero ya existe se le preguntar&aacute; al
     * usuario si desea sobreescribirlo. En caso de cancelar la operaci&oacute;n
     * se devolvera <code>null</code>, si la operaci&oacute;n finaliza correctamente se
     * devolver&aacute; la ruta completa del fichero.
     * @param data Datos que se desean almacenar.
     * @param dialogTitle T&iacute;tulo del di&aacute;logo de guardado.
     * @param currentDir Directorio inicial del di&aacute;logo.
     * @param selectedFile Nombre de fichero por defecto.
     * @param filters Filtros del tipo de fichero a guardar.
     * @param parent Componente padre sobre el que se mostrar&aacute; el
     *        di&aacute;logo de guardado.
     * @return Fichero guardado.
     * @throws IOException Si no se puede guardar el fichero */
    File saveDataToFile(final byte[] data,
    					final String dialogTitle,
    					final String currentDir,
    		            final String selectedFile,
    		            final List<GenericFileFilter> filters,
    		            final Object parent) throws IOException;

    /** Muestra un di&aacute;logo de guardado para almacenar los datos indicados.
     * Los datos ser&aacute;n almacenados en el directorio y con el nombre que
     * indique el usuario. Si el fichero ya existe se le preguntar&aacute; al
     * usuario si desea sobreescribirlo. En caso de cancelar la operaci&oacute;n
     * se devolvera <code>null</code>, si la operaci&oacute;n finaliza correctamente se
     * devolver&aacute; la ruta completa del fichero.
     * @param data Datos que se desean almacenar.
     * @param dialogTitle T&iacute;tulo del di&aacute;logo de guardado.
     * @param currentDir Directorio inicial del di&aacute;logo.
     * @param selectedFile Nombre de fichero por defecto.
     * @param filters Filtros del tipo de fichero a guardar.
     * @param defaultFilter Filtro con un valor por defecto, si se indica como nulo, se ignora.
     * @param parent Componente padre sobre el que se mostrar&aacute; el
     *        di&aacute;logo de guardado.
     * @return Fichero guardado.
     * @throws IOException Si no se puede guardar el fichero */
    File saveDataToFile(final byte[] data,
    					final String dialogTitle,
    					final String currentDir,
    		            final String selectedFile,
    		            final List<GenericFileFilter> filters,
    		            final GenericFileFilter defaultFilter,
    		            final Object parent) throws IOException;

}
