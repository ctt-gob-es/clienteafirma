/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.ui;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

/**
 * Factor&iscute;a de elementos de interfaz gr&aacute;fica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class AOUIFactory {

    private AOUIFactory() {
        // No permitimos la instanciacion
    }

    /** JOptionPane.PLAIN_MESSAGE. */
    public static final int PLAIN_MESSAGE;

    /** JOptionPane.YES_NO_OPTION. */
    public static final int YES_NO_OPTION;

    /** JOptionPane.WARNING_MESSAGE. */
    public static final int WARNING_MESSAGE;

    /** JOptionPane.YES_OPTION. */
    public static final int YES_OPTION;

    /** JOptionPane.NO_OPTION. */
    public static final int NO_OPTION;

    /** JOptionPane.OK_CANCEL_OPTION. */
    public static final int OK_CANCEL_OPTION;

    /** JOptionPane.OK_OPTION. */
    public static final int OK_OPTION;

    /** JOptionPane.INFORMATION_MESSAGE. */
    public static final int INFORMATION_MESSAGE;

    /** JOptionPane.QUESTION_MESSAGE. */
    public static final int QUESTION_MESSAGE;

    private static AOUIManager uiManager;

    static {
        try {
            if (Platform.OS.ANDROID.equals(Platform.getOS())) {
                throw new UnsupportedOperationException("No se soporta GUI en Android"); //$NON-NLS-1$
            }
            try {
            	uiManager = (AOUIManager) AOUtil.classForName("es.gob.afirma.ui.core.jse.JSEUIManager").newInstance(); //$NON-NLS-1$
            }
            catch(final Exception e) {
            	Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
        			"No se ha podido instanciar la implementacion de gestor de interfaces graficas: " + e //$NON-NLS-1$
    			);
            }
            PLAIN_MESSAGE = uiManager.getPlainMessageCode();
            YES_NO_OPTION = uiManager.getYesNoOptionCode();
            WARNING_MESSAGE = uiManager.getWarningMessageCode();
            YES_OPTION = uiManager.getYesOptionCode();
            NO_OPTION = uiManager.getNoOptionCode();
            OK_CANCEL_OPTION = uiManager.getOkCancelOptionCode();
            OK_OPTION = uiManager.getOkOptionCode();
            INFORMATION_MESSAGE = uiManager.getInformationMessageCode();
            QUESTION_MESSAGE = uiManager.getQuestionMessageCode();
        }
        catch(final Exception e) {
            throw new UnsupportedOperationException("No se ha podido instanciar el gestor de interfaces graficas: " + e, e); //$NON-NLS-1$
        }
    }

    /**
     * Establece el manejador de interfaces que gestionar&aacute; los di&aacute;logos
     * gr&aacute;ficos que utilizar&aacute; @firma para mostrar o solicitar informaci&oacute;n.
     * @param manager Manejador de interfaces.
     */
    public static void setUIManager(final AOUIManager manager) {
    	if (manager != null) {
    		uiManager = manager;
    	}
    }

    /** Pregunta al usuario por una contrase&ntilde;a.
     * @param text
     *        Texto que se muestra en el di&aacute;logo para pedir la
     *        contrase&ntilde;a
     * @param c
     *        Componente padre (para la modalidad)
     * @return Contrase&ntilde;a introducida por el usuario
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando el usuario cancela el proceso de solicitud de
     *         contrase&ntilde;a */
    public static char[] getPassword(final String text, final Object c) {
        return uiManager.getPassword(text, c);
    }

    /** Pregunta al usuario por una contrase&ntilde;a. S&oacute;lo admite los caracteres
     * incluidos en el par&aacute;metro {@code charset} y, en caso de insertar un
     * car&aacute;cter no permitido, se emitir&aacute;a una advertencia no bloqueante
     * (sonido, vibraci&oacute;n...) si el par&aacute;metro {@code beep} est&aacute;
     * activado.
     * @param text
     *        Texto que se muestra en el di&aacute;logo para pedir la
     *        contrase&ntilde;a
     * @param charset
     *        Cadena con los caracteres permitidos para la contrase&ntilde;a.
     * @param beep
     *        Indica si se debe dar una se&ntilde;al al usuario al intentar insertar
     *        un caracter no v&aacute;lido para la contrase&ntilde;a.
     * @param c
     *        Componente padre (para la modalidad)
     * @return Contrase&ntilde;a introducida por el usuario
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando el usuario cancela el proceso de solicitud de
     *         contrase&ntilde;a */
    public static char[] getPassword(final String text, final String charset, final boolean beep, final Object c) {
        return uiManager.getPassword(text, charset, beep, c);
    }

    /**
     * JOptionPane.showConfirmDialog().
     * @param parentComponent Componente padre (se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing
     * @param message Mensaje
     * @param title Titulo del cuadro de di&aacute;logo
     * @param optionType Tipo de opciones a confirmar
     * @param messageType Tipo de mensaje
     * @return Opci&oacute;n seleccionada
     */
    public static int showConfirmDialog(final Object parentComponent, final Object message, final String title, final int optionType, final int messageType) {
        return uiManager.showConfirmDialog(parentComponent, message, title, optionType, messageType);
    }

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
    public static Object showInputDialog(final Object parentComponent, final Object message, final String title, final int messageType, final Object icon, final Object[] selectionValues, final Object initialSelectionValue) {
        return uiManager.showInputDialog(parentComponent, message, title, messageType, icon, selectionValues, initialSelectionValue);
    }

    /**
     * Di&aacute;logo de selecci&oacute;n de certificados.
     * @param parentComponent Componente padre (se descarta si no es del tipo <code>java.awt.Component</code> en la implementaci&oacute;n Swing
     * @param selectionValues Listado de valores seleccionables Nombre-Certificado.
     * @return Alias del certificado seleccionado o {@code null} si no se seleccion&oacute; ninguno.
     */
    public static Object showCertificateSelectionDialog(final Object parentComponent, final NameCertificateBean[] selectionValues) {
        return uiManager.showCertificateSelectionDialog(parentComponent, selectionValues);
    }

    /** Pregunta al usuario por un nombre de fichero para su carga.
     * @param extensions Extensiones predeterminadas para el fichero
     * @param description Descripci&oacute;n del tipo de fichero correspondiente con las extensiones
     * @param multiSelect <code>true</code> para permitir selecci&oacute;n m&uacute;ltiple, <code>false</code>
     *                    para selecci&oacute;n de un &uacute;nico fichero
     * @param parentComponent Componente padre (para la modalidad)
     * @return Nombre de fichero (con ruta) seleccionado por el usuario */
    public static List<String> getLoadFileName(final String[] extensions, final String description, final boolean multiSelect, final Object parentComponent) {
        return uiManager.getLoadFileName(extensions, description, multiSelect, parentComponent);
    }

    /** Pregunta al usuario por un nombre de fichero para su carga.
     * @param dialogTitle T&iacute;tulo de la ventana de di&aacute;logo.
     * @param extensions Extensiones predeterminadas para el fichero
     * @param description Descripci&oacute;n del tipo de fichero correspondiente con las extensiones
     * @param multiSelect <code>true</code> para permitir selecci&oacute;n m&uacute;ltiple, <code>false</code>
     *                    para selecci&oacute;n de un &uacute;nico fichero
     * @param parentComponent Componente padre (para la modalidad)
     * @return Nombre de fichero (con ruta) seleccionado por el usuario */
    public static List<String> getLoadFileName(final String dialogTitle,
    		                                   final String[] extensions,
    		                                   final String description,
    		                                   final boolean multiSelect,
    		                                   final Object parentComponent) {
        return uiManager.getLoadFileName(dialogTitle, extensions, description, multiSelect, parentComponent);
    }

    /** Pregunta al usuario por la localizaci&oacute;n de un directorio espec&iacute;fico para su carga.
     * @param dialogTitle T&iacute;tulo de la ventana de di&aacute;logo.
     * @param fileName Nombre del directorio a localizar
     * @param parent Componente padre (para la modalidad)
     * @return Ruta absoluta del directorio seleccionado por el usuario
     * @throws es.gob.afirma.core.AOCancelledOperationException Si el usuario cancela la operaci&oacute;n. */
    public static String getLoadDirectory(final String dialogTitle, final String fileName, final Object parent) {
    	return uiManager.getLoadDirectory(dialogTitle, fileName, parent);
    }

    /** Pregunta al usuario por la localizaci&oacute;n de un fichero espec&iacute;fico para su carga.
     * @param dialogTitle T&iacute;tulo de la ventana de di&aacute;logo.
     * @param fileName Nombre del fichero a localizar
     * @param description Descripci&oacute;n del tipo de fichero correspondiente con las extensiones
     * @param parent Componente padre (para la modalidad)
     * @return Fichero seleccionado por el usuario
     * @throws es.gob.afirma.core.AOCancelledOperationException Si el usuario cancela la operaci&oacute;n. */
    public static File getLoadFile(final String dialogTitle, final String fileName, final String description, final Object parent) {
        return uiManager.getLoadFile(dialogTitle, fileName, description, parent);
    }

    /** Pregunta al usuario por la localizaci&oacute;n en la que se desean guardar
     * los datos y los guarda en la misma. Si ocurre un error durante el guardado, se
     * vuelve a preguntar al usuario por una localizaci&oacute;n, Si el usuario
     * cancela el di&aacute;logo, se devolver&aacute; {@code null}.
     * @param data Datos que se desean almacenar.
     * @param dialogTitle T&iacute;tulo del di&aacute;logo de guardado
     * @param selectedFile Localizaci&oacute;n y nombre por defecto del fichero.
     * @param fileFilter Filtro de fichero.
     * @param parent Componente padre (para la modalidad)
     * @return Fichero en el que se almacenan los datos.
     * @throws IOException Si no se puede guardar el fichero*/
    public static File getSaveDataToFile(final byte[] data,
    									 final String dialogTitle,
    		                             final File selectedFile,
    		                             final Object fileFilter,
    		                             final Object parent) throws IOException {
        return uiManager.saveDataToFile(data, dialogTitle, selectedFile, fileFilter, parent);
    }
}
