package es.gob.afirma.core.ui;

import java.io.File;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;

/**
 * Factor&iscute;a de elementos de interfaz gr&aacute;fica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public class AOUIFactory {
    
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
    
    /** JOptionPane.PK_OPTION. */
    public static final int OK_OPTION;
    
    /** JOptionPane.INFORMATION_MESSAGE. */
    public static final int INFORMATION_MESSAGE;
    
    private static final AOUIManager uiManager;
    
    static {
        try {
            if (Platform.OS.ANDROID.equals(Platform.getOS())) {
                throw new UnsupportedOperationException("No se soporta GUI en Android"); //$NON-NLS-1$
            }
            uiManager = (AOUIManager) Class.forName("es.gob.afirma.core.ui.jse.JSEUIManager").newInstance(); //$NON-NLS-1$
            PLAIN_MESSAGE = uiManager.getPlainMessageCode();
            YES_NO_OPTION = uiManager.getYesNoOptionCode();
            WARNING_MESSAGE = uiManager.getWarningMessageCode();
            YES_OPTION = uiManager.getYesOptionCode();
            NO_OPTION = uiManager.getNoOptionCode();
            OK_CANCEL_OPTION = uiManager.getOkCancelOptionCode();
            OK_OPTION = uiManager.getOkOptionCode();
            INFORMATION_MESSAGE = uiManager.getInformationMessageCode();
        }
        catch(final Exception e) {
            throw new UnsupportedOperationException("No se ha podido instanciar el gestor de interfaces graficas: " + e, e); //$NON-NLS-1$
        }
    }
    
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
    public static char[] getPassword(final String text, final Object c) throws AOCancelledOperationException {
        return uiManager.getPassword(text, c);
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
    
    /** Pregunta al usuario por un nombre de fichero para su carga.
     * @param extensions
     *        Extensiones predeterminadas para el fichero
     * @param description
     *        Descripci&oacute;n del tipo de fichero correspondiente con las
     *        extensiones
     * @param parentComponent
     *        Componente padre (para la modalidad)
     * @return Nombre de fichero (con ruta) seleccionado por el usuario */
    public static String getLoadFileName(String[] extensions, String description, Object parentComponent) {
        return uiManager.getLoadFileName(extensions, description, parentComponent);
    }

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
    public static String getLoadFileName(final String dialogTitle,final String[] extensions, final String description, final Object parentComponent) {
        return uiManager.getLoadFileName(dialogTitle, extensions, description, parentComponent);
    }
    
    /** Pregunta al usuario por la localizaci&oacute;n de un fichero espec&iacute;fico para su carga.
     * @param dialogTitle
     *        T&iacute;tulo de la ventana de di&aacute;logo.
     * @param fileName
     *        Nombre del fichero a localizar
     * @param description
     *        Descripci&oacute;n del tipo de fichero correspondiente con las
     *        extensiones
     * @param parent
     *        Componente padre (para la modalidad)
     * @return Fichero seleccionado por el usuario */
    public static File getLoadFile(final String dialogTitle, final String fileName, final String description, final Object parent) {
        return uiManager.getLoadFile(dialogTitle, fileName, description, parent);
    }

}
