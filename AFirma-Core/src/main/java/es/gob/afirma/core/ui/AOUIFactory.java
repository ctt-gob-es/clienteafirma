package es.gob.afirma.core.ui;

import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;

public class AOUIFactory {
    
    // Por defecto los valores sacadas del Fuente de JOptionPane
    public static int PLAIN_MESSAGE = -1;
    public static int YES_NO_OPTION = 0;
    public static int WARNING_MESSAGE = 2;
    public static int YES_OPTION = 0;
    public static int NO_OPTION = 1; 
    public static int OK_CANCEL_OPTION = 2;
    public static int OK_OPTION = 0;
    public static int INFORMATION_MESSAGE = 1;
    
    private static AOUIManager uiManager;
    
    private final static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    static {
        try {
            uiManager = (AOUIManager) Class.forName("").newInstance();
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
            LOGGER.severe("No se ha podido instanciar el gestor de interfaces graficas: " + e); //$NON-NLS-1$
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
    
    public static int showConfirmDialog(final Object parentComponent, final Object message, final String title, final int optionType, final int messageType) {
        return uiManager.showConfirmDialog(parentComponent, message, title, optionType, messageType);
    }
    
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

}
