/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.swing.JOptionPane;
import javax.swing.UIManager;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.ProfileManager;

/** Entrada de la aplicacion */
public class Main {

    /** Almacena el indice a cargar para la ayuda en alto contraste */
    static String helpIndex;

    /** Indica si el SO tiene activado el alto contraste con color negro de fondo */
    public static boolean isOSHighContrast = false;

    static Logger logger = Logger.getLogger(Main.class.getName());

    /** Preferencias generales establecidas para el aplicativo. */
    public static Preferences preferences = Preferences.userRoot().node(Constants.ourNodeName);

    static boolean showHelp = false;

    /** Versi&oacute;n de la interfaz gr&aacute;fica de escritorio. */
    public static final String VERSION = "1.3"; //$NON-NLS-1$

    /** Arranca la interfaz de escritorio del Cliente @firma.
     * @param args Par&aacute;metros de entrada. */
    public static void main(final String[] args) {
        if (System.getProperty("java.version").compareTo("1.6.0_18") < 0) { //$NON-NLS-1$
            CustomDialog.showMessageDialog(null, true, Messages.getString("main.requerido") + //$NON-NLS-1$
                                           System.getProperty("java.version") + ".<br>" + //$NON-NLS-1$
                                           Messages.getString("main.porfavor"), //$NON-NLS-1$
                                           Messages.getString("main.cliente"), //$NON-NLS-1$
                                           JOptionPane.ERROR_MESSAGE);
            System.exit(-5);
        }
        try {
            if (Platform.getOS().equals(Platform.OS.LINUX)) {
                UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
            }
            else {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            }
        }
        catch (final Exception ex) {
            logger.log(Level.SEVERE, null, ex);
        }

        if (ProfileManager.getProfilesNames().length < 1) {
            UserProfile.setCurrentProfileId(null);
            new PrincipalGUI().main();
        }
        else {
            new UserProfile().main();
        }
    }

}
