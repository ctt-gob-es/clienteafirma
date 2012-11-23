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
public final class Main {

	private Main() {
		// Constructor privado
	}

    /** Almacena el indice a cargar para la ayuda en alto contraste */
	private static String helpIndex;

    /** Indica si el SO tiene activado el alto contraste con color negro de fondo */
	private static boolean isOSHighContrast = false;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Preferencias generales establecidas para el aplicativo. */
	private static final Preferences PREFERENCES = Preferences.userRoot().node(Constants.OUR_NODE_NAME);

    /** Versi&oacute;n de la interfaz gr&aacute;fica de escritorio. */
    public static final String VERSION = "2.0"; //$NON-NLS-1$

    /** Arranca la interfaz de escritorio del Cliente @firma.
     * @param args Par&aacute;metros de entrada. */
    public static void main(final String[] args) {
        if (System.getProperty("java.version").compareTo("1.6.0_18") < 0) { //$NON-NLS-1$ //$NON-NLS-2$
            CustomDialog.showMessageDialog(null, true, Messages.getString("main.requerido") + //$NON-NLS-1$
                                           System.getProperty("java.version") + ".<br>" + //$NON-NLS-1$ //$NON-NLS-2$
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
            LOGGER.log(Level.SEVERE, null, ex);
        }

        if (ProfileManager.getProfilesNames().length < 1) {
            UserProfile.setCurrentProfileId(null);
            new PrincipalGUI().main();
        }
        else {
            new UserProfile().main();
        }
    }

	static String getHelpIndex() {
		return helpIndex;
	}

	static void setHelpIndex(final String helpIndex) {
		Main.helpIndex = helpIndex;
	}

	/** Indica si el sistema operativo est&aacute; configurado en modo gr&aacute;fico de alto contraste.
	 * @return <code>true</code> si el sistema operativo est&aacute; configurado en modo gr&aacute;fico de alto contraste,
	 *         <code>false</code> en caso contrario */
	public static boolean isOSHighContrast() {
		return isOSHighContrast;
	}

	static void setOSHighContrast(final boolean isOSHighContrast) {
		Main.isOSHighContrast = isOSHighContrast;
	}

	/** Obtiene el objeto de preferencias de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @return Objeto de preferencias de configuraci&oacute;n de la aplicaci&oacute;n */
	public static Preferences getPreferences() {
		return PREFERENCES;
	}

}
