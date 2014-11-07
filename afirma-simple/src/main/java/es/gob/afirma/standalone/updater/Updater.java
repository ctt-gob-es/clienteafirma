package es.gob.afirma.standalone.updater;

import java.awt.Desktop;
import java.net.URI;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.UrlHttpManagerFactory;
import es.gob.afirma.core.ui.AOUIFactory;

/** Utilidad para la gesti&oacute;n de actualizaciones de la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class Updater {

	private static String version = null;
	private static String currentVersion = null;
	private static String updateSite = "https://github.com/ctt-gob-es/clienteafirma/"; //$NON-NLS-1$
	static String getUpdateSite() {
		return updateSite;
	}

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Obtiene la &uacute;ltima versi&oacute;n disponible del programa.
	 * @return &Uacute;ltima versi&oacute;n disponible del programa o <code>null</code> si no
	 *         se ha podido obtener. */
	private static String getLatestVersion() {
		final Properties p = new Properties();
		try {
			p.load(Updater.class.getResourceAsStream("/properties/updater.properties")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe(
				"No se ha podido cargar el archivo de recursos del actualizador: " + e //$NON-NLS-1$
			);
			return null;
		}
		final String url = p.getProperty("url"); //$NON-NLS-1$

		// Leemos aqui el resto de propiedades
		currentVersion = p.getProperty("currentVersion"); //$NON-NLS-1$
		updateSite = p.getProperty("updateSite"); //$NON-NLS-1$

		if (url == null) {
			LOGGER.severe(
				"El archivo de recursos del actualizador no contiene una URL de comprobacion" //$NON-NLS-1$
			);
		}
		try {
			version = new String(UrlHttpManagerFactory.getInstalledManager().readUrlByGet(url));
		}
		catch (final Exception e) {
			LOGGER.severe(
				"No se ha podido obtener la ultima version disponible desde " + url //$NON-NLS-1$
			);
		}
		return version;
	}

	/** Comprueba si hay disponible una versi&oacute;n m&aacute;s actualizada del aplicativo.
	 * @return <code>true</code> si hay disponible una versi&oacute;n m&aacute;s actualizada del aplicativo,
	 *         <code>false</code> en caso contrario. */
	static boolean isNewVersionAvailable() {
		final String newVersion = getLatestVersion();
		if (newVersion == null) {
			LOGGER.severe("No se puede comprobar si hay versiones nuevas del aplicativo"); //$NON-NLS-1$
			return false;
		}
		if (currentVersion == null) {
			LOGGER.severe("No ha podido comprobar la version actual del aplicativo"); //$NON-NLS-1$
			return false;
		}
		try {
			return Integer.parseInt(newVersion) > Integer.parseInt(currentVersion);
		}
		catch(final Exception e) {
			LOGGER.severe(
				"No ha podido comparar la version actual del aplicativo con la ultima disponible: " + e //$NON-NLS-1$
			);
			return false;
		}
	}

	/** Comprueba si hay actualizaciones del aplicativo, y en caso afirmativo lo notifica al usuario.
	 * @param parent Componente padre para la modalidad. */
	public static void checkForUpdates(final Object parent) {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			SwingUtilities.invokeLater(
				new Runnable() {
					@Override
					public void run() {
						if (isNewVersionAvailable()) {
							if (JOptionPane.YES_OPTION == AOUIFactory.showConfirmDialog(
								parent,
								UpdaterMessages.getString("Updater.1"), //$NON-NLS-1$
								UpdaterMessages.getString("Updater.2"), //$NON-NLS-1$
								JOptionPane.YES_NO_OPTION,
								JOptionPane.INFORMATION_MESSAGE
							)) {
								try {
									Desktop.getDesktop().browse(new URI(getUpdateSite()));
								}
								catch (final Exception e) {
									LOGGER.severe("No se ha podido abrir el sitio Web de actualizaciones del Cliente @firma: " + e); //$NON-NLS-1$
									AOUIFactory.showErrorMessage(
										parent,
										UpdaterMessages.getString("Updater.3"), //$NON-NLS-1$
										UpdaterMessages.getString("Updater.4"), //$NON-NLS-1$
										JOptionPane.ERROR_MESSAGE
									);
									e.printStackTrace();
								}
							}
						}
					}
				}
			);
		}
	}

}
