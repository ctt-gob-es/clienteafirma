package es.gob.afirma.standalone.updater;

import java.awt.Desktop;
import java.net.URI;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.ui.AOUIFactory;

/** Utilidad para la gesti&oacute;n de actualizaciones de la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class Updater {

	private static String version = null;
	private static String currentVersion = null;
	private static String updateSite = "https://github.com/ctt-gob-es/clienteafirma/"; //$NON-NLS-1$

	private static final String AUTOFIRMA_AVOID_UPDATE_CHECK = "AUTOFIRMA_AVOID_UPDATE_CHECK"; //$NON-NLS-1$

	private static Properties updaterProperties = null;

	static {
		loadProperties();
	}

	private Updater() {
		// No instanciable
	}

	static String getUpdateSite() {
		return updateSite;
	}

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static void loadProperties() {
		if (updaterProperties != null) {
			return;
		}
		updaterProperties = new Properties();
		try {
			updaterProperties.load(Updater.class.getResourceAsStream("/properties/updater.properties")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe(
				"No se ha podido cargar el archivo de recursos del actualizador: " + e //$NON-NLS-1$
			);
			updaterProperties = new Properties();
		}
	}

	/** Obtiene la versi&oacute;n actual del aplicativo.
	 * @return Versi&oacute;n actual del aplicativo. */
	private static String getCurrentVersion() {
		if (currentVersion == null) {
			currentVersion = updaterProperties.getProperty("currentVersion", "0"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return currentVersion;
	}

	/** Obtiene la &uacute;ltima versi&oacute;n disponible del programa.
	 * @return &Uacute;ltima versi&oacute;n disponible del programa o <code>null</code> si no
	 *         se ha podido obtener. */
	private static String getLatestVersion() {

		if (updaterProperties == null) {
			return null;
		}

		final String url = updaterProperties.getProperty("url"); //$NON-NLS-1$

		updateSite = updaterProperties.getProperty("updateSite"); //$NON-NLS-1$

		if (url == null) {
			LOGGER.warning(
				"El archivo de recursos del actualizador no contiene una URL de comprobacion" //$NON-NLS-1$
			);
			return null;
		}
		try {
			version = new String(UrlHttpManagerFactory.getInstalledManager().readUrl(url, UrlHttpMethod.GET)).trim();
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
		if (getCurrentVersion() == null) {
			LOGGER.severe("No ha podido comprobar la version actual del aplicativo"); //$NON-NLS-1$
			return false;
		}
		try {
			return Integer.parseInt(newVersion) > Integer.parseInt(getCurrentVersion());
		}
		catch(final Exception e) {
			LOGGER.warning(
				"No ha podido comparar la version actual del aplicativo con la ultima disponible: " + e //$NON-NLS-1$
			);
			return false;
		}
	}

	/** Indica la versi&oacute;n actual del aplicativo es menor que la requerida.
	 * @param neededVersion Versi&oacute;n requerida.
	 * @return <code>true</code> si la versi&oacute;n actual del aplicativo es menor que la requerida,
	 *         <code>false</code> en caso contrario. */
	public static boolean isOldVersion(final String neededVersion) {
		try {
			if (Integer.parseInt(neededVersion) > Integer.parseInt(getCurrentVersion())) {
				return true;
			}
		}
		catch(final Exception e) {
			LOGGER.severe("No se ha podido comprobar si la version actual es menor que la requerida: " + e); //$NON-NLS-1$
		}
		return false;
	}

	/** Comprueba si hay actualizaciones del aplicativo, y en caso afirmativo lo notifica al usuario.
	 * @param parent Componente padre para la modalidad. */
	public static void checkForUpdates(final Object parent) {

		boolean omitCheck = Boolean.parseBoolean(updaterProperties.getProperty("avoidUpdateCheck")); //$NON-NLS-1$
		try {
			omitCheck = Boolean.TRUE.toString().equalsIgnoreCase(System.getenv(AUTOFIRMA_AVOID_UPDATE_CHECK));
		}
		catch(final Exception e) {
			LOGGER.warning(
				"No se ha podido comprobar el valor de la variable de entorno " + AUTOFIRMA_AVOID_UPDATE_CHECK + ": " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		if (Platform.OS.WINDOWS.equals(Platform.getOS()) && !omitCheck) {
			new Thread(() ->  {
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
					else {
						LOGGER.info("Se ha encontrado instalada la ultima version disponible de AutoFirma"); //$NON-NLS-1$
					}
				}
			).start();
		}
	}

}
