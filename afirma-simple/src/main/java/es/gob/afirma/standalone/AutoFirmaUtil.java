package es.gob.afirma.standalone;

import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.ui.hash.CheckHashDialog;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Utilidades generales y de control del autoarranque de AutoFirma en el inicio de Windows.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AutoFirmaUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String REG_CMD = "reg"; //$NON-NLS-1$
	private static final String REG_KEY = "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run"; //$NON-NLS-1$
	private static final String REG_VALUE = "AutoFirma"; //$NON-NLS-1$
	private static final String REG_VALUE_OPT = "/v"; //$NON-NLS-1$

	private static final Image ICON = Toolkit.getDefaultToolkit().getImage(
		CheckHashDialog.class.getResource("/resources/afirma_ico.png") //$NON-NLS-1$
	);

	/** Obtiene el icono por defecto para los di&aacute;logos gr&aacute;fcos.
	 * @return Icono por defecto para los di&aacute;logos gr&aacute;fcos. */
	public static Image getDefaultDialogsIcon() {
		return ICON;
	}

	/** Indica si AutoFirma est&aacute; instalado para ejecutarse al inicio de Windows o no.
	 * @return <code>true</code> si AutoFirma est&aacute; instalado para ejecutarse al inicio de Windows,
	 *         <code>false</code> en caso contrario, si el sistema no es Windows o si no se puede determinar. */
	public static boolean getAutoStartEnabled() {
		if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return false;
		}
		try {
			final Process p = new ProcessBuilder(
				REG_CMD, "QUERY", REG_KEY, REG_VALUE_OPT, REG_VALUE //$NON-NLS-1$
			).start();
			final String res = new String(AOUtil.getDataFromInputStream(p.getInputStream())).trim();
			final String[] tokens = res.split(" "); //$NON-NLS-1$
			for (final String token : tokens) {
				if ("AutoFirma".equals(token)) { //$NON-NLS-1$
					return true;
				}
			}
		}
		catch(final IOException e) {
			LOGGER.severe(
				"No se ha podido leer el registro de Windows para determinar el autoarranque de AutoFirma: " + e //$NON-NLS-1$
			);
		}
		return false;
	}

	/** Establece si AutoFirma debe instalarse para ejecutarse al inicio de Windows o no.
	 * No tiene efecto en sistemas no Windows.
	 * @param enable <code>true</code> para instalar AutoFirma para ejecutarse al inicio de Windows,
	 *         <code>false</code> para desinstalarlo (no tiene efecto si no lo estaba).
	 * @throws IOException En caso de fallo en el proceso. */
	public static void setAutoStartEnabled(final boolean enable) throws IOException {
		if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return;
		}
		if (enable) {
			enableAutoStart();
		}
		else {
			disableAutoStart();
		}
	}


	private static void enableAutoStart() throws IOException {
		if (getAutoStartEnabled()) {
			return;
		}
		final Process p = new ProcessBuilder(
			REG_CMD, "ADD", REG_KEY, REG_VALUE_OPT, REG_VALUE, "/t", "REG_SZ", "/d", "c:\\windows\\system32\\winver.exe" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		).start();
		if (!getAutoStartEnabled()) {
			throw new IOException(
				"No se ha podido habilitar el autoarranque de AutoFirma: " + new String(AOUtil.getDataFromInputStream(p.getErrorStream())) //$NON-NLS-1$
			);
		}
	}

	private static void disableAutoStart() throws IOException {
		if (!getAutoStartEnabled()) {
			return;
		}
		final Process p = new ProcessBuilder(
			REG_CMD, "DELETE", REG_KEY, REG_VALUE_OPT, REG_VALUE, "/f" //$NON-NLS-1$ //$NON-NLS-2$
		).start();
		if (getAutoStartEnabled()) {
			throw new IOException(
				"No se ha podido deshabilitar el autoarranque de AutoFirma: " + new String(AOUtil.getDataFromInputStream(p.getErrorStream())) //$NON-NLS-1$
			);
		}
	}

	/** Recupera el directorio en el que se encuentra la aplicaci&oacute;n.
	 * @return Directorio de ejecuci&oacute;n. */
	public static File getApplicationDirectory() {

		if (isJnlpDeployment()) {
			if (Platform.getOS() == Platform.OS.WINDOWS) {
				final String commonDir = System.getenv("ALLUSERSPROFILE"); //$NON-NLS-1$
				final File appDir = new File (commonDir, "AutoFirma"); //$NON-NLS-1$
				if (appDir.isDirectory() || appDir.mkdirs()) {
					return appDir;
				}
			}
			else if (Platform.getOS() == Platform.OS.MACOSX) {
				final String userDir = System.getenv("HOME"); //$NON-NLS-1$
				final File appDir = new File (userDir, "Library/Application Support/AutoFirma"); //$NON-NLS-1$
				if (appDir.isDirectory() || appDir.mkdirs()) {
					return appDir;
				}
			}
			return new File(System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		}

		// Identificamos el directorio de instalacion
		try {
			return new File(
				AutoFirmaUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()
			).getParentFile();
		}
		catch (final URISyntaxException e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion: " + e); //$NON-NLS-1$
		}

		return null;
	}

	/**
	 * Comprueba si estamos en un despliegue JNLP de la aplicaci&oacute;n.
	 * @return {@code true} si estamos en un despliegue JNLP, {@code false}
	 * en caso contrario.
	 */
	private static boolean isJnlpDeployment() {
		try {
			javax.jnlp.ServiceManager.lookup("javax.jnlp.ExtendedService"); //$NON-NLS-1$
		}
		catch (final Throwable e) {
			return false;
		}
		return true;
	}

	/** Recupera el DPI de la pantalla principal.
	 * @return DPI de la pantalla principal. */
	public static int getDPI() {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			final String[] cmd = {"wmic", "desktopmonitor", "get", "PixelsPerXLogicalInch"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			final ProcessBuilder builder = new ProcessBuilder(cmd);
			try {
				final Process process = builder.start();
				process.waitFor();
				try (
					final BufferedReader bufferedReader = new BoundedBufferedReader(new InputStreamReader(process.getInputStream()));
				) {
					String line;
					int dpi = 0;
					while ((line = bufferedReader.readLine()) != null) {
						try {
							dpi = Integer.parseInt(line.trim());
							break;
						}
						catch (final Exception e) {
							continue;
						}
		            }
		            return dpi;
				}
			}
			catch (final Exception e) {
				LOGGER.log(	Level.SEVERE, "Error obteniendo DPI: " + e); //$NON-NLS-1$
				return 0;
			}
		}
		return 0;
	}

	/** Recupera el n&uacute;mero de pantallas que tiene habilitadas el usuario.
	 * @return N&uacute;mero de pantallas.
	 * @throws HeadlessException Si el equipo no tiene pantalla. */
	public static int getDisplaysNumber() throws HeadlessException {
		return GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices().length;
	}

	/** Devuelve el fichero en su forma can&oacute;nica.
	 * @param file Fichero del cual obtener su forma can&oacute;nica.
	 * @return Fichero en su forma can&oacute;nica o el fichero de entrada si hay error.*/
	public static File getCanonicalFile(final File file) {
		try {
			return file.getCanonicalFile();
		}
		catch(final IOException e) {
			LOGGER.severe(
				"No se ha podido obtener el fichero canonico: " + e //$NON-NLS-1$
			);
			return file;
		}
	}

    /** Establece la configuraci&oacute;n para el servidor <i>Proxy</i> seg&uacute;n los valores
     * de configuraci&oacute;n encontrados. */
    public static void setProxySettings() {
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED, false)) {
    		final String proxyHost = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST, null);
    		final String proxyPort = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT, null);
    		final String proxyUsername = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME, null);
    		final String proxyPassword = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD, null);
    		if (proxyHost != null && proxyPort != null) {
//				if (!proxyHost.startsWith("http")) { //$NON-NLS-1$
//					proxyHost = "http://" + proxyHost; //$NON-NLS-1$
//				}
    			System.setProperty("http.proxyHost", proxyHost); //$NON-NLS-1$
    			System.setProperty("http.proxyPort", proxyPort); //$NON-NLS-1$
    			System.setProperty("https.proxyHost", proxyHost); //$NON-NLS-1$
    			System.setProperty("https.proxyPort", proxyPort); //$NON-NLS-1$
    			System.setProperty("ftp.proxHost", proxyHost); //$NON-NLS-1$
    			System.setProperty("ftp.proxyPort", proxyPort); //$NON-NLS-1$
    			System.setProperty("socksProxyHost", proxyHost); //$NON-NLS-1$
    			System.setProperty("socksProxyPort", proxyPort); //$NON-NLS-1$
        		if (proxyUsername != null && !proxyUsername.trim().isEmpty() && proxyPassword != null) {
        			Authenticator.setDefault(
    					new Authenticator() {
	    			        @Override
	    					public PasswordAuthentication getPasswordAuthentication() {
	    			            return new PasswordAuthentication(
    			            		proxyUsername,
    			            		proxyPassword.toCharArray()
			            		);
	    			        }
	    			    }
					);
        		}
    		}
    		else {
    			System.clearProperty("http.proxyHost"); //$NON-NLS-1$
    			System.clearProperty("http.proxyPort"); //$NON-NLS-1$
    			System.clearProperty("https.proxyHost"); //$NON-NLS-1$
    			System.clearProperty("https.proxyPort"); //$NON-NLS-1$
    			System.clearProperty("ftp.proxHost"); //$NON-NLS-1$
    			System.clearProperty("ftp.proxyPort"); //$NON-NLS-1$
    			System.clearProperty("socksProxyHost"); //$NON-NLS-1$
    			System.clearProperty("socksProxyPort"); //$NON-NLS-1$
    			Authenticator.setDefault(null);
    		}
    	}
    	else {
    		System.clearProperty("http.proxyHost"); //$NON-NLS-1$
			System.clearProperty("http.proxyPort"); //$NON-NLS-1$
			System.clearProperty("https.proxyHost"); //$NON-NLS-1$
			System.clearProperty("https.proxyPort"); //$NON-NLS-1$
			System.clearProperty("ftp.proxHost"); //$NON-NLS-1$
			System.clearProperty("ftp.proxyPort"); //$NON-NLS-1$
			System.clearProperty("socksProxyHost"); //$NON-NLS-1$
			System.clearProperty("socksProxyPort"); //$NON-NLS-1$
			Authenticator.setDefault(null);
    	}
    }

}
