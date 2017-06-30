package es.gob.afirma.standalone;

import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.util.logging.Logger;

import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Utilidades para el manejo y establecimiento del <i>Proxy</i> de red para las
 * conexiones de la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class ProxyUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProxyUtil() {
		// No instanciable
	}

	/** Indica si hay un <i>Proxy</i> establecido en base a variables de entorno
	 * del sistema operativo.
	 * @return <code>true</code> si hay un <i>Proxy</i> establecido en base a
	 * variables de entorno del sistema operativo, <code>false</code> en caso
	 * contrario. */
	public static boolean isSystemProxySet() {
    	final String systemProxyHost = System.getenv("http.proxyHost"); //$NON-NLS-1$
    	final String systemProxyPort = System.getenv("http.proxyPort"); //$NON-NLS-1$
    	return systemProxyHost != null &&
    		   systemProxyPort != null &&
    		   !systemProxyPort.isEmpty() &&
    		   systemProxyPort.matches("\\d+"); //$NON-NLS-1$
	}

	static void configureSystemProxy() {

		final String systemHttpProxyHost  = System.getenv("http.proxyHost"); //$NON-NLS-1$
    	final String systemHttpProxyPort  = System.getenv("http.proxyPort"); //$NON-NLS-1$
    	final String systemHttpsProxyHost = System.getenv("https.proxyHost"); //$NON-NLS-1$
    	final String systemHttpsProxyPort = System.getenv("https.proxyPort"); //$NON-NLS-1$
    	final String systemFtpProxyHost   = System.getenv("ftp.proxyHost"); //$NON-NLS-1$
    	final String systemFtpProxyPort   = System.getenv("ftp.proxyPort"); //$NON-NLS-1$
    	final String systemSocksProxyHost = System.getenv("ftp.proxyHost"); //$NON-NLS-1$
    	final String systemSocksProxyPort = System.getenv("ftp.proxyPort"); //$NON-NLS-1$
    	final String systemProxyUsername  = System.getenv("java.proxyUser"); //$NON-NLS-1$
    	final String systemProxyPassword  = System.getenv("java.proxyPassword"); //$NON-NLS-1$

    	System.setProperty("http.proxyHost", systemHttpProxyHost); //$NON-NLS-1$
		System.setProperty("http.proxyPort", systemHttpProxyPort); //$NON-NLS-1$

		if (systemHttpsProxyHost != null && systemHttpsProxyPort != null) {
			System.setProperty("https.proxyHost", systemHttpsProxyHost); //$NON-NLS-1$
			System.setProperty("https.proxyPort", systemHttpsProxyPort); //$NON-NLS-1$
		}
		else {
			System.setProperty("https.proxyHost", systemHttpProxyHost); //$NON-NLS-1$
			System.setProperty("https.proxyPort", systemHttpProxyPort); //$NON-NLS-1$
		}

		if (systemFtpProxyHost != null && systemFtpProxyPort != null) {
			System.setProperty("ftp.proxyHost", systemFtpProxyHost); //$NON-NLS-1$
			System.setProperty("ftp.proxyPort", systemFtpProxyPort); //$NON-NLS-1$
		}
		else {
			System.setProperty("ftp.proxyHost", systemHttpProxyHost); //$NON-NLS-1$
			System.setProperty("ftp.proxyPort", systemHttpProxyPort); //$NON-NLS-1$
		}

		if (systemSocksProxyHost != null && systemSocksProxyPort != null) {
			System.setProperty("socks.proxyHost", systemSocksProxyHost); //$NON-NLS-1$
			System.setProperty("socks.proxyPort", systemSocksProxyPort); //$NON-NLS-1$
		}
		else {
			System.setProperty("socks.proxyHost", systemHttpProxyHost); //$NON-NLS-1$
			System.setProperty("socks.proxyPort", systemHttpProxyPort); //$NON-NLS-1$
		}

		setProxyAuthenticator(systemProxyUsername, systemProxyPassword);

		LOGGER.info(
			"Se usara el Proxy configurado en sistema para las conexiones de red: " + systemHttpProxyHost + ":" + systemHttpProxyPort //$NON-NLS-1$ //$NON-NLS-2$
		);
	}

	private static void setProxyAuthenticator(final String proxyUsername, final String proxyPassword) {
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

    /** Establece la configuraci&oacute;n para el servidor <i>Proxy</i> seg&uacute;n los valores
     * de configuraci&oacute;n encontrados. */
    public static void setProxySettings() {

    	// Si hay un proxy configurado a nivel de sistema operativo se usa ese y se ignoran
    	// las opciones de AutoFirma
    	if (isSystemProxySet()) {
    		configureSystemProxy();
    	}
    	// Si no hay configuracion de sistema operativo, se usa la configuracion del
    	// GUI de autofirma
    	else if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED, false)) {
    		final String proxyHost = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST, null);
    		final String proxyPort = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT, null);
    		final String proxyUsername = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME, null);
    		final String proxyPassword = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD, null);

    		if (proxyHost != null && proxyPort != null) {

    			LOGGER.info(
					"Establecido Proxy de red desde el GUI de la aplicacion: " + proxyHost + ":" + proxyPort //$NON-NLS-1$ //$NON-NLS-2$
				);

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
    		// Indicar que si se quiere usar proxy pero con los valores en blanco se entiende
    		// como limpiar valores que pudiesen estar ya establecidos en Java
    		else {
    			clearJavaProxy();
    		}
    	}
    	// Si no se indica nada en el GUI de AutoFirma, se dejan los valores por defecto de
    	// la JVM tal y como estaban, nunca se sobreescriben.
    	else {
    		final String javaProxyHost = System.getProperty("http.proxyHost"); //$NON-NLS-1$
    		final String javaProxyPort = System.getProperty("http.proxyHost"); //$NON-NLS-1$
    		if (javaProxyHost != null && javaProxyPort != null) {
    			LOGGER.info(
					"Se usara el Proxy por defecto de Java para las conexiones de red: " + javaProxyHost + ":" + javaProxyPort //$NON-NLS-1$ //$NON-NLS-2$
				);
    		}
    		else {
    			LOGGER.info("No se usara Proxy para las conexiones de red"); //$NON-NLS-1$
    		}
    	}
    }

    private static void clearJavaProxy() {
		LOGGER.info("No se usara Proxy para las conexiones de red"); //$NON-NLS-1$
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
