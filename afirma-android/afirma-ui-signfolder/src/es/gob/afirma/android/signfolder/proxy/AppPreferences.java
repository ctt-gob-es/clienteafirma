package es.gob.afirma.android.signfolder.proxy;

import java.util.Properties;

import android.app.Activity;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;
import es.gob.afirma.android.signfolder.SFConstants;

public final class AppPreferences {

	private static final String CONFIG_PROPERTIES = "config.properties"; //$NON-NLS-1$

	/** Tiempo maximo que se va a esperar por una respuesta del proxy. */
	private static final int DEFAULT_CONNECTION_READ_TIMEOUT = 14000;

	private static final String KEY_CONNECTION_READ_TIMEOUT = "connection.timeout"; //$NON-NLS-1$

	private static final String KEY_FORMATS_SUPPORTED = "supported.format"; //$NON-NLS-1$

	private static final String CONFIG_SEPARATOR = ";"; //$NON-NLS-1$



	private static Properties config;

	private static SharedPreferences sharedPref;

	public static void init(final Activity activity) {

		config = new Properties();
		try {
			config.load(activity.getAssets().open(CONFIG_PROPERTIES));
		} catch (final Exception e) {
			// Esto no deberia ocurrir nunca
			throw new RuntimeException("No se encuentra el fichero de configuracion " + CONFIG_PROPERTIES, e); //$NON-NLS-1$
		}

		sharedPref = activity.getPreferences(Context.MODE_PRIVATE);

	}

	/**
	 * Recupera el tiempo de TimeOut configurado para las conexiones de red.
	 * @return Milisegundos de espera.
	 */
	public static int getConnectionreadTimeout() {

		if (config.containsKey(KEY_CONNECTION_READ_TIMEOUT)) {
			try {
				return Integer.parseInt(config.getProperty(KEY_CONNECTION_READ_TIMEOUT));
			}
			catch (final NumberFormatException e) {
				Log.w(SFConstants.LOG_TAG,
						"Error en el timeout configurado en el fichero de propiedades. Se usara el por defecto: " + DEFAULT_CONNECTION_READ_TIMEOUT); //$NON-NLS-1$
			}
		}
		return DEFAULT_CONNECTION_READ_TIMEOUT;
	}

	/** Recupera el listado de formatos soportados.
	 * @return Listado de formatos soportados. */
	public static String[] getSupportedFormats() {
		return config.getProperty(KEY_FORMATS_SUPPORTED).split(CONFIG_SEPARATOR);
	}

	static String getPreference(final String key) {
		return sharedPref.getString(key, ""); //$NON-NLS-1$
	}

	static String getPreference(final String key, final String defaultValue) {
		return sharedPref.getString(key, defaultValue);
	}

	static void setPreference(final String key, final String value) {
		final SharedPreferences.Editor editor = sharedPref.edit();
		editor.putString(key, value);
		//editor.putString(SFConstants.PREFERENCES_KEY_URL_PROXY, proxyUrl);
		editor.commit();
	}

	/** Establece la URL del proxy para la conexi&oacute;n con el Portafirmas.
	 * @param urlProxy URL del proxy. */
	public static void setUrlProxy(final String urlProxy) {
		setPreference(SFConstants.PREFERENCES_KEY_URL_PROXY, urlProxy);
	}

	/** Recupera la direcci&oacute;n del servidor proxy para la conexi&oacute;n con el Portafirmas.
	 * @return URL del Portafirmas. */
	public static String getUrlProxy() {
		return getPreference(SFConstants.PREFERENCES_KEY_URL_PROXY);
	}
}
