package es.gob.afirma.standalone.ui.tasks;

import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.util.logging.Logger;

import javax.net.ssl.HttpsURLConnection;

import es.gob.afirma.core.misc.http.SslSecurityManager;
import es.gob.afirma.standalone.HttpManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/**
 * Hilo para la configuraci&oacute;n del contexto SSL para las conexiones remotas.
 */
public class SSLContextConfigurationTask extends Thread {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final  String SYSTEM_PREFERENCE_BLOCK_AUTO_IMPORT_TRUSTED_CERTS = "blockAutoImportTrustedCerts"; //$NON-NLS-1$

	private Exception exception;

    @Override
    public void run() {

    	LOGGER.info("Iniciando hilo para la configuracion del contexto SSL: "); //$NON-NLS-1$

    	// Establecemos si deben respetarse las comprobaciones de seguridad de las
    	// conexiones de red
    	final boolean secureConnections = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_SECURE_CONNECTIONS);

    	LOGGER.info("Configuramos el contexto SSL"); //$NON-NLS-1$
    	HttpManager.setSecureConnections(secureConnections);

    	// Establecemos el listado de dominios seguros
    	LOGGER.info("Configuramos el listado de dominios seguros"); //$NON-NLS-1$
    	HttpManager.setSecureDomains(
    			PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_SECURE_DOMAINS_LIST));

    	// Configuramos las propiedades del sistema necesarias para que SSLErrorProcesor las trate correctamente
    	final boolean allowPersonalTruststore = PreferencesManager.getBoolean(PreferencesManager.ADMIN_PREFERENCE_ALLOW_PERSONAL_TRUSTSTORE);
    	final boolean allowAutoImportTrustedCerts = PreferencesManager.getBoolean(PreferencesManager.ADMIN_PREFERENCE_ALLOW_AUTO_IMPORT_TRUSTED_CERTS);

		final boolean allowAutoImport = allowPersonalTruststore && allowAutoImportTrustedCerts;
		if (!allowAutoImport) {
			System.setProperty(SYSTEM_PREFERENCE_BLOCK_AUTO_IMPORT_TRUSTED_CERTS, Boolean.TRUE.toString());
		}

    	// Establecemos los almacenes de claves de Java y de Autofirma como de confianza para las
    	// conexiones remotas
    	if (secureConnections) {

    		boolean truststoreConfigured;

    		try {

    			if (allowPersonalTruststore) {
    				LOGGER.info("Configuramos el almacen de confianza de la aplicacion"); //$NON-NLS-1$
    				truststoreConfigured = SslSecurityManager.configureAfirmaTrustManagers();
    	    		LOGGER.info("El almacen de confianza de la aplicacion existe y se ha configurado: " + truststoreConfigured); //$NON-NLS-1$
    			} else {
    				LOGGER.info("El almacen de confianza no se cargara debido a la configuracion de Autofirma"); //$NON-NLS-1$
    				truststoreConfigured = false;
    			}

    		} catch (IOException | GeneralSecurityException ex) {
    			LOGGER.warning("Error al configurar almacenes de confianza: " + ex); //$NON-NLS-1$
    			truststoreConfigured = false;
    			this.exception = ex;
    		}


    		// Si se ha configurado el almacen de confianza propio de Autofirma, realizamos una
    		// primera conexion, ya que hemos identificado que en algunos casos la primera conexion
    		// puede fallar, pero permitira aplicar los cambios en el contexto SSL
    		if (truststoreConfigured) {
    			LOGGER.info("Realizamos conexion de prueba"); //$NON-NLS-1$
    			try {
    				final URL url = new URI("https://test.conexion.gob.es/").toURL(); //$NON-NLS-1$
    				final HttpsURLConnection conn = (HttpsURLConnection) url.openConnection();
    				conn.setConnectTimeout(100);
    				conn.connect();
    				conn.disconnect();
    			}
    			catch (final Exception ex) { /* La primera vez falla para aplicar cambios en trust managers*/ }
    			LOGGER.info("Fin de la conexion de prueba"); //$NON-NLS-1$
    		}
    	}
    }

    /**
     * Obtiene la excepci&oacute;n que se haya producido durante la configuraci&oacute;n
     * en caso de haberla.
     * @return Excepci&oacute;n durante la configuraci&oacute;n o {@code null}
     * si no se produjo ninguna.
     */
	public Exception getException() {
		return this.exception;
	}

}
