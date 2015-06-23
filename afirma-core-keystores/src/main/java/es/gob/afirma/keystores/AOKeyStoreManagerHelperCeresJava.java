package es.gob.afirma.keystores;

import java.io.IOException;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.cert.CertificateException;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

final class AOKeyStoreManagerHelperCeresJava {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private AOKeyStoreManagerHelperCeresJava() {
		// No permitimos la instanciacion
	}

	/** Inicializa el almac&eacute;n 100% Java para tarjeta CERES.
	 * @param pssCallBack No se usa, no se necesita el PIN en la inicializaci&oacute;n (se pide luego, en la firma).
	 * @return <code>KeyStore</code> inicializado.
	 * @throws AOKeyStoreManagerException Si no se puede inicializar el almac&eacute;n.
	 * @throws IOException Si hay problemas en la lectura de datos. */
    static KeyStore initCeresJava(final PasswordCallback pssCallBack) throws AOKeyStoreManagerException,
    		                                                                 IOException {
    	final Provider p;
    	if (Security.getProvider(AOKeyStore.CERES.getProviderName()) == null) {
    		try {
    			p = (Provider) Class.forName("es.gob.jmulticard.jse.provider.ceres.CeresProvider").newInstance(); //$NON-NLS-1$
    			Security.addProvider(p);
    		}
    		catch (final Exception e) {
    			throw new AOKeyStoreManagerException(
					"No se ha podido instanciar e instalar el proveedor 100% Java para CERES de Afirma: " + e, //$NON-NLS-1$
					e
				);
    		}
    	}

    	final KeyStore ks;
        // Inicializamos
        try {
            ks = KeyStore.getInstance(AOKeyStore.CERES.getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen CERES 100% Java: " + e, e); //$NON-NLS-1$
        }

        LOGGER.info("Cargando KeyStore CERES 100% Java"); //$NON-NLS-1$
        try {
			ks.load(null, null);
		}
        catch (final NoSuchAlgorithmException e) {
        	throw new AOKeyStoreManagerException("Error de algoritmo al obtener el almacen CERES 100% Java: " + e, e);  //$NON-NLS-1$
		}
        catch (final CertificateException e) {
			throw new AOKeyStoreManagerException("Error de certificado al obtener el almacen CERES 100% Java: " + e, e);  //$NON-NLS-1$
		}

        return ks;
    }

}
