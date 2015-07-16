package es.gob.afirma.keystores;

import java.io.IOException;
import java.lang.reflect.Method;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.cert.CertificateException;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

final class AOKeyStoreManagerHelperFullJava {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String PROVIDER_CERES = "es.gob.jmulticard.jse.provider.ceres.CeresProvider"; //$NON-NLS-1$
	private static final String PROVIDER_DNIE = "es.gob.jmulticard.jse.provider.DnieProvider"; //$NON-NLS-1$

	private AOKeyStoreManagerHelperFullJava() {
		// No permitimos la instanciacion
	}

	/** Inicializa el almac&eacute;n 100% Java para tarjeta CERES.
	 * @return <code>KeyStore</code> inicializado.
	 * @throws AOKeyStoreManagerException Si no se puede inicializar el almac&eacute;n.
	 * @throws IOException Si hay problemas en la lectura de datos. */
	static KeyStore initCeresJava(final Object parentComponent) throws AOKeyStoreManagerException,
                                                                       IOException {
		return init(
			AOKeyStore.CERES,
			null,
			parentComponent,
			PROVIDER_CERES
		);
	}

	/** Inicializa el almac&eacute;n 100% Java para DNIe.
	 * @param pssCallBack <code>PasswordCallback</code> para la obtenci&oacute;n del PIN.
	 * @return <code>KeyStore</code> inicializado.
	 * @throws AOKeyStoreManagerException Si no se puede inicializar el almac&eacute;n.
	 * @throws IOException Si hay problemas en la lectura de datos. */
    static KeyStore initDnieJava(final PasswordCallback pssCallBack,
    		                     final Object parentComponent) throws AOKeyStoreManagerException,
    		                                                                 IOException {
    	return init(
			AOKeyStore.DNIEJAVA,
			pssCallBack,
			parentComponent,
			PROVIDER_DNIE
		);
    }


    private static KeyStore init(final AOKeyStore store,
    		                     final PasswordCallback pssCallBack,
    		                     final Object parentComponent,
    		                     final String providerClassName) throws AOKeyStoreManagerException,
    		                                                            IOException {
    	final Provider p;
    	if (Security.getProvider(AOKeyStore.CERES.getProviderName()) == null) {
    		try {
    			p = (Provider) Class.forName(providerClassName).newInstance();
    			Security.addProvider(p);
    		}
    		catch (final Exception e) {
    			throw new AOKeyStoreManagerException(
					"No se ha podido instanciar e instalar el proveedor 100% Java de Afirma para " + store.toString() + ": " + e, //$NON-NLS-1$ //$NON-NLS-2$
					e
				);
    		}
    	}

    	try {
    		final Class<?> managerClass = Class.forName("es.gob.jmulticard.ui.passwordcallback.PasswordCallbackManager"); //$NON-NLS-1$
    		final Method setDialogOwnerFrameMethod = managerClass.getMethod("setDialogOwner", Object.class); //$NON-NLS-1$
    		setDialogOwnerFrameMethod.invoke(null, parentComponent);
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se ha podido establecer el componente padre para los dialogos del almacen: " + e); //$NON-NLS-1$
    	}

    	final KeyStore ks;
        // Inicializamos
        try {
            ks = KeyStore.getInstance(store.getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen 100% Java para " + store.toString() + ": " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        LOGGER.info("Cargando KeyStore 100% Java para " + store.toString()); //$NON-NLS-1$
        try {
			ks.load(null, pssCallBack == null ? null : pssCallBack.getPassword());
		}
        catch (final NoSuchAlgorithmException e) {
        	throw new AOKeyStoreManagerException(
    			"Error de algoritmo al obtener el almacen 100% Java para " + store.toString() + ": " + e, e  //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
        catch (final CertificateException e) {
			throw new AOKeyStoreManagerException(
				"Error de certificado al obtener el almacen 100% Java para " + store.toString() + ": " + e, e  //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

        return ks;
    }

}
