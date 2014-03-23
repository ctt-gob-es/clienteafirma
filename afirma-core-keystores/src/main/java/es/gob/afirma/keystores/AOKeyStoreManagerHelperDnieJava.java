package es.gob.afirma.keystores;

import java.awt.Component;
import java.io.IOException;
import java.lang.reflect.Method;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.cert.CertificateException;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

final class AOKeyStoreManagerHelperDnieJava {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private AOKeyStoreManagerHelperDnieJava() {
		// No permitimos la instanciacion
	}

    static KeyStore initDnieJava(final PasswordCallback pssCallBack,
    		                            final Object parentComponent) throws AOKeyStoreManagerException,
    		                                                                 IOException {
    	final Provider p;
    	if (Security.getProvider(AOKeyStore.DNIEJAVA.getProviderName()) == null) {
    		try {
    			p = (Provider) Class.forName("es.gob.jmulticard.jse.provider.DnieProvider").newInstance(); //$NON-NLS-1$
    			Security.addProvider(p);
    		}
    		catch (final Exception e) {
    			throw new AOKeyStoreManagerException(
					"No se ha podido instanciar e instalar el proveedor 100% Java para DNIe de Afirma: " + e, //$NON-NLS-1$
					e
				);
    		}
    	}

    	try {
    		final Class<?> managerClass = Class.forName("es.gob.jmulticard.ui.passwordcallback.PasswordCallbackManager"); //$NON-NLS-1$
    		final Method setDialogOwnerFrameMethod = managerClass.getMethod("setDialogOwner", Component.class); //$NON-NLS-1$
    		setDialogOwnerFrameMethod.invoke(null, parentComponent);
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se ha podido establecer el componente padre para los dialogos del almacen: " + e); //$NON-NLS-1$
    	}

    	final KeyStore ks;
        // Inicializamos
        try {
            ks = KeyStore.getInstance(AOKeyStore.DNIEJAVA.getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen DNIe 100% Java: " + e, e); //$NON-NLS-1$
        }

        LOGGER.info("Cargando KeyStore DNIe 100% Java"); //$NON-NLS-1$
        try {
			ks.load(null, pssCallBack == null ? null : pssCallBack.getPassword());
		}
        catch (final NoSuchAlgorithmException e) {
        	throw new AOKeyStoreManagerException("Error de algoritmo al obtener el almacen DNIe 100% Java: " + e, e);  //$NON-NLS-1$
		}
        catch (final CertificateException e) {
			throw new AOKeyStoreManagerException("Error de certificado al obtener el almacen DNIe 100% Java: " + e, e);  //$NON-NLS-1$
		}

        return ks;
    }

}
