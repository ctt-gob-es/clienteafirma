package es.gob.afirma.keystores.smartcard;

import java.io.IOException;
import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;

public class JMulticardKeyStoreManager extends AOKeyStoreManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@Override
    public void init(final AOKeyStore type,
	         final InputStream store,
	         final PasswordCallback pssCallBack,
	         final Object[] params,
	         final boolean forceReset) throws AOKeyStoreManagerException,
	                                          IOException {

		if (type == null) {
            throw new IllegalArgumentException("Se ha solicitado inicializar un AOKeyStore nulo"); //$NON-NLS-1$
        }
        LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

        resetCachedAliases();

        // Guardamos los parametros de inicializacion por si hay que reiniciar
        setKeyStoreType(type);
        this.storeIs = store;
        this.storePasswordCallBack = pssCallBack;
        if (params == null) {
        	this.storeParams = null;
        }
        else {
        	// Copia defensiva ante mutaciones
        	this.storeParams = new Object[params.length];
        	System.arraycopy(params, 0, this.storeParams, 0, params.length);
        }

        switch(type) {
        	case SMARTCAFE:
                // En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
        		setParentComponent(params != null && params.length > 0 ? params[0] : null);
        		this.ks = AOKeyStoreManagerHelperFullJava.initSmartCafeJava(
    				getParentComponent()
				);
            	break;
        	case CERES:
                // En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
        		setParentComponent(params != null && params.length > 0 ? params[0] : null);
        		this.ks = AOKeyStoreManagerHelperFullJava.initCeresJava(
    				getParentComponent()
				);
            	break;
           	 /**
           	  * La CERES 4.30 y superiores son exactamente igual a los DNIe y se gestionan
           	  * directamente como tales.
           	  */
        	case CERES_430:
        	case DNIEJAVA:
                // En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
        		setParentComponent(params != null && params.length > 0 ? params[0] : null);
            	this.ks = AOKeyStoreManagerHelperFullJava.initDnieJava(
        			getParentComponent()
    			);
            	break;
            default:
            	throw new UnsupportedOperationException("Tipo de almacen no soportado: " + type); //$NON-NLS-1$
        }
	}

    @Override
	public X509Certificate getCertificate(final String alias) {
        if (alias == null) {
            LOGGER.warning("El alias del certificado es nulo, se devolvera null"); //$NON-NLS-1$
            return null;
        }

        if (this.ks == null) {
            LOGGER.warning(
        		"No se ha podido recuperar el certificado con el alias especificado porque el KeyStore no estaba inicializado, se devolvera null" //$NON-NLS-1$
    		);
            return null;
        }

    	try {
    		return (X509Certificate) this.ks.getCertificate(alias);
    	}
    	catch(final es.gob.jmulticard.CancelledOperationException e) {
    		throw new AOCancelledOperationException("Se cancelo uso de la tarjeta a traves del driver Java: " + e, e); //$NON-NLS-1$
    	}
    	catch(final Exception e) {
    		LOGGER.severe(
				"Error intentando recuperar el certificado, se devolvera null: " + e //$NON-NLS-1$
			);
    		return null;
    	}
    }
}
