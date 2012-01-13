/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.single;

import java.security.AccessController;
import java.security.Provider;

/** Proveedor de seguridad espec&iacute;fico para servicios de <i>KeyStore</i>
 * restringidos a almacenes PKCS#7 y certificados X.509 en Base64. */
public final class SingleCertKeyStoreProvider extends Provider {

    private static final long serialVersionUID = 3525417804439532445L;

    private static final double PROVIDER_VERSION = 0.1d;

    /** Construye un proveedor de seguridad para apertura de certificados en fichero.
     * Estos certificados pueden no tener clave privada, pero se tratan en un <code>KeyStore</code> en vez de
     * en un <code>CertStore</code> para uniformar su uso. */
    public SingleCertKeyStoreProvider() {
        super("PKCS7", PROVIDER_VERSION, "KeyStore for a PKCS7 or X.509 certificate"); //$NON-NLS-1$ //$NON-NLS-2$
        AccessController.doPrivileged(new java.security.PrivilegedAction<Object>() {
        	/** {@inheritdoc} */
            public Object run() {
                put("KeyStore.PKCS7", "es.gob.afirma.keystores.single.SingleCertKeyStore"); //$NON-NLS-1$ //$NON-NLS-2$
                return null;
            }
        });
    }
}
