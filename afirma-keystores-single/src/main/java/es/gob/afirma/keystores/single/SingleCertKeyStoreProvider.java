/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

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
            @Override
			public Object run() {
                put("KeyStore.PKCS7", "es.gob.afirma.keystores.single.SingleCertKeyStore"); //$NON-NLS-1$ //$NON-NLS-2$
                return null;
            }
        });
    }
}
