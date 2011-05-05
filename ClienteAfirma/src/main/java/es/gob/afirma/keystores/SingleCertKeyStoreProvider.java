/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores;

import java.security.AccessController;
import java.security.Provider;

/**
 * Proveedor de seguridad espec&iacute;fico para servicios de <i>KeyStore</i> restringidos a almacenes
 * PKCS#7 y certificados X.509 en Base64.
 */
final class SingleCertKeyStoreProvider extends Provider {

	private static final long serialVersionUID = 3525417804439532445L;

	protected SingleCertKeyStoreProvider() {
	   super("PKCS7", 0.1d, "KeyStore for a PKCS7 or X509 certificate");
		
       AccessController.doPrivileged(new java.security.PrivilegedAction<Object>() {
			public Object run() {
				put("KeyStore.PKCS7", "es.gob.afirma.keystores.SingleCertKeyStore");
				return null;
			}
       });
	}
}
