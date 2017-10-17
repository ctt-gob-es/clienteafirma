/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;

import javax.crypto.BadPaddingException;
import javax.security.auth.callback.PasswordCallback;

final class AOKeyStoreManagerHelperJava {

	private AOKeyStoreManagerHelperJava() {
		// No permitimos la instanciacion
	}

	static KeyStore initJava(final InputStream store,
								   final PasswordCallback pssCallBack,
			                       final AOKeyStore ksType) throws AOKeyStoreManagerException,
			                                                       IOException {

		// Suponemos que el proveedor SunJSSE esta instalado. Hay que tener cuidado con esto
		// si alguna vez se usa JSS, que a veces lo retira
		if (store == null) {
			throw new IOException(
				"Es necesario proporcionar el fichero KeyStore"); //$NON-NLS-1$
		}

		final KeyStore ks;

		try {
			ks = KeyStore.getInstance(ksType.getProviderName());
		}
		catch (final Exception e) {
			throw new AOKeyStoreManagerException(
				"No se ha podido obtener el almacen JavaKeyStore", e); //$NON-NLS-1$
		}

		// TODO: Revisar si el KeyStore de Java requiere contrasena
		try {
			ks.load(store, pssCallBack != null ? pssCallBack.getPassword() : null);
		}
		catch (final IOException e) {
			if (e.getCause() instanceof UnrecoverableKeyException || e.getCause() instanceof BadPaddingException) {
				throw new IOException("Contrasena invalida: " + e, e); //$NON-NLS-1$
			}
			throw e;
		}
		catch (final CertificateException e) {
			throw new AOKeyStoreManagerException(
				"No se han podido cargar los certificados del almacen JavaKeyStore solicitado", e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOKeyStoreManagerException(
				"No se ha podido verificar la integridad del almacen JavaKeyStore solicitado", e); //$NON-NLS-1$
		}

		return ks;

	}

}
