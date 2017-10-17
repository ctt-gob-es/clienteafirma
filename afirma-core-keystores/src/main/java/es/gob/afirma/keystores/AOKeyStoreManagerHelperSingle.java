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
import java.security.Provider;
import java.security.Security;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;

import javax.crypto.BadPaddingException;
import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.MissingLibraryException;

final class AOKeyStoreManagerHelperSingle {

	private AOKeyStoreManagerHelperSingle() {
		// No permitimos la instanciacion
	}

	static KeyStore initSingle(final InputStream store,
			                         final PasswordCallback pssCallBack) throws AOKeyStoreManagerException,
			                                                                     IOException {
		if (store == null) {
			throw new AOKeyStoreManagerException(
				"Es necesario proporcionar el fichero X.509 o PKCS#7"); //$NON-NLS-1$
		}

		final Provider pkcs7Provider;
		try {
			pkcs7Provider = (Provider) Class.forName(
				"es.gob.afirma.keystores.single.SingleCertKeyStoreProvider").getConstructor().newInstance(); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new MissingLibraryException(
				"No se ha podido instanciar el proveedor SingleCertKeyStoreProvider: " + e, e); //$NON-NLS-1$
		}
		Security.addProvider(pkcs7Provider);

		final KeyStore ks;

		try {
			ks = KeyStore.getInstance(AOKeyStore.SINGLE.getProviderName(), pkcs7Provider);
		}
		catch (final Exception e) {
			throw new AOKeyStoreManagerException(
				"No se ha podido obtener el almacen PKCS#7 / X.509", e); //$NON-NLS-1$
		}

		try {
			ks.load(store, pssCallBack != null ? pssCallBack.getPassword() : null);
		}
		catch (final IOException e) {
			if (e.getCause() instanceof UnrecoverableKeyException
					|| e.getCause() instanceof BadPaddingException) {
						throw new IOException("Contrasena invalida: " + e, e); //$NON-NLS-1$
			}
			throw new AOKeyStoreManagerException(
				"No se ha podido abrir el almacen PKCS#7 / X.509 solicitado", e); //$NON-NLS-1$
		}
		catch (final CertificateException e) {
			throw new AOKeyStoreManagerException(
				"No se han podido cargar los certificados del almacen PKCS#7 / X.509 solicitado", e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOKeyStoreManagerException(
				"No se ha podido verificar la integridad del almacen PKCS#7 / X.509 solicitado", e); //$NON-NLS-1$
		}

		return ks;

	}

}
