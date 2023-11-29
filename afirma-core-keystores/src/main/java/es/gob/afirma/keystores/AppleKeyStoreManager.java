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
import java.math.BigInteger;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.InvalidOSException;
import es.gob.afirma.core.misc.Platform;

/** Gestor de claves del llavero de Apple OS X.
 * OS X necesita su propio gestor por la peculiaridades en la recuperaci&oacute;n de claves privadas
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class AppleKeyStoreManager extends AOKeyStoreManager {

	AppleKeyStoreManager() {
		setKeyStoreType(AOKeyStore.APPLE);
	}

	@Override
	public void init(final AOKeyStore type,
	         final InputStream store,
	         final PasswordCallback pssCallBack,
	         final Object[] params,
	         final boolean forceReset) throws AOKeyStoreManagerException,
	                                          IOException {
		setKeyStore(AppleKeyStoreManager.initApple(store));

		getAliasesWithoutDuplicates();
	}

	@Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias) throws KeyStoreException {
		if (getKeyStore() == null) {
			throw new IllegalStateException(
				"Se han pedido claves a un almacen no inicializado" //$NON-NLS-1$
			);
		}
		if (alias == null) {
			throw new IllegalArgumentException("El alias no puede ser nulo"); //$NON-NLS-1$
		}
		if (getKeyStore().containsAlias(alias)) {
            PrivateKey key = null;
            try {
                LOGGER.info("Llavero de Mac OS X, se tratan directamente las claves privadas"); //$NON-NLS-1$
                key = (PrivateKey) getKeyStore().getKey(alias, "dummy".toCharArray()); //$NON-NLS-1$
            }
            catch (final Exception e) {
            	LOGGER.warning("Error recuperando directamente la clave privada en Mac OS X: " + e); //$NON-NLS-1$
            }
            if (key == null) {
            	throw new UnsupportedOperationException("La entrada no tiene clave privada"); //$NON-NLS-1$
            }
            return new KeyStore.PrivateKeyEntry(key, getCertificateChain(alias));
		}
		LOGGER.warning("El almacen no contiene ninguna clave con el alias especificado, se devolvera null"); //$NON-NLS-1$
		return null;
	}

	private static KeyStore initApple(final InputStream store) throws AOKeyStoreManagerException, IOException {
		if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
			throw new InvalidOSException("Apple Mac OS X"); //$NON-NLS-1$
		}

		final KeyStore tmpks;

		// Inicializamos
		try {
			tmpks = KeyStore.getInstance(AOKeyStore.APPLE.getProviderName());
		}
		catch (final Exception e) {
			throw new AOKeyStoreManagerException(
				"No se ha podido obtener el almacen Apple.KeychainStore", e); //$NON-NLS-1$
		}

		try {
			tmpks.load(store, null);
		}
		catch (final CertificateException e) {
			throw new AOKeyStoreManagerException(
				"No se han podido cargar los certificados del almacen Apple.KeychainStore", e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOKeyStoreManagerException(
				"No se ha podido verificar la integridad del almacen Apple.KeychainStore", e); //$NON-NLS-1$
		}
		return tmpks;
	}

	private void getAliasesWithoutDuplicates() throws AOKeyStoreManagerException {

		// Obtenemos los objetos del llavero
		final Enumeration<String> aliases;
		try {
			aliases = getKeyStore().aliases();
		}
		catch (final KeyStoreException e) {
			throw new AOKeyStoreManagerException("Error obteniendo los alias: " + e, e); //$NON-NLS-1$
		}

		// Identificamos aquellos que son certificados con clave privada
		final List<String> tmpAliasesWithPrivateKey = new ArrayList<>();
		while (aliases.hasMoreElements()) {
			final String alias = aliases.nextElement();
			try {
				if (getKeyStore().isKeyEntry(alias)) {
					tmpAliasesWithPrivateKey.add(alias);
				}
			}
			catch (final KeyStoreException e) {
            	LOGGER.info(
        			"Se ignora uno de los certificados por no poderse operar con su clave privada: " + e //$NON-NLS-1$
    			);
			}
		}

		// Filtramos para admitir solo un certificado por cada numero de serie
		final List<String> tmpAliases = new ArrayList<>();
		final Set<BigInteger> addedSerials = new HashSet<>();
		for (final String al : tmpAliasesWithPrivateKey) {
			final X509Certificate tmpCert = getCertificate(al);
			if (tmpCert != null) {
				if (!addedSerials.contains(tmpCert.getSerialNumber())) {
					addedSerials.add(tmpCert.getSerialNumber());
					tmpAliases.add(al);
				}
				else {
					LOGGER.info(
							String.format("Se retira el certificado con numero de serie=%1s por estar duplicado", //$NON-NLS-1$
									tmpCert.getSerialNumber()));
				}
			}
			else {
				LOGGER.warning(String.format("No se pudo cargar el certificado con alias '%1s'", al)); //$NON-NLS-1$
			}
		}
		setCachedAliases(tmpAliases.toArray(new String[tmpAliases.size()]));
	}

}
