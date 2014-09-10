/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.pkcs12;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;

import javax.crypto.BadPaddingException;
import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves tipo PKCS#12 / PFX.
 * Contempla la posibilidad de que el almac&eacute;n y las claves tengan distintas contrase&ntilde;as
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class Pkcs12KeyStoreManager extends AOKeyStoreManager {

	private PasswordCallback cachePasswordCallback;

	/** {@inheritDoc} */
	@Override
	public void init(final AOKeyStore type,
                     final InputStream store,
                     final PasswordCallback pssCallBack,
                     final Object[] params,
                     final boolean forceReset) throws AOKeyStoreManagerException,
                                                      IOException {
		setKeyStoreType(AOKeyStore.PKCS12);
		setKeyStore(init(store, pssCallBack));
	}

	/** Inicializa un almac&eacute;n de claves y certificados de tipo PKCS#12 / PFX.
	 * @param store Flujo de datos hacia el propio almac&eacute;n (fichero)
	 * @param pssCallBack <i>PasswordCallback</i> para la obtenci&oacute;n de la contrase&ntilde;a del almac&eacute;n
	 *                    y los propios certificados (las contrase&ntilde;as pueden ser distintas)
	 * @return Lista con un &uacute;nico <code>KeyStore</code> correspondiente al PKCS#12
	 * @throws AOKeyStoreManagerException Si hay errores en el tratamiento del almac&eacute;n
	 * @throws IOException Si hay errores en la lectura del almac&eacute;n */
	private KeyStore init(final InputStream store,
                          final PasswordCallback pssCallBack) throws AOKeyStoreManagerException,
                                                                     IOException {
        // Suponemos que el proveedor SunJSSE esta instalado. Hay que tener
        // cuidado con esto si alguna vez se usa JSS, que a veces lo retira

        if (store == null) {
            throw new IllegalArgumentException("Es necesario proporcionar el fichero PKCS12 / PFX"); //$NON-NLS-1$
        }

        final KeyStore ks;
        try {
        	ks = KeyStore.getInstance(getType().getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen PKCS#12 / PFX", e); //$NON-NLS-1$
        }

        this.cachePasswordCallback = pssCallBack != null ? new CachePasswordCallback(pssCallBack.getPassword()) : NullPasswordCallback.getInstance();
        try {
            ks.load(store, this.cachePasswordCallback.getPassword());
        }
        catch (final IOException e) {
            if (e.getCause() instanceof UnrecoverableKeyException ||
                e.getCause() instanceof BadPaddingException ||
                e.getCause() instanceof ArithmeticException) { // Caso probable de contrasena nula
                	throw new IOException("Contrasena invalida: " + e, e); //$NON-NLS-1$
            }
            throw e;
        }
        catch (final CertificateException e) {
            throw new AOKeyStoreManagerException("No se han podido cargar los certificados del almacen PKCS#12 / PFX solicitado.", e); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
            throw new AOKeyStoreManagerException("No se ha podido verificar la integridad del almacen PKCS#12 / PFX solicitado.", e); //$NON-NLS-1$
		}
        try {
            store.close();
        }
        catch (final Exception e) {
         // Ignoramos errores en el cierre
        }
        return ks;

	}

    /** {@inheritDoc} */
    @Override
	public AOKeyStore getType() {
        return AOKeyStore.PKCS12;
    }

    /** Obtiene la clave privada de un certificado en un almac&eacute;n PKCS#12.
     * @param alias Alias del certificado
     * @param pssCallback <i>CallBback</i> para obtener la contrase&ntilde;a del certificado que contiene la clave,
     *                    se utilizar&aacute; en caso de que sea distinta a la del propio lmac&eacute;n
     * @return Clave privada del certificado correspondiente al alias
     * @throws KeyStoreException Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
     * @throws NoSuchAlgorithmException Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave.
     * @throws UnrecoverableEntryException Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando el usuario cancela el proceso antes de que finalice */
    @Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias,
    		                                    final PasswordCallback pssCallback) throws KeyStoreException,
    		                                                                               NoSuchAlgorithmException,
    		                                                                               UnrecoverableEntryException {
        if (lacksKeyStores()) {
            throw new IllegalStateException("Se han pedido claves a un almacen no inicializado"); //$NON-NLS-1$
        }

        // Primero probamos si la contrasena de la clave es la misma que la del almacen
        try {
        	return super.getKeyEntry(
    			alias, this.cachePasswordCallback
			);
        }
        catch(final Exception e) {
        	LOGGER.warning("La contrasena del certificado no coincide con la del almacen"); //$NON-NLS-1$
        }

        // Luego probamos con null
        try {
        	return super.getKeyEntry(
    			alias, NullPasswordCallback.getInstance()
			);
        }
        catch(final Exception e) {
        	LOGGER.warning("La contrasena del certificado no es nula"); //$NON-NLS-1$
        }

        // Finalmente pedimos la contrasena
        return super.getKeyEntry(
    		alias, pssCallback
		);
    }

}
