/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.FileInputStream;
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

import es.gob.afirma.keystores.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;

abstract class FileKeyStoreManager extends AOKeyStoreManager {

	/** Contiene una copia de la contrase&ntilde;a que se uso para inicializar el almac&eacute;n. */
	private PasswordCallback cachePasswordCallback;

	private String keystoreFile = null;

	/** Inicializa un almac&eacute;n de claves y certificados de tipo fichero.
	 * @param store Flujo de datos hacia el propio almac&eacute;n (fichero)
	 * @param pssCallBack <i>PasswordCallback</i> para la obtenci&oacute;n de la contrase&ntilde;a del almac&eacute;n
	 *                    y los propios certificados (las contrase&ntilde;as pueden ser distintas)
	 * @return Almac&eacute;n de tipo fichero software.
	 * @throws AOKeyStoreManagerException Si hay errores en el tratamiento del almac&eacute;n
	 * @throws IOException Si hay errores en la lectura del almac&eacute;n */
	protected KeyStore init(final InputStream store,
                            final PasswordCallback pssCallBack) throws AOKeyStoreManagerException,
                                                                     IOException {
        // Suponemos que el proveedor SunJSSE esta instalado. Hay que tener
        // cuidado con esto si alguna vez se usa JSS, que a veces lo retira

        if (store == null) {
            throw new IllegalArgumentException("Es necesario proporcionar el fichero de almacen"); //$NON-NLS-1$
        }

        final KeyStore ks;
        try {
        	ks = KeyStore.getInstance(getType().getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen de tipo " + getType(), e); //$NON-NLS-1$
        }

        this.cachePasswordCallback = pssCallBack != null ? new CachePasswordCallback(pssCallBack.getPassword()) : NullPasswordCallback.getInstance();
        try {
            ks.load(store, this.cachePasswordCallback.getPassword());
        }
        catch (final IOException e) {
            if (e.getCause() instanceof UnrecoverableKeyException ||
                e.getCause() instanceof BadPaddingException ||
                e.getCause() instanceof ArithmeticException) { // Caso probable de contrasena nula
                	throw new IOException("La contrasena del almacen PKCS#12 / PFX no es valida", e); //$NON-NLS-1$
            }
            throw new IOException("El almacen PKCS#12 / PFX no es valido o no pudo cargarse", e); //$NON-NLS-1$
        }
        catch (final CertificateException e) {
            throw new AOKeyStoreManagerException(
        		"No se han podido cargar los certificados del almacen PKCS#12 / PFX solicitado", e //$NON-NLS-1$
    		);
        }
        catch (final NoSuchAlgorithmException e) {
            throw new AOKeyStoreManagerException(
        		"No se ha podido verificar la integridad del almacen PKCS#12 / PFX solicitado", e //$NON-NLS-1$
    		);
		}
        try {
            store.close();
        }
        catch (final Exception e) {
        	LOGGER.warning("Error cerrando el almacen: " + e); //$NON-NLS-1$
        }
        return ks;

	}

    /** Obtiene la clave privada de un certificado en un almac&eacute;n de tipo fichero.
     * @param alias Alias del certificado
     * @return Clave privada del certificado correspondiente al alias
     * @throws KeyStoreException Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
     * @throws NoSuchAlgorithmException Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave.
     * @throws UnrecoverableEntryException Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando el usuario cancela el proceso antes de que finalice */
    @Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias) throws KeyStoreException,
    		                                                               NoSuchAlgorithmException,
    		                                                               UnrecoverableEntryException {
        if (lacksKeyStores()) {
            throw new IllegalStateException("Se han pedido claves a un almacen no inicializado"); //$NON-NLS-1$
        }

        // Hacemos una copia del EntryPasswordCallback para usarla si fallan los primeros intentos
        final PasswordCallback originalPwc = getEntryPasswordCallBack();

        // Primero probamos si la contrasena de la clave es la misma que la del almacen
        setEntryPasswordCallBack(this.cachePasswordCallback);
        try {
        	return super.getKeyEntry(
    			alias
			);
        }
        catch(final Exception e) {
        	LOGGER.warning("La contrasena del certificado no coincide con la del almacen: " + e); //$NON-NLS-1$
        }

        // Luego probamos con null
        setEntryPasswordCallBack(NullPasswordCallback.getInstance());
        try {
        	return super.getKeyEntry(
    			alias
			);
        }
        catch(final Exception e) {
        	LOGGER.warning("La contrasena del certificado no es nula: " + e); //$NON-NLS-1$
        }

        // Finalmente pedimos la contrasena
        setEntryPasswordCallBack(originalPwc);
        return super.getKeyEntry(
    		alias
		);
    }


	protected void setKeyStoreFile(final String pkcs12File) {
		this.keystoreFile = pkcs12File;
	}

	protected String getKeyStoreFile() {
		return this.keystoreFile;
	}

	@Override
	public void refresh() throws IOException {
		// Para recargar el almacen, volvemos a cargar el fichero indicado.
		// El metodo de inicializacion ya se encarga de cerrar el fichero.
		try (
			final InputStream fis = new FileInputStream(this.keystoreFile);
		) {
			setKeyStore(init(fis, this.cachePasswordCallback));
		}
		catch (final AOKeyStoreManagerException e) {
			throw new IOException("Error al recargar el almacen PKCS#12", e); //$NON-NLS-1$
		}
	}
}
