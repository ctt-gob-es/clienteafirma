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
import java.lang.reflect.Method;
import java.security.KeyStore;
import java.security.KeyStore.LoadStoreParameter;
import java.security.KeyStore.ProtectionParameter;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.cert.CertificateException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.CallbackHandler;

import es.gob.afirma.keystores.jmulticard.ui.DnieCacheCallbackHandler;
import es.gob.afirma.keystores.jmulticard.ui.PasswordCallbackManager;
import es.gob.afirma.keystores.jmulticard.ui.SmartcardCacheCallbackHandler;
import es.gob.afirma.keystores.jmulticard.ui.SmartcardCallbackHandler;


final class AOKeyStoreManagerHelperFullJava {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private AOKeyStoreManagerHelperFullJava() {
		// No permitimos la instanciacion
	}

	/** Inicializa el almac&eacute;n 100% Java para tarjeta CERES.
	 * @param parentComponent Componente padre para la modalidad del di&aacute;logo de solicitud
	 *                        de PIN.
	 * @return <code>KeyStore</code> inicializado.
	 * @throws AOKeyStoreManagerException Si no se puede inicializar el almac&eacute;n.
	 * @throws IOException Si hay problemas en la lectura de datos. */
	static KeyStore initCeresJava(final Object parentComponent) throws AOKeyStoreManagerException,
                                                                       IOException {
		return init(
			AOKeyStore.CERES,
			buildLoadStoreParameter(new SmartcardCacheCallbackHandler()),
			new es.gob.jmulticard.jse.provider.ceres.CeresProvider(),
			parentComponent
		);
	}


	/** Inicializa el almac&eacute;n 100% Java para tarjeta G&amp;D SmartCafe.
	 * @param parentComponent Componente padre para la modalidad del di&aacute;logo de solicitud
	 *                        de PIN.
	 * @return <code>KeyStore</code> inicializado.
	 * @throws AOKeyStoreManagerException Si no se puede inicializar el almac&eacute;n.
	 * @throws IOException Si hay problemas en la lectura de datos. */
	static KeyStore initSmartCafeJava(final Object parentComponent) throws AOKeyStoreManagerException,
                                                                       IOException {
		return init(
			AOKeyStore.SMARTCAFE,
			buildLoadStoreParameter(new SmartcardCallbackHandler()),
			new es.gob.jmulticard.jse.provider.gide.SmartCafeProvider(),
			parentComponent
		);
	}

	/** Inicializa el almac&eacute;n 100% Java para DNIe.
	 * @param parentComponent Componente padre para la modalidad del di&aacute;logo de solicitud
	 *                        de PIN.
	 * @return <code>KeyStore</code> inicializado.
	 * @throws AOKeyStoreManagerException Si no se puede inicializar el almac&eacute;n.
	 * @throws IOException Si hay problemas en la lectura de datos. */
    static KeyStore initDnieJava(final Object parentComponent) throws AOKeyStoreManagerException,
    		                                                          IOException {
    	return init(
			AOKeyStore.DNIEJAVA,
			buildLoadStoreParameter(new DnieCacheCallbackHandler()),
			new es.gob.jmulticard.jse.provider.DnieProvider(),
			parentComponent
		);
    }

	/** Inicializa el almac&eacute;n 100% Java para DNIe.
	 * @param parentComponent Componente padre para la modalidad del di&aacute;logo de solicitud
	 *                        de PIN.
	 * @return <code>KeyStore</code> inicializado.
	 * @throws AOKeyStoreManagerException Si no se puede inicializar el almac&eacute;n.
	 * @throws IOException Si hay problemas en la lectura de datos. */
    static KeyStore initJMulticard(final Object parentComponent) throws AOKeyStoreManagerException,
    		                                                          IOException {

    	Provider provider;
		try {
			final Method method = es.gob.jmulticard.jse.provider.DnieProvider.class.getMethod("configure", String.class); //$NON-NLS-1$
    		final String config = "name=JMulticard\nallowAnotherCards=true"; //$NON-NLS-1$
    		provider = (Provider) method.invoke(new es.gob.jmulticard.jse.provider.DnieProvider(), config);
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido configurar el proveedor de DNIe con la configuracion indicada", e); //$NON-NLS-1$
			provider = new es.gob.jmulticard.jse.provider.DnieProvider();
		}

    	return init(
			AOKeyStore.CERES_430,
			buildLoadStoreParameter(new SmartcardCacheCallbackHandler()),
			provider,
			parentComponent
		);
    }

    private static KeyStore init(final AOKeyStore store,
    							 final LoadStoreParameter loadStoreParameter,
    							 final Provider provider,
    		                     final Object parentComponent) throws AOKeyStoreManagerException,
    		                                                            IOException {

    	if (Security.getProvider(provider.getName()) == null) {
    		Security.addProvider(provider);
    	}

    	PasswordCallbackManager.setDialogOwner(parentComponent);

        // Inicializamos
    	final KeyStore ks;
        try {
            ks = KeyStore.getInstance(store.getProviderName());
        }
        catch (final Exception e) {
            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen 100% Java para " + store.toString() + ": " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        LOGGER.info("Cargando KeyStore 100% Java para " + store.toString()); //$NON-NLS-1$
		try {
			ks.load(loadStoreParameter);
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOKeyStoreManagerException(
    			"Error de algoritmo al obtener el almacen 100% Java para " + store.toString() + ": " + e, e  //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		catch (final CertificateException e) {
			throw new AOKeyStoreManagerException(
				"Error de certificado al obtener el almacen 100% Java para " + store.toString() + ": " + e, e  //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

        return ks;
    }

    /** Construye un {@code LoadStoreParameter} en el que se hace uso de un {@code CallbackHandler} para controlar
     * el paso de mensajes  con el almac&eacute;n.
     * @param callbackHandler Manejador de mensajes para el almac&eacute;n.
     * @return Controlador seguro para el paso de mensajes. */
    private static LoadStoreParameter buildLoadStoreParameter(final CallbackHandler callbackHandler) {
		return new LoadStoreParameter() {
			@Override
			public ProtectionParameter getProtectionParameter() {
				return new KeyStore.CallbackHandlerProtection(callbackHandler);
			}
		};
    }
}
