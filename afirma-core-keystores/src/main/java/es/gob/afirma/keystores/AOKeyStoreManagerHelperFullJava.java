/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
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
import java.util.logging.Logger;

import javax.security.auth.callback.CallbackHandler;

final class AOKeyStoreManagerHelperFullJava {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String PROVIDER_CERES = "es.gob.jmulticard.jse.provider.ceres.CeresProvider"; //$NON-NLS-1$
	private static final String PROVIDER_DNIE = "es.gob.jmulticard.jse.provider.DnieProvider"; //$NON-NLS-1$

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

    	CallbackHandler callbackHandler;
    	try {
    		callbackHandler = (CallbackHandler) Class.forName("es.gob.jmulticard.ui.passwordcallback.gui.CeresCallbackHandler").getConstructor().newInstance(); //$NON-NLS-1$
    	}
    	catch (final Exception e) {
    		throw new AOKeyStoreManagerException("No se han encontrado el gestor de contrasenas para CERES", e); //$NON-NLS-1$
    	}

		return init(
			AOKeyStore.CERES,
			buildLoadStoreParameter(callbackHandler),
			parentComponent,
			PROVIDER_CERES
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

    	CallbackHandler callbackHandler;
    	try {
    		callbackHandler = (CallbackHandler) Class.forName("es.gob.jmulticard.ui.passwordcallback.gui.DnieCallbackHandler").getConstructor().newInstance(); //$NON-NLS-1$
    	}
    	catch (final Exception e) {
    		throw new AOKeyStoreManagerException("No se han encontrado el gestor de contrasenas para DNIe", e); //$NON-NLS-1$
    	}

    	return init(
			AOKeyStore.DNIEJAVA,
			buildLoadStoreParameter(callbackHandler),
			parentComponent,
			PROVIDER_DNIE
		);
    }


    private static KeyStore init(final AOKeyStore store,
    							 final LoadStoreParameter loadStoreParameter,
    		                     final Object parentComponent,
    		                     final String providerClassName) throws AOKeyStoreManagerException,
    		                                                            IOException {
    	final Provider p;
    	if (Security.getProvider(store.getProviderName()) == null) {
    		try {
    			p = (Provider) Class.forName(providerClassName).getConstructor().newInstance();
    			Security.addProvider(p);
    		}
    		catch (final Exception e) {
    			throw new AOKeyStoreManagerException(
					"No se ha podido instanciar e instalar el proveedor 100% Java de Afirma para " + store.toString() + ": " + e, //$NON-NLS-1$ //$NON-NLS-2$
					e
				);
    		}
    	}

    	try {
    		final Class<?> managerClass = Class.forName("es.gob.jmulticard.ui.passwordcallback.PasswordCallbackManager"); //$NON-NLS-1$
    		final Method setDialogOwnerFrameMethod = managerClass.getMethod("setDialogOwner", Object.class); //$NON-NLS-1$
    		setDialogOwnerFrameMethod.invoke(null, parentComponent);
    	}
    	catch (final Exception e) {
    		LOGGER.warning("No se ha podido establecer el componente padre para los dialogos del almacen: " + e); //$NON-NLS-1$
    	}

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
		} catch (final NoSuchAlgorithmException e) {
			throw new AOKeyStoreManagerException(
	    			"Error de algoritmo al obtener el almacen 100% Java para " + store.toString() + ": " + e, e  //$NON-NLS-1$ //$NON-NLS-2$
				);
		} catch (final CertificateException e) {
			throw new AOKeyStoreManagerException(
					"Error de certificado al obtener el almacen 100% Java para " + store.toString() + ": " + e, e  //$NON-NLS-1$ //$NON-NLS-2$
				);
		}

        return ks;
    }

    /**
     * Construye un {@code LoadStoreParameter} en el que se hace uso de un {@code CallbackHandler} para controlar
     * el paso de mensajes  con el almac&eacute;n.
     * @param callbackHandler Manejador de mensajes para el almac&eacute;n.
     * @return Controlador seguro para el paso de mensajes.
     */
    private static LoadStoreParameter buildLoadStoreParameter(final CallbackHandler callbackHandler) {

		return new LoadStoreParameter() {
			@Override
			public ProtectionParameter getProtectionParameter() {
				return new KeyStore.CallbackHandlerProtection(callbackHandler);
			}
		};
    }
}
