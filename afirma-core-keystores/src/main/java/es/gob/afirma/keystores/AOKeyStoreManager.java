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
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.keystores.KeyStoreManager;

/** Clase gestora de claves y certificados. B&aacute;sicamente se encarga de
 * crear KeyStores de distintos tipos, utilizando el proveedor JCA apropiado para cada caso
 * @version 0.4 */
public class AOKeyStoreManager implements KeyStoreManager {

    protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private String[] cachedAliases = null;
    protected void resetCachedAliases() {
    	this.cachedAliases = null;
    }
    protected String[] getCachedAliases() {
    	return this.cachedAliases;
    }
    protected void setCachedAliases(final String[] ca) {
    	this.cachedAliases = ca.clone();
    }

    /** Tipo de almac&eacute;n. */
    private AOKeyStore ksType;

    /** Almacenes de claves. */
    private KeyStore ks;
    protected void setKeyStore(final KeyStore k) {
    	if (k == null) {
    		throw new IllegalArgumentException("El almacen no puede ser nulo"); //$NON-NLS-1$
    	}
    	this.ks = k;
    }
    protected KeyStore getKeyStore() {
    	return this.ks;
    }

    @Override
	public void refresh() throws IOException {
    	resetCachedAliases();
    	try {
			this.init(this.ksType, this.storeIs, this.callBack, this.storeParams, true);
		}
    	catch (final AOKeyStoreManagerException e) {
			throw new IOException("Error al refrescar el almacen: " + e, e); //$NON-NLS-1$
		}
    }

    protected boolean lacksKeyStores() {
    	return this.ks == null;
    }

    protected final void setKeyStoreType(final AOKeyStore type) {
    	this.ksType = type;
    }

    /** Devuelve el tipo de almac&eacute;n de claves.
     * @return Tipo de almac&eacute;n de claves */
    public AOKeyStore getType() {
        return this.ksType;
    }

    private InputStream storeIs;
    private PasswordCallback callBack;
    private Object[] storeParams;


    /** Inicializa el almac&eacute;n. Se encarga tambi&eacute;n de a&ntilde;adir o
     * retirar los <i>Provider</i> necesarios para operar con dicho almac&eacute;n
     * @param type Tipo del almac&eacute;n de claves
     * @param store Flujo para la lectura directa del almac&eacute;n de claves
     *        (solo para los almacenes en disco)
     * @param pssCallBack CallBack encargado de recuperar la contrase&ntilde;a del Keystore
     * @param params Par&aacute;metros adicionales (dependen del tipo de almac&eacute;n)
     * @param forceReset Fuerza un reinicio del almac&eacute;n, no se reutiliza una instancia previa
     * @throws AOKeyStoreManagerException Cuando ocurre cualquier problema durante la inicializaci&oacute;n
     * @throws IOException Se ha insertado una contrase&ntilde;a incorrecta para la apertura del
     *         almac&eacute;n de certificados.
     * @throws es.gob.afirma.core.MissingLibraryException Cuando faltan bibliotecas necesarias para la inicializaci&oacute;n
     * @throws es.gob.afirma.core.InvalidOSException Cuando se pide un almac&eacute;n disponible solo en un sistema operativo
     *                            distinto al actual */
    public void init(final AOKeyStore type,
    		         final InputStream store,
    		         final PasswordCallback pssCallBack,
    		         final Object[] params,
    		         final boolean forceReset) throws AOKeyStoreManagerException,
    		                                          IOException {
        if (type == null) {
            throw new IllegalArgumentException("Se ha solicitado inicializar un AOKeyStore nulo"); //$NON-NLS-1$
        }
        LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

        resetCachedAliases();

        // Guardamos los parametros de inicializacion por si hay que reiniciar
        this.ksType = type;
        this.storeIs = store;
        this.callBack = pssCallBack;
        if (params == null) {
        	this.storeParams = null;
        }
        else {
        	// Copia defensiva ante mutaciones
        	this.storeParams = new Object[params.length];
        	System.arraycopy(params, 0, this.storeParams, 0, params.length);
        }

        switch(this.ksType) {
        	case SINGLE:
        		this.ks =  AOKeyStoreManagerHelperSingle.initSingle(store, pssCallBack);
        		break;
        	case CERES:
        		this.ks = AOKeyStoreManagerHelperFullJava.initCeresJava(
    				params != null && params.length > 0 ? params[0] : null
				);
            	break;
        	case DNIEJAVA:
                // En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
            	this.ks = AOKeyStoreManagerHelperFullJava.initDnieJava(
        			pssCallBack,
        			params != null && params.length > 0 ? params[0] : null
    			);
            	break;
        	case JAVACE:
        	case JCEKS:
        		this.ks = AOKeyStoreManagerHelperJava.initJava(store, pssCallBack, this.ksType);
        		break;
        	case WINCA:
        	case WINADDRESSBOOK:
        		this.ks = AOKeyStoreManagerHelperCapiAddressBook.initCAPIAddressBook(this.ksType);
        		break;
        	case PKCS11:

                // En el "params" debemos traer los parametros:
                // [0] -p11lib: Biblioteca PKCS#11, debe estar en el Path (Windows) o en el LD_LIBRARY_PATH (UNIX, Linux, Mac OS X)
                // [1] -desc: Descripcion del token PKCS#11 (opcional)
                // [2] -slot: Numero de lector de tarjeta (Sistema Operativo) [OPCIONAL]

        		// Hacemos una copia por la mutabilidad
        		Object[] newParams = null;
        		if (params != null) {
        			newParams = new Object[params.length];
        			System.arraycopy(params, 0, newParams, 0, params.length);
        		}
                this.ks = AOKeyStoreManagerHelperPkcs11.initPKCS11(pssCallBack, newParams);
                break;
        	case APPLE:
        		this.ks = AOKeyStoreManagerHelperApple.initApple(store);
        		break;
            default:
            	throw new UnsupportedOperationException("Tipo de almacen no soportado: " + store); //$NON-NLS-1$
        }

    }

    @Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias,
    		                                    final PasswordCallback pssCallback) throws KeyStoreException,
    		                                                                               NoSuchAlgorithmException,
    		                                                                               UnrecoverableEntryException {
        if (this.ks == null) {
            throw new IllegalStateException("Se han pedido claves a un almacen no inicializado"); //$NON-NLS-1$
        }
        if (alias == null) {
        	throw new IllegalArgumentException("El alias no puede ser nulo"); //$NON-NLS-1$
        }
		return (KeyStore.PrivateKeyEntry) this.ks.getEntry(alias, pssCallback != null ? new KeyStore.PasswordProtection(pssCallback.getPassword()) : null);
    }

    @Override
	public X509Certificate getCertificate(final String alias) {
        if (alias == null) {
            LOGGER.warning("El alias del certificado es nulo, se devolvera null"); //$NON-NLS-1$
            return null;
        }

        if (this.ks == null) {
            LOGGER.warning(
        		"No se ha podido recuperar el certificado con alias '" + alias + "' porque el KeyStore no estaba inicializado, se devolvera null" //$NON-NLS-1$ //$NON-NLS-2$
    		);
            return null;
        }

    	try {
    		return (X509Certificate) this.ks.getCertificate(alias);
    	}
    	catch(final Exception e) {
    		LOGGER.severe(
				"Error intentando recuperar el certificado con el alias '" + alias + "', se devolvera null: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
    		return null;
    	}
    }

     @Override
	public X509Certificate[] getCertificateChain(final String alias) {
         if (alias == null) {
             LOGGER.warning("El alias del certificado es nulo, se devolvera una cadena vacia"); //$NON-NLS-1$
             return new X509Certificate[0];
         }

         if (this.ks == null) {
             LOGGER.warning(
         		"No se ha podido recuperar el certificado con alias '" + alias + "' porque el KeyStore no estaba inicializado, se devolvera una cadena vacia" //$NON-NLS-1$ //$NON-NLS-2$
     		 );
             return new X509Certificate[0];
         }

     	 try {
     	     // No hacemos directamente el Cast de Certificate[] a X509Certificate[] porque
     	     // en ciertas ocasiones encontramos errores en el proceso, especialmente en OS X
     	     final Certificate[] certs = this.ks.getCertificateChain(alias);
     	     if (certs == null) {
     	         return new X509Certificate[0];
     	     }
     	     final List<X509Certificate> ret = new ArrayList<X509Certificate>();
     	     for (final Certificate c : certs) {
     	         if (c instanceof X509Certificate) {
     	             ret.add((X509Certificate) c);
     	         }
     	     }
    		 return ret.toArray(new X509Certificate[0]);
     	 }
     	 catch(final Exception e) {
     		LOGGER.severe(
 				"Error intentando recuperar la cadena del certificado con alias '" + alias + "', se continuara con el siguiente almacen: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
     	 }
         LOGGER.warning("El almacen no contiene ningun certificado con el alias '" + alias + "', se devolvera una cadena vacia"); //$NON-NLS-1$ //$NON-NLS-2$
         return new X509Certificate[0];
    }

    @Override
	public String[] getAliases() {

    	if (this.ks == null) {
            throw new IllegalStateException("Se han pedido alias a un almacen no inicializado"); //$NON-NLS-1$
        }
        if (this.cachedAliases != null) {
        	return this.cachedAliases;
        }
		try {
			this.cachedAliases = Collections.list(this.ks.aliases()).toArray(new String[0]);
		}
		catch (final KeyStoreException e) {
			LOGGER.severe(
 				"Error intentando recuperar los alias, se devolvera una lista vacia: " + e //$NON-NLS-1$
			);
			return new String[0];
		}
		return this.cachedAliases;

    }

    @Override
    public String toString() {
        final StringBuilder ret = new StringBuilder("Gestor de almacenes de claves"); //$NON-NLS-1$
        if (this.ksType != null) {
            String tmpStr = this.ksType.getName();
            if (tmpStr != null) {
                ret.append(" de tipo "); //$NON-NLS-1$
                ret.append(tmpStr);
            }
            tmpStr = this.ksType.getName();
            if (tmpStr != null) {
                ret.append(" con nombre "); //$NON-NLS-1$
                ret.append(tmpStr);
            }
            ret.append(" de clase "); //$NON-NLS-1$
            ret.append(this.ksType.toString());
        }
        return ret.toString();
    }

	@Override
	public boolean isKeyEntry(final String alias) throws KeyStoreException {
		return getKeyStore().isKeyEntry(alias);
	}
}
