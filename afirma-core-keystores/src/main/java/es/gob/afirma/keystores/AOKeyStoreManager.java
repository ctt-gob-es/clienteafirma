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
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

/** Clase gestora de claves y certificados. B&aacute;sicamente se encarga de
 * crear KeyStores de distintos tipos, utilizando el proveedor JCA apropiado para cada caso
 * @version 0.4 */
public class AOKeyStoreManager {

    protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Almacenes de claves. */
    private List<KeyStore> kss = new ArrayList<KeyStore>();
    protected void setKeyStores(final List<KeyStore> k) {
    	this.kss = k == null ?
			this.kss = new ArrayList<KeyStore>() :
				k;
    }
    protected boolean lacksKeyStores() {
    	return this.kss == null || this.kss.isEmpty();
    }
    protected void addKeyStore(final KeyStore ks) {
    	if (ks != null) {
    		this.kss.add(ks);
    	}
    }

    /** A&ntilde;ade almacenes al conjunto actual. */
    protected void addKeyStores(final List<KeyStore> newStores) {
    	if (newStores != null) {
    		for (final KeyStore ks : newStores) {
    			this.kss.add(ks);
    		}
    	}
    }

    /** Tipo de almac&eacute;n. */
    private AOKeyStore ksType;
    protected void setKeyStoreType(final AOKeyStore type) {
    	this.ksType = type;
    }

    /** Devuelve el tipo de almac&eacute;n de claves.
     * @return Tipo de almac&eacute;n de claves */
    public AOKeyStore getType() {
        return this.ksType;
    }

    /** Inicializa el almac&eacute;n. Se encarga tambi&eacute;n de a&ntilde;adir o
     * retirar los <i>Provider</i> necesarios para operar con dicho almac&eacute;n
     * @param type Tipo del almac&eacute;n de claves
     * @param store Flujo para la lectura directa del almac&eacute;n de claves
     *        (solo para los almacenes en disco)
     * @param pssCallBack CallBack encargado de recuperar la contrase&ntilde;a del Keystore
     * @param params Par&aacute;metros adicionales (dependen del tipo de almac&eacute;n)
     * @throws AOKeyStoreManagerException Cuando ocurre cualquier problema durante la inicializaci&oacute;n
     * @throws IOException Se ha insertado una contrase&ntilde;a incorrecta para la apertura del
     *         almac&eacute;n de certificados.
     * @throws es.gob.afirma.core.MissingLibraryException Cuando faltan bibliotecas necesarias para la inicializaci&oacute;n
     * @throws es.gob.afirma.core.InvalidOSException Cuando se pide un almac&eacute;n disponible solo en un sistema operativo
     *                            distinto al actual */
    public void init(final AOKeyStore type,
    		         final InputStream store,
    		         final PasswordCallback pssCallBack,
    		         final Object[] params) throws AOKeyStoreManagerException,
    		                                       IOException {
        if (type == null) {
            throw new IllegalArgumentException("Se ha solicitado inicializar un AOKeyStore nulo"); //$NON-NLS-1$
        }
        LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

        this.ksType = type;

        switch(this.ksType) {
        	case SINGLE:
        		this.kss =  AOKeyStoreManagerHelperSingle.initSingle(store, pssCallBack);
        		break;
        	case DNIEJAVA:
                // En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
            	this.kss = AOKeyStoreManagerHelperDnieJava.initDnieJava(
        			pssCallBack, params != null && params.length > 0 ? params[0] : null
    			);
            	break;
        	case JAVA:
        	case JAVACE:
        	case JCEKS:
        		this.kss = AOKeyStoreManagerHelperJava.initJava(store, pssCallBack, this.ksType);
        		break;
        	case WINCA:
        	case WINADDRESSBOOK:
        		this.kss = AOKeyStoreManagerHelperCapiAddressBook.initCAPIAddressBook(this.ksType);
        		break;
        	case PKCS11:
                // En el "params" debemos traer los parametros:
                // [0] -p11lib: Biblioteca PKCS#11, debe estar en el Path (Windows) o en el LD_LIBRARY_PATH (UNIX, Linux, Mac OS X)
                // [1] -desc: Descripcion del token PKCS#11 (opcional)
                // [2] -slot: Numero de lector de tarjeta (Sistema Operativo) [OPCIONAL]
                this.kss = AOKeyStoreManagerHelperPkcs11.initPKCS11(pssCallBack, params);
                break;
        	case APPLE:
        		this.kss = AOKeyStoreManagerHelperApple.initApple(store);
        		break;
            default:
            	throw new UnsupportedOperationException("Tipo de almacen no soportado: " + store); //$NON-NLS-1$
        }

    }

    /** Obtiene la clave privada de un certificado.
     * @param alias
     *        Alias del certificado
     * @param pssCallback
     *        <i>CallBback</i> para obtener la contrase&ntilde;a del
     *        certificado que contiene la clave
     * @return Clave privada del certificado correspondiente al alias
     * @throws KeyStoreException
     * 		   Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
     * @throws NoSuchAlgorithmException
     * 		   Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave.
     * @throws UnrecoverableEntryException
     * 		   Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada
     * @throws es.gob.afirma.core.AOCancelledOperationException
     * 		   Cuando el usuario cancela el proceso antes de que finalice
     */
    public KeyStore.PrivateKeyEntry getKeyEntry(final String alias,
    		                                    final PasswordCallback pssCallback) throws KeyStoreException,
    		                                                                               NoSuchAlgorithmException,
    		                                                                               UnrecoverableEntryException {
        if (this.kss == null || this.kss.isEmpty()) {
            throw new IllegalStateException("Se han pedido claves a un almacen no inicializado"); //$NON-NLS-1$
        }
        if (alias == null) {
        	throw new IllegalArgumentException("El alias no puede ser nulo"); //$NON-NLS-1$
        }
        for (final KeyStore ks : this.kss) {
        	if (ks.containsAlias(alias)) {
        		return (KeyStore.PrivateKeyEntry) ks.getEntry(alias, pssCallback != null ? new KeyStore.PasswordProtection(pssCallback.getPassword()) : null);
        	}
        }
        LOGGER.warning("El almacen no contiene ninguna clave con el alias '" + alias + "', se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
    }

    /** Obtiene un certificado del keystore activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return El certificado o {@code null} si no se pudo recuperar. */
    public X509Certificate getCertificate(final String alias) {
        if (alias == null) {
            LOGGER.warning("El alias del certificado es nulo, se devolvera null"); //$NON-NLS-1$
            return null;
        }

        if (this.kss == null || this.kss.isEmpty()) {
            LOGGER.warning(
        		"No se ha podido recuperar el certificado con alias '" + alias + "' porque el KeyStore no estaba inicializado, se devolvera null" //$NON-NLS-1$ //$NON-NLS-2$
    		);
            return null;
        }

        for (final KeyStore ks : this.kss) {
        	try {
	        	if (ks.containsAlias(alias)) {
	        		return (X509Certificate) ks.getCertificate(alias);
	        	}
        	}
        	catch(final Exception e) {
        		LOGGER.severe(
    				"Error intentando recuperar el certificado con el alias '" + alias + "', se continuara con el siguiente almacen: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
        	}
        }
        LOGGER.warning("El almacen no contiene ningun certificado con alias '" + alias + "', se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
    }

    /** Obtiene la cadena de certificaci&oacute;n de un certificado del keystore activo a partir de su alias.
     * @param alias Alias del certificado.
     * @return Certificados de la cadena de certificaci&oacute;n o {@code null} si no se pudo recuperar. */
     public X509Certificate[] getCertificateChain(final String alias) {
         if (alias == null) {
             LOGGER.warning("El alias del certificado es nulo, se devolvera una cadena vacia"); //$NON-NLS-1$
             return new X509Certificate[0];
         }

         if (this.kss == null || this.kss.isEmpty()) {
             LOGGER.warning(
         		"No se ha podido recuperar el certificado con alias '" + alias + "' porque el KeyStore no estaba inicializado, se devolvera una cadena vacia" //$NON-NLS-1$ //$NON-NLS-2$
     		 );
             return new X509Certificate[0];
         }
         for (final KeyStore ks : this.kss) {
         	try {
 	        	if (ks.containsAlias(alias)) {
 	        		return (X509Certificate[]) ks.getCertificateChain(alias);
 	        	}
         	}
         	catch(final Exception e) {
         		LOGGER.severe(
     				"Error intentando recuperar la cadena del certificado con alias '" + alias + "', se continuara con el siguiente almacen: " + e //$NON-NLS-1$ //$NON-NLS-2$
 				);
         	}
         }
         LOGGER.warning("El almacen no contiene ningun certificado con el alias '" + alias + "', se devolvera una cadena vacia"); //$NON-NLS-1$ //$NON-NLS-2$
         return new X509Certificate[0];
    }

    /** Obtiene todos los alias de los certificados del almac&eacute;n actual.
     * @return Todos los alias encontrados en el almac&eacute;n actual */
    public String[] getAliases() {
        if (this.kss == null || this.kss.isEmpty()) {
            throw new IllegalStateException("Se han pedido alias a un almacen no inicializado"); //$NON-NLS-1$
        }
    	final List<String> aliases = new ArrayList<String>();
    	Enumeration<String> partAliases;
    	for (final KeyStore ks : this.kss) {
    		try {
				partAliases = ks.aliases();
			}
    		catch (final KeyStoreException e) {
    			LOGGER.severe(
     				"Error intentando recuperar los alias del almacen '" + ks.getType() + "', se continuara con el siguiente: " + e //$NON-NLS-1$ //$NON-NLS-2$
 				);
				continue;
			}
    		while (partAliases.hasMoreElements()) {
    			aliases.add(partAliases.nextElement().toString());
    		}
    	}
    	return aliases.toArray(new String[0]);
    }

    /** Devuelve el <code>keyStore</code> en uso.
     * @return Almac&eacute;n de claves (<code>KeyStore</code>) actual */
    public List<KeyStore> getKeyStores() {
        return this.kss;
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
}
