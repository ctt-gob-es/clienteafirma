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
import java.security.KeyStore.ProtectionParameter;
import java.security.KeyStoreException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.misc.AOUtil;

/** Clase gestora de claves y certificados. B&aacute;sicamente se encarga de
 * crear KeyStores de distintos tipos, utilizando el proveedor JCA apropiado para cada caso
 * @version 0.6 */
public class AOKeyStoreManager implements KeyStoreManager {

	private Object parentComponent = null;

	/** Obtiene el componente padre para la modalidad de los elementos gr&aacute;ficos.
	 * @return Componente padre para la modalidad de los elementos gr&aacute;ficos. */
	protected Object getParentComponent() {
		return this.parentComponent;
	}

	@Override
	public void setParentComponent(final Object p) {
		this.parentComponent = p;
	}

	/** <code>Logger</code> de la clase y sus hijas. */
    protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final Set<String> deactivatedCertificatesThumbprints = new HashSet<>();

    private String[] cachedAliases = null;

    /** Borra la lista de alias precargados. */
    protected void resetCachedAliases() {
    	this.cachedAliases = null;
    }

    /** Obtiene la lista de alias precargados.
     * @return Lista de alias precargados. */
    protected String[] getCachedAliases() {
    	return this.cachedAliases != null ? this.cachedAliases.clone() : null;
    }

    /** Establece la lista de alias precargados.
     * @param ca Lista de alias precargados. */
    protected void setCachedAliases(final String[] ca) {
    	this.cachedAliases = ca.clone();
    }

    private boolean preferred = false;

    /** Indica si este gestor de almacenes es el preferente.
     * @return <code>true</code> si este gestor de almacenes es el preferente,
     *         <code>false</code> en caso contrario. */
    protected boolean isPreferred() {
    	return this.preferred;
    }

    void setPreferred(final boolean p) {
    	this.preferred = p;
    }

    /** Tipo de almac&eacute;n. */
    private AOKeyStore ksType;

    /** Almacenes de claves. */
    private KeyStore ks;


    /** Establece el almac&eacute;n de claves principal.
     * @param k Almac&eacute;n de claves principal. */
    protected void setKeyStore(final KeyStore k) {
    	this.ks = k;
    }

    /** Obtiene el <code>KeyStore</code> principal asociado a este gestor de almacenes.
     * En caso de un gestor de m&uacute;ltiples almacenes, no hay garant&iacute;a de que
     * este m&eacute;todo devuelva
     * @return <code>KeyStore</code> principal asociado a este gestor de almacenes. */
    public KeyStore getKeyStore() {
    	return this.ks;
    }

    @Override
	public void refresh() throws IOException {
    	resetCachedAliases();
    	try {
			init(this.ksType, this.storeIs, this.storePasswordCallBack, this.storeParams, true);
		}
    	catch (final AOKeyStoreManagerException e) {
    		this.ks = null;
			throw new IOException("Error al refrescar el almacen, se ocultaran sus entradas " + e, e); //$NON-NLS-1$
		}
    }

    /** Indica si este gestor de almacenes no tiene ning&uacute;n almac&eacute;n a gestionar.
     * @return <code>true</code> si este gestor de almacenes no tiene ning&uacute;n almac&eacute;n a gestionar,
     *         <code>false</code> en caso contrario. */
    protected boolean lacksKeyStores() {
    	return this.ks == null;
    }

    /** Establece el tipo del almac&eacute;n principal de este gestor.
     * @param type Tipo del almac&eacute;n principal de este gestor. */
    protected final void setKeyStoreType(final AOKeyStore type) {
    	this.ksType = type;
    }

    /** Devuelve el tipo de almac&eacute;n de claves.
     * @return Tipo de almac&eacute;n de claves */
    public AOKeyStore getType() {
        return this.ksType;
    }

    /** Devuelve el tipo de almac&eacute;n de claves para un alias determinado.
     * @param alias Alias de la entrada para la cual se desea conocer su tipo de almac&eacute;n.
     * @return Tipo de almac&eacute;n de claves para el alias indicado. */
    protected AOKeyStore getType(final String alias) {
    	return getType();
    }

    private InputStream storeIs;
    private PasswordCallback storePasswordCallBack;

    private PasswordCallback entryPasswordCallBack = null;

    @Override
    public void setEntryPasswordCallBack(final PasswordCallback pwc) {
    	this.entryPasswordCallBack = pwc;
    }

    /** Obtiene el <code>PasswordCallBack</code> para las entradas del almac&eacute;n.
     * @return <code>PasswordCallBack</code> para las entradas del almac&eacute;n. */
    protected PasswordCallback getEntryPasswordCallBack() {
    	return this.entryPasswordCallBack;
    }

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
     *                            distinto al actual
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando se cancela algun di&aacute;logo de PIN. */
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
        this.storePasswordCallBack = pssCallBack;
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
        	case SMARTCAFE:
                // En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
        		setParentComponent(params != null && params.length > 0 ? params[0] : null);
        		this.ks = AOKeyStoreManagerHelperFullJava.initSmartCafeJava(
    				getParentComponent()
				);
            	break;
        	case CERES:
                // En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
        		setParentComponent(params != null && params.length > 0 ? params[0] : null);
        		this.ks = AOKeyStoreManagerHelperFullJava.initCeresJava(
    				getParentComponent()
				);
            	break;
        	case DNIEJAVA:
                // En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
        		setParentComponent(params != null && params.length > 0 ? params[0] : null);
            	this.ks = AOKeyStoreManagerHelperFullJava.initDnieJava(
        			getParentComponent()
    			);
            	break;
        	case CERES_430:
        		// En el "params" debemos traer los parametros:
                // [0] -parent: Componente padre para la modalidad
        		setParentComponent(params != null && params.length > 0 ? params[0] : null);
            	this.ks = AOKeyStoreManagerHelperFullJava.initJMulticard(
        			getParentComponent()
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
                this.ks = AOKeyStoreManagerHelperPkcs11.initPKCS11(pssCallBack, newParams, forceReset,
                		getParentComponent());
                break;
            default:
            	throw new UnsupportedOperationException("Tipo de almacen no soportado: " + this.ksType); //$NON-NLS-1$
        }

    }

    @Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias) throws KeyStoreException,
    		                                                               NoSuchAlgorithmException,
    		                                                               UnrecoverableEntryException {
        if (this.ks == null) {
            throw new IllegalStateException("Se han pedido claves a un almacen no inicializado"); //$NON-NLS-1$
        }
        if (alias == null) {
        	throw new IllegalArgumentException("El alias no puede ser nulo"); //$NON-NLS-1$
        }

        final ProtectionParameter protParam;
        if (this.entryPasswordCallBack != null) {
        	protParam = new KeyStore.PasswordProtection(this.entryPasswordCallBack.getPassword());
        }
        else {
        	final PasswordCallback pwc = getType(alias).getCertificatePasswordCallback(getParentComponent());
        	if (pwc != null) {
        		protParam = new KeyStore.PasswordProtection(pwc.getPassword());
        	}
        	else {
        		protParam = null;
        	}
        }

		return (KeyStore.PrivateKeyEntry) this.ks.getEntry(
			alias,
			protParam
		);
    }

    @Override
	public X509Certificate getCertificate(final String alias) {
        if (alias == null) {
            LOGGER.warning("El alias del certificado es nulo, se devolvera null"); //$NON-NLS-1$
            return null;
        }

        if (this.ks == null) {
            LOGGER.warning(
        		"No se ha podido recuperar el certificado con el alias especificado porque el KeyStore no estaba inicializado, se devolvera null" //$NON-NLS-1$
    		);
            return null;
        }

    	try {
    		return (X509Certificate) this.ks.getCertificate(alias);
    	}
    	catch(final es.gob.jmulticard.CancelledOperationException e) {
    		throw new AOCancelledOperationException("Se cancelo uso de la tarjeta a traves del driver Java: " + e, e); //$NON-NLS-1$
    	}
    	catch(final Exception e) {
    		LOGGER.severe(
				"Error intentando recuperar el certificado, se devolvera null: " + e //$NON-NLS-1$
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
         		"No se ha podido recuperar el certificado con el alias especificado porque el KeyStore no estaba inicializado, se devolvera una cadena vacia" //$NON-NLS-1$
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
     	     final List<X509Certificate> ret = new ArrayList<>();
     	     for (final Certificate c : certs) {
     	         if (c instanceof X509Certificate) {
     	             ret.add((X509Certificate) c);
     	         }
     	     }
    		 return ret.toArray(new X509Certificate[0]);
     	 }
     	 catch(final Exception e) {
     		LOGGER.severe(
 				"Error intentando recuperar la cadena del certificado, se continuara con el siguiente almacen: " + e //$NON-NLS-1$
			);
     	 }
         LOGGER.warning("El almacen no contiene ningun certificado con el alias especificado, se devolvera una cadena vacia"); //$NON-NLS-1$
         return new X509Certificate[0];
    }

    @Override
	public String[] getAliases() {

    	if (this.ks == null) {
            throw new IllegalStateException("Se han pedido alias a un almacen no inicializado"); //$NON-NLS-1$
        }
        if (this.cachedAliases != null) {
        	return this.cachedAliases.clone();
        }
		try {
			this.cachedAliases = cleanDeactivatedAliases(
				Collections.list(this.ks.aliases()).toArray(new String[0])
			);
		}
		catch (final KeyStoreException e) {
			LOGGER.severe(
 				"Error intentando recuperar los alias, se devolvera una lista vacia: " + e //$NON-NLS-1$
			);
			return new String[0];
		}
		return this.cachedAliases.clone();

    }

    @Override
    public String toString() {
        final StringBuilder ret = new StringBuilder("Gestor de almacenes de claves "); //$NON-NLS-1$
        ret.append(this.ksType);
        if (this.ksType != null && this.ksType.getName() != null) {
            ret.append(" con nombre "); //$NON-NLS-1$
            ret.append(this.ksType.getName());
        }
        return ret.toString();
    }

	@Override
	public boolean isKeyEntry(final String alias) throws KeyStoreException {
		return getKeyStore().isKeyEntry(alias);
	}

	/** Limpia la lista de alias deshabilitados.
	 * @param currentAliases Lista actual de alias, sin los deshabilitados.
	 * @return Lista de alias con todas las entradas habilitadas. */
	protected String[] cleanDeactivatedAliases(final String[] currentAliases) {

		if (this.deactivatedCertificatesThumbprints.isEmpty()) {
			return currentAliases;
		}
		final List<String> cleanedAliases = new ArrayList<>();
		final MessageDigest md;
		try {
			md = MessageDigest.getInstance("SHA1"); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			LOGGER.warning(
				"No se ha podido instanciar el generador de huellas digitales SHA1, pueden aparecer duplicados en la lista de certificados: " + e //$NON-NLS-1$
			);
			return currentAliases;
		}
		for (final String alias : currentAliases) {
			final String currentThumbprint;
			try {
				currentThumbprint = AOUtil.hexify(
					md.digest(getCertificate(alias).getEncoded()),
					false
				);
			}
			catch (final CertificateEncodingException e) {
				LOGGER.severe(
					"No se ha podido obtener la huella del certificado actual, pueden aparecer duplicados en la lista de certificados: " + e //$NON-NLS-1$
				);
				continue;
			}
			if (!this.deactivatedCertificatesThumbprints.contains(currentThumbprint)) {
				cleanedAliases.add(alias);
			}
		}
		return cleanedAliases.toArray(new String[cleanedAliases.size()]);
	}

	@Override
	public void deactivateEntry(final String certificateThumbprint) {
		if (certificateThumbprint != null) {
			this.deactivatedCertificatesThumbprints.add(certificateThumbprint);
		}
		resetCachedAliases();
	}
}
