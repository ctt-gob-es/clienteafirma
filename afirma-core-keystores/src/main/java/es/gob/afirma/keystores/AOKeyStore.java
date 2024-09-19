/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Almacenes de claves y certificados soportados. */
public enum AOKeyStore {

    /** Windows / Internet Explorer (CAPI, certificados de usuario). */
    WINDOWS(
		"Windows", //$NON-NLS-1$
		0,
		"Windows-MY", //$NON-NLS-1$
		new CachePasswordCallback("winmydummy".toCharArray()), //$NON-NLS-1$
		NullPasswordCallback.getInstance()
	),
    /** Apple Mac OS X / Safari Keychain. */
    APPLE(
		"Llavero de Mac", //$NON-NLS-1$
		1,
		"KeychainStore", //$NON-NLS-1$
		new CachePasswordCallback("osxdummy".toCharArray()), //$NON-NLS-1$
		new CachePasswordCallback("osxdummy".toCharArray()) //$NON-NLS-1$
	),
	/** NSS compartido (de sistema). */
	SHARED_NSS(
		"NSS", //$NON-NLS-1$
		2,
		"PKCS11", //$NON-NLS-1$
		NullPasswordCallback.getInstance(),
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.13")) //$NON-NLS-1$
	),
    /** PKCS#12. */
    PKCS12(
		"PKCS#12 / PFX", //$NON-NLS-1$
		3,
		"PKCS12", //$NON-NLS-1$
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.0")), //$NON-NLS-1$
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.1")) //$NON-NLS-1$
	),
    /** Java KeyStore (JKS). */
    JAVA(
		"Java KeyStore / JKS", //$NON-NLS-1$
		5,
		"JKS", //$NON-NLS-1$
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.2")), //$NON-NLS-1$
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.3")) //$NON-NLS-1$
	),
    /** PKCS#11. */
    PKCS11(
		"PKCS#11", //$NON-NLS-1$
		6,
		"PKCS11", //$NON-NLS-1$
		NullPasswordCallback.getInstance(),
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.4")) //$NON-NLS-1$
	),
    /** PKCS#7 / X.509 en Base64. */
    SINGLE(
		"PKCS#7 / X.509", //$NON-NLS-1$
		7,
		"PKCS7", //$NON-NLS-1$
		NullPasswordCallback.getInstance(),
		NullPasswordCallback.getInstance()
	),
    /** Mozilla / Firefox (NSS / PKCS#11, con m&oacute;dulos de seguridad
     * internos y externos unificados). */
    MOZ_UNI(
		"Mozilla / Firefox (unificado)", //$NON-NLS-1$
		8,
		"PKCS11", //$NON-NLS-1$
		NullPasswordCallback.getInstance(),
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.5")) //$NON-NLS-1$
	),
    /** Java Cryptography Extension KeyStore (JCEKS). */
    JCEKS(
		"Java Cryptography Extension KeyStore (JCEKS)", //$NON-NLS-1$
		9,
		"JCEKS", //$NON-NLS-1$
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.6")), //$NON-NLS-1$
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.7")) //$NON-NLS-1$
	),
    /** Java KeyStore (JKS, distinguiendo entre may&uacute;sculas y
     * min&uacute;sculas). */
    JAVACE(
		"Java KeyStore / JKS (Case Exact)", //$NON-NLS-1$
		10,
		"CaseExactJKS", //$NON-NLS-1$
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.8")), //$NON-NLS-1$
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.9")) //$NON-NLS-1$
	),
	/** Tarjeta del Ministerio de Defensa. */
	TEMD(
		"TEMD (Tarjeta del Ministerio de Defensa)", //$NON-NLS-1$
		11,
		"PKCS11", //$NON-NLS-1$
		null, // Sin contrasena para los certificados sueltos
		new UIPasswordCallback(KeyStoreMessages.getString("AOKeyStore.14")) //$NON-NLS-1$
	),
    /** Windows / Internet Explorer (CAPI, certificados de otras personas /
     * libreta de direcciones). */
    WINADDRESSBOOK(
		"Windows / Internet Explorer (otras personas / libreta de direcciones)", //$NON-NLS-1$
		12,
		"Windows-ADDRESSBOOK", //$NON-NLS-1$
		new CachePasswordCallback("winotherdummy".toCharArray()), //$NON-NLS-1$
		NullPasswordCallback.getInstance()
	),
    /** Windows / Internet Explorer (CAPI, certificados CA intermedias). */
    WINCA(
		"Windows / Internet Explorer (CA intermedias)", //$NON-NLS-1$
		13,
		"Windows-ROOT", //$NON-NLS-1$
		new CachePasswordCallback("wincadummy".toCharArray()), //$NON-NLS-1$
		NullPasswordCallback.getInstance()
	),
	/** Tarjeta CERES con controlador nativo Java. */
	CERES(
		"Tarjeta FNMT-RCM CERES", //$NON-NLS-1$
		14,
		"CERES", //$NON-NLS-1$
		null, // El PIN se pide al firmar
		null  // Sin password para el almacen en si
	),
    /** DNIe con controlador nativo Java. */
    DNIEJAVA(
		"DNIe y tarjetas FNMT-TIF", //$NON-NLS-1$
		15,
		"DNI", //$NON-NLS-1$
		null, // El PIN se pide al firmar
		null  // Sin password para el almacen en si
	),
    /** Conjunto de tarjetas conocidas, accedidas v&iacute;a PKCS#11. */
    KNOWN_SMARTCARDS(
    	"Tarjetas inteligentes conocidas mediante PKCS#11", //$NON-NLS-1$
    	16,
    	"SMARTCARDS", //$NON-NLS-1$
    	null,
    	null
	),
    /** G&amp;D SmartCafe con Applet PKCS#15. */
    SMARTCAFE(
    	"G&D SmartCafe con Applet PKCS#15", //$NON-NLS-1$
    	17,
    	"GDSCPKCS15", //$NON-NLS-1$
    	null,
    	null
	),
    /** Tarjeta CERES 430 con controlador nativo Java. */
	CERES_430(
		"Tarjeta FNMT-RCM CERES 4.30 o superior", //$NON-NLS-1$
		18,
		"CERES430", //$NON-NLS-1$
		null, // El PIN se pide al firmar
		null  // Sin password para el almacen en si
	),
	/** Otro tipo de almac&eacute;n (deber&aacute;n inicializarse manualmente sus par&aacute;metros operativos). */
	OTHER;


    private String name;
    private final int ordinal;
    private String providerName = null;
    private PasswordCallback certificatePasswordCallback = null;
    private PasswordCallback storePasswordCallback = null;

    AOKeyStore(final String n,
    		           final int o,
    		           final String pn,
    		           final PasswordCallback certPwc,
    		           final PasswordCallback storePwc) {
        this.name = n;
        this.ordinal = o;
        this.providerName = pn;
        this.certificatePasswordCallback = certPwc;
        this.storePasswordCallback = storePwc;
    }

    AOKeyStore() {
    	this.name = "Tipo desconocido"; //$NON-NLS-1$
    	this.ordinal = 18;
    	this.providerName = null;
    	this.certificatePasswordCallback = null;
    	this.storePasswordCallback = null;
    }

    @Override
    public String toString() {
        return getName();
    }

    /** Obtiene el nombre del proveedor de seguridad que da acceso al almac&eacute;n de
     * claves y certificados.
     * @return Nombre del proveedor. */
    public String getProviderName() {
        return this.providerName;
    }

    /** Obtiene el n&uacute;mero interno del n del almac&eacute;n de claves.
     * @return N&uacute;mero interno del almac&eacute;n de claves. */
    public int getOrdinal() {
        return this.ordinal;
    }

    /** Obtiene el Nombre del almac&eacute;n de claves.
     * @return Nombre del almac&eacute;n de claves. */
    public String getName() {
        return this.name;
    }

    /** Asigna el Nombre del almac&eacute;n de claves.
     * @param name Nombre del almac&eacute;n de claves. */
    public void setName(final String name) {
        this.name = name;
    }

	/** Obtiene el <i>PasswordCallback</i> necesario para usar una clave del almac&eacute;n despu&eacute;s
	 * de haberlo abierto.
	 * @param parent Componente padre para la modalidad.
	 * @return <i>PasswordCallback</i> necesario para usar una cave del almac&eacute;n. */
	public PasswordCallback getCertificatePasswordCallback(final Object parent) {
		if (this.certificatePasswordCallback instanceof UIPasswordCallback) {
			((UIPasswordCallback)this.certificatePasswordCallback).setParent(parent);
		}
		return this.certificatePasswordCallback;
	}

	/** Obtiene el <i>PasswordCallback</i> necesario para abrir el almac&eacute;n.
	 * @param parent Componente padre para la modalidad.
	 * @return <i>PasswordCallback</i> necesario para abrir el almac&eacute;n. */
	public PasswordCallback getStorePasswordCallback(final Object parent) {
		return getStorePasswordCallback(parent, null);
	}

	/** Obtiene el <i>PasswordCallback</i> necesario para abrir el almac&eacute;n.
	 * @param parent Componente padre para la modalidad.
	 * @param prompt Texto del di&aacute;logo para solicitar la contrase&ntilde;a.
	 * @return <i>PasswordCallback</i> necesario para abrir el almac&eacute;n. */
	public PasswordCallback getStorePasswordCallback(final Object parent, final String prompt) {
		if (this.storePasswordCallback instanceof UIPasswordCallback) {
			((UIPasswordCallback)this.storePasswordCallback).setParent(parent);
			((UIPasswordCallback)this.storePasswordCallback).setPrompt(prompt);
		}
		return this.storePasswordCallback;
	}

	/**
	 * Obtiene el almac&eacute;n que corresponda al SO indicado por par&aacute;metro.
	 * @param os Sistema operativo
	 * @return Almac&eacute;n correspondiente al sistema operativo.
	 */
    public static AOKeyStore getDefaultKeyStoreTypeByOs(final OS os) {
    	AOKeyStore aoks = null;
		if (Platform.OS.WINDOWS.equals(os)) {
			aoks = AOKeyStore.WINDOWS;
		}
		if (Platform.OS.MACOSX.equals(os)) {
			aoks = AOKeyStore.APPLE;
		}
		if (Platform.OS.LINUX.equals(os)) {
			aoks = AOKeyStore.SHARED_NSS;
		}
		if (Platform.OS.SOLARIS.equals(os)) {
			aoks = AOKeyStore.MOZ_UNI;
		}
		return aoks;
    }

}
