/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.main.common;

/** Almacenes de claves y certificados soportados. */
public enum AOKeyStore {
    /** Windows / Internet Explorer (CAPI, certificados de usuario). */
    WINDOWS("Windows / Internet Explorer", 0, "Windows-MY"),  //$NON-NLS-1$//$NON-NLS-2$
    /** Apple Mac OS X / Safari Keychain. */
    APPLE("Mac OS X / Safari", 1, "KeychainStore"),  //$NON-NLS-1$//$NON-NLS-2$
    /** PKCS#12. */
    PKCS12("PKCS#12 / PFX", 3, "PKCS12"),  //$NON-NLS-1$//$NON-NLS-2$
    /** Java KeyStore (JKS). */
    JAVA("Java KeyStore / JKS", 4, "JKS"),  //$NON-NLS-1$//$NON-NLS-2$
    /** PKCS#11. */
    PKCS11("PKCS#11", 5, "PKCS11"), //$NON-NLS-1$ //$NON-NLS-2$
    /** PKCS#7 / X.509 en Base64. */
    SINGLE("PKCS#7 / X.509", 6, "PKCS7"), //$NON-NLS-1$ //$NON-NLS-2$
    /** Mozilla / Firefox (NSS / PKCS#11, con m&oacute;dulos de seguridad
     * internos y externos unificados). */
    MOZ_UNI("Mozilla / Firefox (unificado)", 7, "PKCS11"),  //$NON-NLS-1$//$NON-NLS-2$
    /** Java Cryptography Extension KeyStore (JCEKS). */
    JCEKS("Java Cryptography Extension KeyStore (JCEKS)", 8, "JCEKS"), //$NON-NLS-1$ //$NON-NLS-2$
    /** Java KeyStore (JKS, distinguiendo entre may&uacute;sculas y
     * min&uacute;sculas). */
    JAVACE("Java KeyStore / JKS (Case Exact)", 9, "CaseExactJKS"), //$NON-NLS-1$ //$NON-NLS-2$
    /** Windows / Internet Explorer (CAPI, certificados ra&iacute;z). */
    WINROOT("Windows / Internet Explorer (raiz)", 10, "Windows-ROOT"),  //$NON-NLS-1$//$NON-NLS-2$
    /** Windows / Internet Explorer (CAPI, certificados de otras personas /
     * libreta de direcciones). */
    WINADDRESSBOOK("Windows / Internet Explorer (otras personas / libreta de direcciones)", 11, "Windows-ADDRESSBOOK"),  //$NON-NLS-1$//$NON-NLS-2$
    /** Windows / Internet Explorer (CAPI, certificados CA intermedias). */
    WINCA("Windows / Internet Explorer (CA intermedias)", 12, "Windows-CA"), //$NON-NLS-1$ //$NON-NLS-2$
    /** DNIe, en cualquier plataforma soportada mediante PKCS#11. */
    DNIE("DNI Electronico", 13, "PKCS11"), //$NON-NLS-1$ //$NON-NLS-2$
    /** DNIe con controlador nativo Java. */
    DNIEJAVA("DNIe 100% Java", 14, "Afirma-DNIE") //$NON-NLS-1$ //$NON-NLS-2$
    // /** Windows (MY) con proveedor alternativo (JRE Deploy). */
    // WINDEPLOY("Windows / Internet Explorer (despliegue)", 15, "WIExplorerMy")
    ;

    private AOKeyStore(final String d, final int o, final String n) {
        this.description = d;
        this.ordinal = o;
        this.name = n;
    }

    @Override
    public String toString() {
        return getDescription();
    }

    private String description;
    private final int ordinal;
    private final String name;

    /** Obtiene el nombre del almac&eacute;n de claves y certificados.
     * @return Nombre del almac&eacute;n de claves y certificados */
    public String getName() {
        return this.name;
    }

    /** Obtiene el n&uacute;mero interno del n del almac&eacute;n de claves.
     * @return N&uacute;mero interno del almac&eacute;n de claves */
    public int getOrdinal() {
        return this.ordinal;
    }

    /** Obtiene la descripci&oacute;n del almac&eacute;n de claves.
     * @return Descripci&oacute;n del almac&eacute;n de claves */
    public String getDescription() {
        return this.description;
    }

}
