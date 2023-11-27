/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */
package es.gob.afirma.core.keystores;

import java.security.cert.X509Certificate;

/** Identificador de uso de certificados correspondiente a la subestructura
 * X.509 ASN.1:
 * <pre>
 *  KeyUsage ::= BIT STRING {
 *   digitalSignature        (0),
 *   nonRepudiation          (1),
 *   keyEncipherment         (2),
 *   dataEncipherment        (3),
 *   keyAgreement            (4),
 *   keyCertSign             (5),
 *   cRLSign                 (6),
 *   encipherOnly            (7),
 *   decipherOnly            (8)
 *  }
 * </pre>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class KeyUsage {

	private static final int KEYUSAGE_NBITS = 9;

	private final Boolean[] usage;

	/** Uso para firma electr&oacute;nica. */
	private static final KeyUsage EMPTY_USAGES = new KeyUsage(
		new Boolean[] {
            null, // digitalSignature
            null, // nonRepudiation
            null, // keyEncipherment
            null, // dataEncipherment
            null, // keyAgreement
            null, // keyCertSign
            null, // cRLSign
            null, // encipherOnly
            null  // decipherOnly
		}
	);

	/** Uso para firma electr&oacute;nica. */
	public static final KeyUsage SIGN = new KeyUsage(
		new Boolean[] {
            null, // digitalSignature
            Boolean.TRUE, // nonRepudiation
            null, // keyEncipherment
            null, // dataEncipherment
            null, // keyAgreement
            null, // keyCertSign
            null, // cRLSign
            null, // encipherOnly
            null  // decipherOnly
		}
	);

	/** Uso para autenticaci&oacute;n. */
	public static final KeyUsage AUTH = new KeyUsage(
		new Boolean[] {
			Boolean.TRUE, // digitalSignature
            null, // nonRepudiation
            null, // keyEncipherment
            null, // dataEncipherment
            null, // keyAgreement
            null, // keyCertSign
            null, // cRLSign
            null, // encipherOnly
            null  // decipherOnly
		}
	);

	/** Uso para cifrado. */
	public static final KeyUsage CYPH = new KeyUsage(
		new Boolean[] {
			null, // digitalSignature
            null, // nonRepudiation
            null, // keyEncipherment
            Boolean.TRUE, // dataEncipherment
            null, // keyAgreement
            null, // keyCertSign
            null, // cRLSign
            null, // encipherOnly
            null  // decipherOnly
		}
	);

	@Override
	public boolean equals(final Object e) {
		if (e == null || !(e instanceof KeyUsage)) {
			return false;
		}
		final KeyUsage ku = (KeyUsage) e;
		Boolean[] other = ku.getUsage();
		if (other == null) {
			other = EMPTY_USAGES.getUsage();
		}
		Boolean[] me = getUsage();
		if (me == null) {
			me = EMPTY_USAGES.getUsage();
		}

		// Comprobamos que ambos sean iguales
		if (other != null && me != null && other.length != me.length) {
			return false;
		}
		if (me !=null && other != null) {
			for (int i = 0; i < me.length; i++) {
				if (other[i] != me[i]) {
					return false;
				}
			}
		}
		return true;
	}

	/** Indica si el objeto actual permite tambi&eacute;n los usos del objeto proporcionado.
	 * @param ku Uso de certificado a comprobar si tambi&eacute;n est&aacute;n soportados por esta instancia.
	 * @return <code>true</code> si el objeto actual permite los usos del objeto proporcionado,
	 *         <code>false</code> en caso contrario. */
	public boolean includes(final KeyUsage ku) {
		if (ku == null) {
			return false;
		}
		final Boolean[] other = ku.getUsage();
		final Boolean[] me = getUsage();
		if (other == null || me == null || other.length != me.length) {
			return false;
		}
		for (int i=0;i<me.length;i++) {
			if (other[i] != null && other[i] != me[i]) {
				return false;
			}
		}
		return true;
	}

	@Override
	public int hashCode() {
		int ret = 0;
		if (this.usage != null) {
			for (final Boolean b : this.usage) {
				ret = ret + b.hashCode();
			}
		}
		return ret;
	}

    /** Construye una identificador de uso de certificados.
     * @param keyUsage M&aacute;scara de bits para filtro por <i>KeyUsage</i><br>
     *        Cada certificado puede permitir simult&aacute;neamente cualquiera de
     *        estos 8 usos:<br>
     *        <ol>
     *        <li><b>digitalSignature</b></li>
     *        <li><b>nonRepudiation</b></li>
     *        <li><b>keyEncipherment</b></li>
     *        <li><b>dataEncipherment</b></li>
     *        <li><b>keyAgreement</b></li>
     *        <li><b>keyCertSign</b></li>
     *        <li><b>cRLSign</b></li>
     *        <li><b>encipherOnly</b></li>
     *        <li><b>decipherOnly</b></li>
     *        </ol>
     *        Cada uno de los elementos del array designan en orden a uno de estos 8
     *        usos. El valor de cada elemento puede ser:
     *        <ul>
     *        <li>{@code null}: No se comprueba el valor de este uso.</li>
     *        <li>{@code false}: El certificado no debe tener permitido ese uso.</li>
     *        <li>{@code true}: El certificado debe tener permitido ese uso.</li>
     *        </ul> */
	public KeyUsage(final Boolean[] keyUsage) {
		if (keyUsage == null || keyUsage.length != KEYUSAGE_NBITS) {
			throw new IllegalArgumentException(
				"El uso debe proporcionarse como un Boolean[] no nulo de exactamente 9 posiciones" //$NON-NLS-1$
			);
		}
		this.usage = keyUsage.clone();
	}

	/** Construye una identificador de uso de certificados a partir de un certificado X.509.
	 * @param cert Certificado de origen. */
	public KeyUsage(final X509Certificate cert) {
		if (cert == null) {
			throw new IllegalArgumentException(
				"El certificado de origen no puede ser nulo" //$NON-NLS-1$
			);
		}
		final boolean[] ke = cert.getKeyUsage();
		if (ke == null) {
			this.usage = null;
			return;
		}
		if (ke.length != KEYUSAGE_NBITS) {
			throw new IllegalArgumentException(
					"El certificado de origen tiene un KeyUsage con un numero de posiciones no soportado: " + ke.length //$NON-NLS-1$
					);
		}
		this.usage = new Boolean[KEYUSAGE_NBITS];
		for (int i=0; i<KEYUSAGE_NBITS; i++) {
			this.usage[i] = Boolean.valueOf(ke[i]);
		}
	}

	Boolean[] getUsage() {
		return this.usage == null ? null : this.usage.clone();
	}

	@Override
	public String toString() {
		if (this.usage == null) {
			return "Desconocido"; //$NON-NLS-1$
		}
		if (includes(SIGN) && includes(AUTH) && includes(CYPH)) {
			return "Firma, autenticaci\u00F3n y cifrado"; //$NON-NLS-1$
		}
		if (includes(AUTH) && includes(CYPH)) {
			return "Autenticaci\u00F3n y cifrado"; //$NON-NLS-1$
		}
		if (includes(SIGN) && includes(AUTH) && includes(CYPH)) {
			return "Firma y cifrado"; //$NON-NLS-1$
		}
		if (includes(SIGN) && includes(AUTH)) {
			return "Firma y autenticaci\u00F3n"; //$NON-NLS-1$
		}
		if (includes(AUTH)) {
			return "Autenticaci\u00F3n"; //$NON-NLS-1$
		}
		if (includes(SIGN)) {
			return "Firma"; //$NON-NLS-1$
		}
		if (includes(CYPH)) {
			return "Cifrado"; //$NON-NLS-1$
		}
		return "Otros"; //$NON-NLS-1$
	}

}
