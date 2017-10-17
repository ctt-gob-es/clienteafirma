/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.tsp.pkcs7;

/** Extensi&oacute;n para una solicitud de TSA seg&uacute;n RFC 2161. */
public final class TsaRequestExtension {

	private final String oid;
	private final boolean critical;
	private final byte[] value;

	@Override
	public String toString() {
		return "Extension [OID: " + this.oid + ", citical: " + this.critical + ", value: " + new String(this.value) + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

	/** Crea una extensi&oacute;n para una solicitud de TSA seg&uacute;n RFC 2161.
	 * @param oid OID de la extensi&oacute;n
	 * @param isCritical <code>true</code> si la extensi&oacute;n es cr&iacute;tica, <code>false</code> en caso contrario
	 * @param value Valor de la extensi&oacute;n */
	public TsaRequestExtension(final String oid, final boolean isCritical, final byte[] value) {
		if (oid == null || oid.isEmpty()) {
			throw new IllegalArgumentException("Las extensiones TSA necesitan obligatoriamente un OID"); //$NON-NLS-1$
		}
		if (value == null || value.length < 1) {
			throw new IllegalArgumentException("Las extensiones TSA necesitan obligatoriamente un valor"); //$NON-NLS-1$
		}
		this.oid = oid;
		this.critical = isCritical;
		this.value = value.clone();
	}

	boolean isCritical() {
		return this.critical;
	}

	String getOid() {
		return this.oid;
	}

	byte[] getValue() {
		return this.value.clone();
	}

	/** Constructor de uso restringido a la serializaci&oacute;n JAXB. */
	@SuppressWarnings("unused")
	private TsaRequestExtension() {
		this.oid = null;
		this.critical = false;
		this.value = null;
	}
}
