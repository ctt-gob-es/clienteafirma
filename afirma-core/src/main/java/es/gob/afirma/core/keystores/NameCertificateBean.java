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

/** Tupla Nombre-Certificado con la que mantener la asociaci&oacute;n entre un nombre
 * identificador y su certificado asociado. */
public class NameCertificateBean {

	private final String alias;

	private final String name;

	private final X509Certificate[] certificateChain;

	/** Construye la tupla Nombre-Certificado.
	 * @param alias Alias del certificado.
	 * @param name Nombre identificador del certificado.
	 * @param certChain Cadena de certificados del firmante. */
	public NameCertificateBean(final String alias, final String name, final X509Certificate[] certChain) {
		this.alias = alias;
		this.name = name;
		this.certificateChain = certChain == null ? null : certChain.clone();
	}

	/** Devuelve el alias del certificado.
	 * @return Alias. */
	public String getAlias() {
		return this.alias;
	}

	/** Devuelve el nombre indentificador del certificado.
	 * @return Nombre. */
	public String getName() {
		return this.name;
	}

	/** Devuelve el certificado.
	 * @return Certificado. */
	public X509Certificate getCertificate() {
		return this.certificateChain != null && this.certificateChain.length > 0 ?
				this.certificateChain[0] : null;
	}

	/** Devuelve la cadena de certificaci&oacute;n del certificado.
	 * @return Cadena de certificaci&oacute;n. */
	public X509Certificate[] getCertificateChain() {
		return this.certificateChain == null ? null : this.certificateChain.clone();
	}

	@Override
	public String toString() {
		return this.name;
	}
}
