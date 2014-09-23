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
		return this.certificateChain[0];
	}

	/** Devuelve la cadena de certificaci&oacute;n del certificado.
	 * @return Cadena de certificaci&oacute;n. */
	public X509Certificate[] getCertificateChain() {
		return this.certificateChain;
	}

	@Override
	public String toString() {
		return this.name;
	}
}
