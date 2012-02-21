package es.gob.afirma.core.ui;

import java.security.cert.X509Certificate;

/**
 * Tupla Nombre-Certificado con la que mantener la asociaci&oacute;n entre un nombre
 * identificador y su certificado asociado.
 * @author A122466
 *
 */
public class NameCertificateBean {

	private final String alias;

	private final String name;

	private final X509Certificate certificate;

	/**
	 * Construye la tupla Nombre-Certificado.
	 * @param alias Alias del certificado.
	 * @param name Nombre identificador del certificado.
	 * @param cert Certificado.
	 */
	public NameCertificateBean(final String alias, final String name, final X509Certificate cert) {
		this.alias = alias;
		this.name = name;
		this.certificate = cert;
	}

	/**
	 * Devuelve el alias del certificado.
	 * @return Alias.
	 */
	public String getAlias() {
		return this.alias;
	}

	/**
	 * Devuelve el nombre indentificador del certificado.
	 * @return Nombre.
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Devuelve el certificado.
	 * @return Certificado.
	 */
	public X509Certificate getCertificate() {
		return this.certificate;
	}
}
