package es.gob.afirma.core.ui;

import java.security.cert.X509Certificate;

/**
 * Tupla Nombre-Certificado con la que mantener la asociaci&oacute;n entre un nombre
 * identificador y su certificado asociado. 
 * @author A122466
 *
 */
public class NameCertificateBean {

	private String name;
	
	private X509Certificate certificate;
	
	/**
	 * Construye la tupla Nombre-Certificado.
	 * @param name Nombre identificador del certificado.
	 * @param cert Certificado.
	 */
	public NameCertificateBean(final String name, final X509Certificate cert) {
		this.name = name;
		this.certificate = cert;
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
