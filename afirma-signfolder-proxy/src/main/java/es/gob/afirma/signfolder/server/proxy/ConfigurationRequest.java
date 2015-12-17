package es.gob.afirma.signfolder.server.proxy;

/**
 * Solicitud de obtenci&oacute;n de la configuracion de la Portafirmas.
 */
public class ConfigurationRequest {

	private final byte[] certEncoded;

	/**
	 * Crea una solicitud de la configuraci&oacute;n del Portafirmas.
	 * @param certEncoded Certificado con el que se realiza la autenticaci&oacute;n.
	 */
	public ConfigurationRequest(final byte[] certEncoded) {
		this.certEncoded = certEncoded;
	}

	/**
	 * Obtiene el certificado con el que autenticarse en el sistema.
	 * @return Codificaci&oacute;n del certificado en base 64.
	 */
	public byte[] getCertEncoded() {
		return this.certEncoded;
	}
}
