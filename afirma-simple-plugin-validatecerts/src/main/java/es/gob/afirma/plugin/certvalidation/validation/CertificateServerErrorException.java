package es.gob.afirma.plugin.certvalidation.validation;

import java.security.cert.CertificateException;

/** Certificado con estado de validez desconocido por error interno o del
 * servidor de validaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateServerErrorException extends CertificateException {

	/** Crea una excepci&oacute;n de certificado con estado de validez desconocido por
	 * error interno o del servidor de validaci&oacute;n.
	 * @param msg Descripci&oacute;n de la excepci&oacute;n. */
	public CertificateServerErrorException(final String msg) {
		super(msg);
	}

	private static final long serialVersionUID = 3008070241539014457L;

}
