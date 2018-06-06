package es.gob.afirma.plugin.certvalidation.validation;

import java.security.cert.CertificateException;

/** Certificado con estado de validez desconocido por no poderse descargar la CRL.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateCannotDownloadCrlException extends CertificateException {

	/** Crea una excepci&oacute;n de certificado con estado de validez desconocido por no poderse descargar la CRL.
	 * @param msg Descripci&oacute;n de la excepci&oacute;n. */
	public CertificateCannotDownloadCrlException(final String msg) {
		super(msg);
	}

	private static final long serialVersionUID = 3008070241539014457L;

}
