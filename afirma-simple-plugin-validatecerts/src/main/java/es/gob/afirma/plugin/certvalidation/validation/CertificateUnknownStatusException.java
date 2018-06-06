package es.gob.afirma.plugin.certvalidation.validation;

import java.security.cert.CertificateException;

/** Certificado con estado de validez desconocido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateUnknownStatusException extends CertificateException {

	/** Crea una excepci&oacute;n de certificado con estado de validez desconocido.
	 * @param msg Descripci&oacute;n de la excepci&oacute;n. */
	public CertificateUnknownStatusException(final String msg) {
		super(msg);
	}

	private static final long serialVersionUID = 3008070241539014457L;

}
