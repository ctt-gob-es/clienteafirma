package es.gob.afirma.plugin.certvalidation.validation;

import java.security.cert.CertificateException;

/** Certificado revocado.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateRevokedException extends CertificateException {

	/** Crea una excepci&oacute;n de certificado revocado.
	 * @param msg Descripci&oacute;n de la excepci&oacute;n. */
	public CertificateRevokedException(final String msg) {
		super(msg);
	}

	private static final long serialVersionUID = 3008070241539014457L;

}
