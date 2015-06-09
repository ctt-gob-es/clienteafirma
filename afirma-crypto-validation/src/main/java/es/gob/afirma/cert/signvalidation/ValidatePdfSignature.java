package es.gob.afirma.cert.signvalidation;

import java.io.IOException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.logging.Logger;

import com.lowagie.text.pdf.AcroFields;
import com.lowagie.text.pdf.PdfPKCS7;
import com.lowagie.text.pdf.PdfReader;

import es.gob.afirma.cert.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.cert.signvalidation.SignValidity.VALIDITY_ERROR;

/** Validador de firmas PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ValidatePdfSignature {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ValidatePdfSignature() {
		// No instanciable
	}

	/** Valida una firma PDF (PKCS#7/PAdES).
     * @param sign PDF firmado.
     * @return Validez de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma. */
	public static SignValidity validate(final byte[] sign) throws IOException {
		final PdfReader reader = new PdfReader(sign);
		final AcroFields af = reader.getAcroFields();
		final List<String> sigNames = af.getSignatureNames();
		for (final String name : sigNames) {
			final PdfPKCS7 pk = af.verifySignature(name);
			try {
				if (!pk.verify()) {
					return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_MATCH_DATA);
				}
			}
			catch (final Exception e) {
				LOGGER.warning("Error validando la firma '" + name + "' del PDF: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CORRUPTED_SIGN);
			}
			final X509Certificate signCert = pk.getSigningCertificate();
			try {
				signCert.checkValidity();
			}
			catch (final CertificateExpiredException e) {
				// Certificado caducado
	            return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CERTIFICATE_EXPIRED);
			}
			catch (final CertificateNotYetValidException e) {
				// Certificado aun no valido
	            return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CERTIFICATE_NOT_VALID_YET);
			}
		}
		return new SignValidity(SIGN_DETAIL_TYPE.OK, null);
	}

}
