package es.gob.afirma.signers.pades;

import java.io.IOException;

import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.pdf.PdfReader;

import es.gob.afirma.core.ui.AOUIFactory;

final class PdfUtil {

	private PdfUtil() {
		// No instanciable
	}

	static PdfReader getPdfReader(final byte[] inPDF,
			                      final String ownerPassword,
			                      final String userPassword,
			                      final boolean headLess) throws BadPdfPasswordException,
			                                                     InvalidPdfException,
			                                                     IOException {
		PdfReader pdfReader;
		try {
			if (ownerPassword != null) {
				pdfReader = new PdfReader(inPDF, ownerPassword.getBytes());
			}
			else if (userPassword != null) {
				pdfReader = new PdfReader(inPDF, userPassword.getBytes());
			}
			else {
				pdfReader = new PdfReader(inPDF);
			}
		}
		catch (final BadPasswordException e) {
			// Comprobamos que el signer esta en modo interactivo, y si no lo
			// esta no pedimos contrasena por dialogo, principalmente para no interrumpir un firmado por lotes
			// desatendido
			if (headLess) {
				throw new BadPdfPasswordException(e);
			}
			// La contrasena que nos han proporcionada no es buena o no nos
			// proporcionaron ninguna
			final String ownerPwd = new String(
				AOUIFactory.getPassword(
					ownerPassword == null ? CommonPdfMessages.getString("AOPDFSigner.0") : CommonPdfMessages.getString("AOPDFSigner.1"), //$NON-NLS-1$ //$NON-NLS-2$
					null
				)
			);
			try {
				pdfReader = new PdfReader(inPDF, ownerPwd.getBytes());
			}
			catch (final BadPasswordException e2) {
				throw new BadPdfPasswordException(e2);
			}
		}
		catch (final IOException e) {
			throw new InvalidPdfException(e);
		}
		return pdfReader;

	}

}
