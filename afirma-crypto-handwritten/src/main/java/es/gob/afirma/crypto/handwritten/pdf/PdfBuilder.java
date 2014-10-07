package es.gob.afirma.crypto.handwritten.pdf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.crypto.handwritten.SignatureResult;
import es.gob.afirma.crypto.handwritten.SignerInfoBean;
import es.gob.afirma.signers.pades.PdfPreProcessor;

/**
 * @author Astrid Idoate */
public final class PdfBuilder {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private PdfBuilder() {
		// No instanciable
	}

	/** En un PDF de entrada, y por cada una de las firmas proporcionadas a&ntilde;ade:
	 * <ul>
	 *  <li>La imagen de la r&uacute;brica, poniendo antes un pie.</li>
	 *  <li>La informaci&oacute;n biom&eacute;trica en el diccionario <i>MoreInfo</i></li>
	 *  <li>La informaci&oacute;n biom&eacute;trica como XMP</li>
	 * </ul>
	 * @param srList Mapa de firmantes y resultados de sus firmas.
	 * @param inPdf PDF de entrada.
	 * @return PDF con la informaci&oacute; a&ntilde;adida.
	 * @throws IOException Si hay problemas en el tratamiento de datos. */
	public static byte[] buildPdf(final Map<SignerInfoBean, SignatureResult> srList,
			                      final byte[] inPdf) throws IOException {

		LOGGER.info("Numero de firmas: "  + srList.size()); //$NON-NLS-1$

		// Va a contener el PDF modificado
		final ByteArrayOutputStream fos = new ByteArrayOutputStream();

		final PdfReader reader = new PdfReader(inPdf);
		final Calendar globalCalendar = new GregorianCalendar();
		final PdfStamper pdfStamper;
		try {
			pdfStamper = new PdfStamper(reader, fos, globalCalendar);
		}
		catch (final DocumentException e) {
			throw new IOException("Error creando el PDFStamper: " + e, e); //$NON-NLS-1$
		}

		Set<SignerInfoBean> keys = srList.keySet();
		for (final SignerInfoBean signer : keys) {
			// Insertamos el pie de pagina a la rubrica

			// Insertamos la imagen

		}

		// Insertamos la informacion biometrica como MoreInfo
		Map<String, String> moreInfo = new HashMap<String, String>(srList.size());
		int count = 0;
		for (final SignerInfoBean signer : keys) {
			moreInfo.put(
				"Firma " + count + " - Firmante",
				signer.toString()
			);
			moreInfo.put(
				"Firma " + count + " - Tableta de captura",
				srList.get(signer).getSignaturePadInfo().toString()
			);
			if (srList.get(signer).getSignatureData() != null) {
				moreInfo.put(
					"Firma " + count + " - Datos biométricos ISO 19794-7",
					Base64.encode(srList.get(signer).getSignatureData())
				);
			}
			if (srList.get(signer).getSignatureRawData() != null) {
				moreInfo.put(
					"Firma " + count + " - Datos biométricos nativos",
					Base64.encode(srList.get(signer).getSignatureRawData())
				);
			}
			count++;
		}
		PdfPreProcessor.addMoreInfo(moreInfo, pdfStamper);

		for (final SignerInfoBean signer : keys) {
			// Insertamos la informacion biometrica como XMP

		}

		// Cerramos el PDF
		try {
			pdfStamper.close(globalCalendar);
		}
		catch (final DocumentException e) {
			throw new IOException("Error cerrando el PDFStamper: " + e, e); //$NON-NLS-1$
		}
		reader.close();

		// Devolvemos el PDF con toda la informacion

//
//
//		// Ponemos pie de firma a las firmas
//		for(int i = 0; i < srList.size(); i ++) {
//
//			final SignatureResult sr = signatures.get(i);
//
//			final byte[] jpg = sr.getSignatureJpegImage();
//
//			try {
//
//				final byte[] signFooter = JseUtil.addFooter(jpg, "nombre");
//
//			} catch (final IOException e) {
//
//				LOGGER.warning("No se ha podido añadir el pie de firma al usuario: " + e); //$NON-NLS-1$
//
//			}
//		}
//		// Insertamos las firmas en el pdf
//
//		// Añadimos MoreInfo
//
//		// Añadimos XMP
//
//		// Firmamos el fichero con el Cliente

		return fos.toByteArray();
	}

	private void addFooterToSign() {

	}
}
