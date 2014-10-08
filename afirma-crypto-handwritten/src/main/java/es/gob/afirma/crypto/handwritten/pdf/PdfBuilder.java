package es.gob.afirma.crypto.handwritten.pdf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.crypto.handwritten.HandwrittenMessages;
import es.gob.afirma.crypto.handwritten.JseUtil;
import es.gob.afirma.crypto.handwritten.Rectangle;
import es.gob.afirma.crypto.handwritten.SignatureResult;
import es.gob.afirma.crypto.handwritten.SignerInfoBean;
import es.gob.afirma.crypto.handwritten.SingleBioSignData;
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
	 * @param bioSignDataList Lista de datos de la tarea de firma de cada firmante.
	 * @param xml Contiene los datos de la tarea de firma.
	 * @return PDF con la informaci&oacute; a&ntilde;adida.
	 * @throws IOException Si hay problemas en el tratamiento de datos. */
	public static byte[] buildPdf(final Map<SignerInfoBean, SignatureResult> srList,
			                      final byte[] inPdf,
			                      final List<SingleBioSignData> bioSignDataList) throws IOException {

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

		// Insertamos el pie de pagina a la rubrica
		int count = 0;
		for (final SignerInfoBean signer : keys) {

			// Obtenemos la imagen de firma
			byte[] jpg = srList.get(signer).getSignatureJpegImage();
			// Añadimos el pie de firma
			byte[] signature = JseUtil.addFooter(jpg, signer.getSignerName());
			// Datos de la tarea de firma para el firmante
			SingleBioSignData singleSing = getSingleBioSignData(bioSignDataList, signer.getId());
			// Area en la que se posiciona la firma en el pdf
			Rectangle signatureRubricPositionOnPdf = singleSing.getSignatureRubricPositionOnPdf();

			// Insertamos la imagen en el pdf
			PdfPreProcessor.addImage(
				signature,
				signatureRubricPositionOnPdf.width,
				signatureRubricPositionOnPdf.height,
				signatureRubricPositionOnPdf.x,
				signatureRubricPositionOnPdf.y,
				singleSing.getSignatureRubricPageOnPdf(),
				null,
				pdfStamper
			);
			count++;
		}


		// Insertamos la informacion biometrica como MoreInfo
		Map<String, String> moreInfo = new HashMap<String, String>(srList.size());
		count = 0;
		for (final SignerInfoBean signer : keys) {
			moreInfo.put(
				HandwrittenMessages.getString("PdfBuilder.1", Integer.toString(count)), //$NON-NLS-1$
				signer.toString()
			);
			moreInfo.put(
				HandwrittenMessages.getString("PdfBuilder.2", Integer.toString(count)), //$NON-NLS-1$
				srList.get(signer).getSignaturePadInfo().toString()
			);
			if (srList.get(signer).getSignatureData() != null) {
				moreInfo.put(
					HandwrittenMessages.getString("PdfBuilder.3", Integer.toString(count)), //$NON-NLS-1$
					Base64.encode(srList.get(signer).getSignatureData())
				);
			}
			if (srList.get(signer).getSignatureRawData() != null) {
				moreInfo.put(
					HandwrittenMessages.getString("PdfBuilder.4", Integer.toString(count)), //$NON-NLS-1$
					Base64.encode(srList.get(signer).getSignatureRawData())
				);
			}
			count++;
		}
		PdfPreProcessor.addMoreInfo(moreInfo, pdfStamper);


		// Insertamos la informacion biometrica como XMP
		count = 0;
		for (final SignerInfoBean signer : keys) {
			PdfXmpHelper.addBioXmpDataToPdf(pdfStamper, srList.get(signer).getSignatureData());
			count ++;
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
		return fos.toByteArray();
	}

	// Metodo para obtener los datos de firma para un identificador especifico.
	private static SingleBioSignData getSingleBioSignData( final List<SingleBioSignData> signDataList, final String id) {
		for(SingleBioSignData sign : signDataList) {

			if(sign.getSignerData().getId().equals(id) ) {
				return sign;
			}
		}
		return null;
	}
}


