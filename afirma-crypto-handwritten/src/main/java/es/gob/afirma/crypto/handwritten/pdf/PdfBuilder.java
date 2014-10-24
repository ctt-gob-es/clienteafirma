package es.gob.afirma.crypto.handwritten.pdf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.crypto.handwritten.BioDataStructure;
import es.gob.afirma.crypto.handwritten.HandwrittenMessages;
import es.gob.afirma.crypto.handwritten.JseUtil;
import es.gob.afirma.crypto.handwritten.Rectangle;
import es.gob.afirma.crypto.handwritten.SignatureResult;
import es.gob.afirma.crypto.handwritten.SignerInfoBean;
import es.gob.afirma.crypto.handwritten.SimpleCryptoHelper;
import es.gob.afirma.crypto.handwritten.SingleBioSignData;
import es.gob.afirma.signers.pades.PdfPreProcessor;

/** Genera el PDF final con toda la informaci&oacute;n de las firmas.
 * @author Astrid Idoate */
public final class PdfBuilder {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String DIGEST_ALGORITHM = "SHA-512"; //$NON-NLS-1$

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
	 * @param cert Certificado X.509 cuya clave se usar&aacute; para cifrar la informaci&oacute;n biom&eacute;trica.
	 * @return PDF con la informaci&oacute; a&ntilde;adida.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 * @throws NoSuchAlgorithmException Si no se encuentra alg&uacute;n algoritmo necesario
	 *                                  para la huella o para el cifrado. */
	public static byte[] buildPdf(final Map<SignerInfoBean, SignatureResult> srList,
			                      final byte[] inPdf,
			                      final List<SingleBioSignData> bioSignDataList,
			                      final X509Certificate cert) throws IOException, NoSuchAlgorithmException {

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

		final Set<SignerInfoBean> keys = srList.keySet();

		// Insertamos el pie de pagina a la rubrica
		int count = 0;
		for (final SignerInfoBean signer : keys) {

			// Obtenemos la imagen de firma
			final byte[] jpg = srList.get(signer).getSignatureJpegImage();

			// Anadimos el pie de firma
			final byte[] signature = JseUtil.addFooter(jpg, new SimpleDateFormat("dd/mm/yyyy hh:mm").format(new Date())); //$NON-NLS-1$

			// Datos de la tarea de firma para el firmante
			final SingleBioSignData singleSing = getSingleBioSignData(bioSignDataList, signer.getId());

			// Area en la que se posiciona la firma en el pdf
			final Rectangle signatureRubricPositionOnPdf = singleSing.getSignatureRubricPositionOnPdf();

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

		// *******************************************************************************
		// ***** Generamos los sobres digitales con los BioDataStructure cifrados ********

		// Calculamos la huella del PDF de entrada
		final byte[] inPdfDigest = SimpleCryptoHelper.messageDigest(inPdf, DIGEST_ALGORITHM);

		// Creamos una lista para ir amacenando las estructuras creadas
		final Map<SignerInfoBean, byte[]> cypheredBioStructures = new ConcurrentHashMap<SignerInfoBean, byte[]>(keys.size());

		// Iteramos las firmas creando las estructuras
		for (final SignerInfoBean signer : keys) {

			// Creamos la estructura
			final BioDataStructure bds = new BioDataStructure(
				srList.get(signer).getSignatureData(),
				srList.get(signer).getSignatureRawData(),
				inPdfDigest,
				DIGEST_ALGORITHM
			);

			if (cert != null) {
				// La ciframos
				final byte[] cipheredBds;
				try {
					cipheredBds = SimpleCryptoHelper.cipherData(bds.getEncoded(), cert);
				}
				catch (final Exception e) {
					fos.close();
					throw new IOException("Error cifrando los datos biometricos de '" + signer + "': " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
				}

				// Y la metemos en la lista
				cypheredBioStructures.put(signer, cipheredBds);
			}
			else {
				cypheredBioStructures.put(signer, bds.getEncoded());
			}

		}


		// Insertamos la informacion biometrica como MoreInfo
		final Map<String, String> moreInfo = new HashMap<String, String>(srList.size());
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
					Base64.encode(cypheredBioStructures.get(signer))
				);
			}
			count++;
		}
		PdfPreProcessor.addMoreInfo(moreInfo, pdfStamper);

		// Insertamos la informacion biometrica como XMP
		count = 0;
		final List<XmpSignStructure> xmpList = new ArrayList<XmpSignStructure>();
		for (final SignerInfoBean signer : keys) {

			xmpList.add(
				new XmpSignStructure (
					signer,
					cypheredBioStructures.get(signer),
					cert != null ? cert.getSubjectX500Principal().toString() : "" //$NON-NLS-1$
				)
			);
			count ++;
		}

		PdfXmpHelper.addBioXmpDataToPdf(pdfStamper, PdfXmpHelper.buildXmp(xmpList));

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
		for (final SingleBioSignData sign : signDataList) {
			if(sign.getSignerData().getId().equals(id) ) {
				return sign;
			}
		}
		return null;
	}
}


