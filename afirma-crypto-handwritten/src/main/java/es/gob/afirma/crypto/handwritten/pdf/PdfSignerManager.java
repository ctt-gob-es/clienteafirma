package es.gob.afirma.crypto.handwritten.pdf;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.security.NoSuchAlgorithmException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.crypto.handwritten.Rectangle;
import es.gob.afirma.crypto.handwritten.SignaturePadInfoBean;
import es.gob.afirma.crypto.handwritten.SignerInfoBean;
import es.gob.afirma.signers.pades.PdfPreProcessor;
import es.gob.afirma.signers.pades.PdfTimestamper;

/** gestor de firmas manuscritas digitalizadas en documentos PDF. */
public class PdfSignerManager {

	/** Logger para la impresi&oacute;n de trazas. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Inserta todos los datos de una firma biom&eacute;trica en un PDF.
	 * @param pdfDoc Documento PDF.
	 * @param bioMetadata Informaci&oacute;n biom&eacute;trica.
	 * @param jpgImage Imagen de la r&uacute;brica de la firma.
	 * @param signer Informaci&oacute;n del firmante.
	 * @param pad Informaci&oacute;n de la tableta de captura.
	 * @param extraParams Par&aacute;metros adicionales de la inserci&oacute;n.
	 * @return PDF con todos los datos insertados.
	 * @throws IOException Si hay problemas en el tratamiento de datos o en el
	 *                     acceso al servidor de sello de tiempo.
	 * @throws AOException Cuando ocurre un error al insertar el sello de tiempo.
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella digital
	 *                                  indicado para el sello de tiempo. */
	public static byte[] addPdfInfo(final byte[] pdfDoc,
			                        final byte[] bioMetadata,
			                        final byte[] jpgImage,
			                        final SignerInfoBean signer,
			                        final SignaturePadInfoBean pad,
			                        final Properties extraParams) throws IOException, AOException, NoSuchAlgorithmException {

		// Creamos un temporal en donde se gestionaran los cambios del PDF
		final File outputPdfFile = File.createTempFile("temp", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		final FileOutputStream fos = new FileOutputStream(outputPdfFile);

		// Creamos los objetos necesarios para la lectura y gestion del PDF
		final PdfReader pdfReader = new PdfReader(pdfDoc);
		final Calendar globalCalendar = new GregorianCalendar();
		final PdfStamper pdfStamper;
		try {
			pdfStamper = new PdfStamper(pdfReader, fos, globalCalendar);
		}
		catch (final DocumentException e) {
			fos.close();
			throw new IOException("Error creando el PDFStamper: " + e, e); //$NON-NLS-1$
		}

		System.out.println(pad.toString());
		System.out.println(signer.toString());

		// Insertamos los datos biometricos
		insertBioData(pdfStamper, bioMetadata);

		// Insertamos la imagen de rubrica
		insertImage(pdfStamper, jpgImage, extraParams);

		// Cerramos el PDF
		try {
			pdfStamper.close(globalCalendar);
		}
		catch (final DocumentException e) {
			fos.close();
			throw new IOException("Error cerrando el PDFStamper: " + e, e); //$NON-NLS-1$
		}
		fos.close();
		pdfReader.close();

		// Leemos el resultado de fichero y los borramos
		final FileInputStream fis = new FileInputStream(outputPdfFile);
		final byte[] pdf = AOUtil.getDataFromInputStream(fis);
		fis.close();

		// Eliminamos los temporales
		deleteFile(outputPdfFile);

		// Insertamos el sello de tiempo
		return addTimeStamp(pdf, extraParams);
	}

	private static void insertBioData(final PdfStamper pdfStamper, final byte[] bioData) throws IOException {
		PdfXmpHelper.addBioXmpDataToPdf(pdfStamper, bioData);
	}

	private static void insertImage(final PdfStamper pdfStamper, final byte[] signatureJpegImage, final Properties extraParams) throws IOException {

		final Rectangle rect = calculateRectangle(extraParams);
		final int pageNum = getPageImage(extraParams);

		PdfPreProcessor.addImage(
				signatureJpegImage,
				rect.width,
				rect.height,
				rect.x,
				rect.y,
				pageNum,
				null,
				pdfStamper);
	}

	/** Obtiene el rect&aacute;ngulo en el que estampar la r&uacute;brica de la firma en el PDF.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n de la firma.
	 * @return Rect&aacute;ngulo para la r&uacute;brica. */
	private static Rectangle calculateRectangle(final Properties extraParams) {

		if (extraParams == null) {
    		LOGGER.severe("No se ha proporcionado la configuracion de firma"); //$NON-NLS-1$
    		throw new IllegalArgumentException("No se ha proporcionado la configuracion de firma"); //$NON-NLS-1$
    	}
    	if (extraParams.getProperty("PositionOnPageLowerLeftX") == null || //$NON-NLS-1$
    		extraParams.getProperty("PositionOnPageLowerLeftY") == null || //$NON-NLS-1$
			extraParams.getProperty("PositionOnPageUpperRightX") == null || //$NON-NLS-1$
			extraParams.getProperty("PositionOnPageUpperRightY") == null //$NON-NLS-1$
		) {
    		LOGGER.severe("No se ha proporcionado la configuracion con la posicion de la rubrica para la firma"); //$NON-NLS-1$
    		throw new IllegalArgumentException("No se ha proporcionado la configuracion con la posicion de la rubrica para la firma"); //$NON-NLS-1$
    	}

    	try {
    		return new Rectangle(Integer.parseInt(extraParams.getProperty("PositionOnPageLowerLeftX")), //$NON-NLS-1$
    				Integer.parseInt(extraParams.getProperty("PositionOnPageLowerLeftY")), //$NON-NLS-1$
    				Integer.parseInt(extraParams.getProperty("PositionOnPageUpperRightX")), //$NON-NLS-1$
    				Integer.parseInt(extraParams.getProperty("PositionOnPageUpperRightY")) //$NON-NLS-1$
    				);
    	}
    	catch (final Exception e) {
    		LOGGER.severe("Se han proporcionado valores no validos como posicion de firma: " + e); //$NON-NLS-1$
    		throw new IllegalArgumentException("Se han proporcionado valores no validos como posicion de firma", e); //$NON-NLS-1$
    	}

	}

	/**
	 * Obtiene el n&uacute;mero de p&aacute;gina en el que se tiene que insertar la
	 * r&uacute;brica de un documento.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n de la firma.
	 * @return N&uacute;mero de p&aacute;gina.
	 */
	private static int getPageImage(final Properties extraParams) {
		final String imagePage = extraParams.getProperty("imagePage"); //$NON-NLS-1$
		if (imagePage == null) {
			throw new IllegalArgumentException("No se ha indicado el numero de pagina en el que insertar la imagen"); //$NON-NLS-1$
		}

		final int numPage;
		try {
			numPage = Integer.parseInt(imagePage);
		}
		catch (final NumberFormatException e) {
			throw new IllegalArgumentException("No se ha introducido un valor valido como numero de pagina", e); //$NON-NLS-1$
		}
		return numPage;
	}

	/**
	 * Agrega un sello de tiempo a un PDF.
	 * @param inPdf Documento PDF.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n para la operaci&oacute;n de firma.
	 * @return Documento PDF con el sello de tiempo o la misma entrada si no se configuro la operaci&oacute;n
	 * en los {@code extraParams}.
	 * @throws AOException Cuando ocurri&oacute; un error al insertar el sello de tiempo.
	 * @throws IOException Cuando ocurri&oacute; un error de lectura del documento o de acceso a la TSA.
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella digital indicado. */
	private static byte[] addTimeStamp(final byte[] inPdf,
			                           final Properties extraParams) throws AOException,
			                                                                IOException,
			                                                                NoSuchAlgorithmException {
		return PdfTimestamper.timestampPdf(inPdf, extraParams, new GregorianCalendar());
	}

	/**
	 * Borra un fichero.
	 * @param file Fichero a borrar.
	 */
	private static void deleteFile(final File file) {
		try {
			Files.delete(file.toPath());
		} catch (final IOException e) {
			LOGGER.warning("No se ha podido borrar el fichero: " + e); //$NON-NLS-1$
		}
	}
}
