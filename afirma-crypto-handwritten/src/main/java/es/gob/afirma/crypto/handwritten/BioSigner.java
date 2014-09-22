package es.gob.afirma.crypto.handwritten;

import java.awt.Component;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPublicKey;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.crypto.handwritten.pdf.PdfSignerManager;
import es.gob.afirma.crypto.handwritten.pdf.PdfXmpHelper;
import es.gob.afirma.crypto.handwritten.wacom.WacomSignatureWindow;

/** Firmador de documentos PDF con firma manuscrita digitalizada biom&eacute;trica.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class BioSigner implements SignaturePadListener {

	/** Logger para la impresi&oacute;n de trazas. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Algoritmo de huella digital interno. */
	private static final String DIGEST_ALGO = "SHA-512"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro a utilizar para la subida de datos. */
	private static final String DEFAULT_URL_PARAM_TO_UPLOAD_DATA = "data"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro para el HTTP POST de guardado de documento. */
	private static final String DATA_URL_PARAM_NAME = "postParamData"; //$NON-NLS-1$

	private Object parentComponent = null;
	private byte[] pdfDoc = null;
	private URL targetUrl = null;
	private Rectangle signatureAreaOnPad = null;
	private X509Certificate pK = null;
	private SignerInfoBean signerInfo = null;
	private Properties extraParams = null;

	private WacomSignatureWindow signatureWindow = null;

	/** Firma biom&eacute;tricamente un documento PDF.
	 * Se sigue el siguiente proceso:
	 * <ol>
	 *  <li>Se descarga el documento a firmar de <i>retrieveUrl</i>.</li>
	 *  <li>Se obtiene la huella digital del documento a firmar.</li>
	 *  <li>Se obtiene la firma biom&eacute;trica (muestras e imagen).</li>
	 *  <li>
	 *    Se cifran las muestras de la firma biom&eacute;trica junto a la huella
	 *    digital del documento a firmar mediante la clave p&uacute;blica <i>pK</i>.
	 *  </li>
	 *  <li>
	 *    Se inserta el resultado del cifrado en el PDF como XMP junto a los
	 *    metadatos del firmante.
	 *  </li>
	 *  <li>Se inserta la imagen de la firma (r&uacute;brica) en el PDF.</li>
	 *  <li>Se inserta un sello de tiempo en el PDF.</li>
	 *  <li>Se devuelve el PDF mediante <i>storeUrl</i>.</li>
	 * </ol>
	 * @param parent Componente padre sobre el que mostrar los elementos visuales.
	 * @param retrieveUrl URL para la recuperac&oacute;n (GET HTTP) del PDF a firmar.
	 * @param storeUrl URL para el almac&eacute;n del documento una vez firmado (HTTP POST,
	 *                 en un par&aacute;metro que se debe llamar <i>data</i>).
	 * @param template  Plantilla en formato HTML a mostrar en la tableta de firma.
	 * @param signatureArea Area en la que el usuario pueden firmar dentro de la pantalla
	 *                      de la tableta de firma.
	 * @param cypherCert Certificado de donde obtener la clave publica para el cifrado de los
	 *                   datos.
	 * @param signerData Informaci&oacute;n del firmante.
	 * @param params Par&aacute;metros adicionales de configuraci&oacute;n de firma.
	 * @throws IOException Cuando ocurre un error en la descarga de los datos o la codificacion
	 * de la plantilla a mostrar en la tableta de firma.
	 * @throws SignaturePadException Cuando no se ha podido inicializar la tableta de firma. */
	public void sign(final Object parent,
					 final URL retrieveUrl,
					 final URL storeUrl,
					 final String template,
					 final Rectangle signatureArea,
					 final X509Certificate cypherCert,
					 final SignerInfoBean signerData,
					 final Properties params) throws IOException, SignaturePadException {

		this.parentComponent = parent;
		this.targetUrl = storeUrl;
		this.signatureAreaOnPad = signatureArea;
		this.pK = cypherCert;
		this.signerInfo = signerData;
		this.extraParams = params;

		LOGGER.info("Se inicia la descarga del documento"); //$NON-NLS-1$

		this.pdfDoc = downloadDocument(retrieveUrl);

		LOGGER.info("Se ha terminado la descarga del documento. Bytes: " + this.pdfDoc.length); //$NON-NLS-1$

		this.signatureWindow = new WacomSignatureWindow(
			parent,
			template,
			this.signatureAreaOnPad,
			signerData
		);
		this.signatureWindow.addSignatureListener(this);
		this.signatureWindow.captureSign();
	}

	/** Firma biom&eacute;tricamente un documento PDF.
	 * Se sigue el siguiente proceso:
	 * <ol>
	 *  <li>Se descarga el documento a firmar de <i>retrieveUrl</i>.</li>
	 *  <li>Se obtiene la huella digital del documento a firmar.</li>
	 *  <li>Se obtiene la firma biom&eacute;trica (muestras e imagen).</li>
	 *  <li>
	 *    Se cifran las muestras de la firma biom&eacute;trica junto a la huella
	 *    digital del documento a firmar mediante la clave p&uacute;blica <i>pK</i>.
	 *  </li>
	 *  <li>
	 *    Se inserta el resultado del cifrado en el PDF como XMP junto a los
	 *    metadatos del firmante.
	 *  </li>
	 *  <li>Se inserta la imagen de la firma (r&uacute;brica) en el PDF.</li>
	 *  <li>Se inserta un sello de tiempo en el PDF.</li>
	 *  <li>Se devuelve el PDF mediante <i>storeUrl</i>.</li>
	 * </ol>
	 * @param parent Componente padre sobre el que mostrar los elementos visuales.
	 * @param retrieveUrl URL para la recuperac&oacute;n (GET HTTP) del PDF a firmar.
	 * @param storeUrl URL para el almac&eacute;n del documento una vez firmado (HTTP POST,
	 *                 en un par&aacute;metro que se debe llamar <i>data</i>).
	 * @param jpegImage Imagen a mostrar en la tableta de firma.
	 * @param signatureArea Area en la que el usuario pueden firmar dentro de la pantalla
	 *                      de la tableta de firma.
	 * @param cypherCert Certificado de donde obtener la clave publica para el cifrado de los
	 *                   datos.
	 * @param signatureArea Area en la que el usuario pueden firmar dentro de la pantalla
	 *                      de la tableta de firma.
	 * @param signerData Informaci&oacute;n del firmante.
	 * @param params Par&aacute;metros adicionales de configuraci&oacute;n de firma.
	 * @throws IOException Cuando ocurre un error en la descarga de los datos o la codificacion
	 * de la plantilla a mostrar en la tableta de firma.
	 * @throws SignaturePadException Cuando no se ha podido inicializar la tableta de firma. */
	public void sign(final Object parent,
					 final URL retrieveUrl,
					 final URL storeUrl,
					 final byte[] jpegImage,
					 final Rectangle signatureArea,
					 final X509Certificate cypherCert,
					 final SignerInfoBean signerData,
					 final Properties params) throws IOException, SignaturePadException {

		this.parentComponent = parent;
		this.targetUrl = storeUrl;
		this.signatureAreaOnPad = signatureArea;
		this.pK = cypherCert;
		this.signerInfo = signerData;
		this.extraParams = params;

		this.pdfDoc = downloadDocument(retrieveUrl);

		this.signatureWindow = new WacomSignatureWindow(
			parent,
			jpegImage,
			this.signatureAreaOnPad,
			signerData
		);
		this.signatureWindow.addSignatureListener(this);
		this.signatureWindow.captureSign();
	}


	@Override
	public void signatureFinished(final SignatureResult sr) {

		LOGGER.info("La firma se realizo correctamente"); //$NON-NLS-1$

		// Obtenemos la huella digital de los datos
		final byte[] mdDoc = messageDigest(this.pdfDoc, DIGEST_ALGO);

		// Obtenemos los metadatos
		LOGGER.info("Obtenemos los metadatos de la firma"); //$NON-NLS-1$
		final byte[] metadata;
		try {
			metadata = buildXmpMetadata(
				sr.getSignatureData(),
				sr.getSignatureRawData(),
				mdDoc,
				this.pK,
				this.signerInfo
			);
		}
		catch(final InvalidKeyException e) {
			e.printStackTrace();
			showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.0")); //$NON-NLS-1$
			return;
		}
		catch(final CipherException e) {
			e.printStackTrace();
			showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.1")); //$NON-NLS-1$
			return;
		}
		catch(final Exception e) {
			e.printStackTrace();
			showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.2")); //$NON-NLS-1$
			return;
		}

		// Insertamos todo en el PDF
		LOGGER.info("Agregamos al PDF la informacion de firma"); //$NON-NLS-1$
		final byte[] signedPdf;
		try {
			signedPdf = PdfSignerManager.addPdfInfo(
				this.pdfDoc,
				metadata,
				sr.getSignatureJpegImage(),
				sr.getSignerInfo(),
				sr.getSignaturePadInfo(),
				this.extraParams
			);
		}
		catch(final Exception e) {
			e.printStackTrace();
			showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.3")); //$NON-NLS-1$
			return;
		}

		// Enviamos el PDF
		LOGGER.info("Se envia el PDF"); //$NON-NLS-1$
		final String dataUrlParam = getDataUrlParam(this.extraParams);
		try {
			sendPdf(this.targetUrl, signedPdf, dataUrlParam);
		}
		catch(final IOException e) {
			e.printStackTrace();
			showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.4")); //$NON-NLS-1$
		}
	}

	@Override
	public void signatureCancelled() {

		LOGGER.info("Firma cancelada"); //$NON-NLS-1$

		//showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.5")); //$NON-NLS-1$
	}

	@Override
	public void signatureAborted(final Throwable e) {

		LOGGER.info("Ocurrio un error durante la firma y fue abortada: " + e); //$NON-NLS-1$

		this.signatureWindow.clearScreen();

		e.printStackTrace();
		showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.6") + e); //$NON-NLS-1$
	}

	/**
	 * Descarga el contenido de una URL.
	 * @param retrieverUrl URL de descarga.
	 * @return Contenido accesible a partir de la URL.
	 * @throws IOException Cuando no es posible acceder al recurso remoto o descargarlo.
	 */
	private static byte[] downloadDocument(final URL retrieverUrl) throws IOException {

		System.out.println("Usamos documento en disco para depuracion");
		return AOUtil.getDataFromInputStream(
			BioSigner.class.getResourceAsStream("/AppCampus_Tomas_Garcia-Meras.pdf")
		);

//		final InputStream is = retrieverUrl.openStream();
//		byte[] data = AOUtil.getDataFromInputStream(is);
//		is.close();
//
//		return data;
	}

	/**
	 * Calcula la huella digital de unos datos con el algoritmo especificado.
	 * @param data Datos de los que calcular la huella digital.
	 * @param algorithm Algoritmo de huella digital.
	 * @return Huella digital de los datos.
	 */
	private static byte[] messageDigest(final byte[] data, final String algorithm) {
		try {
			return MessageDigest.getInstance(algorithm).digest(data);
		}
		catch (final NoSuchAlgorithmException e) {
			// Nunca se deberia llegar a este punto
			LOGGER.severe("Se ha configurado un algoritmo interno de huella digital no valido: " + e); //$NON-NLS-1$
			throw new RuntimeException(e);
		}
	}

	/** Crea el XMP con los metadatos para el PDF.
	 * @param isoBioData Datos biom&eacute;tricos de firma seg&uacute;n la ISO.
	 * @param rawBioData Datos biom&eacute;tricos de firma en bruto.
	 * @param md Huella digital de los datos.
	 * @param cypherCert Certificado para el cifrado de datos.
	 * @param signerInfo Informaci&oacute;n del firmante.
	 * @return Metadatos del PDF.
	 * @throws IOException Cuando
	 * @throws InvalidKeyException Cuando la clave p&uacute;blica no es v&aacute;lida.
	 * @throws CipherException Cuando no se pueden cifrar los documetos. */
	private static byte[] buildXmpMetadata(final byte[] isoBioData,
			                               final byte[] rawBioData,
			                               final byte[] md,
			                               final X509Certificate cypherCert,
			                               final SignerInfoBean signerInfo) throws IOException,
			                                                                       InvalidKeyException,
			                                                                       CipherException {

		final byte[] bioData = new BioDataStructure(isoBioData, rawBioData, md, DIGEST_ALGO).getEncoded();

		byte[] bioDataCiphered;
		try {
			bioDataCiphered = cipherData(bioData, (RSAPublicKey) cypherCert.getPublicKey());
		}
		catch (final Exception e) {
			throw new CipherException(e);
		}
		return PdfXmpHelper.buildXmp(
			bioDataCiphered,
			cypherCert.getIssuerX500Principal().toString(),
			signerInfo
		);
	}

	/**
	 * Cifra los datos con una clave p&uacute;blica.
	 * @param data Datos a cifrar.
	 * @param pk Clave p&uacute;blica.
	 * @return Datos cifrados.
	 */
	private static byte[] cipherData(final byte[] data, final RSAPublicKey pk) {

		//TODO:Implementar el cifrado

//		final AOCMSEnveloper enveloper = new AOCMSEnveloper();
//        final AOCipherConfig cipherConfig = new AOCipherConfig(AOCipherAlgorithm.AES, null, null);
//
//        enveloper.createCMSEnvelopedData(data, null, cipherConfig, recipientsCerts, null);

		return data;
	}

	/**
	 * Env&iacute;o de datos.
	 * @param storeUrl URL a la que enviar los datos.
	 * @param signedPdf Datos a enviar.
	 * @param dataUrlParam Par&aacute;metro en el que se enviar&aacute;n los datos al servicio externo.
	 * @throws IOException Cuando ocurre alg&uacute;n error durante el env&iacute;o.
	 */
	private static void sendPdf(final URL storeUrl, final byte[] signedPdf, final String dataUrlParam) throws IOException {

		System.out.println("Almacenamos el PDF en disco para depuracion");
		final File tmpFile = File.createTempFile("BIO_", ".pdf");
		final OutputStream fos = new FileOutputStream(tmpFile);
		fos.write(signedPdf);
		fos.flush();
		fos.close();
		System.out.println(tmpFile.getAbsolutePath());

//		final Map<String, String> params = new HashMap<String, String>();
//		params.put(dataUrlParam, Base64.encode(signedPdf, true));
//
//		try {
//			HttpConnectionManager.sendDataByPost(storeUrl, params);
//		} catch (IOException e) {
//			LOGGER.severe("Ocurrio un error durante el envio del PDF firmado al servidor remoto: " + e); //$NON-NLS-1$
//			throw e;
//		}
	}

	/** Obtiene el nombre de par&aacute;metro configurado para el env&iacute;o de los datos.
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n.
	 * @return Nombre del par&aacute;metro configurado o el nombre por defecto si no se indic&oacute; ninguno. */
	private static String getDataUrlParam(final Properties extraParams) {
		return extraParams != null ?
			extraParams.containsKey(DATA_URL_PARAM_NAME) ?
				extraParams.getProperty(DATA_URL_PARAM_NAME) : DEFAULT_URL_PARAM_TO_UPLOAD_DATA :
					DEFAULT_URL_PARAM_TO_UPLOAD_DATA;
	}

	/** Muestra un mensaje de error al usuario.
	 * @param parent Componente padre sobre el que mostrar el mensaje.
	 * @param message Mensaje a mostrar. */
	private static void showErrorMessage(final Object parent, final String message) {
		LOGGER.severe(message);
		JOptionPane.showMessageDialog(
			parent instanceof Component ? (Component) parent : null,
			message,
			HandWrittenMessages.getString("BioSigner.7"), //$NON-NLS-1$
			JOptionPane.ERROR_MESSAGE
		);
	}

	/** Excepci&oacute;n que identifica un error en el cifrado de datos. */
	private static class CipherException extends Exception {

		private static final long serialVersionUID = -4712179728841694716L;

		/** Construye una excepci&oacute;n que identifica un error en el cifrado de datos..
		 * @param cause Causa de la excepci&oacute;n. */
		public CipherException(final Throwable cause) {
			super(cause);
		}
	}
}
