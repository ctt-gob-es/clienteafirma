package es.gob.afirma.crypto.handwritten;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.crypto.handwritten.pdf.PdfSignerManager;
import es.gob.afirma.crypto.handwritten.pdf.PdfXmpHelper;

/** Ejecutor de procesos de firma biom&eacute;trica
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class BioSignerRunner implements SignaturePadListener {

	/** Logger para la impresi&oacute;n de trazas. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Algoritmo de huella digital interno. */
	private static final String DIGEST_ALGO = "SHA-512"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro a utilizar para la subida de datos. */
	private static final String DEFAULT_URL_PARAM_TO_UPLOAD_DATA = "data"; //$NON-NLS-1$

	// *********************************************************************
	// ************* Atributos comunes para todas las firmas ***************

	/** Componente padre para la modalidad. */
	private final Object parentComponent;

	/** Certificado para el cifrado. */
	private final X509Certificate cert;

	/** Documento a firmar. */
	private final byte[] pdfDoc;

	/** Par&aacute;metro del POST donde indicar el PDF al servicio Web de env&iacute;o de PDF firmado. */
	private final String postDataParamName;

	/** URL del POST del servicio Web de env&iacute;o de PDF firmado. */
	private final URL targetUrl;

	// *********************************************************************
	// *********************************************************************

	private final SignerInfoBean signerInfo = null;
	private final Properties extraParams = null;

	/** Crea el ejecutor de procesos de firma biom&eacute;trica.
	 * @param parent Componente padre sobre el que mostrar los elementos visuales.
	 * @param retrieveUrl URL para la recuperac&oacute;n (GET HTTP) del PDF a firmar.
	 * @param postDataParam Par&aacute;metro del POST donde indicar el PDF al servicio Web de
	 *                      env&iacute;o de PDF firmado.
	 * @param sendUrl URL del POST del servicio Web de env&iacute;o de PDF firmado.
	 * @param cypherCert Certificado de donde obtener la clave publica para el cifrado de los
	 *                   datos.
	 * @throws IOException Si hay problemas descargando el PDF a firmar.
	 */
	public BioSignerRunner(final Object parent,
			               final URL retrieveUrl,
			               final URL sendUrl,
			               final String postDataParam,
			               final X509Certificate cypherCert) throws IOException {

		if (retrieveUrl == null) {
			throw new IllegalArgumentException(
				"Es necesario indicar un origen del documento a firmar" //$NON-NLS-1$
			);
		}
		if (sendUrl == null) {
			throw new IllegalArgumentException(
				"Es necesario indicar un destino para el documento firmado" //$NON-NLS-1$
			);
		}
		if (cypherCert == null) {
			LOGGER.warning(
				"No se ha proporcionado un certificado de cifrado, el resultado no se cifrara" //$NON-NLS-1$
			);
		}
		if (postDataParam == null) {
			LOGGER.info(
				"No se ha indicado el nombre del parametro del POST donde indicar el PDF al servicio Web " + //$NON-NLS-1$
				"de env&iacute;o de PDF firmado, se usara '" + DEFAULT_URL_PARAM_TO_UPLOAD_DATA + "'" //$NON-NLS-1$ //$NON-NLS-2$
			);
			this.postDataParamName = DEFAULT_URL_PARAM_TO_UPLOAD_DATA;
		}
		else {
			this.postDataParamName = postDataParam;
		}

		LOGGER.info("Se inicia la descarga del documento a firmar"); //$NON-NLS-1$
		this.pdfDoc = downloadDocument(retrieveUrl);
		LOGGER.info("Se ha terminado la descarga del documento a firma. Bytes recibidos: " + this.pdfDoc.length); //$NON-NLS-1$

		this.parentComponent = parent;
		this.cert = cypherCert;
		this.targetUrl = sendUrl;

	}

	@Override
	public void signatureFinished(final SignatureResult sr) {

		LOGGER.info("La firma se realizo correctamente"); //$NON-NLS-1$

		// Obtenemos la huella digital de los datos
		byte[] mdDoc;
		try {
			mdDoc = SimpleCryptoHelper.messageDigest(this.pdfDoc, DIGEST_ALGO);
		} catch (final NoSuchAlgorithmException e1) {
			e1.printStackTrace();
			throw new RuntimeException();
		}

		// Obtenemos los metadatos
		LOGGER.info("Obtenemos los metadatos de la firma"); //$NON-NLS-1$
		final byte[] metadata;
		try {
			metadata = buildXmpMetadata(
				sr.getSignatureData(),
				sr.getSignatureRawData(),
				mdDoc,
				this.cert,
				this.signerInfo
			);
		}
		catch(final InvalidKeyException e) {
			e.printStackTrace();
			return;
		}
		catch(final CipherException e) {
			e.printStackTrace();
			return;
		}
		catch(final Exception e) {
			e.printStackTrace();
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
				this.signerInfo,
				sr.getSignaturePadInfo(),
				this.extraParams
			);
		}
		catch(final Exception e) {
			e.printStackTrace();
			return;
		}

		// Enviamos el PDF
		LOGGER.info("Se envia el PDF"); //$NON-NLS-1$
		try {
			sendPdf(this.targetUrl, signedPdf);
		}
		catch(final IOException e) {
			e.printStackTrace();
			//showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.4")); //$NON-NLS-1$
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
			bioDataCiphered = SimpleCryptoHelper.cipherData(bioData, cypherCert);
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

	@Override
	public void signatureCancelled(final String id) {

		LOGGER.info("Firma cancelada: " + id); //$NON-NLS-1$

		//showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.5")); //$NON-NLS-1$
	}

	@Override
	public void signatureAborted(final Throwable e, final String id) {

		LOGGER.info("Ocurrio un error durante la firma (" + id + ") y fue abortada: " + e); //$NON-NLS-1$ //$NON-NLS-2$

		e.printStackTrace();
		//showErrorMessage(this.parentComponent, HandWrittenMessages.getString("BioSigner.6") + e); //$NON-NLS-1$
	}

	/** Descarga el contenido de una URL.
	 * @param retrieverUrl URL de descarga.
	 * @return Contenido accesible a partir de la URL.
	 * @throws IOException Cuando no es posible acceder al recurso remoto o descargarlo. */
	private static byte[] downloadDocument(final URL retrieverUrl) throws IOException {

		System.out.println("Usamos documento en disco para depuracion"); //$NON-NLS-1$
		return AOUtil.getDataFromInputStream(
			BioSigner.class.getResourceAsStream("/AppCampus_Tomas_Garcia-Meras.pdf") //$NON-NLS-1$
		);

//		final InputStream is = retrieverUrl.openStream();
//		byte[] data = AOUtil.getDataFromInputStream(is);
//		is.close();
//
//		return data;
	}

	/** Env&iacute;a los datos indicados mediante POST a la URL proporcionada.
	 * @param storeUrl URL a la que enviar los datos.
	 * @param signedPdf Datos a enviar.
	 * @throws IOException Cuando ocurre alg&uacute;n error durante el env&iacute;o. */
	private static void sendPdf(final URL storeUrl, final byte[] signedPdf) throws IOException {

		System.out.println("Almacenamos el PDF en disco para depuracion"); //$NON-NLS-1$
		final File tmpFile = File.createTempFile("BIO_", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
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


	/** Excepci&oacute;n que identifica un error en el cifrado de datos. */
	private static final class CipherException extends Exception {

		private static final long serialVersionUID = -4712179728841694716L;

		/** Construye una excepci&oacute;n que identifica un error en el cifrado de datos..
		 * @param cause Causa de la excepci&oacute;n. */
		public CipherException(final Throwable cause) {
			super(cause);
		}
	}

}
