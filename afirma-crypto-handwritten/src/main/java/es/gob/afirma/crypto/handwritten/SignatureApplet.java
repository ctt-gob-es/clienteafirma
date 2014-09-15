package es.gob.afirma.crypto.handwritten;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.AccessController;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PrivilegedAction;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.X509EncodedKeySpec;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JApplet;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

public class SignatureApplet extends JApplet {

	/** Serial ID. */
	private static final long serialVersionUID = -2638294277305771916L;

	/** Logger para la impresi&oacute;n de trazas. */
	static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Mensaje de error detectado. */
	private String errorMessage = null;


	public void signWithImage(final String docUrl, final String storeServiceUrl, final String jpegImage, final String signaturePadRectX,
			final String signaturePadRectY, final String signaturePadRectWidth, final String signaturePadRectHeight,
			final String publicKeyB64, final String keyDn, final String signerName, final String signerSurname1,
			final String signerSurname2, final String signerId, final String extraParams) {

		final URL retrieverUrl;
		try {
			retrieverUrl = AOUtil.createURI(docUrl).toURL();
		}
		catch (final Exception e) {
			LOGGER.severe("La URL para la recuperacion del documento no es valida: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.0"), e); //$NON-NLS-1$
			return;
		}

		final URL storageUrl;
		try {
			storageUrl = AOUtil.createURI(storeServiceUrl).toURL();
		}
		catch (final Exception e) {
			LOGGER.severe("La URL para el guardado de la firma no es valida: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.1"), e); //$NON-NLS-1$
			return;
		}

		final Rectangle signatureArea;
		try {
			signatureArea = new Rectangle(
					Integer.parseInt(signaturePadRectX),
					Integer.parseInt(signaturePadRectY),
					Integer.parseInt(signaturePadRectWidth),
					Integer.parseInt(signaturePadRectHeight));
		}
		catch (final Exception e) {
			LOGGER.severe("Se han introducido valores invalidos para el recuadro de firma en la tableta: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.2"), e); //$NON-NLS-1$
			return;
		}

		final RSAPublicKey publicKey;
		try {
			publicKey = buildPublicKey(publicKeyB64);
		}
		catch (final Exception e) {
			LOGGER.severe("Ocurrio un error al decodificar la clave publica de cifrado: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.3"), e); //$NON-NLS-1$
			return;
		}

		final SignerInfoBean signerInfo = new SignerInfoBean(signerName, signerSurname1, signerSurname2, signerId);


		final Properties extraParamsProperties;

		try {
			final InputStream is = new ByteArrayInputStream(extraParams.replace("\\r\\n", "\n").replace("\\r", "\r").replace("\\n", "\n").getBytes()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
			extraParamsProperties = new Properties();
			extraParamsProperties.load(is);
			is.close();
		}
		catch (final Exception e) {
			LOGGER.severe("Ocurrio un error al decodificar los parametros extra para la configuracion de la firma: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.4"), e); //$NON-NLS-1$
			return;
		}

		AccessController.doPrivileged(new PrivilegedAction<Void>() {
			@Override
			public Void run() {
				try {
					//TODO: Realizar llamada en otro hilo para evitar el bloqueo de JavaScript
					new BioSigner().sign(
							this,
							retrieverUrl,
							storageUrl,
							jpegImage,
							signatureArea,
							publicKey,
							keyDn,
							signerInfo,
							extraParamsProperties);
				} catch (final Exception e) {
					LOGGER.severe("Ocurrio un error al iniciar el proceso de firma manuscrita:" + e); //$NON-NLS-1$
					setError(AppletMessages.getString("SignatureApplet.5"), e); //$NON-NLS-1$
				}
				return null;
			}
		});
	}

	public void signWithHtml(final String docUrl, final String storeServiceUrl, final String htmlTemplate, final String signaturePadRectX,
			final String signaturePadRectY, final String signaturePadRectWidth, final String signaturePadRectHeight,
			final String publicKeyB64, final String keyDn, final String signerName, final String signerSurname1,
			final String signerSurname2, final String signerId, final String extraParams) {

		LOGGER.info("Se invoca a signWithHtml"); //$NON-NLS-1$

		final URL retrieverUrl;
		try {
			retrieverUrl = AOUtil.createURI(docUrl).toURL();
		}
		catch (final Exception e) {
			LOGGER.severe("La URL para la recuperacion del documento no es valida: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.6"), e); //$NON-NLS-1$
			return;
		}

		final URL storageUrl;
		try {
			storageUrl = AOUtil.createURI(storeServiceUrl).toURL();
		}
		catch (final Exception e) {
			LOGGER.severe("La URL para el guardado de la firma no es valida: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.8"), e); //$NON-NLS-1$
			return;
		}

		final Rectangle signatureArea;
		try {
			signatureArea = new Rectangle(
					Integer.parseInt(signaturePadRectX),
					Integer.parseInt(signaturePadRectY),
					Integer.parseInt(signaturePadRectWidth),
					Integer.parseInt(signaturePadRectHeight));
		}
		catch (final Exception e) {
			LOGGER.severe("Se han introducido valores invalidos para el recuadro de firma en la tableta: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.10"), e); //$NON-NLS-1$
			return;
		}

		final RSAPublicKey publicKey;
		try {
			publicKey = buildPublicKey(publicKeyB64);
		}
		catch (final Exception e) {
			LOGGER.severe("Ocurrio un error al decodificar la clave publica de cifrado: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.12"), e); //$NON-NLS-1$
			return;
		}

		final SignerInfoBean signerInfo = new SignerInfoBean(signerName, signerSurname1, signerSurname2, signerId);

		final Properties extraParamsProperties;
		try {
			final InputStream is = new ByteArrayInputStream(extraParams.replace("\\r\\n", "\n").replace("\\r", "\r").replace("\\n", "\n").getBytes()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
			extraParamsProperties = new Properties();
			extraParamsProperties.load(is);
			is.close();
		}
		catch (final Exception e) {
			LOGGER.severe("Ocurrio un error al decodificar los parametros extra para la configuracion de la firma: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.15"), e); //$NON-NLS-1$
			return;
		}

		try {
			AccessController.doPrivileged(new PrivilegedAction<Void>() {
				@Override
				public Void run() {
					try {
						new BioSigner().sign(
								this,
								retrieverUrl,
								storageUrl,
								htmlTemplate,
								signatureArea,
								publicKey,
								keyDn,
								signerInfo,
								extraParamsProperties);
					} catch (final Exception e) {
						LOGGER.severe("Ocurrio un error al iniciar el proceso de firma manuscrita:" + e); //$NON-NLS-1$
						setError(AppletMessages.getString("SignatureApplet.18"), e); //$NON-NLS-1$
					}

					return null;
				}
			});
		}
		catch (final Throwable e) {
			LOGGER.severe("Ocurrio un problema al ejecutar la accion privilegiada de firma: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignatureApplet.19"), e); //$NON-NLS-1$
		}
	}

	private static RSAPublicKey buildPublicKey(final String publicKey) throws IOException, NoSuchAlgorithmException, InvalidKeySpecException {

		final byte[] encKey = Base64.decode(publicKey.replace("-----BEGIN RSA PUBLIC KEY-----", "") //$NON-NLS-1$ //$NON-NLS-2$
			.replace("-----END RSA PUBLIC KEY-----", "") //$NON-NLS-1$ //$NON-NLS-2$
			.replace("\n", "").replace("\r", "").replace("\t", "").replace(" ", "").trim()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$

		final X509EncodedKeySpec pubKeySpec = new X509EncodedKeySpec(encKey);
		final KeyFactory keyFactory = KeyFactory.getInstance("RSA"); //$NON-NLS-1$
		return (RSAPublicKey) keyFactory.generatePublic(pubKeySpec);
	}

	public String getErrorMessage() {
		return this.errorMessage;
	}

	/**
	 * Devuelve el nombre de la aplicaci&oacute;n.
	 * @return Nombre de la aplicaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	public String echo() {
		final String echo = "Atos Firma2E"; //$NON-NLS-1$
		LOGGER.info(echo);

		return echo;
	}

	void setError(String msg, Throwable cause) {
		this.errorMessage = msg;
		if (cause != null) {
			cause.printStackTrace();
		}
	}


}
