package es.gob.afirma.triphase.server;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.pades.InvalidPdfException;
import es.gob.afirma.signers.pades.PAdESTriPhaseSigner;
import es.gob.afirma.signers.pades.PdfSignResult;

final class PAdESTriPhasePreProcessor implements TriPhasePreProcessor {

	/** Momento de la firma, establecido en el servidor. */
	private static final String PROPERTY_NAME_SIGN_TIME = "TIME"; //$NON-NLS-1$

	/** Identificador interno del PDF. */
	private static final String PROPERTY_NAME_PDF_UNIQUE_ID = "PID"; //$NON-NLS-1$

	/** Indica si la postfirma requiere la prefirma. */
	private static final String PROPERTY_NAME_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$

	/** Indica si la postfirma requiere el documento original. */
	private static final String PROPERTY_NAME_NEED_DATA = "NEED_DATA"; //$NON-NLS-1$

	/** Prefijo para cada prefirma. */
	private static final String PROPERTY_NAME_PRESIGN_PREFIX = "PRE."; //$NON-NLS-1$

	/** Nombre de la propiedad de los sesi&oacute;n necesarios para completar la firma. */
	private static final String PROPERTY_NAME_SESSION_DATA_PREFIX = "SESSION."; //$NON-NLS-1$

	/** Car&aacute;cter por el que se sustituye a {@code #EQUAL} en los Properties anidados. */
	private static final String EQUAL_REPLACEMENT = "%%%"; //$NON-NLS-1$

	/** Car&aacute;cter igual. */
	private static final String EQUAL = "="; //$NON-NLS-1$

	/** Car&aacute;cter por el que se sustituye a {@code #CR} en los Properties anidados. */
	private static final String CR_REPLACEMENT = "&&&"; //$NON-NLS-1$

	/** Car&aacute;cter de salto de l&iacute;nea usado para los Properties. */
	private static final String CR = "\n"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN_PREFIX = "PK1."; //$NON-NLS-1$

	/** Manejador de log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@Override
	public byte[] preProcessPreSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {

		LOGGER.info("Prefirma PAdES - Firma - INICIO"); //$NON-NLS-1$

		final GregorianCalendar signTime = new GregorianCalendar();

		// Primera fase (servidor)
		LOGGER.info("Se invocan las funciones internas de prefirma PAdES"); //$NON-NLS-1$
		final PdfSignResult preSignature;
		try {
			preSignature = PAdESTriPhaseSigner.preSign(
					AOSignConstants.getDigestAlgorithmName(algorithm),
					data,
					new X509Certificate[] { cert },
					signTime,
					extraParams
					);
		}
		catch (final DocumentException e) {
			LOGGER.severe("El documento no es un PDF y no se puede firmar: " + e); //$NON-NLS-1$
			throw new InvalidPdfException(e);
		}

		LOGGER.info("Se prepara la respuesta de la prefirma PAdES"); //$NON-NLS-1$

		// Ahora pasamos al cliente 2 cosas:
		// 1.- La prefirma para que haga el PKCS#1
		// 2.- Los datos de sesion necesarios para recomponer la firma con el PKCS#1 correcto. Estos son:
		//    - La fecha generada en el servidor para reutilizarla en la postfirma
		//    - El ID de PDF para reutilizarlo en la postfirma
		//    - La propia prefirma
		final StringBuilder sessionBuilder = new StringBuilder();
		sessionBuilder.append(PROPERTY_NAME_SIGN_TIME).append(EQUAL).append(Long.toString(signTime.getTimeInMillis())).append(CR);
		sessionBuilder.append(PROPERTY_NAME_PDF_UNIQUE_ID).append(EQUAL).append(preSignature.getFileID());

		final StringBuilder urlParamBuilder = new StringBuilder();
		urlParamBuilder.append(PROPERTY_NAME_PRESIGN_PREFIX).append(0).append(EQUAL).append(Base64.encode(preSignature.getSign())).append(CR);
		urlParamBuilder.append(PROPERTY_NAME_NEED_PRE).append(EQUAL).append(true).append(CR);
		urlParamBuilder.append(PROPERTY_NAME_NEED_DATA).append(EQUAL).append(true).append(CR);
		urlParamBuilder.append(PROPERTY_NAME_SESSION_DATA_PREFIX).append(0).append(EQUAL).append(sessionBuilder.toString().replace(EQUAL, EQUAL_REPLACEMENT).replace(CR, CR_REPLACEMENT));

		LOGGER.info("Prefirma PAdES - Firma - FIN"); //$NON-NLS-1$

		return urlParamBuilder.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostSign(final byte[] docBytes,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final Properties sessionData) throws NoSuchAlgorithmException, AOException, IOException {

		LOGGER.info("Postfirma PAdES - Firma - INICIO"); //$NON-NLS-1$

		checkSessionProperties(sessionData);

		final Properties configParams = new Properties();
		configParams.load(new ByteArrayInputStream(
				sessionData.getProperty(PROPERTY_NAME_SESSION_DATA_PREFIX + 0)
				.replace(CR_REPLACEMENT, CR).replace(EQUAL_REPLACEMENT, EQUAL).getBytes()
				));

		// Preparo la fecha de firma
		final GregorianCalendar cal = (GregorianCalendar) Calendar.getInstance();
		try {
			cal.setTimeInMillis(Long.parseLong(configParams.getProperty(PROPERTY_NAME_SIGN_TIME)));
		}
		catch (final Exception e) {
			LOGGER.warning("La hora de firma indicada no es valida: " + e.toString()); //$NON-NLS-1$
		}

		// Ya con todos los datos hacemos la postfirma
		final PdfSignResult signResult = new PdfSignResult(
				configParams.getProperty(PROPERTY_NAME_PDF_UNIQUE_ID),
				Base64.decode(sessionData.getProperty(PROPERTY_NAME_PRESIGN_PREFIX + 0)),
				cal,
				extraParams);

		// Datos firmados
		LOGGER.info("Se invocan las funciones internas de postfirma PAdES"); //$NON-NLS-1$
		final byte[] postsign = PAdESTriPhaseSigner.postSign(
				AOSignConstants.getDigestAlgorithmName(algorithm),
				docBytes,
				new X509Certificate[] { cert },
				Base64.decode(sessionData.getProperty(PROPERTY_NAME_PKCS1_SIGN_PREFIX + 0)),
				signResult,
				null,
				null
				);

		LOGGER.info("Postfirma PAdES - Firma - FIN"); //$NON-NLS-1$

		return postsign;
	}

	@Override
	public byte[] preProcessPreCoSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {
		return preProcessPreSign(data, algorithm, cert, extraParams);
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final Properties sessionData) throws NoSuchAlgorithmException, AOException, IOException {
		return preProcessPostSign(data, algorithm, cert, extraParams, sessionData);
	}

	@Override
	public byte[] preProcessPreCounterSign(final byte[] sign, final String algorithm,
			final X509Certificate cert, final Properties extraParams,
			final CounterSignTarget targets) throws IOException, AOException {
		throw new UnsupportedOperationException("La operacion de contrafirma no esta soportada en PAdES."); //$NON-NLS-1$
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign, final String algorithm,
			final X509Certificate cert, final Properties extraParams, final Object sessionData,
			final CounterSignTarget targets) throws NoSuchAlgorithmException,
			AOException, IOException {
		throw new UnsupportedOperationException("La operacion de contrafirma no esta soportada en PAdES."); //$NON-NLS-1$
	}

	/**
	 * Comprueba que hayan proporcionado todos los datos de sesi&oacute;n necesarios.
	 * @param sessionData Properties con los datos de sesi&oacute;n necesarios para una postfirma.
	 * @throws AOException Cuando se encuentra un error en los dados de sesi&oacute;n.
	 */
	private static void checkSessionProperties(final Properties sessionData) throws AOException {

		if (sessionData == null) {
			throw new AOException("No se han recibido los datos de sesion de firma para la postfirma PAdES"); //$NON-NLS-1$
		}
		try {
			if (!sessionData.containsKey(PROPERTY_NAME_SESSION_DATA_PREFIX + 0)) {
				throw new AOException("Los datos de sesion no contienen los metadados para la configuracion de la operacion"); //$NON-NLS-1$
			}
			if (!sessionData.containsKey(PROPERTY_NAME_PRESIGN_PREFIX + 0)) {
				throw new AOException("Los datos de sesion no contienen la prefirma de los datos"); //$NON-NLS-1$
			}
			if (!sessionData.containsKey(PROPERTY_NAME_PKCS1_SIGN_PREFIX + 0)) {
				throw new AOException("Los datos de sesion no contienen el resultado de la firma en cliente"); //$NON-NLS-1$
			}
		} finally {
			LOGGER.severe("Datos de sesion contenidos:"); //$NON-NLS-1$
			for (final String key : sessionData.keySet().toArray(new String[sessionData.size()])) {
				LOGGER.severe(key);
			}
			LOGGER.severe("---------------------------"); //$NON-NLS-1$
		}
	}
}
