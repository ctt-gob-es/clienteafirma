package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.padestri.server.PAdESTriPhaseSignerServerSide;
import es.gob.afirma.signers.padestri.server.PAdESTriPhaseSignerServerSide.PdfPreSignResult;

final class PAdESTriPhasePreProcessor implements TriPhasePreProcessor {

	/** Prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Datos de sesi&oacute;n necesarios para completar la firma. */
	private static final String PROPERTY_NAME_SESSION_DATA = "SESSION"; //$NON-NLS-1$

	/** Momento de la firma, establecido en el servidor. */
	private static final String PROPERTY_NAME_SIGN_TIME = "TIME"; //$NON-NLS-1$

	/** Identificador interno del PDF. */
	private static final String PROPERTY_NAME_PDF_UNIQUE_ID = "PID"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	@Override
	public byte[] preProcessPreSign(final byte[] docBytes,
			                        final String algorithm,
			                        final X509Certificate signerCert,
			                        final Properties extraParams) throws IOException, AOException {


		final GregorianCalendar signTime = new GregorianCalendar();

        // Primera fase (servidor)
		final PdfPreSignResult preSignature = PAdESTriPhaseSignerServerSide.preSign(
		     AOSignConstants.getDigestAlgorithmName(algorithm),
		     docBytes,
		     new X509Certificate[] { signerCert },
		     signTime,
		     extraParams
		);

        // Ahora pasamos al cliente 2 cosas:
        // 1.- La prefirma para que haga el PKCS#1
        // 2.- Los datos de sesion necesarios para recomponer la firma con el PKCS#1 correcto. Estos son:
		//    - La fecha generada en el servidor para reutilizarla en la postfirma
        //    - El ID de PDF para reutilizarlo en la postfirma
		//    - La propia prefirma
        final StringBuilder builder = new StringBuilder();
        builder.append(PROPERTY_NAME_SIGN_TIME).append("=").append(Long.toString(signTime.getTimeInMillis())).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		builder.append(PROPERTY_NAME_PDF_UNIQUE_ID).append("=").append(preSignature.getFileID()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		builder.append(PROPERTY_NAME_PRESIGN).append("=").append(Base64.encode(preSignature.getPreSign())); //$NON-NLS-1$

     	final StringBuilder builder2 = new StringBuilder();
        builder2.append(PROPERTY_NAME_PRESIGN).append("=").append(Base64.encode(preSignature.getPreSign())).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		builder2.append(PROPERTY_NAME_SESSION_DATA).append("=").append(Base64.encode(builder.toString().getBytes())); //$NON-NLS-1$

        return builder2.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostSign(final byte[] docBytes,
			                         final String algorithm,
			                         final X509Certificate signerCert,
			                         final Properties extraParams) throws NoSuchAlgorithmException, AOException, IOException {

		final Properties sessionData = new Properties();
		sessionData.load(new ByteArrayInputStream(Base64.decode(extraParams.getProperty(PROPERTY_NAME_SESSION_DATA))));

		// Preparo la fecha de firma
		final Calendar cal = Calendar.getInstance();
		try {
			cal.setTimeInMillis(Long.parseLong(sessionData.getProperty(PROPERTY_NAME_SIGN_TIME)));
		}
		catch (final NumberFormatException e) {
			Logger.getLogger("es.gob.afirma").warning("La hora de firma indicada no es valida: " + e.toString()); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final Properties enhancerConfig = new Properties();

		// Ya con todos los datos hacemos la postfirma

		// Datos firmados
		return PAdESTriPhaseSignerServerSide.postSign(
			AOSignConstants.getDigestAlgorithmName(algorithm),
			docBytes,
			new X509Certificate[] { signerCert },
			extraParams,
			Base64.decode(extraParams.getProperty(PROPERTY_NAME_PKCS1_SIGN)),
			Base64.decode(sessionData.getProperty(PROPERTY_NAME_PRESIGN)),
			sessionData.getProperty(PROPERTY_NAME_PDF_UNIQUE_ID),
			cal,
			null,
			enhancerConfig
		);

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
			                           final Properties extraParams) throws NoSuchAlgorithmException, AOException, IOException {
		return preProcessPostSign(data, algorithm, cert, extraParams);
	}

}
