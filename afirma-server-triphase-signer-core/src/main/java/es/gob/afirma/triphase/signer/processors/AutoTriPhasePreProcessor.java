package es.gob.afirma.triphase.signer.processors;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;

/** Procesador de firmas trif&aacute;sicas de formato autom&aacute;tico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AutoTriPhasePreProcessor implements TriPhasePreProcessor {

	private final boolean installXmlDSig;

	/** Crea un procesador de firmas trif&aacute;sicas de formato autom&aacute;tico.
	 * @param installXmlDSigProvider Indica si se debe instalar expresamente un proveedor de firmas XML. */
	public AutoTriPhasePreProcessor(final boolean installXmlDSigProvider) {
		this.installXmlDSig = installXmlDSigProvider;
	}

	@Override
	public TriphaseData preProcessPreSign(final byte[] data,
			                        final String algorithm,
			                        final X509Certificate[] cert,
			                        final Properties extraParams) throws IOException,
			                                                             AOException {
		final String format = getSignFormat(data);
		final TriPhasePreProcessor prep = getPreProcessor(format);
		final TriphaseData preSign = prep.preProcessPreSign(data, algorithm, cert, extraParams);
		preSign.setFormat(format);
		return preSign;
	}

	@Override
	public TriphaseData preProcessPreCoSign(final byte[] data,
			                          final String algorithm,
			                          final X509Certificate[] cert,
			                          final Properties extraParams) throws IOException,
			                                                               AOException {
		final String format = getSignFormat(data);
		final TriPhasePreProcessor prep = getPreProcessor(format);
		final TriphaseData preSign = prep.preProcessPreCoSign(data, algorithm, cert, extraParams);
		preSign.setFormat(format);
		return preSign;
	}

	@Override
	public TriphaseData preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate[] cert,
			                               final Properties extraParams,
			                               final CounterSignTarget targets) throws IOException,
			                                                                       AOException {
		final String format = getSignFormat(sign);
		final TriPhasePreProcessor prep = getPreProcessor(format);
		final TriphaseData preSign = prep.preProcessPreCounterSign(sign, algorithm, cert, extraParams, targets);
		preSign.setFormat(format);
		return preSign;
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final TriphaseData sessionData,
			                                final CounterSignTarget targets) throws NoSuchAlgorithmException,
			                                                                        AOException,
			                                                                        IOException {
		final TriPhasePreProcessor prep = getPreProcessor(sessionData.getFormat());
		return prep.preProcessPostCounterSign(sign, algorithm, cert, extraParams, sessionData, targets);
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final TriphaseData sessionData) throws NoSuchAlgorithmException,
			                                                                IOException,
			                                                                AOException {
		final TriPhasePreProcessor prep = getPreProcessor(sessionData.getFormat());
		return prep.preProcessPostSign(data, algorithm, cert, extraParams, sessionData);
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final TriphaseData sessionData) throws NoSuchAlgorithmException,
			                                                                  AOException,
			                                                                  IOException {
		final TriPhasePreProcessor prep = getPreProcessor(sessionData.getFormat());
		return prep.preProcessPostCoSign(data, algorithm, cert, extraParams, sessionData);
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final byte[] session) throws NoSuchAlgorithmException,
			                                                      IOException,
			                                                      AOException {
		if (session == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}
		return preProcessPostSign(data, algorithm, cert, extraParams, TriphaseData.parser(session));
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final byte[] session,
			                                final CounterSignTarget targets) throws NoSuchAlgorithmException,
			                                                                        AOException,
			                                                                        IOException {
		if (session == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}
		return preProcessPostCounterSign(sign, algorithm, cert, extraParams, TriphaseData.parser(session), targets);
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final byte[] session) throws NoSuchAlgorithmException,
			                                                        AOException,
			                                                        IOException {
		if (session == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}
		return preProcessPostCoSign(data, algorithm, cert, extraParams, TriphaseData.parser(session));
	}

	private TriPhasePreProcessor getPreProcessor(final String format) {
		if (AOSignConstants.SIGN_FORMAT_PADES.equalsIgnoreCase(format) ||
			AOSignConstants.SIGN_FORMAT_PADES_TRI.equalsIgnoreCase(format)) {
					return new PAdESTriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_CADES_TRI.equalsIgnoreCase(format)) {
					return new CAdESTriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_XADES.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_XADES_TRI.equalsIgnoreCase(format)) {
					return new XAdESTriPhasePreProcessor(this.installXmlDSig);
		}
		if (AOSignConstants.SIGN_FORMAT_CADES_ASIC_S.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_CADES_ASIC_S_TRI.equalsIgnoreCase(format)) {
					return new CAdESASiCSTriPhasePreProcessor();
		}
		if (AOSignConstants.SIGN_FORMAT_XADES_ASIC_S.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_XADES_ASIC_S_TRI.equalsIgnoreCase(format)) {
					return new XAdESASiCSTriPhasePreProcessor(this.installXmlDSig);
		}
		if (AOSignConstants.SIGN_FORMAT_FACTURAE.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_FACTURAE_TRI.equalsIgnoreCase(format) ||
				 AOSignConstants.SIGN_FORMAT_FACTURAE_ALT1.equalsIgnoreCase(format)) {
					return new FacturaETriPhasePreProcessor(this.installXmlDSig);
		}
		throw new IllegalArgumentException("Formato de firma no soportado: " + format); //$NON-NLS-1$
	}

	private static String getSignFormat(final byte[] data) {
		if (data == null || data.length < 1) {
			throw new IllegalArgumentException(
				"Los datos a firmar no pueden ser nulos ni vacios" //$NON-NLS-1$
			);
		}
		// PDF siempre con PAdES
		if (isPDF(data)) {
			return AOSignConstants.SIGN_FORMAT_PADES;
		}
		// FacturaE siempre con FacturaE
		if (isFacturae(data)) {
			return AOSignConstants.SIGN_FORMAT_FACTURAE;
		}
		// Otro XML siempre con XAdES
		if (AOFileUtils.isXML(data)) {
			return AOSignConstants.SIGN_FORMAT_XADES;
		}
		// En cualquier otro caso, CAdES
		return AOSignConstants.SIGN_FORMAT_CADES;
	}

    /** Comprueba si los datos introducidos se corresponden a un fichero PDF.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son un PDF. */
    private static boolean isPDF(final byte[] data) {
        try {
            return new AOPDFSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Indica si los datos son una factura electr&oacute;nica.
     * @param file Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    private static boolean isFacturae(final byte[] file) {
        try {
            return new AOFacturaESigner().isValidDataFile(file);
        }
        catch(final Exception e) {
            return false;
        }
    }
}
