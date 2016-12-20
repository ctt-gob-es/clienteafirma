package es.gob.afirma.triphase.signer.processors;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.signers.xades.asic.AOXAdESASiCSSigner;

/** Procesador de firmas trif&aacute;sicas XAdES-ASiC-S.
 * @author Tom&aacute;s Garc&iacute;a Mer&aacute;s. */
public final class XAdESASiCSTriPhasePreProcessor extends XAdESTriPhasePreProcessor {

	/** Manejador de firma XAdES-ASiC trif&aacute;sico.
	 * @param installXmlDSigProvider Indica si se debe instalar expresamente un proveedor de firmas XML. */
	public XAdESASiCSTriPhasePreProcessor(final boolean installXmlDSigProvider) {
		super(installXmlDSigProvider);
	}

	@Override
	public TriphaseData preProcessPreSign(final byte[] data,
			                        final String algorithm,
			                        final X509Certificate[] cert,
			                        final Properties extraParams) throws IOException,
			                                                             AOException {
		return super.preProcessPreSign(
			data,
			algorithm,
			cert,
			AOXAdESASiCSSigner.setASiCProperties(extraParams, data)
		);
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final byte[] triphaseDataBytes) throws NoSuchAlgorithmException,
			                                                      AOException,
			                                                      IOException {

		return preProcessPostSign(data, algorithm, cert, extraParams, TriphaseData.parser(triphaseDataBytes));
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                      AOException,
			                                                      IOException {
		final byte[] xadesSignature = super.preProcessPostSign(
			data,
			algorithm,
			cert,
			AOXAdESASiCSSigner.setASiCProperties(extraParams, data),
			triphaseData
		);
		return ASiCUtil.createSContainer(
			xadesSignature,
			data,
			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
			extraParams.getProperty("asicsFilename") //$NON-NLS-1$
		);
	}

	@Override
	public TriphaseData preProcessPreCoSign(final byte[] data,
			                          final String algorithm,
			                          final X509Certificate[] cert,
			                          final Properties extraParams) throws IOException,
			                                                               AOException {
		throw new UnsupportedOperationException();
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final byte[] triphaseDataBytes) throws NoSuchAlgorithmException,
			                                                        AOException,
			                                                        IOException {
		throw new UnsupportedOperationException();
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                        AOException,
			                                                        IOException {
		throw new UnsupportedOperationException();
	}

	@Override
	public TriphaseData preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate[] cert,
			                               final Properties extraParams,
			                               final CounterSignTarget targets) throws IOException,
			                                                                       AOException {
		throw new UnsupportedOperationException();
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final byte[] triphaseDataBytes,
			                                final CounterSignTarget targets) throws NoSuchAlgorithmException,
			                                                                        AOException,
			                                                                        IOException {
		throw new UnsupportedOperationException();
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final TriphaseData triphaseData,
			                                final CounterSignTarget targets) throws NoSuchAlgorithmException,
			                                                                        AOException,
			                                                                        IOException {
		throw new UnsupportedOperationException();
	}

}
