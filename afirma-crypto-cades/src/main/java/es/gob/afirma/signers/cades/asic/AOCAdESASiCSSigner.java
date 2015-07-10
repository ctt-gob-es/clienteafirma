package es.gob.afirma.signers.cades.asic;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.cades.AOCAdESSigner;

/** Firmador CAdES ASiC-S.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOCAdESASiCSSigner implements AOSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties xParams) throws AOException,
			                                            IOException {
		final Properties extraParams = xParams != null ? xParams : new Properties();
		final byte[] signature = new AOCAdESSigner().sign(data, algorithm, key, certChain, extraParams);
		return ASiCUtil.createSContainer(
			signature,
			data,
			extraParams.getProperty("asicsFilename") //$NON-NLS-1$
		);
	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign,
			                               final boolean asSimpleSignInfo) throws AOInvalidFormatException,
			                                                                      IOException {
		return new AOCAdESSigner().getSignersStructure(
			ASiCUtil.getASiCSSignature(sign),
			asSimpleSignInfo
		);
	}

	@Override
	public boolean isSign(final byte[] is) throws IOException {
		final byte[] sign;
		try {
			sign = ASiCUtil.getASiCSSignature(is);
		}
		catch(final Exception e) {
			LOGGER.info("La firma proporcionada no es ASiC-S: " + e); //$NON-NLS-1$
			return false;
		}
		return new AOCAdESASiCSSigner().isSign(sign);
	}

	@Override
	public boolean isValidDataFile(final byte[] is) throws IOException {
		return true;
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".asics"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public byte[] getData(final byte[] signData) throws AOException, IOException {
		return ASiCUtil.getASiCSData(signData);
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] signData) throws AOException, IOException {
		return new AOCAdESASiCSSigner().getSignInfo(ASiCUtil.getASiCSSignature(signData));
	}

	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) {
		throw new UnsupportedOperationException("ASiC-S no soporta cofirmas"); //$NON-NLS-1$
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) {
		throw new UnsupportedOperationException("ASiC-S no soporta cofirmas"); //$NON-NLS-1$
	}

	@Override
	public byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties extraParams) {
		throw new UnsupportedOperationException("ASiC-S no soporta contrafirmas"); //$NON-NLS-1$
	}

}
