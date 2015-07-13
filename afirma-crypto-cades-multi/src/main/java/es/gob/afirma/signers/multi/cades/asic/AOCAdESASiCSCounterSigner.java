package es.gob.afirma.signers.multi.cades.asic;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.signers.multi.cades.AOCAdESCounterSigner;

/** Operaciones de contrafirma CAdES ASiC-S.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOCAdESASiCSCounterSigner implements AOCounterSigner {

	/** {@inheritDoc} */
	@Override
	public byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties extraParams) throws AOException,
			                                                       IOException {
		// Extraemos firma y datos del ASiC
		final byte[] packagedData = ASiCUtil.getASiCSData(sign);
		final byte[] packagedSign = ASiCUtil.getASiCSBinarySignature(sign);

		// Creamos la contrafirma
		final byte[] newCounterSign = new AOCAdESCounterSigner().countersign(
			packagedSign,
			algorithm,
			targetType,
			targets,
			key,
			certChain,
			extraParams
		);

		// Devolvemos un nuevo ASiC
		return ASiCUtil.createSContainer(
			newCounterSign,
			packagedData,
			ASiCUtil.getASiCSDataFilename(sign),
			ASiCUtil.ENTRY_NAME_BINARY_SIGNATURE
		);
	}
}
