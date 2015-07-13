package es.gob.afirma.signers.multi.cades.asic;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.signers.multi.cades.AOCAdESCoSigner;

/** Operaciones de cofirma CAdES ASiC-S.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOCAdESASiCSCoSigner implements AOCoSigner {

	/** {@inheritDoc} */
	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException, IOException {
		return cosign(sign, algorithm, key, certChain, extraParams);
	}

	/** {@inheritDoc} */
	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException,
			                                                  IOException {
		// Extraemos firma y datos del ASiC
		final byte[] packagedData = ASiCUtil.getASiCSData(sign);
		final byte[] packagedSign = ASiCUtil.getASiCSBinarySignature(sign);

		// Creamos la cofirma
		final byte[] newSign = new AOCAdESCoSigner().cosign(packagedData, packagedSign, algorithm, key, certChain, extraParams);

		// Devolvemos un nuevo ASiC
		return ASiCUtil.createSContainer(
			newSign,
			packagedData,
			ASiCUtil.getASiCSDataFilename(sign),
			ASiCUtil.ENTRY_NAME_BINARY_SIGNATURE
		);
	}

}
