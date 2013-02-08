package es.gob.afirma.jse.signfolder;

import java.io.IOException;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signfolder.Base64Codec;

/**
 * Implementaci&oacute;n del codec de Base64.
 * @author Carlos Gamuci
 */
public class Base64CodecImpl implements Base64Codec {

	/** {@inheritDoc} */
	@Override
	public byte[] base64Decode(final String base64EncodedData) throws IOException {
		return Base64.decode(base64EncodedData);
	}

	/** {@inheritDoc} */
	@Override
	public String base64EncodeData(final byte[] data) throws IOException {
		return Base64.encode(data);
	}

	/** {@inheritDoc} */
	@Override
	public String base64ToUrlEncoding(final String base64Data) {
		return base64Data.replace(" ", "").replace("\r", "").replace("\n", "") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
		.replace("=", "%61").replace("/", "%47").replace("+", "%43"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
	}
}
