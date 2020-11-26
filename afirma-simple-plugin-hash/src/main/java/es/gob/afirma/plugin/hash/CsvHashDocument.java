package es.gob.afirma.plugin.hash;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

public class CsvHashDocument extends HashDocument {

	private static final Logger LOGGER = Logger.getLogger(CsvHashDocument.class.getName());

	@Override
	public byte[] generate() throws DocumentException {

		final Map<String, byte[]> hashes = getHashes();
		if (hashes == null || hashes.size() < 1) {
			LOGGER.warning("No hay huellas, se genera un CSV vacio"); //$NON-NLS-1$
			return new byte[0];
		}
		final StringBuilder sb = new StringBuilder();
		for (final Map.Entry<String, byte[]> entry : hashes.entrySet()) {
		    sb.append("\"" + entry.getKey() + "\",\"" + AOUtil.hexify(entry.getValue(), false) + "h\"\r\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		return sb.toString().getBytes(getCharset() != null ? getCharset() : StandardCharsets.UTF_8);
	}

	@Override
	void load(final byte[] document) throws DocumentException {
		throw new UnsupportedOperationException("No se soporta la validacion de documentos CSV de hashes"); //$NON-NLS-1$
	}

}
