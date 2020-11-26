package es.gob.afirma.plugin.hash;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

public class TxtHashDocument extends HashDocument {

	private static final String LINE_HEADER_PREFIX = ";"; //$NON-NLS-1$
	private static final String LINE_HEADER_CHARSET = LINE_HEADER_PREFIX + "charset="; //$NON-NLS-1$
	private static final String LINE_HEADER_ALGORITHM = LINE_HEADER_PREFIX + "hashAlgorithm="; //$NON-NLS-1$
	private static final String LINE_HEADER_RECURSIVE = LINE_HEADER_PREFIX + "recursive="; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger(TxtHashDocument.class.getName());

	public TxtHashDocument() {
		super();
	}

	@Override
	public byte[] generate() throws DocumentException {

		final StringBuilder buffer = new StringBuilder();
		buffer.append(";charset=").append(getCharset()).append("\r\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append(";hashAlgorithm=").append(getAlgorithm()).append("\r\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append(";recursive=").append(isRecursive()).append("\r\n"); //$NON-NLS-1$ //$NON-NLS-2$

		final Map<String, byte[]> hashes = getHashes();
		final Iterator<String> it = hashes.keySet().iterator();
		while (it.hasNext()) {
			final String path = it.next();
			buffer.append(path).append(";").append(AOUtil.hexify(hashes.get(path), false)).append("\r\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return buffer.toString().getBytes(getCharset());
	}

	@Override
	void load(final byte[] document) throws DocumentException, IOException {


		// Comenzamos identificando la codificacion
		Charset charset = StandardCharsets.UTF_8;
		if (document.length > 30) {
			final String firstLine = new String(document, 0, 30);
			if (firstLine.startsWith(LINE_HEADER_CHARSET) && firstLine.indexOf("\r") > 0) { //$NON-NLS-1$
				final String charsetName = firstLine.substring(LINE_HEADER_CHARSET.length(), firstLine.indexOf("\r")); //$NON-NLS-1$
				charset = Charset.forName(charsetName);
			}
		}
		else {
			throw new DocumentException("No dispone de la longitud minima para considerarlo un documento de hashes TXT"); //$NON-NLS-1$
		}

		// Analizamos el fichero cargandolo con la codificacion resultante
		try (	ByteArrayInputStream bais = new ByteArrayInputStream(document);
				InputStreamReader isr = new InputStreamReader(bais, charset);
				BufferedReader reader = new BufferedReader(isr)) {

			// Procesamos la cabeceras
			boolean processingHeaders = true;
			String line;
			do {
				line = reader.readLine();
				if (line.startsWith(LINE_HEADER_PREFIX)) {
					if (line.startsWith(LINE_HEADER_ALGORITHM)) {
						setAlgorithm(line.substring(LINE_HEADER_ALGORITHM.length()));
					}
					else if (line.startsWith(LINE_HEADER_RECURSIVE)) {
						setRecursive(Boolean.parseBoolean(line.substring(LINE_HEADER_RECURSIVE.length())));
					}
					else if (line.startsWith(LINE_HEADER_CHARSET)) {
						try {
							setCharset(Charset.forName(line.substring(LINE_HEADER_CHARSET.length())));
						}
						catch (final Exception e) {
							LOGGER.log(Level.WARNING, "Juego de caracteres no reconocido. Se usara el por defecto: " + getCharset(), e); //$NON-NLS-1$
						}
					}
					// Ignoramos cualquier otro valor por compatibilidad con versiones futuras
				}
				else {
					processingHeaders = false;
				}
			} while (processingHeaders);

			// Procesamos los hashes cuidando de que se mantenga el formato
			final Map<String, byte[]> hashes = getHashes();
			while(line != null) {
				if (line.length() > 0) {
					final int pos = line.indexOf(";"); //$NON-NLS-1$
					if (pos <= 0 || pos == line.length() - 1) {
						throw new DocumentException("Se encontro una linea de hash no compatible"); //$NON-NLS-1$
					}

					// Descodificamos el hexadecimal (CUIDADO: esto no fallara si la cadena no es hexadecimal)
					final byte[] hash = HexUtils.hexStringToByteArray(line.substring(pos + 1));
					hashes.put(line.substring(0, pos), hash);
				}
				line = reader.readLine();
			}
		}
	}
}
