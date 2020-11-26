package es.gob.afirma.plugin.hash;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

/**
 * Documento con los hashes calculados de los ficheros de un directorio.
 */
public abstract class HashDocument {

	private static final String DEFAULT_ALGORITHM = "SHA-256"; //$NON-NLS-1$

	private Map<String, byte[]> hashes;

	private boolean recursive;

	private String algorithm;

	private Charset charset;

	public HashDocument() {
		this.hashes = new HashMap<>();
		this.recursive = false;
		this.algorithm = DEFAULT_ALGORITHM;
		this.charset = StandardCharsets.UTF_8;
	}

	public Map<String, byte[]> getHashes() {
		return this.hashes;
	}

	public void setHashes(final Map<String, byte[]> hashes) {
		this.hashes = hashes;
	}

	public boolean isRecursive() {
		return this.recursive;
	}

	public void setRecursive(final boolean recursive) {
		this.recursive = recursive;
	}

	public String getAlgorithm() {
		return this.algorithm;
	}

	public void setAlgorithm(final String algorithm) {
		this.algorithm = algorithm;
	}

	public Charset getCharset() {
		return this.charset;
	}

	public void setCharset(final Charset charset) {
		this.charset = charset;
	}

	/**
	 * Genera el documento de hashes.
	 * @return Contenido del documento.
	 * @throws DocumentException Cuando ocurre un error durante la generaci&oacute;n del documento.
	 */
	public abstract byte[] generate() throws DocumentException;

	/**
	 * Carga un documento de hashes.
	 * @param document Contenido del documento de hashes.
	 * @throws DocumentException Cuando el documento de hashes no tiene el formato esperado.
	 * @throws IOException Cuando no se puede cargar el documento u otro recurso requerido.
	 * @throws CorruptedDocumentException Cuando se ha encontrado que el documento tiene un
	 * formato correcto (hasta el momento de identificar el actual problema), pero que los
	 * datos proporcionados no pueden ser v&aacute;lidos.
	 */
	abstract void load(byte[] document) throws DocumentException, IOException, CorruptedDocumentException;
}
