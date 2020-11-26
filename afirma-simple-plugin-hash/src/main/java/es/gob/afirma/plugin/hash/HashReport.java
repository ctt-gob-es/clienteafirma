package es.gob.afirma.plugin.hash;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Informe de la comprobaci&oacute;n de hashes. Permite ir agregando las rutas de los
 * ficheros y hashes comprobados al informe seg&uacute;n el resultado obtenido.
 * <p>IMPORTANTE: Una vez se comienzan a recuperarse los listados de rutas de los ficheros
 * para comprobar el resultado, no deben agregarse nuevos ficheros al informe. No hay
 * garant&iacute;a de que los nuevos ficheros aparezcan en los listados.
 */
public class HashReport {

	private final List<String> matchingHash;
	private final List<String> noMatchingHash;
	private final List<String> hashWithoutFile;
	private final List<String> fileWithoutHash;

	private boolean recursive;

	private String algorithm;

	private Charset charset;

	public HashReport() {
		this.matchingHash = Collections.synchronizedList(new ArrayList<>());
		this.noMatchingHash = Collections.synchronizedList(new ArrayList<>());
		this.hashWithoutFile = Collections.synchronizedList(new ArrayList<>());
		this.fileWithoutHash = Collections.synchronizedList(new ArrayList<>());

		this.charset = StandardCharsets.UTF_8;
	}

	public void reportMatchingHash(final String path) {
		synchronized (this.matchingHash) {
			this.matchingHash.add(path);
		}
	}

	public void reportNoMatchingHash(final String path) {
		synchronized (this.noMatchingHash) {
			this.noMatchingHash.add(path);
		}
	}

	public void reportHashWithoutFile(final String path) {
		synchronized (this.hashWithoutFile) {
			this.hashWithoutFile.add(path);
		}
	}

	public void reportFileWithoutHash(final String path) {
		synchronized (this.fileWithoutHash) {
			this.fileWithoutHash.add(path);
		}
	}

	public Iterator<String> getMatchingHashIterator() {
		return this.matchingHash.iterator();
	}

	public Iterator<String> getNoMatchingHashIterator() {
		return this.noMatchingHash.iterator();
	}

	public Iterator<String> getHashWithoutFileIterator() {
		return this.hashWithoutFile.iterator();
	}

	public Iterator<String> getFileWithoutHashIterator() {
		return this.fileWithoutHash.iterator();
	}

	public boolean hasErrors() {
		return this.fileWithoutHash.size() > 0 ||
				this.hashWithoutFile.size() > 0 ||
				this.noMatchingHash.size() > 0;
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
	 * Devuelve la cantidad de ficheros de los que se ha comprobado el hash,
	 * independientemente de que el hash haya sido correcto o no. No cuenta
	 * aquellos ficheros de los que no se tiene el hash.
	 * @return N&uacute;mero de ficheros procesados.
	 */
	public int getProcessedFilesCount() {
		return this.matchingHash.size() + this.noMatchingHash.size();
	}
}
