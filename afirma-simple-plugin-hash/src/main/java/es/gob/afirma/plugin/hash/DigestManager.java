/*
 * Este fichero forma parte de la plataforma de @firma.
 * La plataforma de @firma es de libre distribuci&oacute;n cuyo c&oacute;digo fuente puede ser consultado
 * y descargado desde http://administracionelectronica.gob.es
 *
 * Copyright 2005-2019 Gobierno de Espa&ntilde;a
 * Este fichero se distribuye bajo las licencias EUPL versi&oacute;n 1.1, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompa&ntilde;a.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

/**
 * <p>Fichero: UtilsHash.java</p>
 * <p>Descripci&oacute;n: </p>
 * <p>Empresa: Telvent Interactiva </p>
 * <p>Fecha creaci&oacute;n: 12-enero-2006</p>
 * @author Jorge
 * @version 1.0
 *
 */
package es.gob.afirma.plugin.hash;

import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;


/**
 * Clase con utilidades para c&aacute;lculo y gesti&oacute;n de hashes
 *
 * @author Jorge
 */
public class DigestManager {

	private MessageDigest md = null;

	// Tamanos para el calculo de hashes por trozos
	// 1 KB
	public static final int SIZE_1KB = 1000;
	// 10 KB
	public static final int SIZE_10KB = 10000;
	// 100 KB
	public static final int SIZE_100KB = 100000;
	// 1 MB
	public static final int SIZE_1MB = 1000000;
	// 5 MB
	public static final int SIZE_5MB = 5 * 1024 * 1024;
	// 15 MB
	public static final int SIZE_15MB = 15 * 1024 * 1024;
	// 25 MB
	public static final int SIZE_25MB = 25 * 1024 * 1024;

	/**
	 * Constructor. Crea una instancia MessageDigest para el procesamiento
	 * de hashes
	 *
	 * @param algorithm Algoritmo Hash con el cual realizar las operaciones
	 * @param provider Proveedor criptogr&aacute;fico. Si es null se coger&aacute; el proveedor por defecto.
	 * @throws NoSuchAlgorithmException En caso que el algoritmo no est&eacute; soportado
	 */
	public DigestManager(final String algorithm, final Provider provider) throws NoSuchAlgorithmException {
		try {
			if (provider != null) {
				this.md = MessageDigest.getInstance(algorithm, provider);
			} else {
				this.md = MessageDigest.getInstance(algorithm);
			}
		} catch (final NoSuchAlgorithmException e) {
			throw e;
		}
	}

	/**
	 * M&eacute;todo que actualiza el digest a calcular
	 *
	 * @param data Datos a a&ntilde;adir al c&aacute;lculo del digest
	 */
	public void addDataToCompute(final byte data) {
		this.md.update(data);
	}

	/**
	 * M&eacute;todo que actualiza el digest a calcular
	 *
	 * @param data Datos a a&ntilde;adir al c&aacute;lculo del digest
	 */
	public void addDataToCompute(final byte[ ] data) {
		this.md.update(data);
	}

	/**
	 * M&eacute;todo que actualiza el digest a calcular, sobre una parte de los datos
	 *
	 * @param data Datos a a&ntilde;adir al c&aacute;lculo del digest
	 * @param offset byte a partir del cual leer la info de los datos
	 * @param len bytes a leer
	 */
	public void addDataToCompute(final byte[ ] data, final int offset, final int len) {
		this.md.update(data, offset, len);
	}

	/**
	 * M&eacute;todo que actualiza el digest a calcular
	 *
	 * @param data Datos a a&ntilde;adir al c&aacute;lculo del digest
	 */
	public void addDataToCompute(final String data) {
		this.md.update(data.getBytes());
	}

	/**
	 * M&eacute;todo que actualiza el digest con los datos proporcionados y realiza
	 * los c&aacute;lculos finales del digest.
	 *
	 * @param data Datos a a&ntilde;adir al c&aacute;lculo del digest
	 * @return El digest resultante
	 */
	public byte[ ] computeHash(final byte[ ] data) {
		// Update the digest with the supplied data and performs the final
		// computation
		return this.md.digest(data);
	}

	/**
	 * M&eacute;todo que actualiza el digest con los datos proporcionados y realiza
	 * los c&aacute;lculos finales del digest.
	 *
	 * @param data Datos a a&ntilde;adir al c&aacute;lculo del digest
	 * @return El digest resultante
	 */
	public byte[ ] computeHash(final String data) {
		// Update the digest with the supplied data and performs the final
		// computation
		return this.md.digest(data.getBytes());
	}

	/**
	 * M&eacute;todo que realiza los c&aacute;lculos finales del digest.

	 * @return El digest resultante
	 */
	public byte[ ] computeHash() {
		// Performs the hash computation
		return this.md.digest();
	}

	/**
	 * M&eacute;todo para el c&aacute;lculo de hashes por trozos
	 *
	 * @param is InputStream a los datos sobre los que calcular el hash
	 * @param size Tama&ntilde;o del trozo parcial sobre el que se va calculando el hash
	 * @return hash de los datos
	 * @throws IOException Cuando falla la creaci&oacute;n del hash.
	 */
	public byte[ ] computeHashOptimized(final InputStream is, final int size) throws IOException {
		final byte[ ] buff = new byte[size];
		int r;
		while ((r = is.read(buff)) > 0) {
			addDataToCompute(buff, 0, r);
		}

		return computeHash();
	}

	/**
	 * M&eacute;todo que compara los dos digests recibidos
	 *
	 * @param hash1 Primer digest a ser comparado
	 * @param hash2 Segundo digest a ser comparado
	 * @return true en caso de ser iguales, false en caso contrario
	 */
	public static synchronized boolean equalHashes(final byte[ ] hash1, final byte[ ] hash2) {
		return MessageDigest.isEqual(hash1, hash2);
	}
}
