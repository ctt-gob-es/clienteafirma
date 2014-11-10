package es.gob.afirma.core.misc.protocol;

import java.net.URL;
import java.util.Arrays;
import java.util.Properties;

/** Par&aacute;metros de la URL de llamada a la aplicaci&oacute;n. */
public final class UrlParametersToSign {

	/** Identificador de la operaci&oacute;n de firma. */
	public static final int OP_SIGN = 1;

	/** Identificador de la operaci&oacute;n de cofirma. */
	public static final int OP_COSIGN = 2;

	/** Identificador de la operaci&oacute;n de contrafirma. */
	public static final int OP_COUNTERSIGN = 3;

	private String id;
	private int operation;
	private byte[] desKey;
	private String signFormat;
	private String signAlgorithm;
	private byte[] data;
	private String fileId;
	private URL storageServer;
	private URL retrieveServer;
	private Properties extraParams;
	private String defaultKeyStore;
	private String minimumVerstion;

	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumVersion() {
		return this.minimumVerstion;
	}

	/** Obtiene el nombre del almac&eacute;n de claves a usar por defecto.
	 * @return Nombre del almac&eacute;n de claves a usar por defecto */
	public String getDefaultKeyStore() {
		return this.defaultKeyStore;
	}

	/** Obtiene el identificador de sesi&oacute;n.
	 * @return Identificador de sesi&oacute;n */
	public String getId() {
		return this.id;
	}

	/** Tipo de operaci&oacute;n a realizar (firma, cofirma o contrafirma).
	 * @return Operaci&oacute;n. */
	public int getOperation() {
		return this.operation;
	}

	/** Obtiene la clave DES de cifrado.
	 * @return Clave DES de cifrado */
	public byte[] getDesKey() {
		return this.desKey;
	}

	/** Obtiene el formato de firma.
	 * @return Formato de firma */
	public String getSignatureFormat() {
		return this.signFormat;
	}

	/** Obtiene el algoritmo de firma.
	 * @return Algoritmo de firma */
	public String getSignatureAlgorithm() {
		return this.signAlgorithm;
	}

	/** Obtiene los datos a firmar.
	 * @return Datos a firmar */
	public byte[] getData() {
		return this.data;
	}

	/** Obtiene el identificador de fichero a firmar.
	 * @return Identificador del fichero */
	public String getFileId() {
		return this.fileId;
	}

	/** Obtiene la URL del servlet de almacenamiento temporal en servidor.
	 * @return URL del servlet de almacenamiento temporal en servidor */
	public URL getStorageServletUrl() {
		return this.storageServer;
	}

	/** Obtiene la URL del servlet de recuperaci&oacute;n de ficheros del servidor temporal.
	 * @return URL del servlet de recuperaci&oacute;n de ficheros. */
	public URL getRetrieveServletUrl() {
		return this.retrieveServer;
	}

	/** Obtiene los par&aacute;metros adicionales de la firma.
	 * @return Par&aacute;metros adicionales de la firma */
	public Properties getExtraParams() {
		return this.extraParams;
	}

	UrlParametersToSign() {
		this.data = null;
		this.fileId = null;
		this.retrieveServer = null;
	}

	void setSessionId(final String sessionId) {
		this.id = sessionId;
	}

	void setOperation(final int operation) {
		this.operation = operation;
	}

	void setDesKey(final byte[] key) {
		this.desKey = key != null ? Arrays.copyOf(key, key.length) : null;
	}

	void setSignFormat(final String format) {
		this.signFormat = format;
	}

	void setSignAlgorithm(final String algo) {
		this.signAlgorithm = algo;
	}

	/** Establece los datos a tratar (firmar, guardar, etc.).
	 * @param dat Datos a tratar */
	public void setData(final byte[] dat) {
		this.data = dat != null ? Arrays.copyOf(dat, dat.length) : null;
	}

	void setFileId(final String fileId) {
		this.fileId = fileId;
	}

	void setStorageServletUrl(final URL url) {
		this.storageServer = url;
	}

	void setRetrieveServletUrl(final URL url) {
		this.retrieveServer = url;
	}

	void setExtraParams(final Properties properties) {
		this.extraParams = properties != null ? properties : new Properties();
	}

	void setMinimumVersion(final String minVer) {
		this.minimumVerstion = minVer;
	}

	/** Establece el nombre del almac&eacute;n de claves a usar por defecto.
	 * @param storeName Nombre del almac&eacute;n de claves a usar por defecto */
	void setDefaultKeyStore(final String storeName) {
		this.defaultKeyStore = storeName;
	}

}
