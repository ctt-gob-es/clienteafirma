package es.gob.afirma.standalone.ui;

import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signvalidation.SignValidity;

/** Configuraci&oacute;n de una operaci&oacute;n de firma. */
public final class SignOperationConfig {

	private FileType fileType;
	private File dataFile;
	private File signatureFile;
	private CryptoOperation cryptoOperation;
	private AOSigner signer;
	private Properties extraParams;
	private String signatureFormatName;
	private List<SignValidity> signValidity;
	private String invalidSignatureText;
	private String digestAlgorithm;

	/** Construye la configuraci&oacute;n de firma. */
	public SignOperationConfig() {
		this.fileType = FileType.BINARY;
		this.signer = null;
		this.signatureFormatName = null;
		this.dataFile = null;
		this.signatureFile = null;
		this.cryptoOperation = CryptoOperation.SIGN;
	}

	/** Recupera el tipo de fichero que se va a firmar.
	 * @return Tipo de fichero. */
	public FileType getFileType() {
		return this.fileType;
	}

	/** Establece el tipo de fichero que se va a firmar.
	 * @param fileType Tipo de fichero. */
	public void setFileType(final FileType fileType) {
		this.fileType = fileType;
	}

	/** Recupera el manejador de firma que se va a utilizar.
	 * @return Manejador de firma. */
	public AOSigner getSigner() {
		return this.signer;
	}

	/** Establece el manejador de firma que se va a utilizar.
	 * @param signer Manejador de firma. */
	public void setSigner(final AOSigner signer) {
		this.signer = signer;
	}

	/** Recupera la configuraci&oacute;n de firma que se va a utilizar.
	 * @return Configuraci&oacute; de firma. */
	public Properties getExtraParams() {
		return this.extraParams;
	}

	/** Establece la configuraci&oacute;n de firma que se va a utilizar.
	 * Si se indica <code>null</code> se establece un objeto de propiedades vac&iacute;o.
	 * @param xParams Configuraci&oacute; de firma. */
	public void setExtraParams(final Properties xParams) {
		this.extraParams = xParams != null ? xParams : new Properties();
	}

	/** A&ntilde;ade una colecci&oacute;n de par&aacute;metros adicionales a la configuraci&oacute;n.
	 * @param xParams Colecci&oacute;n de par&aacute;metros adicionales. */
	public void addExtraParams(final Properties xParams) {
		if (xParams == null) {
			return;
		}
		if (this.extraParams == null) {
			this.extraParams = new Properties();
		}
		this.extraParams.putAll(xParams);
	}

	/** A&ntilde;ade una colecci&oacute;n de par&aacute;metros adicionales a la configuraci&oacute;n.
	 * @param key Clave del par&aacute;metro adicional.
	 * @param value Valor del par&aacute;metro adicional. */
	public void addExtraParam(final String key, final String value) {
		if (key == null || value == null) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No se pueden establecer parametros adicionales nulos" //$NON-NLS-1$
			);
		}
		if (this.extraParams == null) {
			this.extraParams = new Properties();
		}
		this.extraParams.put(key, value);
	}

	/** Recupera el texto descriptivo del formato de firma que se va a utilizar.
	 * @return Descripci&oacute;n del formato. */
	public String getSignatureFormatName() {
		return this.signatureFormatName;
	}

	/** Establece el texto descriptivo del formato de firma que se va a utilizar.
	 * @param signatureFormatName Descripci&oacute;n del formato. */
	public void setSignatureFormatName(final String signatureFormatName) {
		this.signatureFormatName = signatureFormatName;
	}

	/** Recupera la informaci&oacute;n de validaci&oacute;n de la firma.
	 * @return Resultado de la validaci&oacute;n de la firma. */
	public List<SignValidity> getSignValidity() {
		return this.signValidity;
	}

	/** Establece la informaci&oacute;n de validaci&oacute;n de la firma.
	 * @param signValidity Resultado de la validaci&oacute;n de la firma. */
	public void setSignValidity(final List<SignValidity> signValidity) {
		this.signValidity = signValidity;
	}

	/** Recupera el texto descriptivo del error de la firma cargada.
	 * @return Error de la firma o {@code null} si no hay ninguno. */
	public String getInvalidSignatureText() {
		return this.invalidSignatureText;
	}

	/** Establece el texto descriptivo del error de la firma cargada.
	 * @param invalidSignatureText Error de la firma. */
	public void setInvalidSignatureText(final String invalidSignatureText) {
		this.invalidSignatureText = invalidSignatureText;
	}

	/** Recupera el fichero con los datos que se van a firmar.
	 * @return Fichero de datos. */
	public File getDataFile() {
		return this.dataFile;
	}

	/** Establece el fichero con los datos que se van a firmar.
	 * @param file Fichero de datos. */
	public void setDataFile(final File file) {
		this.dataFile = file;
	}

	/** Recupera el fichero de firma que se va a generar.
	 * @return Fichero de firma. */
	public File getSignatureFile() {
		return this.signatureFile;
	}

	/** Establece el fichero de firma que se va a generar.
	 * @param file Fichero de firma. */
	public void setSignatureFile(final File file) {
		this.signatureFile = file;
	}

	/**
	 * Recupera la operaci&oacute;n criptogr&aacute;fica de firma que se va a utilizar.
	 * @return Operaci&oacute;n criptogr&aacute;fica de firma.
	 */
	public CryptoOperation getCryptoOperation() {
		return this.cryptoOperation;
	}

	/**
	 * Establece la operaci&oacute;n criptogr&aacute;fica de firma que se va a utilizar.
	 * @param cryptoOperation Operaci&oacute;n criptogr&aacute;fica de firma.
	 */
	public void setCryptoOperation(final CryptoOperation cryptoOperation) {
		this.cryptoOperation = cryptoOperation;
	}

	/**
	 * Recupera el algoritmo de huella usado como parte del algoritmo de firma.
	 * @return Algoritmo de huella usado como parte del algoritmo de firma.
	 */
	public String getDigestAlgorithm() {
		return this.digestAlgorithm;
	}

	/**
	 * Establece el algoritmo de huella usado como parte del algoritmo de firma.
	 * @param signatureAlgorithm Algoritmo de huella usado como parte del algoritmo de firma.
	 */
	public void setDigestAlgorithm(final String signatureAlgorithm) {
		this.digestAlgorithm = signatureAlgorithm;
	}

	@Override
	protected SignOperationConfig clone() {

		// La copia contendra los mismos elementos, ha excepcion de
		// los extraParams, que sera una copia de ellos
		final SignOperationConfig config = new SignOperationConfig();
		config.setFileType(this.fileType);
		config.setDataFile(this.dataFile);
		config.setSignatureFile(this.signatureFile);
		config.setCryptoOperation(this.cryptoOperation);
		config.setSigner(this.signer);
		config.setExtraParams(this.extraParams != null ? (Properties) this.extraParams.clone() : null);
		config.setSignatureFormatName(this.signatureFormatName);
		config.setSignValidity(this.signValidity != null ? Collections.unmodifiableList(this.signValidity) : null);
		config.setInvalidSignatureText(this.invalidSignatureText);
		config.setDigestAlgorithm(this.digestAlgorithm);

		return config;
	}

	/** Operaci&oacute;n criptogr&aacute;fica que debe aplicarse durante
	 * un proceso de firma. */
	public enum CryptoOperation {
		/** Firma electr&oacute;nica simple. */
		SIGN,
		/** Cofirma. */
		COSIGN,
		/** Contrafirma de los nodos hoja. */
		COUNTERSIGN_LEAFS,
		/** Contrafirma de todos los nodos de firma. */
		COUNTERSIGN_TREE
	}
}
