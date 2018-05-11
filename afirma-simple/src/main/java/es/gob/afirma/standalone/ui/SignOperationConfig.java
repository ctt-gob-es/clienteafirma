package es.gob.afirma.standalone.ui;

import java.io.File;
import java.util.Properties;

import es.gob.afirma.core.signers.AOSigner;

/**
 * Configuraci&oacute;n de una operaci&oacute;n de firma.
 */
public class SignOperationConfig {

	private FileType fileType;
	private File dataFile;
	private File signatureFile;
	private CryptoOperation cryptoOperation;
	private AOSigner signer;
	private Properties extraParams;
	private String signatureFormatName;

	/**
	 * Construye la configuraci&oacute;n de firma.
	 */
	public SignOperationConfig() {
		this.fileType = FileType.BINARY;
		this.signer = null;
		this.signatureFormatName = null;
		this.dataFile = null;
		this.signatureFile = null;
		this.cryptoOperation = CryptoOperation.SIGN;
	}

	/**
	 * Recupera el tipo de fichero que se va a firmar.
	 * @return Tipo de fichero.
	 */
	public FileType getFileType() {
		return this.fileType;
	}
	/**
	 * Establece el tipo de fichero que se va a firmar.
	 * @param fileType Tipo de fichero.
	 */
	public void setFileType(FileType fileType) {
		this.fileType = fileType;
	}
	/**
	 * Recupera el manejador de firma que se va a utilizar.
	 * @return Manejador de firma.
	 */
	public AOSigner getSigner() {
		return this.signer;
	}
	/**
	 * Establece el manejador de firma que se va a utilizar.
	 * @param signer Manejador de firma.
	 */
	public void setSigner(AOSigner signer) {
		this.signer = signer;
	}
	/**
	 * Recupera la configuraci&oacute;n de firma que se va a utilizar.
	 * @return Configuraci&oacute; de firma.
	 */
	public Properties getExtraParams() {
		return this.extraParams;
	}
	/**
	 * Establece la configuraci&oacute;n de firma que se va a utilizar.
	 * @param extraParams Configuraci&oacute; de firma.
	 */
	public void setExtraParams(Properties extraParams) {
		this.extraParams = extraParams;
	}
	/**
	 * Recupera el texto descriptivo del formato de firma que se va a utilizar.
	 * @return Descripci&oacute;n del formato.
	 */
	public String getSignatureFormatName() {
		return this.signatureFormatName;
	}
	/**
	 * Establece el texto descriptivo del formato de firma que se va a utilizar.
	 * @param signatureFormatName Descripci&oacute;n del formato.
	 */
	public void setSignatureFormatName(String signatureFormatName) {
		this.signatureFormatName = signatureFormatName;
	}
	/**
	 * Recupera el fichero con los datos que se van a firmar.
	 * @return Fichero de datos.
	 */
	public File getDataFile() {
		return this.dataFile;
	}
	/**
	 * Establece el fichero con los datos que se van a firmar.
	 * @param file Fichero de datos.
	 */
	public void setDataFile(File file) {
		this.dataFile = file;
	}
	/**
	 * Recupera el fichero de firma que se va a generar.
	 * @return Fichero de firma.
	 */
	public File getSignatureFile() {
		return this.signatureFile;
	}
	/**
	 * Establece el fichero de firma que se va a generar.
	 * @param file Fichero de firma.
	 */
	public void setSignatureFile(File file) {
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
	public void setCryptoOperation(CryptoOperation cryptoOperation) {
		this.cryptoOperation = cryptoOperation;
	}

	/**
	 * Operaci&oacute;n criptogr&aacute;fica que debe aplicarse durante
	 * un proces de firma.
	 */
	enum CryptoOperation {
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
