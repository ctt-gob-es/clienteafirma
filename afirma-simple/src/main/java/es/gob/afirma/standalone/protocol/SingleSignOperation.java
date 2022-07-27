package es.gob.afirma.standalone.protocol;

import java.util.Properties;

/**
 * Configuraci&oacute;n de una firma de un lote.
 */
public class SingleSignOperation {

	private String docId;
	private Operation cryptoOperation;
	private byte[] data;
	private String format;
	private String algorithm;
	private Properties extraParams;

	public String getDocId() {
		return this.docId;
	}
	public void setDocId(final String docId) {
		this.docId = docId;
	}
	public Operation getCryptoOperation() {
		return this.cryptoOperation;
	}
	public void setCryptoOperation(final Operation cryptoOperation) {
		this.cryptoOperation = cryptoOperation;
	}
	public byte[] getData() {
		return this.data;
	}
	public void setData(final byte[] data) {
		this.data = data;
	}
	public String getFormat() {
		return this.format;
	}
	public void setFormat(final String format) {
		this.format = format;
	}
	public String getAlgorithm() {
		return this.algorithm;
	}
	public void setAlgorithm(final String algorithm) {
		this.algorithm = algorithm;
	}
	public Properties getExtraParams() {
		return this.extraParams != null ? (Properties) this.extraParams.clone() : null;
	}
	public void setExtraParams(final Properties extraParams) {
		this.extraParams = extraParams != null ? (Properties) extraParams.clone() : null;
	}

	public enum Operation {
		/** Operaci&oacute;n de firma. */
		SIGN,

		/** Operaci&oacute;n de cofirma. */
		COSIGN,

		/** Operaci&oacute;n de contrafirma. */
		COUNTERSIGN;

		/** Obtiene el tipo de operaci&oacute;n de firma a partir de su nombre, o <code>null</code>
		 * si el nombre no corresponde a ninguna operaci&oacute;n conocida.
		 * @param opName Nombre de la operaci&oacute;n de firma.
		 * @return Operaci&oacute;n de firma. */
		public static Operation getOperation(final String opName) {
			if ("SIGN".equalsIgnoreCase(opName)) { //$NON-NLS-1$
				return SIGN;
			}
			if ("COSIGN".equalsIgnoreCase(opName)) { //$NON-NLS-1$
				return COSIGN;
			}
			if ("COUNTERSIGN".equalsIgnoreCase(opName)) { //$NON-NLS-1$
				return COUNTERSIGN;
			}
			return null;
		}
	}
}
