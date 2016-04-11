package es.gob.afirma.android.signfolder.proxy;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;

import es.gob.afirma.core.misc.Base64;

/** Datos temporales de un documento para su firma en tres fases
 * @author Carlos Gamuci Mill&aacute;n */
public final class TriphaseSignDocumentRequest {

	static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$
	static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$
	static final String CRYPTO_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	private static final String DEFAULT_ALGORITHM = "SHA-512"; //$NON-NLS-1$

	private static final String DEFAULT_CRYPTO_OPERATION = "sign"; //$NON-NLS-1$

	/** Identificador del documento. */
	private final String id;

	/** Operaci&oacute;n que se debe realizar sobre el documento (sign, cosign o countersign). */
	private final String cryptoOperation;

	/** Formato de firma electr&oacute;nica que se desea utilizar. */
	private final String signatureFormat;

	/** Propiedades de configuracion de la firma codificadas en Base64. */
	private final String params;

	/** Atributos de configuracion trifasica de la firma. */
	private TriphaseConfigData partialResult;

	/** Algoritmo de firma. */
	private final String algorithm;

	/** Construye un objeto petici&oacute;n de prefirma de un documento.
	 * @param docId Identificador del documento.
	 * @param signatureFormat Formato de firma electr&oacute;nica.
	 * @param messageDigestAlgorithm Algoritmo de huella digital utilizado en la operaci&oacute;n de firma.
	 * @param params Par&aacute;metros de configuraci&oacute;n de la prefirma.
	 */
	TriphaseSignDocumentRequest(final String docId, final String signatureFormat, final String messageDigestAlgorithm, final String params) {
		this(docId, DEFAULT_CRYPTO_OPERATION, signatureFormat, messageDigestAlgorithm, params, null);
	}

	/** Construye un objeto petici&oacute;n de postfirma de un documento.
	 * @param docId Identificador del documento.
	 * @param cryptoOperation Operaci&oacute;n criptogr&aacute;fica a realizar (sign, cosign o countersign).
	 * @param signatureFormat Formato de firma electr&oacute;nica.
	 * @param messageDigestAlgorithm Algoritmo de huella digital utilizado en la operaci&oacute;n de firma.
	 * @param params Propiedades de configuraci&oacute;n utilizadas en la prefirma codificadas en base 64.
	 * @param partialResult Resultado parcial de la firma trifasica. */
	TriphaseSignDocumentRequest(final String docId,
								final String cryptoOperation,
			                    final String signatureFormat,
			                    final String messageDigestAlgorithm,
			                    final String params,
			                    final TriphaseConfigData partialResult) {
		this.id = docId;
		this.cryptoOperation = cryptoOperation;
		this.signatureFormat = signatureFormat;
		this.params = params;
		this.partialResult = partialResult;
		this.algorithm = messageDigestAlgorithm != null ? messageDigestAlgorithm : DEFAULT_ALGORITHM;
	}

	/** Recupera el identificador del documento.
	 * @return Identificador del documento. */
	public String getId() {
		return this.id;
	}

	/** Recupera el identificador de la operaci&oacute;n a realizar.
	 * @return Identificador de la operaci&oacute;n. */
	public String getCryptoOperation() {
		return this.cryptoOperation;
	}

	/** Recupera el formato de firma.
	 * @return Formato de firma. */
	public String getSignatureFormat() {
		return this.signatureFormat;
	}

	/** Recupera el algoritmo de huella digital de la operaci&oacute;n de firma.
	 * @return Algoritmo de huella digital. */
	public String getMessageDigestAlgorithm() {
		return this.algorithm;
	}

	/** Recupera las propiedades de configuraci&oacute;n para la firma.
	 * @return Propiedades de configuraci&oacute;n codificadas en Base64 o {@code null} si
	 * no hay par&aacute;metros establecidos. */
	public String getParams() {
		return this.params;
	}

	/** Recupera la configuraci&oacute;n del resultado parcial obtenido en la fase actual de la firma.
	 * @return Datos actuales de la firma (que dependen de la fase en la que est&eacute;)
	 * o {@code null} si aun no se ha iniciado el proceso y por lo tanto todav&iacute;a no hay ning&uacute;n resultado. */
	public TriphaseConfigData getPartialResult() {
		return this.partialResult;
	}

	/** Establece el resultado parcial de la firma en tres fases.
	 * @param result Resultado parcial de la firma en tres fases (su significado depende de la fase actual de firma) */
	public void setPartialResult(final TriphaseConfigData result) {
		this.partialResult = result;
	}

	/** Clase que almacena los resultados parciales de la firma trif&aacute;sica. */
	public static final class TriphaseConfigData extends HashMap<String, String> {

		/** Serial Id. */
		private static final long serialVersionUID = 6376166034628843888L;

		private static final String NODE_PART_1 = "<p n='"; //$NON-NLS-1$
		private static final String NODE_PART_2 = "'>"; //$NON-NLS-1$
		private static final String NODE_PART_3 = "</p>"; //$NON-NLS-1$

		private static final String PARAM_PRE = "PRE"; //$NON-NLS-1$
		private static final String PARAM_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$
		private static final String PARAM_NEED_DATA = "NEED_DATA"; //$NON-NLS-1$
		private static final String PARAM_PKCS1 = "PK1"; //$NON-NLS-1$

		/** Obtiene la prefirma del documento.
		 * @return Prefirma.
		 * @throws IOException El valor no es un base64 v&aacute;lido. */
		public byte[] getPreSign() throws IOException {
			return Base64.decode(get(PARAM_PRE));
		}

		/** Indica si el proceso necesita la prefirma para realizar la postfirma.
		 * @return <code>true</code> si el proceso necesita la prefirma,
		 *         <code>false</code> en caso contrario (por defecto). */
		public Boolean isNeedPreSign() {
			return Boolean.valueOf(get(PARAM_NEED_PRE));
		}

		/** Indica si el proceso necesita los datos para realizar la postfirma.
		 * @return <code>true</code> si el proceso necesita los datos (por defecto),
		 *         <code>false</code> en caso contrario. */
		public Boolean isNeedData() {
			String needData = get(PARAM_NEED_DATA);
			if (needData == null) {
				needData = Boolean.TRUE.toString();
			}
			return Boolean.valueOf(needData);
		}

		/** Elimina la prefirma de entre los datos de la operaci&oacute;n. */
		public void removePreSign() {
			remove(PARAM_PRE);
		}

		/** Obtiene la firma PKCS#1 de la firma indicada.
		 * @param index &Iacute;ndice de la firma de la cual se quiere obtener el PKCS#1.
		 * @return Firma PKCS#1.
		 * @throws IOException El valor no es un base64 v&aacute;lido. */
		public byte[] getPk1(final int index) throws IOException {
			return Base64.decode(get(PARAM_PKCS1));
		}

		/** Almacena la firma PKCS#1.
		 * @param pkcs1 Firma PKCS#1. */
		public void setPk1(final byte[] pkcs1) {
			put(PARAM_PKCS1, Base64.encode(pkcs1));
		}

		/**
		 * Devuelve la configuraci&oacute;n a modo de listado XML.
		 * @return Cadena con el listado XML de elementos.
		 */
		public String toXMLParamList() {

			String key;
			final StringBuilder builder = new StringBuilder();
			final Iterator<String> it = keySet().iterator();
			while (it.hasNext()) {
				key = it.next();
				builder.append(NODE_PART_1).append(key).append(NODE_PART_2).append(get(key)).append(NODE_PART_3);
			}

			return builder.toString();
		}
	}
}
