package es.gob.afirma.signfolder.server.proxy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.core.misc.Base64;


/** Datos temporales de un documento para su firma en tres fases
 * @author Carlos Gamuci Mill&aacute;n */
public final class TriphaseSignDocumentRequest {

	public static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$
	public static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$
	public static final String CRYPTO_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	private static final String DEFAULT_ALGORITHM = "SHA-512"; //$NON-NLS-1$

	private static final String DEFAULT_CRYPTO_OPERATION = "sign"; //$NON-NLS-1$

	/** Identificador del documento. */
	private final String id;

	/** Operaci&oacute;n que se debe realizar sobre el documento (sign, cosign o countersign). */
	private String cryptoOperation;
	
	/** Formato de firma electr&oacute;nica que se desea utilizar. */
	private final String signatureFormat;

	/** Propiedades de configuracion de la firma codificadas en Base64. */
	private String params;

	/** Contenido del documento en Base64 URL SAFE. */
	private String content;

	/** Atributos de configuracion trifasica de la firma. */
	private TriphaseConfigDataBean partialResult;

	/** Algoritmo de firma. */
	private final String algorithm;

	/** Resultado de la firma del documento. */
	private byte[] result;
	
	/** Construye un objeto petici&oacute;n de prefirma de un documento.
	 * @param docId Identificador del documento.
	 * @param signatureFormat Formato de firma electr&oacute;nica.
	 * @param messageDigestAlgorithm Algoritmo de huella digital utilizado en la operaci&oacute;n de firma.
	 * @param params Par&aacute;metros de configuraci&oacute;n de la prefirma.
	 */
	TriphaseSignDocumentRequest(final String docId, final String signatureFormat, final String messageDigestAlgorithm, final String params) {
		this(docId, DEFAULT_CRYPTO_OPERATION, signatureFormat, messageDigestAlgorithm, params, null, null);
	}

	/** Construye un objeto petici&oacute;n de postfirma de un documento.
	 * @param docId Identificador del documento.
	 * @param cryptoOperation Operaci&oacute;n criptogr&aacute;fica a realizar (sign, cosign o countersign).
	 * @param signatureFormat Formato de firma electr&oacute;nica.
	 * @param messageDigestAlgorithm Algoritmo de huella digital utilizado en la operaci&oacute;n de firma.
	 * @param params Propiedades de configuraci&oacute;n utilizadas en la prefirma codificadas en base 64.
	 * @param contentB64 Contenido del documento en base 64 URL SAFE.
	 * @param partialResult Resultado parcial de la firma trifasica. */
	TriphaseSignDocumentRequest(final String docId,
								final String cryptoOperation,                    
								final String signatureFormat,
			                    final String messageDigestAlgorithm,
			                    final String params,
			                    final String contentB64,
			                    final TriphaseConfigDataBean partialResult) {
		this.id = docId;
		this.cryptoOperation = cryptoOperation == null ? DEFAULT_CRYPTO_OPERATION : cryptoOperation;
		this.signatureFormat = signatureFormat;
		this.params = params;
		this.content = contentB64;
		this.partialResult = partialResult;
		this.algorithm = (messageDigestAlgorithm != null) ? messageDigestAlgorithm : DEFAULT_ALGORITHM;
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
	
	/**
	 * Establece el tipo de operaci&oacute;n criptogr&aacute;fica que debe realizarse sobre
	 * el documento.
	 * @param cryptoOperation Operaci&oacute;n criptogr&aacute;fica.
	 */
	public void setCryptoOperation(final String cryptoOperation) {
		this.cryptoOperation = cryptoOperation;
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

	/** Establece las propiedades de configuraci&oacute;n para la firma.
	 * @param params Propiedades de configuraci&oacute;n codificadas en Base64. */
	public void setParams(final String params) {
		this.params = params;
	}
	
	/** Recupera el contenido del documento en Base64 URL SAFE.
	 * @return Contenido del documento codificado en base64 URL SAFE o {@code null} si
	 * no se ha establecido. */
	public String getContent() {
		return this.content;
	}
	
	/** Establece el contenido del documento en Base64 URL SAFE.
	 * @param content Contenido del documento codificado en base64 URL SAFE. */
	public void setContent(String content) {
		this.content = content;
	}
	
	/** Recupera la configuraci&oacute;n del resultado parcial obtenido en la fase actual de la firma.
	 * @return Datos actuales de la firma (que dependen de la fase en la que est&eacute;)
	 * o {@code null} si aun no se ha iniciado el proceso y por lo tanto todav&iacute;a no hay ning&uacute;n resultado. */
	public TriphaseConfigDataBean getPartialResult() {
		return this.partialResult;
	}

	/** Establece el resultado parcial de la firma en tres fases.
	 * @param result Resultado parcial de la firma en tres fases (su significado depende de la fase actual de firma) */
	public void setPartialResult(final TriphaseConfigDataBean result) {
		this.partialResult = result;
	}

	/** Recupera el resultado final de la firma.
	 * @return Firma del documento. */
	public byte[] getResult() {
		return this.result;
	}
	
	/** Establece el resultado final de la firma.
	 * @param result Firma del documento. */
	public void setResult(final byte[] result) {
		this.result = result;
	}

	/**
	 * Clase que almacena los resultados parciales de la firma trif&aacute;sica. 
	 */
	public static class TriphaseConfigDataBean {
		
		private static final String NODE_PART_1 = "<p k='"; //$NON-NLS-1$
		private static final String NODE_PART_2 = "'>"; //$NON-NLS-1$
		private static final String NODE_PART_3 = "</p>"; //$NON-NLS-1$
		
		private final List<String> preSign;
		private Boolean needPreSign;
		private Boolean needData;
		private Integer signCount;
		private final List<String> session;
		private final List<String> pk1;
		
		public TriphaseConfigDataBean() {
			this.preSign = new ArrayList<String>();
			this.needPreSign = null;
			this.needData = null;
			this.signCount = null;
			this.session = new ArrayList<String>();
			this.pk1 = new ArrayList<String>();
		}
		
		public String getPreSign(int index) {
			return this.preSign.get(index);
		}

		public void addPreSign(String preSign) {
			this.preSign.add(preSign);
		}

		public Boolean isNeedPreSign() {
			return this.needPreSign;
		}

		public void setNeedPreSign(Boolean needPreSign) {
			this.needPreSign = needPreSign;
		}

		public Boolean isNeedData() {
			return this.needData;
		}

		public void setNeedData(Boolean needData) {
			this.needData = needData;
		}
		
		public Integer getSignCount() {
			return this.signCount;
		}

		public void setSignCount(Integer signCount) {
			this.signCount = signCount;
		}

		public String getSession(int index) {
			return this.session.get(index);
		}

		/**
		 * Los datos de sesi&oacute;n siempre se almacenan como Base64.
		 * @param session Datos de sesi&oacute;n.
		 */
		public void addSession(String session) {
			this.session.add(session);
		}

		public String getPk1(int index) {
			return this.pk1.get(index);
		}

		public void addPk1(String pk1) {
			this.pk1.add( pk1);
		}

		public String toXMLConfig() {
			final StringBuilder builder = new StringBuilder();
			if (this.signCount != null)
			builder.append(NODE_PART_1).append("sc").append(NODE_PART_2).append(this.signCount.intValue()).append(NODE_PART_3); //$NON-NLS-1$
			
			if (this.needData != null) {
				builder.append(NODE_PART_1).append("nd").append(NODE_PART_2).append(this.needData.booleanValue()).append(NODE_PART_3); //$NON-NLS-1$
			}
			
			if (this.needPreSign != null) {
				builder.append(NODE_PART_1).append("np").append(NODE_PART_2).append(this.needPreSign.booleanValue()).append(NODE_PART_3); //$NON-NLS-1$
			}

			if (this.preSign != null) {
				for (int i = 0; i < this.preSign.size(); i++) {
					builder.append(NODE_PART_1).append("pre.").append(i).append(NODE_PART_2).append(this.preSign.get(i)).append(NODE_PART_3); //$NON-NLS-1$
				}
			}
			
			if (this.session != null) {
				for (int i = 0; i < this.session.size(); i++) {
					if (this.session.get(i) != null) {
						builder.append(NODE_PART_1).append("ss.").append(i).append(NODE_PART_2).append(this.session.get(i)).append(NODE_PART_3); //$NON-NLS-1$
					}
				}
			}
			
			if (this.pk1 != null) {
				for (int i = 0; i < this.pk1.size(); i++) {
					builder.append(NODE_PART_1).append("pk1.").append(i).append(NODE_PART_2).append(this.pk1.get(i)).append(NODE_PART_3); //$NON-NLS-1$
				}
			}
			
			return builder.toString();
		}
		
		/**
		 * Devuelve la cadena Base 64 URL SAFE resultado de codificar la cadena que representa los datos
		 * de la configuraci&oacute;n del objeto representados como un Properties.
		 * @return Cadena Base 64 URL SAFE.
		 * @throws IOException  Cuando los datos de sesi&oacute;n no estaban correctamente codificados.
		 */
		public String toPropertiesBase64() throws IOException {
			final StringBuilder builder = new StringBuilder();
			if (this.signCount != null) {
				builder.append("SIGN_COUNT=").append(this.signCount.intValue()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (this.needPreSign != null) {
				builder.append("NEED_DATA=").append(this.needData.booleanValue()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			
			if (this.needPreSign != null) {
				if (this.needPreSign.booleanValue() && this.preSign != null) {
					builder.append("NEED_PRE=").append(this.needPreSign.booleanValue()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
					for (int i = 0; i < this.preSign.size(); i++) {
						builder.append("PRE.").append(i).append("=").append(this.preSign.get(i)).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}
				}
			}
			if (this.session != null) {
				try {
					for (int i = 0; i < this.session.size(); i++) {
						builder.append("SESSION.").append(i).append("=").append(new String(Base64.decode(this.session.get(i)))).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}
				} catch (IOException e) {
					throw new IOException("Error en la codificacion de los datos de sesion", e); //$NON-NLS-1$
				}
			}
			if (this.pk1 != null) {
				for (int i = 0; i < this.pk1.size(); i++) {
					builder.append("PK1.").append(i).append("=").append(this.pk1.get(i)).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
			}
			
			try {
				return Base64.encodeBytes(builder.toString().getBytes(), Base64.URL_SAFE);
			} catch (Exception e) {
				// Este caso nunca se dara
				return null;
			}
			
		}
	}
}
