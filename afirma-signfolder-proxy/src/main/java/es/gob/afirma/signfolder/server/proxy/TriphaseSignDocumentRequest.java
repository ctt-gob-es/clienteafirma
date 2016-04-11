package es.gob.afirma.signfolder.server.proxy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;


/** Datos temporales de un documento para su firma en tres fases
 * @author Carlos Gamuci Mill&aacute;n */
public final class TriphaseSignDocumentRequest {

	/** Operaci&oacute;n de firma simple. */
	public static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$

	/** Operaci&oacute;n de firma paralela o cofirma. */
	public static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$

	/** Operaci&oacute;n de firma en cascada o contrafirma. */
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
	private TriphaseData partialResult;

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
			                    final TriphaseData partialResult) {
		this.id = docId;
		this.cryptoOperation = cryptoOperation == null ? DEFAULT_CRYPTO_OPERATION : cryptoOperation;
		this.signatureFormat = signatureFormat;
		this.params = params;
		this.content = contentB64;
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
	 * @return Propiedades de configuraci&oacute;n codificadas en Base64 URL SAFE o {@code null} si
	 * no hay par&aacute;metros establecidos. */
	public String getParams() {
		return this.params;
	}

	/** Establece las propiedades de configuraci&oacute;n para la firma.
	 * @param params Propiedades de configuraci&oacute;n codificadas en Base64 URL SAFE. */
	public void setParams(final String params) {
		this.params = params;
	}

	/** Recupera el contenido del documento en Base64 URL SAFE.
	 * @return Contenido del documento codificado en Base64 URL SAFE o {@code null} si
	 * no se ha establecido. */
	public String getContent() {
		return this.content;
	}

	/** Establece el contenido del documento en Base64 URL SAFE.
	 * @param content Contenido del documento codificado en base64 URL SAFE. */
	public void setContent(final String content) {
		this.content = content;
	}

	/** Recupera la configuraci&oacute;n del resultado parcial obtenido en la fase actual de la firma.
	 * @return Datos actuales de la firma (que dependen de la fase en la que est&eacute;)
	 * o {@code null} si aun no se ha iniciado el proceso y por lo tanto todav&iacute;a no hay ning&uacute;n resultado. */
	public TriphaseData getPartialResult() {
		return this.partialResult;
	}

	/** Establece el resultado parcial de la firma en tres fases.
	 * @param result Resultado parcial de la firma en tres fases (su significado depende de la fase actual de firma) */
	public void setPartialResult(final TriphaseData result) {
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

		private final List<String> preSigns;
		private Boolean needPreSign;
		private Boolean needData;
		private Integer signCount;
		private final List<String> sessions;
		private final List<String> pk1s;

		/**
		 * Inicializa el objeto sin configuraci&oacute;n establecida.
		 */
		public TriphaseConfigDataBean() {
			this.preSigns = new ArrayList<String>();
			this.needPreSign = null;
			this.needData = null;
			this.signCount = null;
			this.sessions = new ArrayList<String>();
			this.pk1s = new ArrayList<String>();
		}

		/**
		 * Recupera la prefirma el elemento indicado.
		 * @param index &Iacute;ndice del elemento del que obtener la prefirma.
		 * @return Prefirma en base 64.
		 */
		public String getPreSign(final int index) {
			return this.preSigns.get(index);
		}

		/**
		 * Agrega una nueva prefirma al listado.
		 * @param preSign Prefirma en base 64.
		 */
		public void addPreSign(final String preSign) {
			this.preSigns.add(preSign);
		}

		/**
		 * Indica si ser&aacute; necesaria la prefirma para generar la postfirma.
		 * @return {@code true} si la prefirma ser&aacute; necesaria, {@code false} en caso contrario.
		 */
		public Boolean isNeedPreSign() {
			return this.needPreSign;
		}

		/**
		 * Establece si la prefirma ser&aacute; necesaria para la generaci&oacute;n de la postfirma.
		 * @param needPreSign {@code true} si la prefirma ser&aacute; necesarios, {@code false} en
		 * caso contrario.
		 */
		public void setNeedPreSign(final Boolean needPreSign) {
			this.needPreSign = needPreSign;
		}

		/**
		 * Indica si ser&aacute;n necesarios los datos para generar la postfirma.
		 * @return {@code true} si los datos ser&aacute;n necesarios, {@code false} en caso contrario.
		 */
		public Boolean isNeedData() {
			return this.needData;
		}

		/**
		 * Establece si los datos ser&aacute;n necesarios para la generaci&oacute;n de la postfirma.
		 * @param needData {@code true} si los datos ser&aacute;n necesarios, {@code false} en caso
		 * contrario.
		 */
		public void setNeedData(final Boolean needData) {
			this.needData = needData;
		}

		/**
		 * Devuelve el n&uacute;mero de firmas.
		 * @return N&acute;mero de firmas.
		 */
		public Integer getSignCount() {
			return this.signCount;
		}

		/**
		 * Establece el n&uacute;mero de firmas.
		 * @param signCount N&acute;mero de firmas.
		 */
		public void setSignCount(final Integer signCount) {
			this.signCount = signCount;
		}

		/**
		 * Obtiene los datos generados como parte de una operaci&oacute;n de firma trif&aacute;sica.
		 * @param index &Iacute;ndice de la firma.
		 * @return Datos generados (Identificadores aleatorios, hora de firma, etc.)
		 */
		public String getSession(final int index) {
			return this.sessions.get(index);
		}

		/**
		 * Los datos de sesi&oacute;n siempre se almacenan como Base64.
		 * @param session Datos de sesi&oacute;n.
		 */
		public void addSession(final String session) {
			this.sessions.add(session);
		}

		/**
		 * Recupera el PKCS#1 de una de las firmas generadas.
		 * @param index &Iacute;ndice de la firma.
		 * @return PKCS#1 de la firma en base 64.
		 */
		public String getPk1(final int index) {
			return this.pk1s.get(index);
		}

		/**
		 * Agrega al listado un nuevo PKCS#1.
		 * @param pk1 PKCS#1 en base 64.
		 */
		public void addPk1(final String pk1) {
			this.pk1s.add( pk1);
		}

		/**
		 * Genera el XML que seliariza la petici&oacute;n de firma trif&aacute;sica (prefirma o
		 * posfirma) de una solicitud de firma.
		 * @return XML de configuraci&oacute;n.
		 */
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

			if (this.preSigns != null) {
				for (int i = 0; i < this.preSigns.size(); i++) {
					builder.append(NODE_PART_1).append("pre.").append(i).append(NODE_PART_2).append(this.preSigns.get(i)).append(NODE_PART_3); //$NON-NLS-1$
				}
			}

			if (this.sessions != null) {
				for (int i = 0; i < this.sessions.size(); i++) {
					if (this.sessions.get(i) != null) {
						builder.append(NODE_PART_1).append("ss.").append(i).append(NODE_PART_2).append(this.sessions.get(i)).append(NODE_PART_3); //$NON-NLS-1$
					}
				}
			}

			if (this.pk1s != null) {
				for (int i = 0; i < this.pk1s.size(); i++) {
					builder.append(NODE_PART_1).append("pk1.").append(i).append(NODE_PART_2).append(this.pk1s.get(i)).append(NODE_PART_3); //$NON-NLS-1$
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
		public String toXMLBase64() throws IOException {

//			<xml>
//			 <firmas>
//			  <firma Id="ca0831a4-75cb-4565-a987-7e4360a79c4d">
//			   <param n="NEED_PRE">true</param>
//			   <param n="TIME">1450084976458</param>
//			   <param n="PRE">MYICGDAYBgkqhkiG9w0BCQMxCwYJKoZIhvcNAQcBME8GCSqGSIb3DQEJBDFCBEAqMU/8p1J6di92xTgFfHJfIegzwX4vJBrX+zgkMH8ix3LcJH+20vsy6KS68mxF+7ptvJ9fCpED+M5MPjYc/LjBMIIBqQYLKoZIhvcNAQkQAi8xggGYMIIBlDCCAZAwggGMMA0GCWCGSAFlAwQCAwUABEBS3pv8h303FIUMZ5H1WSrrrnfXklrnFQir6KwirhOFZy/sdyOa4ibFdwdebh+yP4WtMpKF1Eo+77TbTQQg+o/cMIIBNzCCASmkggElMIIBITELMAkGA1UEBhMCRVMxEjAQBgNVBAgMCUJhcmNlbG9uYTFYMFYGA1UEBwxPQmFyY2Vsb25hIChzZWUgY3VycmVudCBhZGRyZXNzIGF0IGh0dHA6Ly93d3cuYW5mLmVzL2VzL2FkZHJlc3MtZGlyZWNjaW9uLmh0bWwgKTEnMCUGA1UECgweQU5GIEF1dG9yaWRhZCBkZSBDZXJ0aWZpY2FjaW9uMS4wLAYDVQQLDCVBTkYgQXV0b3JpZGFkIEludGVybWVkaWEgZGUgSWRlbnRpZGFkMRowGAYJKoZIhvcNAQkBFgtpbmZvQGFuZi5lczESMBAGA1UEBRMJRzYzMjg3NTEwMRswGQYDVQQDDBJBTkYgQXNzdXJlZCBJRCBDQTECCAIr7Y/Cv8NA</param>
//			   <param n="PID">Wzw1MzIxNWZiNzQ1ODQzYWUxZGNkYTY5ZjA5NDIwNjQ5ZT48MDlkNDI3NzFhYTMwYjEyNWU3NDBiMzBlOWQwNDA4Mzc+XQ==</param>
//			   <param n="PK1">on0c7fJZPYSfUsSl/UT70VNAwH8QBVoYQp2BsgwRlF9nfGsFWmeAkDcnwKj496iB3Owf4cHIfkYjKyTFi2hILIXdxGhKTccmdGjZzIh9DyZVq82YX9RJEQ1kQcEckF0R43aLcCeOTDY1Yy8eTCrFykKKXs5yT/QPs8Wbk4xTy0zLZF7mqxX7CNyh0yT2QxUaQMUG6bQXlUK+FqJiT7cxRT7h+0RafoZgZ1gQaeItFEJu3SMDC79uyyzaQKqDDy4ATEnOgE+SADWrxFORGBNjPQ2lqtYkjaWQj+aXZ9voXzcMz+I6DJjC9P9hHZ71KQa3O0kLjFyjbnF4qKICj9JT5g==</param>
//			  </firma>
//			 </firmas>
//			</xml>

			final StringBuilder builder = new StringBuilder();
			if (this.signCount != null) {
				builder.append("SIGN_COUNT=").append(this.signCount.intValue()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (this.needData != null) {
				builder.append("NEED_DATA=").append(this.needData.booleanValue()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
			}

			if (this.needPreSign != null) {
				if (this.needPreSign.booleanValue() && this.preSigns != null) {
					builder.append("NEED_PRE=").append(this.needPreSign.booleanValue()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
					for (int i = 0; i < this.preSigns.size(); i++) {
						builder.append("PRE.").append(i).append("=").append(this.preSigns.get(i)).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}
				}
			}
			if (this.sessions != null) {
				try {
					for (int i = 0; i < this.sessions.size(); i++) {
						builder.append("SESSION.").append(i).append("=").append(new String(Base64.decode(this.sessions.get(i)))).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}
				} catch (final IOException e) {
					throw new IOException("Error en la codificacion de los datos de sesion", e); //$NON-NLS-1$
				}
			}
			if (this.pk1s != null) {
				for (int i = 0; i < this.pk1s.size(); i++) {
					builder.append("PK1.").append(i).append("=").append(this.pk1s.get(i)).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
			}

			try {
				return Base64.encode(builder.toString().getBytes(), true);
			} catch (final Exception e) {
				// Este caso nunca se dara
				return null;
			}

		}
	}
}
