package es.gob.afirma.android.network;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import android.util.Log;
import es.gob.afirma.core.misc.Base64;

/** Clase de utilidad para el an&aacute;lisis sint&aacute;ctico de URL.
 * @author Alberto Mart&iacute;nez */
public final class UriParser {

	/** Algoritmos de firma soportados. */
	private static final Set<String> SUPPORTED_SIGNATURE_ALGORITHMS = new HashSet<String>();
	static {
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA1withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA256withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA384withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA512withRSA"); //$NON-NLS-1$
	}

	/** Formatos de firma soportados. Siempre en min&uacute;sculas. */
	private static final Set<String> SUPPORTED_SIGNATURE_FORMATS = new HashSet<String>();
	static {
		SUPPORTED_SIGNATURE_FORMATS.add("cades"); //$NON-NLS-1$
	}

	private static final String DEFAULT_URL_ENCODING = "UTF-8"; //$NON-NLS-1$

	private static final String SERVLET_NAME_STORAGE = "/SignatureStorageServer/storage"; //$NON-NLS-1$
	private static final String SERVLET_NAME_RETRIEVE = "/SignatureRetrieverServer/retrieve"; //$NON-NLS-1$

	/** N&uacute;mero m&aacute;ximo de caracteres permitidos para el identificador de sesi&oacute;n de la firma. */
	private static final int MAX_ID_LENGTH = 20;

	/** Longitud permitida para la clave de cifrado. */
	private static final int CIPHER_KEY_LENGTH = 8;

	/** Par&aacute;metro de entrada con el formato de firma. */
	private static final String FORMAT_PARAM = "format"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el algoritmo de firma. */
	private static final String ALGORITHM_PARAM = "algorithm"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con los datos a firmar. */
	private static final String DATA_PARAM = "dat"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el algoritmo de firma. */
	private static final String STORAGE_SERVLET_PARAM = "stservlet"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el identificador del documento. */
	private static final String ID_PARAM = "id"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la clave para el cifrado del documento. */
	private static final String KEY_PARAM = "key"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con las opciones de configuraci&oacute;n de la firma. */
	private static final String PROPERTIES_PARAM = "properties"; //$NON-NLS-1$

    private UriParser() {
        // Constructor privado. No se permite instancias
    }

    /** Analiza la Url de entrada para obtener la lista de par&aacute;metros asociados
     * @param uri Url de llamada
     * @return Devuelve una tabla <i>hash</i> con cada par&aacute;metro asociado a un valor */
    private static Map<String, String> parser(final String uri) {
        final Map<String, String> params = new HashMap<String, String>();
        final String[] parameters = uri.substring(uri.indexOf('?') + 1).split("&"); //$NON-NLS-1$
        for (final String param : parameters) {
			if (param.indexOf('=') > 0) {
				params.put(
					param.substring(0, param.indexOf('=')),
					param.indexOf('=') == param.length() - 1 ? "" : param.substring(param.indexOf('=') + 1) //$NON-NLS-1$
				);
			}
		}
        return params;
    }

    /** Comprueba que est&eacute;n disponibles todos los parametros disponibles en la entrada de datos.
     * @param uri Url de llamada
     * @return Par&aacute;metros
     * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto */
	public static UrlParameters getParameters(final String uri) throws ParameterException {

		final Map<String, String> params = parser(uri);

		final UrlParameters ret = new UrlParameters();

		// Comprobamos que se ha especificado el identificador para al firma
    	if (!params.containsKey(ID_PARAM)) {
    		throw new ParameterException("No se ha recibido el identificador del documento"); //$NON-NLS-1$
    	}

		// Comprobamos que el identificador de sesion de la firma no sea mayor de un cierto numero de caracteres
		final String signatureSessionId = params.get(ID_PARAM);
		if (signatureSessionId.length() > MAX_ID_LENGTH) {
			throw new ParameterException("La longitud del identificador para la firma es mayor de " + MAX_ID_LENGTH + " caracteres."); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Comprobamos que el identificador de sesion de la firma sea alfanumerico (se usara como nombre de fichero)
		for (final char c : signatureSessionId.toLowerCase(Locale.ENGLISH).toCharArray()) {
			if ((c < 'a' || c > 'z') && (c < '0' || c > '9')) {
				throw new ParameterException("El identificador de la firma debe ser alfanumerico."); //$NON-NLS-1$
			}
		}

		ret.setSessionId(signatureSessionId);

		// Comprobamos que se ha especificado la clave de cifrado
    	if (!params.containsKey(KEY_PARAM)) {
    		throw new ParameterException("No se ha recibido la clave para el cifrado de la firma"); //$NON-NLS-1$
    	}

		// Comprobamos que la clave de cifrado tenga la longitud correcta
    	final String key = params.get(KEY_PARAM);
		if (key == null || key.length() != CIPHER_KEY_LENGTH) {
			throw new ParameterException("La longitud de la clave de cifrado no es correcta"); //$NON-NLS-1$
		}

		ret.setDesKey(key.getBytes());

		// Comprobamos que se ha especificado el servlet
    	if (!params.containsKey(STORAGE_SERVLET_PARAM)) {
    		throw new ParameterException("No se ha recibido la direccion del servlet para el envio de la firma"); //$NON-NLS-1$
    	}

		// Comprobamos que la URL sea valida
		final URL servletUrl;
    	try {
    		servletUrl = new URL(params.get(STORAGE_SERVLET_PARAM));
		}
    	catch (final MalformedURLException e) {
    		throw new ParameterException("La URL proporcionada para el Servlet no es valida: " + e); //$NON-NLS-1$
		}
    	// Comprobamos que el protocolo este soportado
    	if (servletUrl.getProtocol() != "http" &&  servletUrl.getProtocol() != "https") { //$NON-NLS-1$ //$NON-NLS-2$
    		throw new ParameterException("El protocolo de la URL proporcionada para el servlet no esta soportado: " + servletUrl.getProtocol()); //$NON-NLS-1$
    	}
    	// Comprobamos que la URL sea una llamada al servlet y que no sea local
    	if ("localhost".equals(servletUrl.getHost()) || "127.0.0.1".equals(servletUrl.getHost())) { //$NON-NLS-1$ //$NON-NLS-2$
    		throw new ParameterException("El host de la URL proporcionada para el Servlet es local"); //$NON-NLS-1$
    	}
    	if (!(servletUrl.toString().endsWith(SERVLET_NAME_STORAGE) || servletUrl.toString().endsWith(SERVLET_NAME_RETRIEVE))) {
    		throw new ParameterException("El protocolo de la URL proporcionada para el servlet no apunta a un servlet declarado"); //$NON-NLS-1$
    	}

    	ret.setStorageServletUrl(servletUrl);

		// Comprobamos que se nos hayan indicado los datos
    	if (!params.containsKey(DATA_PARAM)) {
    		throw new ParameterException("No se han recibido los datos para firmar"); //$NON-NLS-1$
    	}

    	final byte[] data;
    	// Comprobamos que los datos se pueden tratar como base 64
    	try {
    		data = Base64.decode(URLDecoder.decode(params.get(DATA_PARAM), DEFAULT_URL_ENCODING).replace("_", "/").replace("-", "+")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    	}
    	catch (final Exception e) {
    		throw new ParameterException("Los datos introducidos no se pueden tratar como base 64: " + e); //$NON-NLS-1$
    	}

    	ret.setData(data);

    	// Comprobamos que se ha especificado el formato
    	if (!params.containsKey(FORMAT_PARAM)) {
    		throw new ParameterException("No se ha recibido el formato de firma"); //$NON-NLS-1$
    	}
    	final String format = params.get(FORMAT_PARAM);
    	if (!SUPPORTED_SIGNATURE_FORMATS.contains(format.toLowerCase())) {
    		throw new ParameterException("Formato de firma no soportado"); //$NON-NLS-1$
    	}

    	ret.setSignFormat(format);

		// Comprobamos que se ha especificado el algoritmo
		if (!params.containsKey(ALGORITHM_PARAM)) {
			throw new ParameterException("No se ha recibido el algoritmo de firma"); //$NON-NLS-1$
		}
		final String algo = params.get(ALGORITHM_PARAM);
		if (!SUPPORTED_SIGNATURE_ALGORITHMS.contains(algo)) {
			throw new ParameterException("Algoritmo de firma no soportado"); //$NON-NLS-1$
		}

		ret.setSignAlgorithm(algo);

		final String props = params.get(PROPERTIES_PARAM);
		try {
			ret.setExtraParams(parseB64Properties(props));
		}
		catch (final IOException e) {
			Log.e("es.gob.afirma", "Se han indicado ExtraParams incorrecto, se ignoraran: " + e);  //$NON-NLS-1$//$NON-NLS-2$
			ret.setExtraParams(new Properties());
		}


		return ret;

    }

	/** Error en los par&aacute;metros de la URL recibida por la aplicaci&oacute;n. */
	public static final class ParameterException extends Exception {

		private static final long serialVersionUID = 976364958815642808L;

		ParameterException(final String msg) {
			super(msg);
		}

	}

	/** Par&aacute;metros de la URL de llamada a la aplicaci&oacute;n. */
	public static final class UrlParameters {
		private String id;
		private byte[] desKey;
		private String signFormat;
		private String signAlgorithm;
		private byte[] data;
		private URL storageServer;
		private Properties extraParams;

		/** Obtiene el identificador de sesi&oacute;n.
		 * @return Identificador de sesi&oacute;n */
		public String getId() {
			return this.id;
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

		/** Obtiene la URL del servlet de almacenamiento temporal en servidor.
		 * @return URL del servlet de almacenamiento temporal en servidor */
		public URL getStorageServletUrl() {
			return this.storageServer;
		}

		/** Obtiene los par&aacute;metros adicionales de la firma.
		 * @return Par&aacute;metros adicionales de la firma */
		public Properties getExtraParams() {
			return this.extraParams;
		}

		UrlParameters() {
			// Instanciacion default
		}

		void setSessionId(final String sessionId) {
			this.id = sessionId;
		}

		void setDesKey(final byte[] key) {
			this.desKey = key;
		}

		void setSignFormat(final String format) {
			this.signFormat = format;
		}

		void setSignAlgorithm(final String algo) {
			this.signAlgorithm = algo;
		}

		void setData(final byte[] dat) {
			this.data = dat;
		}

		void setStorageServletUrl(final URL url) {
			this.storageServer = url;
		}

		void setExtraParams(final Properties properties) {
			this.extraParams = properties != null ? properties : new Properties();
		}


	}

    /** Convierte una cadena en Base 64 de propiedades en un Properties.
     * @param prop Listado de propiedades en base 64.
     * @return Objeto de propiedades.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de la cadena. */
    private static Properties parseB64Properties(final String prop) throws IOException {
        final Properties properties = new Properties();
        if (prop != null) {
        	properties.load(new ByteArrayInputStream(Base64.decode(prop)));
        }
        return properties;
    }

}