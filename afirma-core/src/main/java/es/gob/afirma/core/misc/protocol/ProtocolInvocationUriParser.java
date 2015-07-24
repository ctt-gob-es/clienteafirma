package es.gob.afirma.core.misc.protocol;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.DataDownloader;

/** Clase de utilidad para el an&aacute;lisis sint&aacute;ctico de URL.
 * @author Alberto Mart&iacute;nez */
public final class ProtocolInvocationUriParser {

	/** Algoritmos de firma soportados. */
	private static final Set<String> SUPPORTED_SIGNATURE_ALGORITHMS = new HashSet<String>();
	static {
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA1withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA256withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA384withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA512withRSA"); //$NON-NLS-1$
	}

	private static final String DEFAULT_URL_ENCODING = "UTF-8"; //$NON-NLS-1$

	/** N&uacute;mero m&aacute;ximo de caracteres permitidos para el identificador de sesi&oacute;n de la firma. */
	private static final int MAX_ID_LENGTH = 20;

	/** Longitud permitida para la clave de cifrado. */
	private static final int CIPHER_KEY_LENGTH = 8;

	/** Par&aacute;metro que identifica el <i>User Agent</i> del navegador Web usado. */
	private static final String KEYSTORE ="keystore"; //$NON-NLS-1$

	/** Par&aacute;metro que identifica la operaci&oacute;n a realizar. */
	private static final String OPERATION_PARAM = "op"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el formato de firma. */
	private static final String FORMAT_PARAM = "format"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el algoritmo de firma. */
	private static final String ALGORITHM_PARAM = "algorithm"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con los datos a firmar. */
	private static final String DATA_PARAM = "dat"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el identificador del fichero remoto de datos. */
	private static final String FILE_ID_PARAM = "fileid"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el servlet remoto de guardado de datos. */
	private static final String STORAGE_SERVLET_PARAM = "stservlet"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el servlet remoto de recuperaci&oacute;n de datos. */
	private static final String RETRIEVE_SERVLET_PARAM = "rtservlet"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el identificador del documento. */
	private static final String ID_PARAM = "id"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String VER_PARAM = "ver"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la clave para el cifrado del documento. */
	private static final String KEY_PARAM = "key"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con las opciones de configuraci&oacute;n de la firma. */
	private static final String PROPERTIES_PARAM = "properties"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el t&iacute;tulo de la actividad o el di&aacute;logo de guardado. */
	private static final String TITLE_PARAM = "title"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la descripci&oacute;n del tipo de fichero de salida. */
	private static final String FILETYPE_DESCRIPTION = "desc"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con las extensiones recomendadas para el fichero de salida. */
	private static final String FILENAME_EXTS = "exts"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el nombre propuesto para un fichero. */
	private static final String FILENAME_PARAM = "filename"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de firma. */
	private static final String OP_ID_SIGN = "sign"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de cofirma. */
	private static final String OP_ID_COSIGN = "cosign"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de contrafirma. */
	private static final String OP_ID_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	private ProtocolInvocationUriParser() {
		// Constructor privado. No se permite instancias
	}

	/** Analiza la Url de entrada para obtener la lista de par&aacute;metros asociados
	 * @param uri Url de llamada
	 * @return Devuelve una tabla <i>hash</i> con cada par&aacute;metro asociado a un valor */
	private static Map<String, String> parserUri(final String uri) {
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

		// Agregamos como codigo de operacion el nombre de host de la URL
		String path = uri.substring(uri.indexOf("://") + "://".length(), uri.indexOf('?')); //$NON-NLS-1$ //$NON-NLS-2$
		if (path.endsWith("/")) { //$NON-NLS-1$
			path = path.substring(0, path.length() - 1);
		}
		params.put(OPERATION_PARAM, path.substring(path.lastIndexOf('/') + 1));

		return params;
	}

	/** Analiza un XML de entrada para obtener la lista de par&aacute;metros asociados
	 * @param xml XML con el listado de par&aacute;metros.
	 * @return Devuelve una tabla <i>hash</i> con cada par&aacute;metro asociado a un valor
	 * @throws ParameterException Cuando el XML de entrada no es v&acute;lido. */
	private static Map<String, String> parseXml(final byte[] xml) throws ParameterException {
		final Map<String, String> params = new HashMap<String, String>();
		final NodeList elems;

		try {
			final Element docElement = DocumentBuilderFactory.newInstance().newDocumentBuilder()
					.parse(new ByteArrayInputStream(xml)).getDocumentElement();

			// Si el elemento principal es OPERATION_PARAM entendemos que es una firma
			params.put(
				OPERATION_PARAM,
				OPERATION_PARAM.equalsIgnoreCase(docElement.getNodeName()) ?
					OP_ID_SIGN : docElement.getNodeName()
			);

			elems = docElement.getChildNodes();
		}
		catch (final Exception e) {
			throw new ParameterException("Ocurrio un error grave durante el analisis del XML", e); //$NON-NLS-1$
		}

		for (int i = 0; i < elems.getLength(); i++) {
			final Node element = elems.item(i);
			if (!"e".equals(element.getNodeName())) { //$NON-NLS-1$
				throw new ParameterException("El XML no tiene la forma esperada"); //$NON-NLS-1$
			}
			final NamedNodeMap attrs = element.getAttributes();
			final Node keyNode = attrs.getNamedItem("k"); //$NON-NLS-1$
			final Node valueNode = attrs.getNamedItem("v"); //$NON-NLS-1$
			if (keyNode == null || valueNode == null) {
				throw new ParameterException("El XML no tiene la forma esperada"); //$NON-NLS-1$
			}
			params.put(keyNode.getNodeValue(), valueNode.getNodeValue());
		}
		return params;
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma.
	 * @param uri Url de llamada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto
	 * @throws UnsupportedEncodingException Si no se soporta UTF-8 en URL (no debe ocurrir nunca) */
	public static UrlParametersToSign getParametersToSign(final String uri) throws ParameterException, UnsupportedEncodingException {
		return  getParametersToSign(parserUri(uri));
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma.
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto
	 * @throws UnsupportedEncodingException Si no se soporta UTF-8 en URL (no debe ocurrir nunca) */
	public static UrlParametersToSign getParametersToSign(final byte[] xml) throws ParameterException, UnsupportedEncodingException {
		return  getParametersToSign(parseXml(xml));
	}

	/** Comprueba que est&eacute;n disponibles todos los parametros disponibles en la entrada de
	 * datos para la operaci&oacute;n de firma.
	 * @param params Par&aacute;metros para el proceso de firma
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto
	 * @throws UnsupportedEncodingException Si no se soporta UTF-8 en URL (no debe ocurrir nunca) */
	private static UrlParametersToSign getParametersToSign(final Map<String, String> params) throws ParameterException, UnsupportedEncodingException {

		final UrlParametersToSign ret = new UrlParametersToSign();

		// Comprobamos que se ha especificado el identificador para al firma
		if (!params.containsKey(ID_PARAM) && !params.containsKey(FILE_ID_PARAM)) {
			throw new ParameterException("No se ha recibido el identificador del documento"); //$NON-NLS-1$
		}

		// Comprobamos que el identificador de sesion de la firma no sea mayor de un cierto numero de caracteres
		final String signatureSessionId = params.containsKey(ID_PARAM) ? params.get(ID_PARAM) : params.get(FILE_ID_PARAM);
		if (signatureSessionId.length() > MAX_ID_LENGTH) {
			throw new ParameterException("La longitud del identificador para la firma es mayor de " + MAX_ID_LENGTH + " caracteres."); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Comprobamos que el identificador de sesion de la firma sea alfanumerico (se usara como nombre de fichero)
		for (final char c : signatureSessionId.toLowerCase(Locale.ENGLISH).toCharArray()) {
			if ((c < 'a' || c > 'z') && (c < '0' || c > '9')) {
				throw new ParameterException("El identificador de la firma debe ser alfanumerico."); //$NON-NLS-1$
			}
		}

		// Version minima requerida del aplicativo
		if (params.containsKey(VER_PARAM)) {
			ret.setMinimumVersion(params.get(VER_PARAM));
		}
		else {
			ret.setMinimumVersion("0"); //$NON-NLS-1$
		}

		ret.setSessionId(signatureSessionId);

		ret.setDesKey(verifyCipherKey(params));

		// Comprobamos que se nos hayan indicado los datos o, en su defecto, el identificador de fichero remoto
		// para descargar los datos y la ruta del servicio remoto para el fichero
		if (!params.containsKey(DATA_PARAM)) {

			if (params.containsKey(FILE_ID_PARAM)) {
				ret.setFileId(params.get(FILE_ID_PARAM));

				if (!params.containsKey(RETRIEVE_SERVLET_PARAM)){
					throw new ParameterException("No se ha recibido la direccion del servlet para la recuperacion de los datos a firmar"); //$NON-NLS-1$
				}
				final URL retrieveServletUrl;
				try {
					retrieveServletUrl = validateURL(params.get(RETRIEVE_SERVLET_PARAM));
				}
				catch (final ParameterLocalAccessRequestedException e) {
					throw new ParameterLocalAccessRequestedException("La URL del servicio de recuperacion de datos no puede ser local", e); //$NON-NLS-1$
				}
				catch (final ParameterException e) {
					throw new ParameterException("Error al validar la URL del servlet de recuperacion: " + e, e); //$NON-NLS-1$
				}
				ret.setRetrieveServletUrl(retrieveServletUrl);
			}
		}
		else {
			final byte[] data;
			try {
				data = DataDownloader.downloadData(URLDecoder.decode(params.get(DATA_PARAM), DEFAULT_URL_ENCODING));
			}
			catch (final Exception e) {
				throw new ParameterException(
					"No se han podido obtener los datos: " + e, e //$NON-NLS-1$
				);
			}
			ret.setData(data);
		}

		// Tomamos el tipo de operacion
		final String op = params.get(OPERATION_PARAM);
		if (OP_ID_SIGN.equalsIgnoreCase(op)) {
			ret.setOperation(UrlParametersToSign.OP_SIGN);
		}
		else if (OP_ID_COSIGN.equalsIgnoreCase(op)) {
			ret.setOperation(UrlParametersToSign.OP_COSIGN);
		}
		else if (OP_ID_COUNTERSIGN.equalsIgnoreCase(op)) {
			ret.setOperation(UrlParametersToSign.OP_COUNTERSIGN);
		}
		else {
			throw new ParameterException("Se ha indicado un codigo de operacion incorrecto"); //$NON-NLS-1$
		}

		// Si hemos recibido el identificador para la descarga de la configuracion,
		// no encontraremos el resto de parametros
		if (ret.getFileId() != null) {
			return ret;
		}

		// Comprobamos que la validez de la URL del servlet de guardado en caso de indicarse
		if (params.containsKey(STORAGE_SERVLET_PARAM)) {

			// Comprobamos que la URL sea valida
			URL storageServletUrl;
			try {
				storageServletUrl = validateURL(params.get(STORAGE_SERVLET_PARAM));
			}
			catch (final ParameterLocalAccessRequestedException e) {
				throw new ParameterLocalAccessRequestedException("La URL del servicio de guardado no puede ser local", e); //$NON-NLS-1$
			}
			catch (final ParameterException e) {
				throw new ParameterException("Error al validar la URL del servicio de guardado: " + e, e); //$NON-NLS-1$
			}
			ret.setStorageServletUrl(storageServletUrl);
		}

		// Comprobamos que se ha especificado el formato
		if (!params.containsKey(FORMAT_PARAM)) {
			throw new ParameterException("No se ha recibido el formato de firma"); //$NON-NLS-1$
		}

		final String format = URLDecoder.decode(params.get(FORMAT_PARAM), DEFAULT_URL_ENCODING);
		ret.setSignFormat(format);

		// Comprobamos que se ha especificado el algoritmo
		if (!params.containsKey(ALGORITHM_PARAM)) {
			throw new ParameterException("No se ha recibido el algoritmo de firma"); //$NON-NLS-1$
		}
		final String algo = URLDecoder.decode(params.get(ALGORITHM_PARAM), DEFAULT_URL_ENCODING);
		if (!SUPPORTED_SIGNATURE_ALGORITHMS.contains(algo)) {
			throw new ParameterException("Algoritmo de firma no soportado: " + algo); //$NON-NLS-1$
		}

		ret.setSignAlgorithm(algo);

		String props = null;
		if (params.containsKey(PROPERTIES_PARAM)) {
			props = URLDecoder.decode(params.get(PROPERTIES_PARAM), DEFAULT_URL_ENCODING);
		}

		try {
			ret.setExtraParams(parseB64Properties(props));
		}
		catch (final Exception e) {
			ret.setExtraParams(new Properties());
		}

		ret.setDefaultKeyStore(verifyDefaultKeyStoreName(params));

		return ret;
	}

	private static String verifyDefaultKeyStoreName(final Map<String, String> params) throws UnsupportedEncodingException {
		if (params.containsKey(KEYSTORE)) {
			return URLDecoder.decode(params.get(KEYSTORE), DEFAULT_URL_ENCODING);
		}
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return "Windows / Internet Explorer"; //$NON-NLS-1$
		}
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			return "Mac OS X / Safari"; //$NON-NLS-1$
		}
		if (Platform.OS.LINUX.equals(Platform.getOS()) || Platform.OS.SOLARIS.equals(Platform.getOS())) {
			return "Mozilla / Firefox (unificado)"; //$NON-NLS-1$
		}
		return null;
	}

	/** Recupera todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param xml XML con los par&aacute;metros
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto
	 * @throws UnsupportedEncodingException Si no se soporta UTF-8 en URL (no debe ocurrir nunca) */
	public static UrlParametersToSave getParametersToSave(final byte[] xml) throws ParameterException, UnsupportedEncodingException {
		return getParametersToSave(parseXml(xml));
	}

	/** Recupera de una URI todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param uri Url de llamada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto
	 * @throws UnsupportedEncodingException Si no se soporta UTF-8 en URL (no debe ocurrir nunca) */
	public static UrlParametersToSave getParametersToSave(final String uri) throws ParameterException, UnsupportedEncodingException {
		return getParametersToSave(parserUri(uri));
	}

	/** Recupera los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo. Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param params Par&aacute;metros de con la configuraci&oacute;n de la operaci&oacute;n.
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto
	 * @throws UnsupportedEncodingException Si no se soporta UTF-8 en URL (no debe ocurrir nunca) */
	private static UrlParametersToSave getParametersToSave(final Map<String, String> params) throws ParameterException, UnsupportedEncodingException {

		// Comprobamos que se nos hayan indicado los datos o, en su defecto, el identificador de fichero remoto
		// para descargar los datos y la ruta del servicio remoto para el fichero
		if (!params.containsKey(FILE_ID_PARAM) && !params.containsKey(DATA_PARAM)) {
			throw new ParameterException(
				"Error al validar la URL del servlet de recuperacion: " + //$NON-NLS-1$
					"La URI debe contener o un identificador de fichero o los datos a guardar" //$NON-NLS-1$
			);
		}

		final UrlParametersToSave ret = new UrlParametersToSave();

		ret.setDesKey(verifyCipherKey(params));
		ret.setData(verifyData(params));
		ret.setFileId(verifyFileId(params));
		if (ret.getData() == null && ret.getFileId() != null) {
			ret.setRetrieveServletUrl(verifyRetrieveServletUrl(params));
		}
		ret.setTitle(verifyTitle(params));
		ret.setFilename(verifyFilename(params));
		ret.setExtensions(verifyExtensions(params));
		ret.setFileTypeDescription(verifyFileTypeDescription(params));

		return ret;
	}

	private static String verifyFileTypeDescription(final Map<String, String> params) throws UnsupportedEncodingException {
		String desc = null;
		if (params.containsKey(FILETYPE_DESCRIPTION)) {
			desc = URLDecoder.decode(params.get(FILETYPE_DESCRIPTION), DEFAULT_URL_ENCODING);
			// Anadimos las extensiones si fuese preciso
			if (params.containsKey(FILENAME_EXTS) && !desc.endsWith(")")) { //$NON-NLS-1$
				final StringBuilder sb = new StringBuilder(desc).append(" ("); //$NON-NLS-1$
				for (final String ext : URLDecoder.decode(params.get(FILENAME_EXTS), DEFAULT_URL_ENCODING).split(",")) { //$NON-NLS-1$
					sb.append("*."); //$NON-NLS-1$
					sb.append(ext);
				}
				sb.append(")"); //$NON-NLS-1$
				desc = sb.toString();
			}
		}
		return desc;
	}

	private static String verifyFilename(final Map<String, String> params) throws UnsupportedEncodingException, ParameterException {
		String filename = null;
		if (params.containsKey(FILENAME_PARAM)) {
			filename = URLDecoder.decode(params.get(FILENAME_PARAM), DEFAULT_URL_ENCODING);
			// Determinamos si el nombre tiene algun caracter que no consideremos valido para un nombre de fichero
			for (final char invalidChar : "\\/:*?\"<>|".toCharArray()) { //$NON-NLS-1$
				if (filename.indexOf(invalidChar) != -1) {
					throw new ParameterException("Se ha indicado un nombre de fichero con el caracter invalido: " + invalidChar); //$NON-NLS-1$
				}
			}
		}
		return filename;
	}

	private static String verifyExtensions(final Map<String, String> params) throws UnsupportedEncodingException, ParameterException {
		String extensions = null;
		if (params.containsKey(FILENAME_EXTS)) {
			extensions = URLDecoder.decode(params.get(FILENAME_EXTS), DEFAULT_URL_ENCODING);
			// Determinamos si el nombre tiene algun caracter que no consideremos valido para un nombre de fichero
			for (final char invalidChar : "\\/:*?\"<>|; ".toCharArray()) { //$NON-NLS-1$
				if (extensions.indexOf(invalidChar) != -1) {
					throw new ParameterException("Se ha indicado una lista de extensiones de nombre de fichero con caracteres invalidos: " + invalidChar); //$NON-NLS-1$
				}
			}
		}
		return extensions;
	}

	private static String verifyTitle(final Map<String, String> params) throws UnsupportedEncodingException {
		if (params.containsKey(TITLE_PARAM)) {
			return URLDecoder.decode(params.get(TITLE_PARAM), DEFAULT_URL_ENCODING);
		}
		return ProtocoloMessages.getString("ProtocolInvocationUriParser.1"); //$NON-NLS-1$
	}

	private static URL verifyRetrieveServletUrl(final Map<String, String> params) throws ParameterException {
		if (!params.containsKey(RETRIEVE_SERVLET_PARAM)) {
			return null;
		}
		return validateURL(params.get(RETRIEVE_SERVLET_PARAM));
	}

	private static String verifyFileId(final Map<String, String> params) throws ParameterException {

		final String fileId = params.get(FILE_ID_PARAM);
		if (fileId != null) {
			if (!params.containsKey(RETRIEVE_SERVLET_PARAM)){
				throw new ParameterException("No se ha recibido la direccion del servlet para la recuperacion de los datos a firmar"); //$NON-NLS-1$
			}
			if (!params.containsKey(KEY_PARAM)){
				throw new ParameterException("No se ha recibido la clave para el descifrado de los datos remotos"); //$NON-NLS-1$
			}
		}
		return fileId;
	}

	private static byte[] verifyData(final Map<String, String> params) throws ParameterException {
		byte[] data = null;
		if (params.containsKey(DATA_PARAM)) {
			// Comprobamos que los datos se pueden tratar como base 64
			try {
				data = Base64.decode(URLDecoder.decode(params.get(DATA_PARAM), DEFAULT_URL_ENCODING).replace("_", "/").replace("-", "+")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			}
			catch (final Exception e) {
				throw new ParameterException("Los datos introducidos no se pueden tratar como base 64: " + e); //$NON-NLS-1$
			}
		}
		return data;
	}

	/** Extrae y verifica la clave de cifrado de los parametros de entrada. Si no se especifica  se devuelve.
	 *  @param params Par&aacute;metros extra&iacute;dos de la URI.
	 *  @return Clave de cifrado o null si no se declar&oacute; un valor en los par&aacute;metros.
	 *  @throws ParameterException Cuando la clave de cifrado es err&oacute;nea. */
	private static byte[] verifyCipherKey(final Map<String, String> params) throws ParameterException {

		// Comprobamos que se ha especificado la clave de cifrado
		if (!params.containsKey(KEY_PARAM)) {
			return null;
		}

		// Si se ha indicado el parametro pero no un valor, se intepretara que no hay cifrado
		final String key = params.get(KEY_PARAM);
		if (key == null || key.length() == 0) {
			return null;
		}

		// Comprobamos que la clave de cifrado tenga la longitud correcta
		if (key.length() != CIPHER_KEY_LENGTH) {
			throw new ParameterException("La longitud de la clave de cifrado no es correcta"); //$NON-NLS-1$
		}
		return key.getBytes();
	}

	/** Valida una URL para asegurar que cumple con los requisitos m&iacute;nimos de seguridad.
	 * @param url URL que se desea validar.
	 * @return URL formada y validada.
	 * @throws ParameterException Cuando ocurre alg&uacute;n problema al validar la URL. */
	private static URL validateURL(final String url) throws ParameterException {

		// Comprobamos que la URL sea valida
		final URL servletUrl;
		try {
			servletUrl = new URL(url);
		}
		catch (final Exception e) {
			throw new ParameterException("La URL proporcionada para el servlet no es valida (" + url + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		// Comprobamos que el protocolo este soportado
		if (!"http".equals(servletUrl.getProtocol()) && !"https".equals(servletUrl.getProtocol())) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new ParameterException("El protocolo de la URL proporcionada para el servlet no esta soportado: " + servletUrl.getProtocol()); //$NON-NLS-1$
		}
		// Comprobamos que la URL sea una llamada al servlet y que no sea local
		if ("localhost".equals(servletUrl.getHost()) || "127.0.0.1".equals(servletUrl.getHost())) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new ParameterLocalAccessRequestedException("El host de la URL proporcionada para el Servlet es local"); //$NON-NLS-1$
		}
		// El servlet no puede recibir parametros
		if (servletUrl.toString().indexOf('?') != -1 || servletUrl.toString().indexOf('=') != -1) {
			throw new ParameterException("Se han encontrado parametros en la URL del servlet"); //$NON-NLS-1$
		}
		return servletUrl;
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