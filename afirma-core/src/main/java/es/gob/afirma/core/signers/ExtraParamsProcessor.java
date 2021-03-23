/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** Clase de utilidad para el proceso de propiedades enviadas desde JavaScript
 * y recogidas desde Java en formato <code>Properties</code>. */
public final class ExtraParamsProcessor {

	/** Tama&ntilde;o equivalente a 1 MegaBytes en bytes. */
	private static final int SIZE_1MB = 1024 * 1024;

	private static final String ETSI_CADES_DETACHED = "ETSI.CAdES.detached"; //$NON-NLS-1$

	/** Clave expansible para pol&iacute;ticas de firma. */
	private static final String EXPANDIBLE_POLICY_KEY = "expPolicy"; //$NON-NLS-1$

	/**
	 * Taman&ntilde;o m&aacute;ximo de ruta permitido para cargar un documento local
	 * cuando se deshabilita el modo seguro.
	 */
	private static final int MAX_PATH_SIZE = 255;

	/** Manejador del log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ExtraParamsProcessor() {
		/* Constructor no publico */
	}

	/** Transforma la entrada introducida en un <code>Properties</code>.
	 * Las entradas deben estar separadas por salto de l&iacute;nea y tener la forma
	 * {@code CLAVE=VALOR} en donde CLAVE es el identificador del par&aacute;metro y
	 * VALOR el valor asignado a este.
	 * La CLAVE no puede contener ning&uacute;n signo igual ('=') ni empezar por
	 * almohadilla ('#') y se ignorar&aacute;n aquellas entradas que no contengan
	 * el signo igual en una posici&oacute;n la cadena distinta a la primera.
	 * Si se introduce null se devuelve un Properties vac&iacute;o.
	 * @param entries Listado de pares CLAVE - VALOR.
	 * @return Properties con las claves indicadas cargadas como par&aacute;metro. */
	public static Properties convertToProperties(final String entries) {

		final Properties params = new Properties();
		if (entries == null) {
			return params;
		}

		try {
			params.load(new ByteArrayInputStream(entries.getBytes()));
		}
		catch (final Exception e) {
			LOGGER.warning(
				"Se han encontrado entradas no validas en la configuracion de la operacion: " + e//$NON-NLS-1$
			);
		}

		return params;
	}

 	/** Devuelve la colecci&oacute;n de propiedades de entrada con las entradas que correspondan
	 * expandidos. Se expandiran una serie de claves con valores predefinidos y se les
	 * asignar&aacute; el valor correspondiente.
	 * Una vez expandidos, se eliminaran estos par&aacute;metros de la lista. Si el expandir
	 * los par&aacute;metros implica establer otras propiedades y estas ya est&aacute;n
	 * definidas en el Properties, prevalecer&aacute;n los valores expandidos.<br>
	 * Entre los par&aacute;metros clave se encuentran:
	 * <ul>
     *  <li><b>expPolicy</b>: Configuracion de la pol&iacute;tica de firma. Posibles valores:
     *   <ul><li><b>FirmaAGE</b>:
     *    Establece los diversions par&aacute;metros para la configuraci&oacute;n de la
     *    pol&iacute;tica de firma de la AGE.
     *   </li>
     *   </ul>
     *  </li>
     * </ul>
	 * @param params Par&aacute;metros definidos para la operaci&oacute;n.
	 * @return Propiedades expandidas.
	 * @throws IncompatiblePolicyException Si el formato de firma es incompatible con la pol&iacute;tica indicada. */
	public static Properties expandProperties(final Properties params) throws IncompatiblePolicyException {
		return expandProperties(params, null, null);
	}

	/** Devuelve la colecci&oacute;n de propiedades de entrada con las entradas que correspondan
	 * expandidos. Se expandiran una serie de claves con valores predefinidos y se les
	 * asignar&aacute; el valor correspondiente.
	 * Una vez expandidos, se eliminaran estos par&aacute;metros de la lista. Si el expandir
	 * los par&aacute;metros implica establer otras propiedades y estas ya est&aacute;n
	 * definidas en el Properties, prevalecer&aacute;n los valores expandidos.<br>
	 * Entre los par&aacute;metros clave se encuentran:
	 * <ul>
     *  <li><b>expPolicy</b>: Configuracion de la pol&iacute;tica de firma. Posibles valores:
     *   <ul><li><b>FirmaAGE</b>:
     *    Establece los diversions par&aacute;metros para la configuraci&oacute;n de la
     *    pol&iacute;tica de firma de la AGE.
     *   </li>
     *   </ul>
     *  </li>
     * </ul>
	 * @param params Par&aacute;metros definidos para la operaci&oacute;n.
	 * @param signedData Datos firmados.
	 * @param format Formato de firma.
	 * @return Propiedades expandidas.
	 * @throws IncompatiblePolicyException Si el formato de firma es incompatible con la pol&iacute;tica indicada. */
	public static Properties expandProperties(final Properties params, final byte[] signedData, final String format) throws IncompatiblePolicyException {
		final Properties p = new Properties();
		for (final String key : params.keySet().toArray(new String[params.size()])) {
			p.setProperty(key, params.getProperty(key));
		}
		expandPolicyKeys(p, signedData, format);
		return p;
	}

	/** Expande las propiedades de pol&iacute;tica de firma modificando el conjunto de propiedades.
	 * @param p Propiedades configuradas.
	 * @param signedData Datos firmados.
	 * @param format Formato de firma.
	 * @throws IncompatiblePolicyException Si el formato de firma es incompatible con la pol&iacute;tica indicada. */
	private static void expandPolicyKeys(final Properties p, final byte[] signedData, final String format) throws IncompatiblePolicyException {
		if (!p.containsKey(EXPANDIBLE_POLICY_KEY)) {
			return;
		}

		final String policyName = p.getProperty(EXPANDIBLE_POLICY_KEY);

		// Comprobamos que se trate de una politica reconocida que admita la expansion de atributos
		if (!isSupportedPolicy(policyName)) {
			p.remove(EXPANDIBLE_POLICY_KEY);
			throw new IncompatiblePolicyException("No se soporta la expansion de atributos para la politica: " + policyName); //$NON-NLS-1$
		}

		// Normalizamos el nombre de formato para simplificar las comprobaciones futuras
		final String normalizedFormat = normalizeFormat(format);

		// Firma CAdES de la AGE
		if (normalizedFormat.equals(AdESPolicyPropertiesManager.FORMAT_CADES) &&
				(AdESPolicyPropertiesManager.POLICY_ID_AGE.equals(policyName) ||
						AdESPolicyPropertiesManager.POLICY_ID_AGE_1_8.equals(policyName))) {
			setCAdESPolicyAGEAttributes(policyName, p, signedData);
		}
		// Firma XAdES de la AGE
		else if (normalizedFormat.equals(AdESPolicyPropertiesManager.FORMAT_XADES) &&
				(AdESPolicyPropertiesManager.POLICY_ID_AGE.equals(policyName) ||
						AdESPolicyPropertiesManager.POLICY_ID_AGE_1_8.equals(policyName))) {
			setXAdESPolicyAGEAttributes(policyName, p);
		}
		// Firma PAdES de la AGE (Soportada a partir de la politica 1.9)
		else if (normalizedFormat.equals(AdESPolicyPropertiesManager.FORMAT_PADES) &&
				AdESPolicyPropertiesManager.POLICY_ID_AGE.equals(policyName)) {
			setPAdESPolicyAGEAttributes(policyName, p);
		}
		// Cualquier otra combinacion no esta soportada
		else {
			p.remove(EXPANDIBLE_POLICY_KEY);
			throw new IncompatiblePolicyException(String.format(
					"El formato de firma %1s no esta soportado por la politica %2s", //$NON-NLS-1$
					format, policyName));
		}
		p.remove(EXPANDIBLE_POLICY_KEY);
	}

	/**
	 * Normaliza el nombre de formato de firma para simplificar comprobaciones posteriores.
	 * @param format Nombre del formato de firma.
	 * @return Nombre homogenizado del formato de firma o la misma cadena de entrada si no es
	 * uno de los nombres de formato soportados.
	 */
	private static String normalizeFormat(final String format) {
		String normalizedFormat = null;

		// Admitimos las variables normal y trifasica de CAdES. No se admite "CAdES-ASiC"
		if (format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_CADES) ||
				format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_CADES_TRI)) {
			normalizedFormat = AdESPolicyPropertiesManager.FORMAT_CADES;
		}
		// Admitimos las variables normal y trifasica de XAdES y cualquiera del tipo
		// "XAdES Detached" o similar. No se admite "XAdES-ASiC"
		else if (format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_XADES) ||
				format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_XADES_TRI) ||
				format.toLowerCase(Locale.US).startsWith(AOSignConstants.SIGN_FORMAT_XADES.toLowerCase(Locale.US) + " ")) { //$NON-NLS-1$
			normalizedFormat = AdESPolicyPropertiesManager.FORMAT_XADES;
		}
		// Admitimos las variables normal y trifasica de PAdES y PDF.
		else if (format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_PDF) ||
				format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_PADES) ||
				format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_PDF_TRI) ||
				format.equalsIgnoreCase(AOSignConstants.SIGN_FORMAT_PADES_TRI)) {
			normalizedFormat = AdESPolicyPropertiesManager.FORMAT_PADES;
		}
		return normalizedFormat != null ? normalizedFormat : format;
	}

	/**
	 * Indica si la expansi&oacute;n de propiedades est&aacute; habilitada para una
	 * pol&iacute;tica de firma concreta.
	 * @param policyName Nombre de la pol&iacute;tica de firma.
	 * @return {@code true} si la pol&iacute;tica esta soportada, {@code false} en
	 * caso contrario.
	 */
	private static boolean isSupportedPolicy(final String policyName) {
		return AdESPolicyPropertiesManager.POLICY_ID_AGE.equals(policyName) ||
				AdESPolicyPropertiesManager.POLICY_ID_AGE_1_8.equals(policyName);
	}

	/**
	 * Establece los atributos correspondientes a las firmas CAdES para la policita de
	 * firma de la AGE.
	 * @param policyName Nombre de la pol&iacute;tica de firma.
	 * @param params Conjunto de propiedades en donde hay que establecer los atributos
	 * de la pol&iacute;tica de firma.
	 * @param signedData Datos firmados.
	 */
	private static void setCAdESPolicyAGEAttributes(final String policyName, final Properties params,
			final byte[] signedData) {

		// La politica indica que la firma debe ser implicita siempre que el tamano
		// del documento sea razonable. Como no se especifica que tamano es razonable
		// respetaremos el modo indicado por el integrador. En caso de no haberlo
		// indicado, establecemos el limite en 1Mb. Esto solo aplicaria a CAdES ya que
		// PAdES siempre es implicita e ignora este parametro.
		if (!params.containsKey("mode") && signedData != null) { //$NON-NLS-1$
			params.setProperty("mode", signedData.length < SIZE_1MB ? //$NON-NLS-1$
				AOSignConstants.SIGN_MODE_IMPLICIT :
					AOSignConstants.SIGN_MODE_EXPLICIT);
		}

		// La politica de firma 1.9 de la AGE no es compatible con los perfiles baseline
		final String profile = params.getProperty("profile"); //$NON-NLS-1$
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)) {
			LOGGER.warning("Se ignora la configuracion del parametro 'profile' en favor de los requisitos establecidos por la politica de firma"); //$NON-NLS-1$
		}
		params.setProperty("profile", AOSignConstants.SIGN_PROFILE_ADVANCED); //$NON-NLS-1$

		AdESPolicyPropertiesManager.setProperties(params, policyName, AdESPolicyPropertiesManager.FORMAT_CADES);
	}

	/**
	 * Establece los atributos correspondientes a las firmas XAdES para la policita de
	 * firma de la AGE.
	 * @param policyName Nombre de la pol&iacute;tica de firma.
	 * @param params Conjunto de propiedades en donde hay que establecer los atributos
	 * de la pol&iacute;tica de firma.
	 */
	private static void setXAdESPolicyAGEAttributes(final String policyName, final Properties params) {

		// La firma XAdES conforme a la politica de firma de la AGE debe ser Detached o Enveloped. Si se declara
		// un valor distinto a estos, se fuerza a Detached.
		final String format = params.getProperty("format"); //$NON-NLS-1$
		if (!AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equalsIgnoreCase(format) &&
				!AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equalsIgnoreCase(format)) {
			if (format != null) {
				LOGGER.warning("Se ignora la configuracion del parametro 'format' en favor de los requisitos establecidos por la politica de firma"); //$NON-NLS-1$
			}
			params.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
		}

		// La politica de firma 1.9 de la AGE no es compatible con los perfiles baseline de XAdES
		final String profile = params.getProperty("profile"); //$NON-NLS-1$
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)) {
			LOGGER.warning("Se ignora la configuracion del parametro 'profile' en favor de los requisitos establecidos por la politica de firma"); //$NON-NLS-1$
		}
		params.setProperty("profile", AOSignConstants.SIGN_PROFILE_ADVANCED); //$NON-NLS-1$

		AdESPolicyPropertiesManager.setProperties(params, policyName, AdESPolicyPropertiesManager.FORMAT_XADES);
	}

	/**
	 * Establece los atributos correspondientes a las firmas PAdES para la policita de
	 * firma de la AGE.
	 * @param policyName Nombre de la pol&iacute;tica de firma.
	 * @param params Conjunto de propiedades en donde hay que establecer los atributos
	 * de la pol&iacute;tica de firma.
	 * @throws IncompatiblePolicyException Cuando se ha declarado utilizar un subfiltro
	 * no permitido por la pol&iacute;tica de firma de la AGE.
	 */
	private static void setPAdESPolicyAGEAttributes(final String policyName, final Properties params)
			throws IncompatiblePolicyException {

		// El subfiltro de las PAdES acorde politica de la AGE debe ser el de CAdES Detached de la ETSI. Si se
		// declara otro, se lanza un error
		if (params.containsKey("signatureSubFilter") && //$NON-NLS-1$
				!ETSI_CADES_DETACHED.equals(params.getProperty("signatureSubFilter"))) { //$NON-NLS-1$
			throw new IncompatiblePolicyException("En PAdES con politica firma AGE debe usarse siempre el filtro '" + //$NON-NLS-1$
				ETSI_CADES_DETACHED + "'"); //$NON-NLS-1$
		}
		params.setProperty("signatureSubFilter", ETSI_CADES_DETACHED); //$NON-NLS-1$

		AdESPolicyPropertiesManager.setProperties(params, policyName, AdESPolicyPropertiesManager.FORMAT_PADES);
	}

	/** Pol&iacute;tica de firma incompatible con el formato o la configuraci&oacute;n de firma. */
	public static final class IncompatiblePolicyException extends Exception {

		private static final long serialVersionUID = -6420193548487585455L;

		IncompatiblePolicyException(final String description) {
			super(description);
		}
	}

	/** Establece propiedades de firma concretas para cuando el formato indicado sea "AUTO".
	 * Las propiedades dependen del signer que se vaya a usar.
	 * @param signer Firmador usado.
	 * @param data Datos a firmar.
	 * @param params Par&aacute;metros adicionales. */
	public static void configAutoFormat(final AOSigner signer, final byte[] data, final Properties params) {

		final String signerClassname = signer.getClass().getName();
		if (signerClassname.equals("es.gob.afirma.signers.pades.AOPDFSigner")) { //$NON-NLS-1$
			try {
				final Method configureMethod = signer.getClass().getMethod("configureRespectfulProperties", byte[].class, Properties.class); //$NON-NLS-1$
				configureMethod.invoke(null, data, params);
			}
			catch (final Exception e) {
				LOGGER.warning("Error al configurar una firma PAdES igual a las existentes: " + e); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Carga un binario del Properties de las propiedades de configuraci&oacute;n. Este dato
	 * debe estar codificado en Base 64 o, si no est&aacute; habilitado el modo seguro,
	 * podr&iacute;a ser una ruta local desde la que cargar el binario.
	 * @param extraParams Propiedades de configuraci&oacute;n de las que obtener el binario.
	 * @param paramName Nombre del par&aacute;metro en el que se encuentra el binario en Base 64
	 * o desde el que se referencia al mismo.
	 * @param secureMode Indica si se est&aacute; trabajando en modo seguro ({@code true}) y el
	 * dato debe estar en las propiedades, o si no estamos en modo seguro ({@code false}) y
	 * podr&iacute;amos tener una referencia a un fichero local que cargar.
	 * @return Datos binarios cargados.
	 * @throws IOException Cuando no se encuentra el dato en la configuraci&oacute;n o cuando
	 * no se puede cargar.
	 */
	public static byte[] loadByteArrayFromExtraParams(final Properties extraParams, final String paramName,
			final boolean secureMode) throws IOException {

		final String value = extraParams.getProperty(paramName);
		if (value == null) {
			throw new IOException("La propiedad '" + paramName + "' no se encuentra en el extraParams"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		byte[] content;
		if (!secureMode && value.length() > 0 && value.length() < MAX_PATH_SIZE && !Base64.isBase64(value)) {
			try {
				final URI uri = AOUtil.createURI(value);
				try (InputStream is = AOUtil.loadFile(uri)) {
					content = AOUtil.getDataFromInputStream(is);
				}
			} catch (final Exception e) {
				throw new IOException("El propiedad '" + paramName + "' no contiene una ruta valida a un recurso", e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		else {
			content = Base64.decode(value);
		}

		return content;
	}
}
