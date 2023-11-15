/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.util.Enumeration;
import java.util.Locale;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.logging.Logger;

/** Gestiona el fichero con las propiedades de las pol&iacute;ticas de firma configuradas
 * en el fichero de propiedades.
 * El fichero de propiedades permite propiedades compuestas de la siguiente forma:<br>
 * {@code ID_POLITICA.PROPIEDAD.FORMATO}
 * <ul>
 * <li><b>ID_POLITICA</b>: Identificador de la politica. Opcional.</li>
 * <li><b>PROPIEDAD</b>: Propiedad de la politica (identificador, calificador, hash,...). Obligatorio.</li>
 * <li><b>FORMATO</b>: Valor de la propiedad concreto para ese formato de firma. Opcional.</li>
 * </ul>
 * Si no se indica el identificado de la pol&iacute;tica, se buscar&aacute; la propiedad sin
 * identificador (para el formato indicado, o sin formato si no se indic&oacute; o no se encontr&oacute;).<br>
 * Si no se indic&oacute; el formato se buscar&aacute; el valor gen&eacute;rico de la propiedad,
 * (para una pol&iacute;tica de firma si se indic&oacute;n, o para cualquiera si no).
 * @author Carlos Gamuci. */
public final class AdESPolicyPropertiesManager {

	/** Identificador de la &uacute;ltima versi&oacute;n de la pol&iacute;tica de firma de la AGE. */
	static final String POLICY_ID_AGE = "FirmaAGE"; //$NON-NLS-1$

	/** Identificador de la pol&iacute;tica de firma de la AGE versi&oacute;n 1.8. */
	static final String POLICY_ID_AGE_1_8 = "FirmaAGE18"; //$NON-NLS-1$

	static final String PROPERTY_POLICY_IDENTIFIER = "policyIdentifier"; //$NON-NLS-1$

	static final String PROPERTY_POLICY_HASH_ALGORITHM = "policyIdentifierHashAlgorithm"; //$NON-NLS-1$

	static final String PROPERTY_POLICY_HASH = "policyIdentifierHash"; //$NON-NLS-1$

	static final String PROPERTY_POLICY_QUALIFIER = "policyQualifier"; //$NON-NLS-1$

	static final String PROPERTY_POLICY_DESCRIPTION = "policyDescription"; //$NON-NLS-1$

	static final String FORMAT_CADES = "CAdES"; //$NON-NLS-1$

	static final String FORMAT_XADES = "XAdES"; //$NON-NLS-1$

	static final String FORMAT_PADES = "PAdES"; //$NON-NLS-1$

	private static final String HTTP_PREFIX = "http://"; //$NON-NLS-1$
	private static final String HTTPS_PREFIX = "https://"; //$NON-NLS-1$

	/** Manejador del log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String BUNDLE_NAME = "policy"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME, Locale.getDefault());

    private AdESPolicyPropertiesManager() {
        // No permitimos la instanciacion
    }


	/** Establece las propiedades asociadas a una pol&iacute;tica de firma determinada por un identificador
	 * y con los valores adecuados a un formato de firma particular.
	 * @param prop Documento de propiedades al que se agregar&aacute;n las correspondientes a la
	 * 			pol&iacute;tica de firma.
	 * @param policyId Identificador de la pol&iacute;tica. Si no se indica, se usar&aacute;n los valores
	 * 			gen&eacute;ricos configurados.
	 * @param format Formato de firma particular al que deben corresponder los valores. Si no se indica se
	 * 			usar&aacute;n los gen&eacute;ricos de la pol&iacute;tica. */
	static void setProperties(final Properties prop, final String policyId, final String format) {

		// Comprobamos primero si el formato de firma admite la expansion de politica.
		// En caso negativo, se suspende la expansion.
		if (!isSupportedConfiguration(policyId, format)) {
			return;
		}

		String value = getProperty(policyId, PROPERTY_POLICY_IDENTIFIER, format);
		if (value != null) {
			setProperty(prop, PROPERTY_POLICY_IDENTIFIER, value);
		}
		value = getProperty(policyId, PROPERTY_POLICY_HASH_ALGORITHM, format);
		if (value != null) {
			setProperty(prop, PROPERTY_POLICY_HASH_ALGORITHM, value);
		}
		value = getProperty(policyId, PROPERTY_POLICY_HASH, format);
		if (value != null) {
			setProperty(prop, PROPERTY_POLICY_HASH, value);
		}
		value = getProperty(policyId, PROPERTY_POLICY_QUALIFIER, format);
		if (value != null) {
			setProperty(prop, PROPERTY_POLICY_QUALIFIER, value);
		}
		value = getProperty(policyId, PROPERTY_POLICY_DESCRIPTION, format);
		if (value != null) {
			setProperty(prop, PROPERTY_POLICY_DESCRIPTION, value);
		}
	}

	/**
	 * Comprueba que una configuraci&oacute;n de pol&iacute;tica est&aacute; soportada comprobando
	 * que tiene una huella digital definida o que puede calcularse la huella a trav&eacute;s de
	 * su identificador.
	 * @param policyId Identificador de pol&iacute;tica.
	 * @param format Formato de firma normalizado.
	 * @return {@code true} si la pol&iacute;tica de la firma puede expandirse, {@code false} en
	 * caso contrario.
	 */
	private static boolean isSupportedConfiguration(final String policyId, final String format) {

		final String hash = getProperty(policyId, PROPERTY_POLICY_HASH, format);
		if (hash == null || hash.trim().isEmpty()) {
			final String identifier = getProperty(policyId, PROPERTY_POLICY_IDENTIFIER, format);
			if (identifier != null && (!identifier.toLowerCase().startsWith(HTTP_PREFIX) ||
					!identifier.toLowerCase().startsWith(HTTPS_PREFIX))) {
				return false;
			}
		}
		return true;
	}

	/** Recupera una propiedad de la configuraci&oacute;n establecida de pol&iacute;ticas de firma.
	 * @param id Identificador de la pol&iacute;tica.
	 * @param property Propiedad que se desea recuperar.
	 * @param format Modificador relativo al formato de firma.
	 * @return Valor de la propiedad de la pol&iacute;tica. */
	private static String getProperty(final String id, final String property, final String format) {

		if (property == null) {
			throw new IllegalArgumentException("No se ha indicado la propiedad de la politica de firma"); //$NON-NLS-1$
		}

		String key;
		if (id != null) {

			if (format != null) {
				key = id + "." + property + "." + format; //$NON-NLS-1$ //$NON-NLS-2$
				if (RESOURCE_BUNDLE.containsKey(key)) {
					return RESOURCE_BUNDLE.getString(key);
				}
			}
			key = id + "." + property; //$NON-NLS-1$
			if (RESOURCE_BUNDLE.containsKey(key)) {
				return RESOURCE_BUNDLE.getString(key);
			}
		}
		if (format != null) {
			key = property + "." + format; //$NON-NLS-1$
			if (RESOURCE_BUNDLE.containsKey(key)) {
				return RESOURCE_BUNDLE.getString(key);
			}
		}
		key = property;
		if (RESOURCE_BUNDLE.containsKey(key)) {
			return RESOURCE_BUNDLE.getString(key);
		}
		return null;
	}

	/**
	 * Establece una nueva propiedad, informando por log si esta hace que se omita un valor establecido previamente.
	 * @param config Configuraci&oacute;n a la uqe agregar la propiedad.
	 * @param property Nombre de la propiedad.
	 * @param value Valor a establecer.
	 */
	private static void setProperty(final Properties config, final String property, final String value) {

		if (config.containsKey(property)) {
			LOGGER.warning("La siguiente propiedad se ignora en favor del valor derivado de la politica establecida: " + property);  //$NON-NLS-1$
		}
		config.setProperty(property, value);
	}

	/**
	 * Comprueba que un identificador de pol&iacute;tica se corresponda con un identificador de
	 * pol&iacute;tica de la AGE.
	 * @param policyId Identificador de pol&iacute;tica.
	 * @return {@code true} si el identificador coincide con uno de los identificadores de las
	 * pol&iacute;ticas de firma de la AGE, {@code false} en caso contrario.
	 */
	public static boolean isAgePolicyConfigurated(final String policyId) {

		if (policyId == null) {
			return false;
		}

		final Enumeration<String> keys = RESOURCE_BUNDLE.getKeys();
		while (keys.hasMoreElements()) {
			final String key = keys.nextElement();
			if (key.endsWith("." + PROPERTY_POLICY_IDENTIFIER)) { //$NON-NLS-1$
				final String identifier = RESOURCE_BUNDLE.getString(key);
				if (identifier.equals(policyId)) {
					return true;
				}
			}
		}
		return false;
	}
}
