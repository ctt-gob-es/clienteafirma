package es.gob.afirma.miniapplet;

import java.io.IOException;
import java.util.Locale;
import java.util.Properties;
import java.util.ResourceBundle;

/**
 * Gestiona el fichero con las propiedades de las pol&iacute;ticas de firma configuradas
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
 * @author Carlos Gamuci
 */
final class PolicyPropertiesManager {

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

	private static final String BUNDLE_NAME = "policy"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME, Locale.getDefault());

    private PolicyPropertiesManager() {
        // No permitimos la instanciacion
    }


	/**
	 * Establece las propiedades asociadas a una pol&iacute;tica de firma determinada por un identificador
	 * y con los valores adecuados a un formato de firma particular.
	 * @param prop Documento de propiedades al que se agregar&aacute;n las correspondientes a la
	 * 			pol&iacute;tica de firma.
	 * @param policyId Identificador de la pol&iacute;tica. Si no se indica, se usar&aacute;n los valores
	 * 			gen&eacute;ricos configurados.
	 * @param format Formato de firma particular al que deben corresponder los valores. Si no se indica se
	 * 			usar&aacute;n los gen&eacute;ricos de la pol&iacute;tica.
	 * @throws IOException Cuando no se encuentra o no puede leerse el fichero de propiedades.
	 */
	static void setProperties(final Properties prop, final String policyId, final String format) throws IOException {

		String value = getProperty(policyId, PROPERTY_POLICY_IDENTIFIER, format);
		if (value != null) {
			prop.setProperty(PROPERTY_POLICY_IDENTIFIER, value);
		}
		value = getProperty(policyId, PROPERTY_POLICY_HASH_ALGORITHM, format);
		if (value != null) {
			prop.setProperty(PROPERTY_POLICY_HASH_ALGORITHM, value);
		}
		value = getProperty(policyId, PROPERTY_POLICY_HASH, format);
		if (value != null) {
			prop.setProperty(PROPERTY_POLICY_HASH, value);
		}
		value = getProperty(policyId, PROPERTY_POLICY_QUALIFIER, format);
		if (value != null) {
			prop.setProperty(PROPERTY_POLICY_QUALIFIER, value);
		}
		value = getProperty(policyId, PROPERTY_POLICY_DESCRIPTION, format);
		if (value != null) {
			prop.setProperty(PROPERTY_POLICY_DESCRIPTION, value);
		}
	}

	/**
	 * Recupera una propiedad de la configuraci&oacute;n establecida de pol&iacute;ticas de firma.
	 * @param id Identificador de la pol&iacute;tica.
	 * @param property Propiedad que se desea recuperar.
	 * @param format Modificador relativo al formato de firma.
	 * @return Valor de la propiedad de la pol&iacute;tica.
	 */
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
}
