/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone;

import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

/** Nombre de las preferencias de configuraci&oacute;n del programa.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PreferencesManager {

	/** Objecto general de preferencias donde se guarda la configuraci&oacute;n de la
	 * aplicaci&oacute;n. */
	private static Preferences preferences;
	static {
		preferences = Preferences.userNodeForPackage(PreferencesManager.class);
	}

	private PreferencesManager() {
		/** No permitimos la instanciacion */
	}

	/** Algoritmo de firma. */
	public static final String PREFERENCE_SIGNATURE_ALGORITHM = "signatureAlgorithm"; //$NON-NLS-1$

	/** Identificador de la pol&iacute;tica de firma para XAdES. */
	public static final String PREFERENCE_XADES_POLICY_IDENTIFIER = "xadesPolicyIdentifier"; //$NON-NLS-1$

	/** Huella digital del identificador de la pol&iacute;tica de firma para XAdES. */
	public static final String PREFERENCE_XADES_POLICY_IDENTIFIER_HASH = "xadesPolicyIdentifierHash"; //$NON-NLS-1$

	/** Algoritmo de la huella digital del identificador de la pol&iacute;tica de firma para XAdES. */
	public static final String PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM = "xadesPolicyIdentifierHashAlgorithm"; //$NON-NLS-1$

	/** Calificador de la pol&iacute;tica de firma para XAdES. */
	public static final String PREFERENCE_XADES_POLICY_QUALIFIER = "xadesPolicyQualifier"; //$NON-NLS-1$

	/** Identificador de la pol&iacute;tica de firma para PAdES. */
	public static final String PREFERENCE_PADES_POLICY_IDENTIFIER = "padesPolicyIdentifier"; //$NON-NLS-1$

	/** Huella digital del identificador de la pol&iacute;tica de firma para PAdES. */
	public static final String PREFERENCE_PADES_POLICY_IDENTIFIER_HASH = "padesPolicyIdentifierHash"; //$NON-NLS-1$

	/** Algoritmo de la huella digital del identificador de la pol&iacute;tica de firma para PAdES. */
	public static final String PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM = "padesPolicyIdentifierHashAlgorithm"; //$NON-NLS-1$

	/** Calificador de la pol&iacute;tica de firma para PAdES. */
	public static final String PREFERENCE_PADES_POLICY_QUALIFIER = "padesPolicyQualifier"; //$NON-NLS-1$

	/** Identificador de la pol&iacute;tica de firma para CAdES. */
	public static final String PREFERENCE_CADES_POLICY_IDENTIFIER = "cadesPolicyIdentifier"; //$NON-NLS-1$

	/** Huella digital del identificador de la pol&iacute;tica de firma para CAdES. */
	public static final String PREFERENCE_CADES_POLICY_HASH = "cadesPolicyIdentifierHash"; //$NON-NLS-1$

	/** Algoritmo de la huella digital del identificador de la pol&iacute;tica de firma para CAdES. */
	public static final String PREFERENCE_CADES_POLICY_HASH_ALGORITHM = "cadesPolicyIdentifierHashAlgorithm"; //$NON-NLS-1$

	/** Calificador de la pol&iacute;tica de firma para CAdES. */
	public static final String PREFERENCE_CADES_POLICY_QUALIFIER = "cadesPolicyQualifier"; //$NON-NLS-1$

	/** Evitar la pregunta al cerrar la aplicaci&oacute;n o no. */
	public static final String PREFERENCE_OMIT_ASKONCLOSE = "omitAskOnClose"; //$NON-NLS-1$

	/** CAdES en modo impl&iacute;cito (attached) o no. */
	public static final String PREFERENCE_CADES_IMPLICIT = "cadesImplicitMode"; //$NON-NLS-1$

	/** Ciudad de firma para firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY = "xadesSignatureProductionCity"; //$NON-NLS-1$

	/** Provincia de firma para firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE = "xadesSignatureProductionProvince"; //$NON-NLS-1$

	/** C&oacute;digo de firma para firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE = "xadesSignatureProductionPostalCode"; //$NON-NLS-1$

	/** Pa&iacute;s de firma para firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY = "xadesSignatureProductionCountry"; //$NON-NLS-1$

	/** Cargo supuesto para el firmante en firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNER_CLAIMED_ROLE = "xadesSignerClaimedRole"; //$NON-NLS-1$

	/** Formato de las firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGN_FORMAT = "xadesSignFormat"; //$NON-NLS-1$

	/** Motivo de la firma en firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGN_REASON = "padesSignReason"; //$NON-NLS-1$

	/** Ciudad de firma para firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGN_PRODUCTION_CITY = "padesSignProductionCity"; //$NON-NLS-1$

	/** Contacto del firmante en firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGNER_CONTACT = "padesSignerContact"; //$NON-NLS-1$

	/** Formato de firma PAdES (PAdES B&aacute;sico o PAdES-BES). */
	public static final String PREFERENCE_PADES_FORMAT = "padesBasicFormat"; //$NON-NLS-1$

	/** Recupera el valor de una cadena de texto almacenada entre las preferencias de la
	 * aplicaci&oacute;n.
	 * @param key Clave del valor que queremos recuperar.
	 * @param def Valor que se devolver&aacute;a si la preferencia no se encontraba almacenada.
	 * @return La preferencia almacenada o {@code def} si no se encontr&oacute;. */
	public static String get(final String key, final String def) {
		return preferences.get(key, def);
	}

	/** Recupera el valor {@code true} o {@code false} almacenado entre las preferencias de la
	 * aplicaci&oacute;n.
	 * @param key Clave del valor que queremos recuperar.
	 * @param def Valor que se devolver&aacute;a si la preferencia no se encontraba almacenada.
	 * @return La preferencia almacenada o {@code def} si no se encontr&oacute;. */
	public static boolean getBoolean(final String key, final boolean def) {
		return preferences.getBoolean(key, def);
	}

	/** Establece una cadena de texto en la configuraci&oacute;n de la aplicaci&oacute;n
	 * identific&aacute;ndola con una clave. Para realizar el guardado completo, es
	 * necesario ejecutar el m&eacute;todo {@code flush()}.
	 * @param key Clave con la que identificaremos el valor.
	 * @param value Valor que se desea almacenar. */
	public static void put(final String key, final String value) {
		preferences.put(key, value);
	}

	/** Establece un {@code true} o {@code false} en la configuraci&oacute;n de la aplicaci&oacute;n
	 * identific&aacute;ndolo con una clave. Para realizar el guardado completo, es
	 * necesario ejecutar el m&eacute;todo {@code flush()}.
	 * @param key Clave con la que identificaremos el valor.
	 * @param value Valor que se desea almacenar. */
	public static void putBoolean(final String key, final boolean value) {
		preferences.putBoolean(key, value);
	}

	/** Elimina una clave de entre la configuraci&oacute;n de la aplicaci&oacute;n.
	 * @param key Clave que eliminar. */
	public static void remove(final String key) {
		preferences.remove(key);
	}

	/** Almacena en las preferencias de la aplicaci&oacute;n todos los valores
	 * establecidos hasta el momento.
	 * @throws BackingStoreException Cuando ocurre un error durante el guardado. */
	public static void flush() throws BackingStoreException {
		preferences.flush();
	}
}
