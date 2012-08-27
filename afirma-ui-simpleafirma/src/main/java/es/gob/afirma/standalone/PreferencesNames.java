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

/** Nombre de las preferencias de configuraci&oacute;n del programa.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PreferencesNames {

	private PreferencesNames() {
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
	public static final String PREFERENCE_CADES_POLICY_IDENTIFIER_HASH = "cadesPolicyIdentifierHash"; //$NON-NLS-1$

	/** Algoritmo de la huella digital del identificador de la pol&iacute;tica de firma para CAdES. */
	public static final String PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM = "cadesPolicyIdentifierHashAlgorithm"; //$NON-NLS-1$

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

	/** Cargo real del firmante en firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE = "xadesSignerCertifiedRole"; //$NON-NLS-1$

	/** Formato de las firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGN_FORMAT = "xadesSignFormat"; //$NON-NLS-1$


	/** Motivo de la firma en firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGN_REASON = "padesSignReason"; //$NON-NLS-1$

	/** Ciudad de firma para firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGN_PRODUCTION_CITY = "padesSignProductionCity"; //$NON-NLS-1$

	/** Constacto del firmante en firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGNER_CONTACT = "padesSignerContact"; //$NON-NLS-1$

}
