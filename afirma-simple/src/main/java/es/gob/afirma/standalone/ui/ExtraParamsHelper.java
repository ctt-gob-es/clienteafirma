/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.util.Properties;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xades.XAdESExtraParams;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

final class ExtraParamsHelper {

	private ExtraParamsHelper() {
		// No permitimos la instanciacion
	}

	static Properties loadExtraParamsForSigner(final AOSigner signer) {

		final Properties p;
		if (signer instanceof AOFacturaESigner) {
        	p = loadFacturaEExtraParams();
        }
		else if (signer instanceof AOXAdESSigner) {
        	p = loadXAdESExtraParams();
        }
        else if (signer instanceof AOPDFSigner) {
        	p = loadPAdESExtraParams();
        }
        else {
        	p = loadCAdESExtraParams();
        }

		return p;
	}

	/** Obtiene la configuraci&oacute;n para las firmas Factura-E.
	 * @return Propiedades para la configuraci&oacute;n de las firmas Factura-E. */
	private static Properties loadFacturaEExtraParams() {
		final Properties p = new Properties();

        // Preferencias de politica de firma
        final String policyId = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER);

        if (policyId != null && !policyId.trim().isEmpty()) {
        	p.put(XAdESExtraParams.POLICY_IDENTIFIER, policyId);

        	final String policyIdHash = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH);
        	if (policyIdHash != null && !policyIdHash.trim().isEmpty()) {
        		p.put(XAdESExtraParams.POLICY_IDENTIFIER_HASH, policyIdHash);
        	}
        	final String policyHashAlgorithm = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM);
        	if (policyHashAlgorithm != null && !policyHashAlgorithm.trim().isEmpty()) {
        		p.put(XAdESExtraParams.POLICY_IDENTIFIER_HASH_ALGORITHM, policyHashAlgorithm);
        	}
        }

		// Metadatos sobre la "produccion" de la firma de la factura
		final String signatureCity = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY);
        if (signatureCity != null && !signatureCity.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNATURE_PRODUCTION_CITY, signatureCity);
        }
        final String signatureProvince = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE);
        if (signatureProvince != null && !signatureProvince.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNATURE_PRODUCTION_PROVINCE, signatureProvince);
        }
        final String signaturePC = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE);
        if (signaturePC != null && !signaturePC.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNATURE_PRODUCTION_POSTAL_CODE, signaturePC);
        }
        final String signatureCountry = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY);
        if (signatureCountry != null && !signatureCountry.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNATURE_PRODUCTION_COUNTRY, signatureCountry);
        }

        // Papel del firmante de la factura, es un campo acotado
        final String signerRole = PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_SIGNER_ROLE);
        if (signerRole != null && !signerRole.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNER_CLAIMED_ROLES, signerRole);
        }

		return p;
	}

	/** Obtiene la configuraci&oacute;n para las firmas XAdES.
	 * @return Propiedades para la configuraci&oacute;n de las firmas XAdES. */
	private static Properties loadXAdESExtraParams() {

		final Properties p = new Properties();
        p.put(XAdESExtraParams.IGNORE_STYLE_SHEETS, "false"); //$NON-NLS-1$

        // Preferencias de politica de firma
        final String policyId = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER);

        if (policyId != null && !policyId.trim().isEmpty()) {
        	p.put(XAdESExtraParams.POLICY_IDENTIFIER, policyId);

        	final String policyIdHash = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_HASH);
        	if (policyIdHash != null && !policyIdHash.trim().isEmpty()) {
        		p.put(XAdESExtraParams.POLICY_IDENTIFIER_HASH, policyIdHash);
        	}
        	final String policyHashAlgorithm = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_HASH_ALGORITHM);
        	if (policyHashAlgorithm != null && !policyHashAlgorithm.trim().isEmpty()) {
        		p.put(XAdESExtraParams.POLICY_IDENTIFIER_HASH_ALGORITHM, policyHashAlgorithm);
        	}
        	final String policyQualifier = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_QUALIFIER);
        	if (policyQualifier != null && !policyQualifier.trim().isEmpty()) {
        		p.put(XAdESExtraParams.POLICY_QUALIFIER, policyQualifier);
        	}
        }

        final String claimedRole = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE);
        if (claimedRole != null && !claimedRole.trim().isEmpty()) {
            p.put(XAdESExtraParams.SIGNER_CLAIMED_ROLES, claimedRole);
        }

		final String signatureCity = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY);
        if (signatureCity != null && !signatureCity.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNATURE_PRODUCTION_CITY, signatureCity);
        }
        final String signatureProvince = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE);
        if (signatureProvince != null && !signatureProvince.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNATURE_PRODUCTION_PROVINCE, signatureProvince);
        }
        final String signaturePC = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE);
        if (signaturePC != null && !signaturePC.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNATURE_PRODUCTION_POSTAL_CODE, signaturePC);
        }
        final String signatureCountry = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY);
        if (signatureCountry != null && !signatureCountry.trim().isEmpty()) {
        	p.put(XAdESExtraParams.SIGNATURE_PRODUCTION_COUNTRY, signatureCountry);
        }

        final String signFormat = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGN_FORMAT);
        if (signFormat != null && !signFormat.trim().isEmpty()) {
        	p.put(XAdESExtraParams.FORMAT, signFormat);
        }

		return p;
	}

	/** Obtiene la configuraci&oacute;n para las firmas PAdES.
	 * @return Propiedades para la configuraci&oacute;n de las firmas PAdES. */
	private static Properties loadPAdESExtraParams() {

		final Properties p = new Properties();

        // Preferencias de politica de firma PAdES
        final String policyId = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER);
        if (policyId != null && !policyId.trim().isEmpty()) {
        	p.put(PdfExtraParams.POLICY_IDENTIFIER, policyId);

        	final String policyIdHash = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_HASH);
        	if (policyIdHash != null && !policyIdHash.trim().isEmpty()) {
        		p.put(PdfExtraParams.POLICY_IDENTIFIER_HASH, policyIdHash);
        	}
        	final String policyHashAlgorithm = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_HASH_ALGORITHM);
        	if (policyHashAlgorithm != null && !policyHashAlgorithm.trim().isEmpty()) {
        		p.put(PdfExtraParams.POLICY_IDENTIFIER_HASH_ALGORITHM, policyHashAlgorithm);
        	}
        	final String policyQualifier = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_QUALIFIER);
        	if (policyQualifier != null && !policyQualifier.trim().isEmpty()) {
        		p.put(PdfExtraParams.POLICY_QUALIFIER, policyQualifier);
        	}
        }

        // Metadatos PAdES
        final String signReason = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGN_REASON);
        if (signReason != null && !signReason.trim().isEmpty()) {
        	p.put(PdfExtraParams.SIGN_REASON, signReason);
        }
        final String productionCity = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGN_PRODUCTION_CITY);
        if (productionCity != null && !productionCity.trim().isEmpty()) {
        	p.put(PdfExtraParams.SIGNATURE_PRODUCTION_CITY, productionCity);
        }

        final String contact = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGNER_CONTACT);
        if (contact != null && !contact.trim().isEmpty()) {
        	p.put(PdfExtraParams.SIGNER_CONTACT, contact);
        }

        // PAdES BES/Basic
        final String subfilter = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_FORMAT);
        if (subfilter != null && !subfilter.trim().isEmpty()) {
        	p.put(PdfExtraParams.SIGNATURE_SUBFILTER, subfilter);
        }

        // Ofuscacion de los datos del certificado de firma
        final boolean obfuscate = Boolean.parseBoolean(PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_OBFUSCATE_CERT_INFO));
       	p.put(PdfExtraParams.OBFUSCATE_CERT_DATA, Boolean.toString(obfuscate));

		return p;
	}

	/** Obtiene la configuraci&oacute;n para las firmas CAdES.
	 * @return Propiedades para la configuraci&oacute;n de las firmas CAdES. */
	private static Properties loadCAdESExtraParams() {
		final Properties p = new Properties();

        // Preferencias de politica de firma
        final String policyId = PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER);
        if (policyId != null && !policyId.trim().isEmpty()) {
        	p.put(CAdESExtraParams.POLICY_IDENTIFIER, policyId);

        	final String policyIdHash = PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_HASH);
        	if (policyIdHash != null && !policyIdHash.trim().isEmpty()) {
        		p.put(CAdESExtraParams.POLICY_IDENTIFIER_HASH, policyIdHash);
        	}
        	final String policyHashAlgorithm = PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_HASH_ALGORITHM);
        	if (policyHashAlgorithm != null && !policyHashAlgorithm.trim().isEmpty()) {
        		p.put(CAdESExtraParams.POLICY_IDENTIFIER_HASH_ALGORITHM, policyHashAlgorithm);
        	}
        	final String policyQualifier = PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_QUALIFIER);
        	if (policyQualifier != null && !policyQualifier.trim().isEmpty()) {
        		p.put(CAdESExtraParams.POLICY_QUALIFIER, policyQualifier);
        	}
        }

        // Preferencias de CAdES
        // Esta propiedad se comparte con otros formatos, hay que comprobar que signer tenemos
        p.put(
        	CAdESExtraParams.MODE,
    		PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_CADES_IMPLICIT) ?
    				"implicit" : //$NON-NLS-1$
					"explicit" //$NON-NLS-1$
		);
        return p;
	}

	/**
	 * Agrega a la configuraci&oacute;n de firma la opci&oacute;n para permitir firmar documentos certificados.
	 * @param extraParams Configuraci&oacute;n a la que agregar la opci&oacute;n.
	 */
	public static void addParamToCertifiedPdf(final Properties extraParams) {
		extraParams.setProperty(PdfExtraParams.ALLOW_SIGNING_CERTIFIED_PDFS, Boolean.TRUE.toString());
	}

	/**
	 * Agrega a la configuraci&oacute;n de firma la opci&oacute;n para permitir firmar documentos con
	 * firmas previas no registradas.
	 * @param extraParams Configuraci&oacute;n a la que agregar la opci&oacute;n.
	 */
	public static void addParamToUnregisteredPdf(final Properties extraParams) {
		extraParams.setProperty(PdfExtraParams.ALLOW_COSIGNING_UNREGISTERED_SIGNATURES, Boolean.TRUE.toString());
	}
}
