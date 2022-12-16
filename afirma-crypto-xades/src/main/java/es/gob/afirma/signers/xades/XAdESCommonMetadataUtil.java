/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignaturePolicyIdentifier;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignaturePolicyIdentifierImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignatureProductionPlace;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignatureProductionPlaceImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignatureProductionPlaceV2;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignatureProductionPlaceV2Impl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignerRole;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignerRoleImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignerRoleV2;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignerRoleV2Impl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdESBase;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XadesWithBaselineAttributes;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XadesWithBasicAttributes;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XadesWithExplicitPolicy;

final class XAdESCommonMetadataUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

	private XAdESCommonMetadataUtil() {
		// No instanciable
	}

	static void addCommonMetadata(final XAdESBase xades, final Properties extraParams)
			throws AOException {

		// SignaturePolicyIdentifier
		if (xades instanceof XadesWithExplicitPolicy) {
			final SignaturePolicyIdentifier spi;
			try {
				spi = getPolicy(extraParams);
			}
			catch (final NoSuchAlgorithmException e1) {
				throw new AOException(
						"El algoritmo indicado para la politica (" + extraParams.getProperty(XAdESExtraParams.POLICY_IDENTIFIER_HASH_ALGORITHM) + ") no esta soportado: " + e1, e1 //$NON-NLS-1$ //$NON-NLS-2$
						);
			}
			if (spi != null) {
				((XadesWithExplicitPolicy) xades).setSignaturePolicyIdentifier(spi);
			}
		}

		// BES attributes
		if (xades instanceof XadesWithBasicAttributes) {

			// SignatureProductionPlace
			final SignatureProductionPlace spp = getSignatureProductionPlace(extraParams);
			if (spp != null) {
				((XadesWithBasicAttributes) xades).setSignatureProductionPlace(spp);
			}

			// SignerRole
			final SignerRole signerRole = parseSignerRole(extraParams);
			if (signerRole != null) {
				((XadesWithBasicAttributes) xades).setSignerRole(signerRole);
			}
		}

		// Baseline B-Level attributes
		if (xades instanceof XadesWithBaselineAttributes) {

			// SignatureProductionPlaceV2
			final SignatureProductionPlaceV2 spp = getSignatureProductionPlaceV2(extraParams);
			if (spp != null) {
				((XadesWithBaselineAttributes) xades).setSignatureProductionPlaceV2(spp);
			}

			// SignerRoleV2
			final SignerRoleV2 signerRole = parseSignerRoleV2(extraParams);
			if (signerRole != null) {
				((XadesWithBaselineAttributes) xades).setSignerRoleV2(signerRole);
			}
		}
	}

	private static SignerRole parseSignerRole(final Properties extraParams) {
		if (extraParams == null) {
			return null;
		}
		SignerRole signerRole = null;
		final String[] claimedRoles = parseClaimedRoles(extraParams);
		if (claimedRoles != null) {
			signerRole = new SignerRoleImpl();
			for (final String role : claimedRoles) {
				signerRole.addClaimedRole(role);
			}
		}
		return signerRole;
	}

	private static SignerRoleV2 parseSignerRoleV2(final Properties extraParams) {
		if (extraParams == null) {
			return null;
		}
		SignerRoleV2 signerRole = null;
		final String[] claimedRoles = parseClaimedRoles(extraParams);
		if (claimedRoles != null) {
			signerRole = new SignerRoleV2Impl();
			for (final String role : claimedRoles) {
				signerRole.addClaimedRole(role);
			}
		}
		return signerRole;
	}

	private static String[] parseClaimedRoles(final Properties extraParams) {
		String[] claimedRoles = null;
		final String claimedRolesParam = extraParams.getProperty(XAdESExtraParams.SIGNER_CLAIMED_ROLES);
		if (claimedRolesParam != null) {
			claimedRoles = claimedRolesParam.split(Pattern.quote("|")); //$NON-NLS-1$
			final List<String> preClaimedRolesList = new ArrayList<>();
			for (final String role : claimedRoles) {
				if (!role.trim().isEmpty()) {
					preClaimedRolesList.add(role);
				}
			}
			claimedRoles = preClaimedRolesList.toArray(new String[0]);
		}
		return claimedRoles;
	}

	private static SignatureProductionPlace getSignatureProductionPlace(final Properties extraParams) {
		if (extraParams == null) {
			return null;
		}
		return getSignatureProductionPlace(
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCTION_CITY),
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCTION_PROVINCE),
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCTION_POSTAL_CODE),
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCTION_COUNTRY)
		);
	}

	private static SignatureProductionPlace getSignatureProductionPlace(final String city,
			final String province,
			final String postalCode,
			final String country) {
		if (city == null && province == null && postalCode == null && country == null) {
			return null;
		}
		return new SignatureProductionPlaceImpl(city, province, postalCode, country);
	}

	private static SignatureProductionPlaceV2 getSignatureProductionPlaceV2(final Properties extraParams) {
		if (extraParams == null) {
			return null;
		}
		return getSignatureProductionPlaceV2(
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCTION_CITY),
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCCTION_STREET_ADDRESS),
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCTION_PROVINCE),
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCTION_POSTAL_CODE),
			extraParams.getProperty(XAdESExtraParams.SIGNATURE_PRODUCTION_COUNTRY)
		);
	}

    private static SignatureProductionPlaceV2 getSignatureProductionPlaceV2(
    		final String city, final String streetAddress, final String province,
    		final String postalCode, final String country) {
    	if (city == null && streetAddress == null && province == null &&
    			postalCode == null && country == null) {
    		return null;
    	}
    	return new SignatureProductionPlaceV2Impl(city, streetAddress, province, postalCode, country);
    }

	private static SignaturePolicyIdentifier getPolicy(final Properties extraParams) throws NoSuchAlgorithmException {
		if (extraParams == null) {
			return null;
		}
		return getPolicy(
			extraParams.getProperty(XAdESExtraParams.POLICY_IDENTIFIER),
			extraParams.getProperty(XAdESExtraParams.POLICY_IDENTIFIER_HASH),
			extraParams.getProperty(XAdESExtraParams.POLICY_IDENTIFIER_HASH_ALGORITHM),
			extraParams.getProperty(XAdESExtraParams.POLICY_DESCRIPTION),
			extraParams.getProperty(XAdESExtraParams.POLICY_QUALIFIER)
		);
	}

	private static SignaturePolicyIdentifier getPolicy(final String id,
			                                           final String identifierHash,
			                                           final String identifierHashAlgorithm,
			                                           final String description,
			                                           final String qualifier) throws NoSuchAlgorithmException {
		if (id == null) {
			return null;
		}

		String identifier;
		try {
			// Construimos el objeto OID solo para saber si el ID es un OID
			new Oid(id);
			if (id.toLowerCase().startsWith("urn:oid:")) { //$NON-NLS-1$
				identifier= id;
			}
			else {
				LOGGER.warning("Se proporciono directamente un OID como identificador de politica (" + id //$NON-NLS-1$
						+ "), se tranformara en URN con el prefijo 'urn:oid:'"); //$NON-NLS-1$
				identifier = "urn:oid:" + id; //$NON-NLS-1$
			}
		}
		catch (final Exception e1) {
			identifier = id;
		}

		final String hashAlgo = identifierHashAlgorithm != null ? XAdESUtil.getDigestMethodByCommonName(identifierHashAlgorithm) : null;

		final SignaturePolicyIdentifier spi = new SignaturePolicyIdentifierImpl(false);
		try {
			spi.setIdentifier(identifier, hashAlgo != null ? identifierHash
					: null, hashAlgo);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No se ha podido acceder al identificador ('" + identifier + "') de la politica de firma, no se anadira este campo: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return null;
		}
		// Error en JXAdES. Si la descripcion es nula toda la firma falla.
		final String desc = description != null ? description : ""; //$NON-NLS-1$
		spi.setDescription(desc);

		if (qualifier != null) {
			spi.setQualifier(qualifier);
		}
		return spi;
	}

}
