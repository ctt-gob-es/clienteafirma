package es.gob.afirma.signers.xades;

import java.security.NoSuchAlgorithmException;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifier;
import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifierImpl;
import net.java.xades.security.xml.XAdES.SignatureProductionPlace;
import net.java.xades.security.xml.XAdES.SignatureProductionPlaceImpl;
import net.java.xades.security.xml.XAdES.SignerRole;
import net.java.xades.security.xml.XAdES.SignerRoleImpl;
import net.java.xades.security.xml.XAdES.XAdES_EPES;

import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;

final class XAdESCommonMetadataUtil {

	private XAdESCommonMetadataUtil() {
		// No instanciable
	}

	static void addCommonMetadata(final XAdES_EPES xades, final Properties extraParams) throws AOException {

		// SignaturePolicyIdentifier
		final SignaturePolicyIdentifier spi;
		try {
			spi = getPolicy(extraParams);
		}
		catch (final NoSuchAlgorithmException e1) {
			throw new AOException(
				"El algoritmo indicado para la politica (" + extraParams.getProperty("policyIdentifierHashAlgorithm") + ") no esta soportado: " + e1, e1 //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			);
		}
		if (spi != null) {
			xades.setSignaturePolicyIdentifier(spi);
		}

		// SignatureProductionPlace
		final SignatureProductionPlace spp = getSignatureProductionPlace(extraParams);
		if (spp != null) {
			xades.setSignatureProductionPlace(spp);
		}

		// SignerRole
		final SignerRole signerRole = parseSignerRole(extraParams);
		if (signerRole != null) {
			xades.setSignerRole(signerRole);
		}

	}

	private static SignerRole parseSignerRole(final Properties extraParams) {
		if (extraParams == null) {
			return null;
		}
		SignerRole signerRole = null;
		try {
			final String claimedRole = extraParams.getProperty("signerClaimedRoles"); //$NON-NLS-1$
			signerRole = new SignerRoleImpl();
			if (claimedRole != null) {
				final String[] roles = claimedRole.split(Pattern.quote("|")); //$NON-NLS-1$
				for (final String role : roles) {
					signerRole.addClaimedRole(role);
				}
			}
		}
		catch (final Exception e) {
			// Se ignoran los errores, el parametro es opcional
		}
		return signerRole;
	}

	private static SignatureProductionPlace getSignatureProductionPlace(final Properties extraParams) {
		if (extraParams == null) {
			return null;
		}
		return getSignatureProductionPlace(
			extraParams.getProperty("signatureProductionCity"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionProvince"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionPostalCode"), //$NON-NLS-1$
			extraParams.getProperty("signatureProductionCountry") //$NON-NLS-1$
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

	private static SignaturePolicyIdentifier getPolicy(final Properties extraParams) throws NoSuchAlgorithmException {
		if (extraParams == null) {
			return null;
		}
		return getPolicy(
			extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
			extraParams.getProperty("policyIdentifierHash"), //$NON-NLS-1$
			extraParams.getProperty("policyIdentifierHashAlgorithm"), //$NON-NLS-1$
			extraParams.getProperty("policyDescription"), //$NON-NLS-1$
			extraParams.getProperty("policyQualifier") //$NON-NLS-1$
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

		final Logger logger = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

		String identifier;
		try {
			new Oid(id);
			logger.warning(
				"Se proporciono directamente un OID como identificador de politica (" + id + "), se tranformara en URN con el prefijo 'urn:oid:'" //$NON-NLS-1$ //$NON-NLS-2$
			);
			identifier = "urn:oid:" + id; //$NON-NLS-1$
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
			logger.warning(
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
