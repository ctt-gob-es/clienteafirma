package es.gob.afirma.standalone.ui;

import java.util.Properties;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.standalone.PreferencesManager;

final class ExtraParamsHelper {

	private ExtraParamsHelper() {
		// No permitimos la instanciacion
	}

	final static Properties loadExtraParamsForSigner(final AOSigner signer) {

		final Properties p;
		if (signer instanceof AOXAdESSigner || signer instanceof AOFacturaESigner) {
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

	/**
	 * Obtiene la configuraci&oacute;n para las firmas XAdES.
	 * @return Propiedades para la configuraci&oacute;n de las firmas XAdES.
	 */
	private static Properties loadXAdESExtraParams() {

		final Properties p = new Properties();
        p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        // Preferencias de politica de firma
        final String policyId = PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER, ""); //$NON-NLS-1$

        if (!"".equals(policyId)) { //$NON-NLS-1$
        	p.put(
        		"policyIdentifier", //$NON-NLS-1$
        		policyId
        	);

        	if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
        		p.put(
        			"policyIdentifierHash", //$NON-NLS-1$
        			PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, "") //$NON-NLS-1$
        		);
        	}
        	if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
        		p.put(
        			"policyIdentifierHashAlgorithm", //$NON-NLS-1$
        			PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, "") //$NON-NLS-1$
        		);
        	}
        	if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_QUALIFIER, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
        		p.put(
        			"policyQualifier", //$NON-NLS-1$
        			PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_POLICY_QUALIFIER, "") //$NON-NLS-1$
        		);
        	}
        }

        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
            p.put(
        		"signerClaimedRoles", //$NON-NLS-1$
        		PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, "") //$NON-NLS-1$
    		);
        }
        // Esta propiedad se comparte con PAdES, hay que comprobar que signer tenemos
        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionCity", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionProvince", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionPostalCode", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionCountry", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGN_FORMAT, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"format", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_XADES_SIGN_FORMAT, "") //$NON-NLS-1$
			);
        }

		return p;
	}

	/**
	 * Obtiene la configuraci&oacute;n para las firmas PAdES.
	 * @return Propiedades para la configuraci&oacute;n de las firmas PAdES.
	 */
	private static Properties loadPAdESExtraParams() {

		final Properties p = new Properties();
        p.put("allowSigningCertifiedPdfs", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        // Preferencias de politica de firma
        final String policyId = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER, ""); //$NON-NLS-1$


        if (!"".equals(policyId)) { //$NON-NLS-1$
        	p.put(
        		"policyIdentifier", //$NON-NLS-1$
        		policyId
        	);
        	if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
        		p.put(
        			"policyIdentifierHash", //$NON-NLS-1$
        			PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, "") //$NON-NLS-1$
        		);
        	}
        	if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
        		p.put(
        			"policyIdentifierHashAlgorithm", //$NON-NLS-1$
        			PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, "") //$NON-NLS-1$
        		);
        	}
        	if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_QUALIFIER, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
        		p.put(
        			"policyQualifier", //$NON-NLS-1$
        			PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_POLICY_QUALIFIER, "") //$NON-NLS-1$
        		);
        	}
        }

        // Preferencias de PAdES
        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGN_REASON, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signReason", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGN_REASON, "") //$NON-NLS-1$
			);
        }
        // Esta propiedad se comparte con XAdES, hay que comprobar que signer tenemos
        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGN_PRODUCTION_CITY, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionCity", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGN_PRODUCTION_CITY, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGNER_CONTACT, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signerContact", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_SIGNER_CONTACT, "") //$NON-NLS-1$
			);
        }
        if (PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_FORMAT, null) != null) {
        	p.put(
    			"signatureSubFilter", //$NON-NLS-1$
    			PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_FORMAT, AOSignConstants.PADES_SUBFILTER_BASIC)
			);
        }

		return p;
	}

	/** Obtiene la configuraci&oacute;n para las firmas CAdES.
	 * @return Propiedades para la configuraci&oacute;n de las firmas CAdES. */
	private static Properties loadCAdESExtraParams() {
		final Properties p = new Properties();

        // Preferencias de politica de firma
        final String policyId = PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER, ""); //$NON-NLS-1$
        if (!"".equals(policyId)) { //$NON-NLS-1$
        	p.put(
        		"policyIdentifier", //$NON-NLS-1$
        		policyId
        	);
	        if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
				p.put(
			    	"policyIdentifierHash", //$NON-NLS-1$
			        PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH, "") //$NON-NLS-1$
			    );
		    }
		    if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
				p.put(
			    	"policyIdentifierHashAlgorithm", //$NON-NLS-1$
			        PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM, "") //$NON-NLS-1$
				);
		    }
		    if (!"".equals(PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_QUALIFIER, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
				p.put(
			    	"policyQualifier", //$NON-NLS-1$
			        PreferencesManager.get(PreferencesManager.PREFERENCE_CADES_POLICY_QUALIFIER, "") //$NON-NLS-1$
			    );
		    }
        }

        // Preferencias de CAdES
        // Esta propiedad se comparte con otros formatos, hay que comprobar que signer tenemos
        p.put(
    		"mode", //$NON-NLS-1$
    		PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_CADES_IMPLICIT, true) ?
    				"implicit" : "explicit" //$NON-NLS-1$ //$NON-NLS-2$
		);

        return p;
	}
}
