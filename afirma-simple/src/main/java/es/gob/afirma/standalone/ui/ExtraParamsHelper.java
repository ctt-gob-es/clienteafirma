package es.gob.afirma.standalone.ui;

import java.util.Properties;
import java.util.prefs.Preferences;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.standalone.PreferencesNames;

final class ExtraParamsHelper {

	private ExtraParamsHelper() {
		// No permitimos la instanciacion
	}

	final static Properties preferencesToExtraParams(final Preferences preferences, final AOSigner signer) {
        final Properties p = new Properties();
        p.put("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("allowSigningCertifiedPdfs", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        // Preferencias de politica de firma

        String policyId = ""; //$NON-NLS-1$
        if (signer instanceof AOXAdESSigner) {
        	policyId = preferences.get(PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER, ""); //$NON-NLS-1$
        }
        else if (signer instanceof AOPDFSigner) {
        	policyId = preferences.get(PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER, ""); //$NON-NLS-1$
        }
        else if (signer instanceof AOCAdESSigner) {
        	policyId = preferences.get(PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER, ""); //$NON-NLS-1$
        }
        if (!"".equals(policyId)) { //$NON-NLS-1$
        	p.put(
        		"policyIdentifier", //$NON-NLS-1$
        		policyId
    		);
            if (signer instanceof AOXAdESSigner) {
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyIdentifierHash", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, "") //$NON-NLS-1$
		    		);
	            }
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyIdentifierHashAlgorithm", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, "") //$NON-NLS-1$
		    		);
	            }
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_POLICY_QUALIFIER, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyQualifier", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_XADES_POLICY_QUALIFIER, "") //$NON-NLS-1$
		    		);
	            }
            }
            else if (signer instanceof AOPDFSigner) {
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyIdentifierHash", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, "") //$NON-NLS-1$
		    		);
	            }
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyIdentifierHashAlgorithm", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, "") //$NON-NLS-1$
		    		);
	            }
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_PADES_POLICY_QUALIFIER, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyQualifier", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_PADES_POLICY_QUALIFIER, "") //$NON-NLS-1$
		    		);
	            }
            }
            else if (signer instanceof AOCAdESSigner) {
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyIdentifierHash", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH, "") //$NON-NLS-1$
		    		);
	            }
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyIdentifierHashAlgorithm", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM, "") //$NON-NLS-1$
		    		);
	            }
	            if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_CADES_POLICY_QUALIFIER, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
		            p.put(
		        		"policyQualifier", //$NON-NLS-1$
		        		preferences.get(PreferencesNames.PREFERENCE_CADES_POLICY_QUALIFIER, "") //$NON-NLS-1$
		    		);
	            }
            }

        }


        // Preferencias de XAdES
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, ""))) { //$NON-NLS-1$ //$NON-NLS-2$
            p.put(
        		"signerClaimedRole", //$NON-NLS-1$
        		preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, "") //$NON-NLS-1$
    		);
        }
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signerCertifiedRole", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE, "") //$NON-NLS-1$
			);
        }
        // Esta propiedad se comparte con PAdES, hay que comprobar que signer tenemos
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, "")) && signer instanceof AOXAdESSigner) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionCity", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionProvince", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionPostalCode", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionCountry", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_XADES_SIGN_FORMAT, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"format", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_XADES_SIGN_FORMAT, "") //$NON-NLS-1$
			);
        }

        // Preferencias de PAdES
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_PADES_SIGN_REASON, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signReason", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_PADES_SIGN_REASON, "") //$NON-NLS-1$
			);
        }
        // Esta propiedad se comparte con XAdES, hay que comprobar que signer tenemos
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_PADES_SIGN_PRODUCTION_CITY, "")) && signer instanceof AOPDFSigner) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signatureProductionCity", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_PADES_SIGN_PRODUCTION_CITY, "") //$NON-NLS-1$
			);
        }
        if (!"".equals(preferences.get(PreferencesNames.PREFERENCE_PADES_SIGNER_CONTACT, ""))) {  //$NON-NLS-1$//$NON-NLS-2$
        	p.put(
    			"signerContact", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_PADES_SIGNER_CONTACT, "") //$NON-NLS-1$
			);
        }
        if (preferences.get(PreferencesNames.PREFERENCE_PADES_FORMAT, null) != null) {
        	p.put(
    			"signatureSubFilter", //$NON-NLS-1$
    			preferences.get(PreferencesNames.PREFERENCE_PADES_FORMAT, AOSignConstants.PADES_SUBFILTER_BASIC)
			);
        }

        // Preferencias de CAdES
        // Esta propiedad se comparte con otros formatos, hay que comprobar que signer tenemos
        if (signer instanceof AOCAdESSigner) {
        	p.put(
    			"mode", //$NON-NLS-1$
    			preferences.getBoolean(PreferencesNames.PREFERENCE_CADES_IMPLICIT, true) ?
    					"implicit" : "explicit" //$NON-NLS-1$ //$NON-NLS-2$
			);
        }

        return p;

	}

}
