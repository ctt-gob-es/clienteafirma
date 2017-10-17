/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.cades.CAdESSignerMetadataHelper;
import es.gob.afirma.signers.cades.CommitmentTypeIndicationsHelper;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Operaciones de cofirma CAdES. */
public final class AOCAdESCoSigner implements AOCoSigner {

	private static final String SHA1_ALGORITHM = "SHA1"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	 //$NON-NLS-1$

	/** {@inheritDoc} */
	@Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties xParams) throws AOException, IOException {

        CAdESMultiUtil.checkUnsupportedAttributes(sign);

    	//****************************************************************************************************
    	//*************** LECTURA PARAMETROS ADICIONALES *****************************************************

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final boolean onlySigningCertificate = Boolean.parseBoolean(extraParams.getProperty(CAdESExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE));


        final String mode = extraParams.getProperty(CAdESExtraParams.MODE, AOSignConstants.DEFAULT_SIGN_MODE);
        final String precalculatedDigestAlgorithm = extraParams.getProperty(CAdESExtraParams.PRECALCULATED_HASH_ALGORITHM);

        if (precalculatedDigestAlgorithm != null && extraParams.containsKey(CAdESExtraParams.MODE)) {
        	LOGGER.warning("Se ignorara el parametro '" + CAdESExtraParams.MODE + //$NON-NLS-1$
        			"' por haberse proporcionado tambien el parametro '" + CAdESExtraParams.PRECALCULATED_HASH_ALGORITHM + //$NON-NLS-1$
        			"'. La firma sera explicita."); //$NON-NLS-1$
        }

        boolean signingCertificateV2;
        if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
        	signingCertificateV2 = true;
        	if (extraParams.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
        		LOGGER.warning("Se ignorara la propiedad '" + CAdESExtraParams.SIGNING_CERTIFICATE_V2 + //$NON-NLS-1$
        				"' porque las firmas SHA2 siempre usan SigningCertificateV2"); //$NON-NLS-1$
        	}
        }
        else if (extraParams.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
        	signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty(CAdESExtraParams.SIGNING_CERTIFICATE_V2));
        }
        else {
        	signingCertificateV2 = !SHA1_ALGORITHM.equals(AOSignConstants.getDigestAlgorithmName(algorithm));
        }

        final String contentTypeOid = extraParams.getProperty(CAdESExtraParams.CONTENT_TYPE_OID);
        final String contentDescription = extraParams.getProperty(CAdESExtraParams.CONTENT_DESCRIPTION);

        final boolean doNotIncludePolicyOnSigningCertificate = Boolean.parseBoolean(
    		extraParams.getProperty(
				CAdESExtraParams.DO_NOT_INCLUDE_POLICY_ON_SIGNING_CERTIFICATE, Boolean.FALSE.toString()
			)
		);

        //*************** FIN LECTURA PARAMETROS ADICIONALES *************************************************
    	//****************************************************************************************************

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(
    		data,
    		algorithm
		);

        String altContentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
        String altContentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;
		if (data != null && (contentTypeOid == null || contentDescription == null)) {
			try {
				final MimeHelper mimeHelper = new MimeHelper(data);
				altContentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
				altContentDescription = mimeHelper.getDescription();
			}
			catch (final Exception e) {
				LOGGER.warning(
					"No se han podido cargar las librerias para identificar el tipo de dato firmado: " + e //$NON-NLS-1$
				);
			}
		}

		byte[] messageDigest = null;
		if (precalculatedDigestAlgorithm != null) {
            messageDigest = data;
        }

		try {

			final boolean omitContent = AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(mode) || precalculatedDigestAlgorithm != null;

			return new CAdESCoSigner().coSigner(
				csp,
				sign,
				omitContent,
				AdESPolicy.buildAdESPolicy(extraParams),
				signingCertificateV2,
				key,
				onlySigningCertificate ? new X509Certificate[] { (X509Certificate) certChain[0] } : certChain,
				messageDigest,
				contentTypeOid != null ? contentTypeOid : altContentTypeOid,
                contentDescription != null ? contentDescription : altContentDescription,
				CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(extraParams),
				Boolean.parseBoolean(extraParams.getProperty(CAdESExtraParams.INCLUDE_SIGNING_TIME_ATTRIBUTE, Boolean.FALSE.toString())),
				CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams),
				doNotIncludePolicyOnSigningCertificate
			);

		}
		catch (final Exception e) {
			throw new AOException("Error generando la Cofirma CAdES: " + e, e); //$NON-NLS-1$
		}

    }

    /** {@inheritDoc} */
	@Override
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties xParams) throws AOException, IOException {

        CAdESMultiUtil.checkUnsupportedAttributes(sign);

    	//****************************************************************************************************
    	//*************** LECTURA PARAMETROS ADICIONALES *****************************************************

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final boolean onlySigningCertificate = Boolean.parseBoolean(extraParams.getProperty(
    		CAdESExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE)
		);

        boolean signingCertificateV2;
        if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
        	signingCertificateV2 = true;
        	if (extraParams.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
        		LOGGER.warning("Se ignorara la propiedad '" + CAdESExtraParams.SIGNING_CERTIFICATE_V2 + //$NON-NLS-1$
        				"' porque las firmas SHA2 siempre usan SigningCertificateV2"); //$NON-NLS-1$
        	}
        }
        else if (extraParams.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
        	signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty(CAdESExtraParams.SIGNING_CERTIFICATE_V2));
        }
        else {
        	signingCertificateV2 = !SHA1_ALGORITHM.equals(AOSignConstants.getDigestAlgorithmName(algorithm));
        }

        final String contentTypeOid = extraParams.getProperty(CAdESExtraParams.CONTENT_TYPE_OID);
        final String contentDescription = extraParams.getProperty(CAdESExtraParams.CONTENT_DESCRIPTION);

        final boolean doNotIncludePolicyOnSigningCertificate = Boolean.parseBoolean(
    		extraParams.getProperty(
				CAdESExtraParams.DO_NOT_INCLUDE_POLICY_ON_SIGNING_CERTIFICATE, Boolean.FALSE.toString()
			)
		);

        //*************** FIN LECTURA PARAMETROS ADICIONALES *************************************************
    	//****************************************************************************************************

        // algoritmo de firma.
        final String typeAlgorithm = algorithm;

        String altContentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
        String altContentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;
        final byte[] data = new AOCAdESSigner().getData(sign);
        if (data != null && (contentTypeOid == null || contentDescription == null)) {
			try {
				final MimeHelper mimeHelper = new MimeHelper(data);
				altContentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
				altContentDescription = mimeHelper.getDescription();
			}
			catch (final Exception e) {
				LOGGER.warning(
					"No se han podido cargar las librerias para identificar el tipo de dato firmado: " + e //$NON-NLS-1$
				);
			}
		}

        try {
			return new CAdESCoSigner().coSigner(
			    typeAlgorithm,
			    onlySigningCertificate ?
					new X509Certificate[] { (X509Certificate) certChain[0] } :
						(X509Certificate[]) certChain,
			    new ByteArrayInputStream(sign),
			    AdESPolicy.buildAdESPolicy(extraParams),
			    signingCertificateV2,
			    key,
			    certChain,
			    null, // null porque no nos pueden dar un hash en este metodo, tendria que ser en el que incluye datos
			    contentTypeOid != null ? contentTypeOid : altContentTypeOid,
			    contentDescription != null ? contentDescription : altContentDescription,
			    CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(extraParams),
			    Boolean.parseBoolean(extraParams.getProperty(CAdESExtraParams.INCLUDE_SIGNING_TIME_ATTRIBUTE, Boolean.FALSE.toString())),
			    CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams),
			    doNotIncludePolicyOnSigningCertificate
			);
		}
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CADES: " + e, e); //$NON-NLS-1$
        }

    }
}
