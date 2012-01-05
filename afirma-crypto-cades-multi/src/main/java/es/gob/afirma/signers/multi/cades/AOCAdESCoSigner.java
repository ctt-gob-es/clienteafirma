/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.ByteArrayInputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.CAdESValidator;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Operaciones de cofirma CAdES. */
public class AOCAdESCoSigner implements AOCoSigner {
    
    /** Indica si por defecto se debe insertar el atributo SigningCertificateV2 en la firma. */
    static final boolean DEFAULT_USE_SIGNING_CERTIFICATE_V2 = true;

    public byte[] cosign(final byte[] data, 
                         final byte[] sign, 
                         final String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", Boolean.toString(DEFAULT_USE_SIGNING_CERTIFICATE_V2))); //$NON-NLS-1$

        byte[] messageDigest = null;

        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm, (X509Certificate[]) keyEntry.getCertificateChain());
        
        try {

            // Si la firma que nos introducen es SignedData
            //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
            final boolean signedData = CAdESValidator.isCAdESSignedData(sign);
            if (signedData) {

                final String mode = extraParams.getProperty("mode", AOSignConstants.DEFAULT_SIGN_MODE); //$NON-NLS-1$
                final boolean omitContent = mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;

                return new CAdESCoSigner().coSigner(
                    csp,
                    sign,
                    omitContent,
                    new AdESPolicy(extraParams),
                    signingCertificateV2,
                    keyEntry,
                    messageDigest
                );
            }

            return new CAdESCoSignerEnveloped().coSigner(
                 csp,
                 sign,
                 new AdESPolicy(extraParams),
                 signingCertificateV2,
                 keyEntry,
                 messageDigest
            );

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CAdES", e); //$NON-NLS-1$
        }
    }

    public byte[] cosign(final byte[] sign, 
                         final String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", Boolean.toString(DEFAULT_USE_SIGNING_CERTIFICATE_V2))); //$NON-NLS-1$

        // algoritmo de firma.
        final String typeAlgorithm = algorithm;

        // Si la firma que nos introducen es SignedData
        //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
        final boolean signedData = CAdESValidator.isCAdESSignedData(sign);
        if (signedData) {
            try {
                return new CAdESCoSigner().coSigner(
                    typeAlgorithm,
                    (X509Certificate[])keyEntry.getCertificateChain(),
                    new ByteArrayInputStream(sign),
                    new AdESPolicy(extraParams),
                    signingCertificateV2,
                    keyEntry,
                    null // null porque no nos pueden dar un hash en este metodo, tendría que ser en el que incluye datos
                );
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Cofirma CADES", e); //$NON-NLS-1$
            }
        }
        // Signed And Enveloped.

        try {
            return new CAdESCoSignerEnveloped().coSigner(
                 typeAlgorithm,
                 (X509Certificate[])keyEntry.getCertificateChain(),
                 new ByteArrayInputStream(sign),
                 new AdESPolicy(extraParams),
                 signingCertificateV2,
                 keyEntry,
                 null // null porque no nos pueden dar un hash en este metodo, tendría que ser en el que incluye datos            
            );
        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CADES", e); //$NON-NLS-1$
        }
        
    }

}
