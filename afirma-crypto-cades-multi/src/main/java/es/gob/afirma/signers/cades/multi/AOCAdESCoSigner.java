/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.cades.multi;

import java.io.ByteArrayInputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.ValidateCADES;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Operaciones de cofirma CAdES. */
public class AOCAdESCoSigner implements AOCoSigner {
    
    /** Indica si por defecto se debe insertar el atributo SigningCertificateV2 en la firma. */
    static final boolean DEFAULT_USE_SIGNING_CERTIFICATE_V2 = true;

    public byte[] cosign(final byte[] data, 
                         final byte[] sign, String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        if (algorithm.equalsIgnoreCase("RSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;
        }
        else if (algorithm.equalsIgnoreCase("DSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHDSA;
        }

        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", Boolean.toString(DEFAULT_USE_SIGNING_CERTIFICATE_V2))); //$NON-NLS-1$

        byte[] messageDigest = null;

        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        X509Certificate[] xCerts = new X509Certificate[0];
        final Certificate[] certs = keyEntry.getCertificateChain();
        if (certs != null && (certs instanceof X509Certificate[])) {
            xCerts = (X509Certificate[]) certs;
        }
        else {
            final Certificate cert = keyEntry.getCertificate();
            if (cert instanceof X509Certificate) {
                xCerts = new X509Certificate[] {
                                                (X509Certificate) cert
                };
            }
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm, xCerts);
        
        try {

            // Si la firma que nos introducen es SignedData
            //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
            final boolean signedData = new ValidateCADES().isCADESSignedData(sign);
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
        // Array de certificados
        X509Certificate[] aCertificados = new X509Certificate[0];
        final Certificate[] certs = keyEntry.getCertificateChain();
        if (certs != null && (certs instanceof X509Certificate[])) {
            aCertificados = (X509Certificate[]) certs;
        }
        else {
            final Certificate cert = keyEntry.getCertificate();
            if (cert instanceof X509Certificate) {
                aCertificados = new X509Certificate[] {
                                                       (X509Certificate) cert
                };
            }
        }

        // Si la firma que nos introducen es SignedData
        //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
        final boolean signedData = new ValidateCADES().isCADESSignedData(sign);
        if (signedData) {
            try {
                return new CAdESCoSigner().coSigner(typeAlgorithm,
                                                    aCertificados,
                                                    new ByteArrayInputStream(sign),
                                                    new AdESPolicy(extraParams),
                                                    signingCertificateV2,
                                                    keyEntry,
                                                    null // null porque no nos pueden dar un hash
                                                         // en este metodo, tendría que ser en el
                                                         // que incluye datos
                );
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Cofirma CADES", e); //$NON-NLS-1$
            }
        }
        // Signed And Enveloped.

        try {
            return new CAdESCoSignerEnveloped().coSigner(typeAlgorithm,
                                                         aCertificados,
                                                         new ByteArrayInputStream(sign),
                                                         new AdESPolicy(extraParams),
                                                         signingCertificateV2,
                                                         keyEntry,
                                                         null // null porque no nos pueden dar un hash en este
                                                              // metodo, tendría que ser en el que incluye datos
            );
        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CADES", e); //$NON-NLS-1$
        }
        
    }

}
