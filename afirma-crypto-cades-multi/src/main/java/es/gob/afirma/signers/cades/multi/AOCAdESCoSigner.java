package es.gob.afirma.signers.cades.multi;

import java.io.ByteArrayInputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.ValidateCADES;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Operaciones de cofirma CAdES. */
public class AOCAdESCoSigner implements AOCoSigner {
    
    /** Indica si por defecto se debe insertar el atributo SigningCertificateV2 en la firma. */
    static final boolean DEFAULT_USE_SIGNING_CERTIFICATE_V2 = true;

    public byte[] cosign(final byte[] data, final byte[] sign, String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        if (extraParams == null) {
            extraParams = new Properties();
        }

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
            String policyQualifier = extraParams.getProperty("policyQualifier"); //$NON-NLS-1$

            // Si la firma que nos introducen es SignedData
            //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
            final boolean signedData = new ValidateCADES().isCADESSignedData(sign);
            if (signedData) {

                final String mode = extraParams.getProperty("mode", AOSignConstants.DEFAULT_SIGN_MODE); //$NON-NLS-1$
                final boolean omitContent = mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;

                return new CAdESCoSigner().coSigner(csp,
                                                    sign,
                                                    omitContent,
                                                    extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                    policyQualifier,
                                                    signingCertificateV2,
                                                    keyEntry,
                                                    messageDigest);
            }

            return new CAdESCoSignerEnveloped().coSigner(csp,
                                                         sign,
                                                         extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                         policyQualifier,
                                                         signingCertificateV2,
                                                         keyEntry,
                                                         messageDigest);

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CAdES", e); //$NON-NLS-1$
        }
    }

    public byte[] cosign(byte[] sign, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        if (extraParams == null) {
            extraParams = new Properties();
        }
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", Boolean.toString(DEFAULT_USE_SIGNING_CERTIFICATE_V2))); //$NON-NLS-1$

        if (algorithm.equalsIgnoreCase("RSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;
        }
        else if (algorithm.equalsIgnoreCase("DSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHDSA;
        }

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

        final String policyQualifier = extraParams.getProperty("policyQualifier"); //$NON-NLS-1$

        // Si la firma que nos introducen es SignedData
        //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
        final boolean signedData = new ValidateCADES().isCADESSignedData(sign);
        if (signedData) {
            try {
                return new CAdESCoSigner().coSigner(typeAlgorithm,
                                                    aCertificados,
                                                    new ByteArrayInputStream(sign),
                                                    extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                    policyQualifier,
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
                                                         extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                         policyQualifier,
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
