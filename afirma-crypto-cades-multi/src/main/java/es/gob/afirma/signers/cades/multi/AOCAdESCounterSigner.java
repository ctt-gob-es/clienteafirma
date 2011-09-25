package es.gob.afirma.signers.cades.multi;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.signers.cades.ValidateCADES;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;

/** Operaciones de cofirma CAdES. */
public class AOCAdESCounterSigner implements AOCounterSigner {
    
    /** Indica si por defecto se debe insertar el atributo SigningCertificateV2 en la firma. */
    static final boolean DEFAULT_USE_SIGNING_CERTIFICATE_V2 = true;

    public byte[] countersign(final byte[] sign,
                              String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              Properties extraParams) throws AOException {
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

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(sign, algorithm, xCerts);

        // Datos firmados.
        byte[] dataSigned = null;

        // Politica de firma
        String policyIdentifier = null;
        // Nos puede venir como URN o como OID
        try {
            policyIdentifier =
                    extraParams.getProperty("policyIdentifier").replace("urn:oid:", "").replace("URN:oid:", "").replace("Urn:oid:", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
        }
        catch (final Exception e) {
            // Se ignora, podemos no tener politica
        }
        final String policyIdentifierHash = extraParams.getProperty("policyIdentifierHash"); //$NON-NLS-1$
        String policyIdentifierHashAlgorithm = null;
        if (policyIdentifier != null && policyIdentifierHash != null && policyIdentifierHash != "0") { //$NON-NLS-1$
            
        }
        
        // Si la firma que nos introducen es SignedData
        final boolean signedData = new ValidateCADES().isCADESSignedData(sign);
        if (signedData) {
            try {
                // CASO DE FIRMA DE ARBOL
                if (targetType == CounterSignTarget.Tree) {
                    final int[] nodes = {
                        0
                    };

                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.Tree,
                                                                   nodes,
                                                                   keyEntry,
                                                                   policyIdentifier,
                                                                   policyIdentifierHash,
                                                                   policyIdentifierHashAlgorithm,
                                                                   extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
                                                                   signingCertificateV2);
                }
                // CASO DE FIRMA DE HOJAS
                else if (targetType == CounterSignTarget.Leafs) {
                    final int[] nodes = {
                        0
                    };
                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.Leafs,
                                                                   nodes,
                                                                   keyEntry,
                                                                   policyIdentifier,
                                                                   policyIdentifierHash,
                                                                   policyIdentifierHashAlgorithm,
                                                                   extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
                                                                   signingCertificateV2);
                }
                // CASO DE FIRMA DE NODOS
                else if (targetType == CounterSignTarget.Nodes) {
                    int[] nodesID = new int[targets.length];
                    for (int i = 0; i < targets.length; i++) {
                        nodesID[i] = ((Integer) targets[i]).intValue();
                    }
                    nodesID = new ReadNodesTree().simplyArray(nodesID);
                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.Nodes,
                                                                   nodesID,
                                                                   keyEntry,
                                                                   policyIdentifier,
                                                                   policyIdentifierHash,
                                                                   policyIdentifierHashAlgorithm,
                                                                   extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
                                                                   signingCertificateV2);
                }
                // CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
                else if (targetType == CounterSignTarget.Signers) {

                    // clase que lee los nodos de un fichero firmado (p7s, csig,
                    // sig)
                    final String[] signers = new String[targets.length];
                    for (int i = 0; i < targets.length; i++) {
                        signers[i] = (String) targets[i];
                    }
                    final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, sign);
                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.Signers,
                                                                   nodes2,
                                                                   keyEntry,
                                                                   policyIdentifier,
                                                                   policyIdentifierHash,
                                                                   policyIdentifierHashAlgorithm,
                                                                   extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
                                                                   signingCertificateV2);

                }

                return dataSigned;

            }
            catch (final Exception e) {
                throw new AOException("Error generando la Contrafirma CAdES", e); //$NON-NLS-1$
            }
        }
        // Signed and enveloped

        try {
            // CASO DE FIRMA DE ARBOL
            if (targetType == CounterSignTarget.Tree) {
                final int[] nodes = {
                    0
                };

                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.Tree,
                                                                        nodes,
                                                                        keyEntry,
                                                                        policyIdentifier,
                                                                        policyIdentifierHash,
                                                                        policyIdentifierHashAlgorithm,
                                                                        extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
                                                                        signingCertificateV2);
            }
            // CASO DE FIRMA DE HOJAS
            else if (targetType == CounterSignTarget.Leafs) {
                final int[] nodes = {
                    0
                };
                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.Leafs,
                                                                        nodes,
                                                                        keyEntry,
                                                                        policyIdentifier,
                                                                        policyIdentifierHash,
                                                                        policyIdentifierHashAlgorithm,
                                                                        extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
                                                                        signingCertificateV2);
            }
            // CASO DE FIRMA DE NODOS
            else if (targetType == CounterSignTarget.Nodes) {
                int[] nodesID = new int[targets.length];
                for (int i = 0; i < targets.length; i++) {
                    nodesID[i] = ((Integer) targets[i]).intValue();
                }
                nodesID = new ReadNodesTree().simplyArray(nodesID);
                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.Nodes,
                                                                        nodesID,
                                                                        keyEntry,
                                                                        policyIdentifier,
                                                                        policyIdentifierHash,
                                                                        policyIdentifierHashAlgorithm,
                                                                        extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
                                                                        signingCertificateV2);
            }
            // CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
            else if (targetType == CounterSignTarget.Signers) {

                // clase que lee los nodos de un fichero firmado (p7s, csig,
                // sig)
                final String[] signers = new String[targets.length];
                for (int i = 0; i < targets.length; i++) {
                    signers[i] = (String) targets[i];
                }
                final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, sign);
                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.Signers,
                                                                        nodes2,
                                                                        keyEntry,
                                                                        policyIdentifier,
                                                                        policyIdentifierHash,
                                                                        policyIdentifierHashAlgorithm,
                                                                        extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
                                                                        signingCertificateV2);

            }

            return dataSigned;

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Contrafirma CAdES", e); //$NON-NLS-1$
        }
    }

}
