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

import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.signers.cades.CAdESValidator;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;

/** Operaciones de cofirma CAdES. */
public class AOCAdESCounterSigner implements AOCounterSigner {
    
    /** Indica si por defecto se debe insertar el atributo SigningCertificateV2 en la firma. */
    static final boolean DEFAULT_USE_SIGNING_CERTIFICATE_V2 = true;

    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties xParams) throws AOException {
        
        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", Boolean.toString(DEFAULT_USE_SIGNING_CERTIFICATE_V2))); //$NON-NLS-1$

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

        // Si la firma que nos introducen es SignedData
        final boolean signedData = new CAdESValidator().isCAdESSignedData(sign);
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
                                                                   new AdESPolicy(extraParams),
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
                                                                   new AdESPolicy(extraParams),
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
                                                                   new AdESPolicy(extraParams),
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
                                                                   new AdESPolicy(extraParams),
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
                                                                        new AdESPolicy(extraParams),
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
                                                                        new AdESPolicy(extraParams),
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
                                                                        new AdESPolicy(extraParams),
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
                                                                        new AdESPolicy(extraParams),
                                                                        signingCertificateV2);

            }

            return dataSigned;

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Contrafirma CAdES", e); //$NON-NLS-1$
        }
    }

}
