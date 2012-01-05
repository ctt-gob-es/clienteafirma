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

import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.CounterSignTarget;
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

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(sign, algorithm, (X509Certificate[]) keyEntry.getCertificateChain());

        // Datos firmados.
        byte[] dataSigned = null;

        // Si la firma que nos introducen es SignedData
        final boolean signedData = CAdESValidator.isCAdESSignedData(sign);
        if (signedData) {
            try {
                // CASO DE FIRMA DE ARBOL
                if (targetType == CounterSignTarget.TREE) {
                    final int[] nodes = {
                        0
                    };

                    dataSigned = new CAdESCounterSigner().counterSigner(
                    	   csp,
	                       sign,
	                       CounterSignTarget.TREE,
	                       nodes,
	                       keyEntry,
	                       new AdESPolicy(extraParams),
	                       signingCertificateV2
                    );
                }
                // CASO DE FIRMA DE HOJAS
                else if (targetType == CounterSignTarget.LEAFS) {
                    final int[] nodes = {
                        0
                    };
                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.LEAFS,
                                                                   nodes,
                                                                   keyEntry,
                                                                   new AdESPolicy(extraParams),
                                                                   signingCertificateV2);
                }
                // CASO DE FIRMA DE NODOS
                else if (targetType == CounterSignTarget.NODES) {
                    int[] nodesID = new int[targets.length];
                    for (int i = 0; i < targets.length; i++) {
                        nodesID[i] = ((Integer) targets[i]).intValue();
                    }
					nodesID = ReadNodesTree.simplyArray(nodesID);
                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.NODES,
                                                                   nodesID,
                                                                   keyEntry,
                                                                   new AdESPolicy(extraParams),
                                                                   signingCertificateV2);
                }
                // CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
                else if (targetType == CounterSignTarget.SIGNERS) {

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
                                                                   CounterSignTarget.SIGNERS,
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
            if (targetType == CounterSignTarget.TREE) {
                final int[] nodes = {
                    0
                };

                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.TREE,
                                                                        nodes,
                                                                        keyEntry,
                                                                        new AdESPolicy(extraParams),
                                                                        signingCertificateV2);
            }
            // CASO DE FIRMA DE HOJAS
            else if (targetType == CounterSignTarget.LEAFS) {
                final int[] nodes = {
                    0
                };
                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.LEAFS,
                                                                        nodes,
                                                                        keyEntry,
                                                                        new AdESPolicy(extraParams),
                                                                        signingCertificateV2);
            }
            // CASO DE FIRMA DE NODOS
            else if (targetType == CounterSignTarget.NODES) {
                int[] nodesID = new int[targets.length];
                for (int i = 0; i < targets.length; i++) {
                    nodesID[i] = ((Integer) targets[i]).intValue();
                }
				nodesID = ReadNodesTree.simplyArray(nodesID);
                dataSigned = new CAdESCounterSignerEnveloped().counterSigner(
            		csp,
                    sign,
                    CounterSignTarget.NODES,
                    nodesID,
                    keyEntry,
                    new AdESPolicy(extraParams),
                    signingCertificateV2
                );
            }
            // CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
            else if (targetType == CounterSignTarget.SIGNERS) {

                // clase que lee los nodos de un fichero firmado (p7s, csig,
                // sig)
                final String[] signers = new String[targets.length];
                for (int i = 0; i < targets.length; i++) {
                    signers[i] = (String) targets[i];
                }
                final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, sign);
                dataSigned = new CAdESCounterSignerEnveloped().counterSigner(
            		csp,
                    sign,
                    CounterSignTarget.SIGNERS,
                    nodes2,
                    keyEntry,
                    new AdESPolicy(extraParams),
                    signingCertificateV2
                );

            }

            return dataSigned;

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Contrafirma CAdES", e); //$NON-NLS-1$
        }
    }

}
