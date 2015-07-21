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

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSimpleSigner;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.CAdESSignerMetadataHelper;
import es.gob.afirma.signers.cades.CommitmentTypeIndicationsHelper;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;

/** Contrafirmador CAdES. */
public class AOCAdESCounterSigner implements AOCounterSigner {

	private final AOSimpleSigner ss;
	private final Date date;

	/** Crea un contrafirmador CAdES con el firmador PKCS#1 por defecto. */
	public AOCAdESCounterSigner() {
		this.ss = null;
		this.date = null;
	}

	/** Crea un contrafirmador CAdES con un firmador PKCS#1 espec&iacute;fico y una fecha/hora est&aacute;tica.
	 * @param sSigner Firmador PKCS#1 a usar.
	 * @param d Fecha y hora prefijada (se usa esta como atributo CAdES en vez de la del momento exacto de la firma). */
	public AOCAdESCounterSigner(final AOSimpleSigner sSigner, final Date d) {
		if (sSigner == null) {
			throw new IllegalArgumentException("El firmador PKCS#1 no puede ser nulo"); //$NON-NLS-1$
		}
    	if (d == null) {
    		Logger.getLogger("es.gob.afirma").warning("Se ha establecido una fecha nula, se usara la actual"); //$NON-NLS-1$ //$NON-NLS-2$
    	}
		this.ss = sSigner;
		this.date = d;
	}

	/** {@inheritDoc} */
	@Override
	public final byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final java.security.cert.Certificate[] cChain,
                              final Properties xParams) throws AOException, IOException {

        final Properties extraParams = xParams != null ? xParams : new Properties();

        // Control general para todo el metodo de la inclusion de la cadena completa o solo el certificado del firmante
		final java.security.cert.Certificate[] certChain = Boolean.parseBoolean(extraParams.getProperty("includeOnlySignningCertificate", Boolean.FALSE.toString())) ? //$NON-NLS-1$
       		 new X509Certificate[] { (X509Certificate) cChain[0] } :
       			 cChain;

        boolean signingCertificateV2;
        if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
        	signingCertificateV2 = true;
        }
        else if (extraParams.containsKey("signingCertificateV2")) { //$NON-NLS-1$
        	signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2")); //$NON-NLS-1$
        }
        else {
        	signingCertificateV2 = !"SHA1".equals(AOSignConstants.getDigestAlgorithmName(algorithm));	 //$NON-NLS-1$
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(
    		sign,
    		algorithm
		);

        // Creamos el contrafirmador
        final CAdESCounterSigner cadesCountersigner = new CAdESCounterSigner();

        // Le asignamos el firmador PKCS#1 a medida y la fecha prefijada si procede
        if (this.ss != null) {
        	cadesCountersigner.setpkcs1Signer(this.ss, this.date);
        }

        // Datos firmados.
        byte[] dataSigned = null;

        try {
            // CASO DE FIRMA DE ARBOL
            if (targetType == CounterSignTarget.TREE) {
                final int[] nodes = {
                    0
                };

                dataSigned = cadesCountersigner.counterSign(
                	   csp,
                       sign,
                       CounterSignTarget.TREE,
                       nodes,
                       key,
                       certChain,
                       AdESPolicy.buildAdESPolicy(extraParams),
                       signingCertificateV2,
                       CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(extraParams),
                       CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams)
                );
            }
            // CASO DE FIRMA DE HOJAS
            else if (targetType == CounterSignTarget.LEAFS) {
                final int[] nodes = {
                    0
                };
                dataSigned =
                		cadesCountersigner.counterSign(
                    		csp,
                            sign,
                            CounterSignTarget.LEAFS,
                            nodes,
                            key,
                            certChain,
                            AdESPolicy.buildAdESPolicy(extraParams),
							signingCertificateV2,
                            CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(extraParams),
                            CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams)
                		);
            }
            // CASO DE FIRMA DE NODOS
            else if (targetType == CounterSignTarget.NODES) {
                int[] nodesID = new int[targets.length];
                for (int i = 0; i < targets.length; i++) {
                    nodesID[i] = ((Integer) targets[i]).intValue();
                }
				nodesID = ReadNodesTree.simplyArray(nodesID);
                dataSigned =
                		cadesCountersigner.counterSign(
                    		csp,
                            sign,
                            CounterSignTarget.NODES,
                            nodesID,
                            key,
                            certChain,
                            AdESPolicy.buildAdESPolicy(extraParams),
							signingCertificateV2,
                            CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(xParams),
                            CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams)
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
                dataSigned =
                		cadesCountersigner.counterSign(
                    		csp,
                            sign,
                            CounterSignTarget.SIGNERS,
                            nodes2,
                            key,
                            certChain,
                            AdESPolicy.buildAdESPolicy(extraParams),
                            signingCertificateV2,
                            CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(xParams),
                            CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams)
                		);

            }

            return dataSigned;

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Contrafirma CAdES: " + e, e); //$NON-NLS-1$
        }

    }

}
