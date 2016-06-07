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
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.CertificateException;
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
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.cades.CAdESSignerMetadataHelper;
import es.gob.afirma.signers.cades.CommitmentTypeIndicationsHelper;

/** Contrafirmador CAdES. */
public class AOCAdESCounterSigner implements AOCounterSigner {

    private static final String SHA1_ALGORITHM = "SHA1"; //$NON-NLS-1$
	private final AOSimpleSigner ss;
	private final Date date;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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
    		LOGGER.warning("Se ha establecido una fecha nula, se usara la actual"); //$NON-NLS-1$
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
                                    final Properties xParams) throws AOException,
                                                                     IOException {

        final Properties extraParams = xParams != null ? xParams : new Properties();

        // Control general para todo el metodo de la inclusion de la cadena completa o solo el certificado del firmante
		final java.security.cert.Certificate[] certChain = Boolean.parseBoolean(
			extraParams.getProperty(
				CAdESExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE,
				Boolean.FALSE.toString()
			)
		) ? new X509Certificate[] { (X509Certificate) cChain[0] } : cChain;

        boolean signingCertificateV2;
        if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
        	signingCertificateV2 = true;
        	if (extraParams.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
        		LOGGER.warning("Se ignorara la propiedad '" + CAdESExtraParams.SIGNING_CERTIFICATE_V2 + //$NON-NLS-1$
        				"' porque las firmas SHA2 siempre usan SigningCertificateV2"); //$NON-NLS-1$
        	}
        }
        else if (extraParams.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
        	signingCertificateV2 = Boolean.parseBoolean(
    			extraParams.getProperty(
					CAdESExtraParams.SIGNING_CERTIFICATE_V2
				)
			);
        }
        else {
        	signingCertificateV2 = !SHA1_ALGORITHM.equals(AOSignConstants.getDigestAlgorithmName(algorithm));
        }

        final boolean doNotIncludePolicyOnSigningCertificate = Boolean.parseBoolean(
    		extraParams.getProperty(
				CAdESExtraParams.DO_NOT_INCLUDE_POLICY_ON_SIGNING_CERTIFICATE, Boolean.FALSE.toString()
			)
		);

        // Creamos el contrafirmador
        final CAdESCounterSigner cadesCountersigner = new CAdESCounterSigner();

        // Le asignamos el firmador PKCS#1 a medida y la fecha prefijada si procede
        if (this.ss != null) {
        	cadesCountersigner.setPkcs1Signer(this.ss, this.date);
        }

        // Ya no se soportan la contrafirma de nodos y de firmantes
        if (targetType == CounterSignTarget.NODES || targetType == CounterSignTarget.SIGNERS) {
            throw new AOException("No se soporta la firma de nodos individuales"); //$NON-NLS-1$
        }

     // CASO DE CONTRAFIRMA DE ARBOL U HOJAS (Por defecto)
        byte[] dataSigned = null;
        try {
        	dataSigned = cadesCountersigner.counterSign(
        			algorithm,
        			sign,
        			targetType != null ? targetType : CounterSignTarget.LEAFS,
					key,
					certChain,
					AdESPolicy.buildAdESPolicy(extraParams),
					signingCertificateV2,
					CommitmentTypeIndicationsHelper.getCommitmentTypeIndications(extraParams),
					Boolean.parseBoolean(
							extraParams.getProperty(
									CAdESExtraParams.INCLUDE_SIGNING_TIME_ATTRIBUTE,
									Boolean.FALSE.toString()
									)
							),
					CAdESSignerMetadataHelper.getCAdESSignerMetadata(extraParams),
					doNotIncludePolicyOnSigningCertificate
        	);
        }
        catch (final NoSuchAlgorithmException e) {
        	throw new AOException("Error generando la Contrafirma CAdES: " + e, e); //$NON-NLS-1$
		}
        catch (final CertificateException e) {
        	throw new AOException("Error generando la Contrafirma CAdES: " + e, e); //$NON-NLS-1$
		}

    	return dataSigned;
    }

}
