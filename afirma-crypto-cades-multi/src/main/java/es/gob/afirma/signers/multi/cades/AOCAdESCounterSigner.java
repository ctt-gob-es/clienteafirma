/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.multi.cades;

import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.CertificateException;
import java.util.Date;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.SigningLTSException;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.cades.CAdESParameters;
import es.gob.afirma.signers.pkcs7.BinaryErrorCode;

/** Contrafirmador CAdES. */
public class AOCAdESCounterSigner implements AOCounterSigner {

	private final AOPkcs1Signer ss;
	private final Date date;

//	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Crea un contrafirmador CAdES con el firmador PKCS#1 por defecto. */
	public AOCAdESCounterSigner() {
		this.ss = null;
		this.date = null;
	}

//	/** Crea un contrafirmador CAdES con un firmador PKCS#1 espec&iacute;fico y una fecha/hora est&aacute;tica.
//	 * @param sSigner Firmador PKCS#1 a usar.
//	 * @param d Fecha y hora prefijada (se usa esta como atributo CAdES en vez de la del momento exacto de la firma). */
//	public AOCAdESCounterSigner(final AOSimpleSigner sSigner, final Date d) {
//		if (sSigner == null) {
//			throw new IllegalArgumentException("El firmador PKCS#1 no puede ser nulo"); //$NON-NLS-1$
//		}
//    	if (d == null) {
//    		LOGGER.warning("Se ha establecido una fecha nula, se usara la actual"); //$NON-NLS-1$
//    	}
//		this.ss = sSigner;
//		this.date = d;
//	}

	/** {@inheritDoc} */
	@Override
	public final byte[] countersign(final byte[] sign,
                                    final String algorithm,
                                    final CounterSignTarget targetType,
                                    final Object[] targets,
                                    final PrivateKey key,
                                    final java.security.cert.Certificate[] cChain,
                                    final Properties xParams) throws AOException {

        final Properties extraParams = getExtraParams(xParams);

		// Comprobamos que no haya firmas de archivo, salvo que nos indiquen que debe firmarse
		// incluso en ese caso
		final String allowSignLts = extraParams.getProperty(CAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES);
		if (allowSignLts == null || !Boolean.parseBoolean(allowSignLts)) {
			try {
				CAdESMultiUtil.checkLongTermAttributes(sign);
			}
			catch (final SigningLTSException e) {
				// Si se indico expresamente que no se debia permitir la contrafirma de
				// firmas de archivo, se lanza una excepcion bloqueando la ejecucion.
				// Si no, se informa debidamente para que se consulte al usuario
				if (allowSignLts != null) {
					e.setDenied(true);
				}
				throw e;
			}
		}

        final CAdESParameters config = CAdESParameters.load(null, algorithm, extraParams);

        // Si se indico que debia generarse la contrafirma con una fecha
        // concreta, la usamos
        if (this.date != null) {
        	config.setSigningTime(this.date);
        }

        // Creamos el contrafirmador
        final CAdESCounterSigner cadesCountersigner = new CAdESCounterSigner(this.ss);

        // Ya no se soportan la contrafirma de nodos y de firmantes
        if (targetType == CounterSignTarget.NODES || targetType == CounterSignTarget.SIGNERS) {
            throw new AOException("No se soporta la firma de nodos individuales", ErrorCode.Request.UNSUPPORTED_COUNTERSIGN_CONFIG); //$NON-NLS-1$
        }

        // CASO DE CONTRAFIRMA DE ARBOL U HOJAS (Por defecto)
        byte[] dataSigned = null;
        try {
        	dataSigned = cadesCountersigner.counterSign(
        			algorithm,
        			sign,
        			targetType != null ? targetType : CounterSignTarget.LEAFS,
					key,
					cChain,
					config
        	);
        }
        catch (final AOException e) {
			throw e;
		}
        catch (final NoSuchAlgorithmException e) {
        	throw new AOException("Algoritmo de firma o huella digital no soportado", e, ErrorCode.Request.UNSUPPORTED_SIGNATURE_ALGORITHM); //$NON-NLS-1$
		}
        catch (final CertificateException e) {
        	throw new AOException("Error generando la Contrafirma CAdES", e, ErrorCode.Internal.ENCODING_SIGNING_CERTIFICATE); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException("Error generando la contrafirma CAdES: " + e, e, BinaryErrorCode.Internal.UNKWNON_BINARY_SIGNING_ERROR); //$NON-NLS-1$
		}

    	return dataSigned;
    }

    private static Properties getExtraParams(final Properties extraParams) {
    	final Properties newExtraParams = extraParams != null ?
    			(Properties) extraParams.clone() : new Properties();

    	return newExtraParams;
    }
}
