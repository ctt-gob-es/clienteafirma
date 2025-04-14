/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.IOException;
import java.security.PrivateKey;
import java.util.Properties;
import java.util.logging.Logger;

import org.spongycastle.cms.CMSSignedData;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.SigningLTSException;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.cades.CAdESParameters;

/** Operaciones de cofirma CAdES. */
public final class AOCAdESCoSigner implements AOCoSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	 //$NON-NLS-1$

	/** {@inheritDoc} */
	@Override
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties xParams) throws AOException, IOException {

		return cosign(null, sign, algorithm, key, certChain, xParams);
    }

	/** {@inheritDoc} */
	@Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties xParams) throws AOException, IOException {

        final Properties extraParams = getExtraParams(xParams);

		// Comprobamos que no haya firmas de archivo, salvo que nos indiquen que debe firmarse
		// incluso en ese caso
		final String allowSignLts = extraParams.getProperty(CAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES);
		if (allowSignLts == null || !Boolean.parseBoolean(allowSignLts)) {
			try {
				CAdESMultiUtil.checkUnsupportedAttributes(sign);
			}
			catch (final SigningLTSException e) {
				// Si se indico expresamente que no se debia permitir la cofirma de
				// firmas de archivo, se lanza una excepcion bloqueando la ejecucion.
				// Si no, se informa debidamente para que se consulte al usuario
				if (allowSignLts != null) {
					throw new AOException(e.getMessage());
				}
				throw e;
			}
		}

        // Purgamos e informamos de las compatibilidades de la configuracion establecida
        noticeIncompatibleConfig(algorithm, extraParams);

        CMSSignedData signedData;
        try {
        	signedData = new CMSSignedData(sign);
        }
        catch (final Exception e) {
        	throw new AOException("La firma proporcionada no es CMS/CAdES", e); //$NON-NLS-1$
		}

        final CAdESParameters parameters = CAdESParameters.load(data, signedData, algorithm, extraParams);

		try {
			return CAdESCoSigner.coSigner(
					sign,
					algorithm,
					key,
					certChain,
					parameters);

		}
		catch (final AOException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new AOException("Error generando la cofirma CAdES: " + e, e); //$NON-NLS-1$
		}
    }

    private static Properties getExtraParams(final Properties extraParams) {
    	final Properties newExtraParams = extraParams != null ?
    			(Properties) extraParams.clone() : new Properties();

    	return newExtraParams;
    }

    /**
     * Informa a trav&eacute;s de mensajes de consola si se han establecido par&aacute;metros
     * de configuraci&oacute;n que se ignoraran por ser incompatibles.
     * @param algorithm Algoritmo de firma.
     * @param extraParams Configuracion establecida.
     */
    private static void noticeIncompatibleConfig(final String algorithm, final Properties extraParams) {

        if (extraParams.containsKey(CAdESExtraParams.PRECALCULATED_HASH_ALGORITHM)) {
        	LOGGER.warning("Se ignorara el parametro '" + CAdESExtraParams.MODE + //$NON-NLS-1$
        			"' por haberse proporcionado tambien el parametro '" + CAdESExtraParams.PRECALCULATED_HASH_ALGORITHM + //$NON-NLS-1$
        			"'. La firma sera explicita."); //$NON-NLS-1$
        	extraParams.remove(CAdESExtraParams.MODE);
        }

        if (algorithm != null && AOSignConstants.isSHA2SignatureAlgorithm(algorithm) &&
        		extraParams.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
        	LOGGER.warning("Se ignorara la propiedad '" + CAdESExtraParams.SIGNING_CERTIFICATE_V2 + //$NON-NLS-1$
        			"' porque las firmas SHA2 siempre usan SigningCertificateV2"); //$NON-NLS-1$
        	extraParams.remove(CAdESExtraParams.SIGNING_CERTIFICATE_V2);
        }
    }
}
