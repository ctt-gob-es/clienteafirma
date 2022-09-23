/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.Iterator;
import java.util.Properties;
import java.util.logging.Logger;

import org.spongycastle.cert.X509CertificateHolder;
import org.spongycastle.cms.CMSException;
import org.spongycastle.cms.CMSProcessableByteArray;
import org.spongycastle.cms.CMSSignedData;
import org.spongycastle.cms.CMSSignerDigestMismatchException;
import org.spongycastle.cms.DefaultCMSSignatureAlgorithmNameGenerator;
import org.spongycastle.cms.SignerInformation;
import org.spongycastle.cms.SignerInformationVerifier;
import org.spongycastle.jce.provider.BouncyCastleProvider;
import org.spongycastle.operator.DefaultSignatureAlgorithmIdentifierFinder;
import org.spongycastle.operator.OperatorCreationException;
import org.spongycastle.operator.bc.BcDigestCalculatorProvider;
import org.spongycastle.operator.jcajce.JcaContentVerifierProviderBuilder;
import org.spongycastle.util.Store;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;

/** Validador de firmas binarias.
 * @author Carlos Gamuci
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class ValidateBinarySignature extends SignValider {

	@Override
	public SignValidity validate(final byte[] sign) throws IOException {
		return validate(sign, null, true);
	}

	@Override
	public SignValidity validate(final byte[] sign, final boolean checkCertificates) throws IOException {
		return validate(sign, null, true);
	}

	@Override
	public SignValidity validate(final byte[] sign, final Properties params) throws IOException {
		return validate(sign, null, true);
	}

    /** Valida una firma binaria (CMS/CAdES). Si se especifican los datos que se firmaron
     * se comprobar&aacute; que efectivamente fueron estos, mientras que si no se indican
     * se extraer&aacute;n de la propia firma. Si la firma no contiene los datos no se realizara
     * esta comprobaci&oacute;n.
     * Se validan los certificados en local revisando las fechas de validez de los certificados.
     * @param sign Firma binaria.
     * @param data Datos firmados o {@code null} si se desea comprobar contra los datos incrustados
     *             en la firma.
     * @return Validez de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma o los datos. */
	 public static SignValidity validate(final byte[] sign, final byte[] data) throws IOException {
		 return validate(sign, data, true);
	 }

    /** Valida una firma binaria (CMS/CAdES). Si se especifican los datos que se firmaron
     * se comprobar&aacute; que efectivamente fueron estos, mientras que si no se indican
     * se extraer&aacute;n de la propia firma. Si la firma no contiene los datos no se realizara
     * esta comprobaci&oacute;n.
     * Se validan los certificados en local revisando las fechas de validez de los certificados.
     * @param sign Firma binaria.
     * @param data Datos firmados o {@code null} si se desea comprobar contra los datos incrustados
     *             en la firma.
     * @param checkCertificates Indica si debe comprobarse o no el periodo de validez de los certificados.
     * @return Validez de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma o los datos. */
    public static SignValidity validate(final byte[] sign,
    		                            final byte[] data,
    		                            final boolean checkCertificates) throws IOException {
    	if (sign == null) {
    		throw new IllegalArgumentException("La firma a validar no puede ser nula"); //$NON-NLS-1$
    	}

    	AOSigner signer = new AOCAdESSigner();
    	if (!signer.isSign(sign)) {
    	    signer = new AOCMSSigner();
    	    if (!signer.isSign(sign)) {
    	        return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_SIGN);
    	    }
    	}

    	try {
			if (data == null && signer.getData(sign) == null) {
				Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
					"Se ha pedido validar una firma explicita sin proporcionar los datos firmados" //$NON-NLS-1$
				);
				return new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.NO_DATA);
			}
		}
    	catch (final AOInvalidFormatException e1) {
    		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Se ha pedido validar una firma como CAdES, pero no es CAdES: " + e1  //$NON-NLS-1$
			);
    		return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_SIGN, e1);
		}
    	catch (final AOException e1) {
    		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Se encontraron datos en la firma y no se pudieron extraer: " + e1  //$NON-NLS-1$
			);
    		return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.UNKOWN_ERROR);
		}

    	try {
    		verifySignatures(
				sign,
				data != null ? data : new AOCAdESSigner().getData(sign),
				checkCertificates
			);
	    }
    	catch (final CertificateExpiredException e) {
    		// Certificado caducado
    		return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CERTIFICATE_EXPIRED, e);
        }
    	catch (final CertificateNotYetValidException e) {
    		// Certificado aun no valido
    		return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CERTIFICATE_NOT_VALID_YET, e);
        }
    	catch (final CMSSignerDigestMismatchException e) {
    		// La firma no es una firma binaria valida
            return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_MATCH_DATA, e);
    	}
    	catch (final Exception e) {
            // La firma no es una firma binaria valida
            return new SignValidity(SIGN_DETAIL_TYPE.KO, null, e);
        }

    	return new SignValidity(SIGN_DETAIL_TYPE.OK, null);
    }

    /** Verifica la valides de una firma. Si la firma es v&aacute;lida, no hace nada. Si no es
     * v&aacute;lida, lanza una excepci&oacute;n.
     * @param sign Firma que se desea validar.
     * @param data Datos para la comprobaci&oacute;n.
     * @param checkCertificates Indica si debe comprobarse o no el periodo de validez de los certificados.
     * @throws CMSException Cuando la firma no tenga una estructura v&aacute;lida.
     * @throws CertificateExpiredException Cuando el certificado est&aacute;a caducado.
     * @throws CertificateNotYetValidException Cuando el certificado aun no es v&aacute;lido.
     * @throws IOException Cuando no se puede crear un certificado desde la firma para validarlo.
     * @throws OperatorCreationException Cuando no se puede crear el validado de contenido de firma. */
    private static void verifySignatures(final byte[] sign,
    		                             final byte[] data,
    		                             final boolean checkCertificates) throws CMSException,
                                                                                 CertificateException,
                                                                                 IOException,
                                                                                 OperatorCreationException {
        final CMSSignedData s;
        if (data == null) {
        	s = new CMSSignedData(sign);
        }
        else {
        	s = new CMSSignedData(new CMSProcessableByteArray(data), sign);
        }
        final Store<X509CertificateHolder> store = s.getCertificates();

        final CertificateFactory certFactory = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$

        for (final Object si : s.getSignerInfos().getSigners()) {

        	final SignerInformation signer = (SignerInformation) si;

			final Iterator<X509CertificateHolder> certIt = store.getMatches(new CertHolderBySignerIdSelector(signer.getSID())).iterator();
            final X509Certificate cert = (X509Certificate) certFactory.generateCertificate(
        		new ByteArrayInputStream(
    				certIt.next().getEncoded()
				)
    		);
        	if (checkCertificates) {
        		cert.checkValidity();
        	}

            if (!signer.verify(new SignerInformationVerifier(
            	new	DefaultCMSSignatureAlgorithmNameGenerator(),
            	new DefaultSignatureAlgorithmIdentifierFinder(),
        		new JcaContentVerifierProviderBuilder().setProvider(new BouncyCastleProvider()).build(cert),
        		new BcDigestCalculatorProvider()
    		))) {
            	throw new CMSException("Firma no valida"); //$NON-NLS-1$
            }

        }

    }

}
