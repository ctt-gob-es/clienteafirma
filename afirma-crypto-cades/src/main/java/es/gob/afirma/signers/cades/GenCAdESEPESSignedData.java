/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cades;

import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Date;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOPkcs1Signer;

/** Generaci&oacute;n de firmas digitales CMS Advanced Electronic Signatures
 * (CAdES).
 * La implementaci&oacute;n es la misma que para el SignedData de CMS, salvo
 * que en los atributos del SignerInfo, en vez de ir el n&uacute;mero de serie
 * (SerialNumber), va la firma del certificado.
 * <p>La Estructura del mensaje es la siguiente (se omite la parte correspondiente
 * a CMS):</p>
 *
 * <pre>
 * <code>
 *  id-aa-ets-sigPolicyId OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-aa(2) 15 }
 *
 *      SignaturePolicyIdentifier ::= CHOICE {
 *           signaturePolicyId          SignaturePolicyId,
 *           signaturePolicyImplied     SignaturePolicyImplied
 *                                      -- not used in this version
 *   }
 *
 *      SignaturePolicyId ::= SEQUENCE {
 *           sigPolicyId           SigPolicyId,
 *           sigPolicyHash         SigPolicyHash,
 *           sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
 *                                   AOSigPolicyQualifierInfo OPTIONAL}
 * </code>
 * </pre>
 */
public final class GenCAdESEPESSignedData {

    private GenCAdESEPESSignedData() {
        // No permitimos la instanciacion
    }

    /** Genera una firma digital usando una estructura PKCS#7 SignedData.
     * @param signatureAlgorithm Algoritmo de firma que se deber&acute; usar.
     * @param key Referencia a la cl@ve privada de firme.
     * @param certChain Cadena de certificaci&oacute;n del certificado de firma.
     * @param config Configurac&oacute;n de la firma a generar.
     * @return La firma generada codificada en ASN.1 binario.
     * @throws AOException Cuando ocurre alg&uacute;n error durante el proceso de codificaci&oacute;n ASN.1 */
    public static byte[] generateSignedData(
    		final String signatureAlgorithm,
            final PrivateKey key,
            final Certificate[] certChain,
            final CAdESParameters config) throws AOException {

    	if (config == null) {
            throw new IllegalArgumentException("No se ha introducido configuracion para la construccion de la firma"); //$NON-NLS-1$
        }

        final Date signDate = new Date();

        final Certificate[] aplicableCertificateChain = config.isIncludedOnlySigningCertificate() ?
        		new Certificate[] { certChain[0] } : certChain;

        // Obtenemos la estructura con los atributos que hay que firmar (Prefirma)
        final byte[] preSignature = CAdESTriPhaseSigner.preSign(
        		aplicableCertificateChain,
                signDate,
                config
        );

        // Firmamos la prefirma (PKCS#1)
        final byte[] signatureValue = new AOPkcs1Signer().sign(
    		preSignature,
    		signatureAlgorithm,
    		key,
    		aplicableCertificateChain,
    		config.getExtraParams()
		);

        // Componemos la firma completa (Postfirma)
        return CAdESTriPhaseSigner.postSign(
            signatureAlgorithm,
            config.getContentData(),
            aplicableCertificateChain,
            signatureValue,
            preSignature
        );

    }

}
