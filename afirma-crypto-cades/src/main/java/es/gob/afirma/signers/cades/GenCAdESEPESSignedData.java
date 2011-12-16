/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cades;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Date;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

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
 *                                   SigPolicyQualifierInfo OPTIONAL}
 * </code>
 * </pre>
 */
public final class GenCAdESEPESSignedData {

    /** Genera una firma digital usando una estructura PKCS#7
     * SignedData. Puede incluir el contenido del fichero codificado
     * o s&oacute;lo una referencia a este.
     * @param parameters
     *        Par&aacute;metros necesarios para obtener los datos de
     *        SignedData.
     * @param omitContent
     *        <code>false</code> si en la firma se desea incluir el contenido del
     *        fichero o <code>true</code> si s&oacute;lo se desea usar una referencia.
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>SigningCertificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param keyEntry
     *        Entrada a la clave privada para firma.
     * @param messageDigest
     *        Huella digital a aplicar en la firma.
     * @param padesMode <code>true</code> para generar una firma CAdES compatible PAdES, <code>false</code> para generar una firma CAdES normal
     * @return La firma generada codificada en ASN.1 binario.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital indicados
     * @throws CertificateException
     *         En caso de cualquier problema con los certificados de
     *         firma.
     * @throws IOException
     *         En caso de cualquier problema leyendo o escribiendo los
     *         datos
     * @throws AOException
     *         Cuando ocurre alg&uacute;n error durante el proceso de codificaci&oacute;n ASN.1 */
    public byte[] generateSignedData(final P7ContentSignerParameters parameters,
                                     final boolean omitContent,
                                     final AdESPolicy policy,
                                     final boolean signingCertificateV2,
                                     final PrivateKeyEntry keyEntry,
                                     byte[] messageDigest,
                                     boolean padesMode) throws NoSuchAlgorithmException, CertificateException, IOException, AOException {

        if (parameters == null) {
            throw new IllegalArgumentException("Los parametros no pueden ser nulos"); //$NON-NLS-1$
        }
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();

        final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();
        
        final Date signDate = new Date();
        
        final byte[] preSignature = CAdESTriPhaseSigner.preSign(
            AOSignConstants.getDigestAlgorithmName(signatureAlgorithm), 
            (omitContent) ? null : parameters.getContent(), 
            signerCertificateChain, 
            policy, 
            signingCertificateV2, 
            (messageDigest == null && parameters.getContent() != null) ?
                MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(signatureAlgorithm)).digest(parameters.getContent()) :
                    messageDigest,
            signDate,
            padesMode
        );
        
        final byte[] signature = PKCS1ExternalizableSigner.sign(signatureAlgorithm, keyEntry.getPrivateKey(), preSignature);
        
        return CAdESTriPhaseSigner.postSign(
            AOSignConstants.getDigestAlgorithmName(signatureAlgorithm),
            (omitContent) ? null : parameters.getContent(), 
            signerCertificateChain,  
            signature,
            // Volvemos a crear la prefirma simulando una firma trifasica en la que la postfirma no cuenta con el
            // resultado de la prefirma
            CAdESTriPhaseSigner.preSign(
                AOSignConstants.getDigestAlgorithmName(signatureAlgorithm), 
                (omitContent) ? null : parameters.getContent(), 
                signerCertificateChain, 
                policy, 
                signingCertificateV2, 
                (messageDigest == null && parameters.getContent() != null) ?
                    MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(signatureAlgorithm)).digest(parameters.getContent()) :
                        messageDigest,
                signDate,
                padesMode
            )
        );

    }

}
