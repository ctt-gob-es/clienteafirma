/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cades;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.util.Date;
import java.util.List;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOPkcs1Signer;
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
 *                                   AOSigPolicyQualifierInfo OPTIONAL}
 * </code>
 * </pre>
 */
public final class GenCAdESEPESSignedData {

    private GenCAdESEPESSignedData() {
        // No permitimos la instanciacion
    }

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
     * @param key Clave privada para firma.
     * @param certChain Cadena de certificados del firmante
     * @param dataDigest Huella digital de los datos a firmar cuando esta se proporciona precalculada. Si los datos a firmar (que se proporcionan
     *                   en el par&aacute;metro <code>parameters</code> (de tipo <code>P7ContentSignerParameters</code>) <u>no</u> son nulos este valor
     *                   se ignora, us&aacute;ndose &uacute;nicamente cuando el par&aacute;metro <code>parameters</code> es nulo.
     * @param dataDigestAlgorithmName Algoritmo de huella digital usado para calcular el valor indicado en el par&aacute;metro <code>dataDigest</code>.
     *                                Si <code>dataDigest</code> es nulo el valor de este par&aacute;metro se ignora.
     * @param includeSigningTimeAttribute <code>true</code> para incluir el atributo <i>SigningTime</i> de PKCS#9 (OID:1.2.840.113549.1.9.5),
     *                                    <code>false</code> para no incluirlo. Este atributo nunca se incluye en el modo PAdES.
     * @param padesMode <code>true</code> para generar una firma CAdES compatible PAdES, <code>false</code> para generar una firma CAdES normal.
     * @param contentType Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return La firma generada codificada en ASN.1 binario.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital indicados
     * @throws CertificateException En caso de cualquier problema con los certificados de firma.
     * @throws IOException En caso de cualquier problema leyendo o escribiendo los datos
     * @throws AOException Cuando ocurre alg&uacute;n error durante el proceso de codificaci&oacute;n ASN.1 */
    public static byte[] generateSignedData(final P7ContentSignerParameters parameters,
                                            final boolean omitContent,
                                            final AdESPolicy policy,
                                            final boolean signingCertificateV2,
                                            final PrivateKey key,
                                            final Certificate[] certChain,
                                            final byte[] dataDigest,
                                            final String dataDigestAlgorithmName,
                                            final boolean includeSigningTimeAttribute,
                                            final boolean padesMode,
                                            final String contentType,
                                            final String contentDescription,
                                            final List<CommitmentTypeIndicationBean> ctis,
                                            final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
                                                                                  CertificateException,
                                                                                  IOException,
                                                                                  AOException {
        if (parameters == null) {
            throw new IllegalArgumentException("Los parametros no pueden ser nulos"); //$NON-NLS-1$
        }
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();

        final Date signDate = new Date();

        // Ya que el contenido de la firma puede ser grande, lo obtenemos solo al principio
        final byte[] content = parameters.getContent();

        final byte[] preSignature = CAdESTriPhaseSigner.preSign(
    		AOSignConstants.getDigestAlgorithmName(dataDigestAlgorithmName),
            omitContent ? null : content,
            certChain,
            policy,
            signingCertificateV2,
            dataDigest,
            signDate,
            includeSigningTimeAttribute,
            padesMode,
            contentType,
            contentDescription,
            ctis,
            csm
        );

        final byte[] signature = new AOPkcs1Signer().sign(preSignature, signatureAlgorithm, key, certChain, null);

        return CAdESTriPhaseSigner.postSign(
            AOSignConstants.getDigestAlgorithmName(signatureAlgorithm),
            omitContent ? null : content,
            certChain,
            signature,
            preSignature
        );

    }

}
