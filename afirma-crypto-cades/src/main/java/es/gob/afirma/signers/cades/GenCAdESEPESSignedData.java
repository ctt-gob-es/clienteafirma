/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
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
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Clase que implementa firma digital CMS Advanced Electronic Signatures
 * (CAdES).
 * La implementaci&oacute;n es la misma que para el Signed Data de CMS, salvo
 * que en los atributos del SignerInfo en vez de ir el n&uacute;mero de serie
 * (SerialNumber), va la firma del certificado.<br>
 * La Estructura del mensaje es la siguiente (Se omite la parte correspondiente
 * a CMS):<br>
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
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje SignedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */

public final class GenCAdESEPESSignedData {

    /** M&eacute;odo que genera una firma digital usando el sitema conocido como
     * SignedData y que podr&aacute; ser con el contenido del fichero codificado
     * o s&oacute;lo como referencia del fichero.
     * @param parameters
     *        Par&aacute;metros necesarios para obtener los datos de
     *        SignedData.
     * @param omitContent
     *        Par&aacute;metro qeu indica si en la firma va el contenido del
     *        fichero o s&oacute;lo va de forma referenciada.
     * @param policyIdentifier Identificador de la pol&iacute;tica de firma (OID, directo o como URN)
     * @param policyIdentifierHash Huella digital de la pol&iacute;tica de firma en formato ASN.1 procesable identificado por
     *                             el OID indicado en <code>policyIdentifier</code>. Puede ser nulo
     * @param policyIdentifierHashAlgorithm Algoritmo de huella digital usado para el c&aacute;lculo del valor indicado
     *                                      en <code>policyIdentifierHashAlgorithm</code>. Es obligatorio si el valor
     *                                      indicado en <code>policyIdentifierHashAlgorithm</code> no es ni nulo ni
     *                                      <code>0</code>
     * @param policyQualifier URL que apunta a una descripci&oacute;n legible de la pol&iacute;tica (normalmente un PDF)
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param keyEntry
     *        Entrada a la clave privada para firma.
     * @param messageDigest
     *        Hash a aplicar en la firma.
     * @return La firma generada codificada.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws java.security.cert.CertificateException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws AOException
     *         Cuando ocurre un error durante el proceso de descifrado
     *         (formato o clave incorrecto,...) */
    public byte[] generateSignedData(final P7ContentSignerParameters parameters,
                                     final boolean omitContent,
                                     final String policyIdentifier,
                                     final String policyIdentifierHash,
                                     final String policyIdentifierHashAlgorithm,
                                     final String policyQualifier,
                                     final boolean signingCertificateV2,
                                     final PrivateKeyEntry keyEntry,
                                     byte[] messageDigest) throws NoSuchAlgorithmException, CertificateException, IOException, AOException {

        if (parameters == null) {
            throw new IllegalArgumentException("Los parametros no pueden ser nulos"); //$NON-NLS-1$
        }
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();

        final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();
        
        final byte[] content = (omitContent) ? null : parameters.getContent();
        
        if (messageDigest == null && parameters.getContent() != null) {
            final MessageDigest md = MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(signatureAlgorithm));
            md.update(parameters.getContent());
            messageDigest = md.digest();
        }
        
        final Date signDate = new Date();
        
        final byte[] preSignature = CAdESTriPhaseSigner.preSign(
            AOSignConstants.getDigestAlgorithmName(signatureAlgorithm), 
            content, 
            signerCertificateChain, 
            policyIdentifier,
            policyIdentifierHash,
            policyIdentifierHashAlgorithm,
            policyQualifier, 
            signingCertificateV2, 
            messageDigest,
            signDate
        );
        
        final byte[] signature = PKCS1ExternalizableSigner.sign(signatureAlgorithm, keyEntry, preSignature);
        
        return CAdESTriPhaseSigner.postSign(
            AOSignConstants.getDigestAlgorithmName(signatureAlgorithm),
            content, 
            signerCertificateChain,  
            signature,
            // Volvemos a crear la prefirma simulando una firma trifasica en la que la postfirma no cuenta con el
            // resultado de la prefirma
            CAdESTriPhaseSigner.preSign(
                AOSignConstants.getDigestAlgorithmName(signatureAlgorithm), 
                content, 
                signerCertificateChain, 
                policyIdentifier,
                policyIdentifierHash,
                policyIdentifierHashAlgorithm,
                policyQualifier, 
                signingCertificateV2, 
                messageDigest,
                signDate
            )
        );

    }

}
