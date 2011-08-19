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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.BERConstructedOctetString;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.cms.CMSProcessable;
import org.bouncycastle.cms.CMSProcessableByteArray;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.GenSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;
import es.gob.afirma.signers.pkcs7.Utils;

import sun.security.x509.AlgorithmId;

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

    private ASN1Set signedAttr2;

    /** M&eacute;odo que genera una firma digital usando el sitema conocido como
     * SignedData y que podr&aacute; ser con el contenido del fichero codificado
     * o s&oacute;lo como referencia del fichero.
     * @param parameters
     *        Par&aacute;metros necesarios para obtener los datos de
     *        SignedData.
     * @param omitContent
     *        Par&aacute;metro qeu indica si en la firma va el contenido del
     *        fichero o s&oacute;lo va de forma referenciada.
     * @param policy
     *        Url de la Politica aplicada.
     * @param qualifier
     *        OID de la pol&iacute;tica.
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
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
                                     final String policy,
                                     final String qualifier,
                                     final boolean signingCertificateV2,
                                     final String dataType,
                                     final PrivateKeyEntry keyEntry,
                                     final byte[] messageDigest) throws NoSuchAlgorithmException, CertificateException, IOException, AOException {

        if (parameters == null) {
            throw new IllegalArgumentException("Los parametros no pueden ser nulos"); //$NON-NLS-1$
        }

        // 1. VERSION
        // la version se mete en el constructor del signedData y es 1

        // 2. DIGESTALGORITM
        // buscamos que timo de algoritmo es y lo codificamos con su OID

        final ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
        AlgorithmIdentifier digAlgId;

        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        String digestAlgorithm = null;
        String keyAlgorithm = null;
        final int with = signatureAlgorithm.indexOf("with"); //$NON-NLS-1$
        if (with > 0) {
            digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
            final int and = signatureAlgorithm.indexOf("and", with + 4); //$NON-NLS-1$
            if (and > 0) {
                keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
            }
            else {
                keyAlgorithm = signatureAlgorithm.substring(with + 4);
            }
        }

        final AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);

        try {
            digAlgId = SigUtils.makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e); //$NON-NLS-1$
        }

        digestAlgs.add(digAlgId);

        // 3. CONTENTINFO
        // si se introduce el contenido o no

        ContentInfo encInfo = null;
        final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(dataType.toString());

        if (omitContent == false) {
            final ByteArrayOutputStream bOut = new ByteArrayOutputStream();
            final byte[] content2 = parameters.getContent();
            final CMSProcessable msg = new CMSProcessableByteArray(content2);
            try {
                msg.write(bOut);
            }
            catch (final Exception ex) {
                throw new IOException("Error en la escritura del procesable CMS: " + ex); //$NON-NLS-1$
            }
            encInfo = new ContentInfo(contentTypeOID, new BERConstructedOctetString(bOut.toByteArray()));
        }
        else {
            encInfo = new ContentInfo(contentTypeOID, null);
        }

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados

        ASN1Set certificates = null;
        final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        if (signerCertificateChain.length != 0) {
            final List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.createBerSetFromList(ce);
        }

        final ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());

        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // AlgorithmIdentifier
        digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());

        // // ATRIBUTOS

        final ASN1EncodableVector contextExcepcific =
                Utils.generateSignerInfo(signerCertificateChain[0],
                                         digestAlgorithmId,
                                         digestAlgorithm,
                                         digAlgId,
                                         parameters.getContent(),
                                         policy,
                                         qualifier,
                                         signingCertificateV2,
                                         dataType,
                                         messageDigest);
        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));
        final ASN1Set signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        final AlgorithmIdentifier encAlgId;
        try {
            encAlgId = SigUtils.makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e); //$NON-NLS-1$
        }

        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final AOException ex) {
            throw ex;
        }

        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, null // unsignedAttr
        ));

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedData, new SignedData(new DERSet(digestAlgs),
                                                                                encInfo,
                                                                                certificates,
                                                                                certrevlist,
                                                                                new DERSet(signerInfos))).getDEREncoded();

    }

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param keyEntry
     *        Clave para firmar.
     * @return Firma de los atributos.
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1OctetString firma(final String signatureAlgorithm, final PrivateKeyEntry keyEntry) throws AOException {

        Signature sig = null;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        byte[] tmp = null;

        try {
            tmp = this.signedAttr2.getEncoded(ASN1Encodable.DER);
        }
        catch (final IOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, ex.toString(), ex);
            throw new AOException("Error al detectar la codificacion de los datos ASN.1", ex); //$NON-NLS-1$
        }

        // Indicar clave privada para la firma
        try {
            sig.initSign(keyEntry.getPrivateKey());
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada", e); //$NON-NLS-1$
        }

        // Actualizamos la configuracion de firma
        try {
            sig.update(tmp);
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar la informacion de firma", e); //$NON-NLS-1$
        }

        // firmamos.
        byte[] realSig = null;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

        final ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;

    }

}
