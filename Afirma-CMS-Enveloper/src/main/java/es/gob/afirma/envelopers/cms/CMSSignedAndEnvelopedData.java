/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x500.style.RFC4519Style;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.GenSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que implementa firma digital PKCS#7/CMS SignedAndEnvelopedData basado
 * en las especificaciones de RFC-2315.
 * La Estructura del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *    SignedAndEnvelopedData ::= SEQUENCE {
 *    version Version,
 *    recipientInfos RecipientInfos,
 *    digestAlgorithms DigestAlgorithmIdentifiers,
 *    encryptedContentInfo EncryptedContentInfo,
 *    certificates
 *      [0] IMPLICIT ExtendedCertificatesAndCertificates
 *         OPTIONAL,
 *    crls
 *      [1] IMPLICIT CertificateRevocationLists OPTIONAL,
 *    signerInfos SignerInfos }
 *
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje SignedAndEnvelopedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */
public final class CMSSignedAndEnvelopedData {

    /** Clave de cifrado. La almacenamos internamente. */
    private SecretKey cipherKey;
    private ASN1Set signedAttr2;

    /** M&eacute;todo que genera la firma de tipo SignedAndEnvelopedData.
     * @param parameters
     *        Par&aacute;metros necesarios para la generaci&oacute;n de este
     *        tipo.
     * @param config
     *        Configuraci&oacute;n del algoritmo para firmar
     * @param certDest
     *        Certificado del destino al cual va dirigido la firma.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param keyEntry
     *        Eatrada a la clave privada para firma
     * @param atrib
     *        Conjunto de atributos firmados.
     * @param uatrib
     *        Conjunto de atributos no firmados.
     * @return La firma de tipo SignedAndEnvelopedData.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.cert.CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital */
    public byte[] genSignedAndEnvelopedData(final P7ContentSignerParameters parameters,
                                            final AOCipherConfig config,
                                            final X509Certificate[] certDest,
                                            final Oid dataType,
                                            final PrivateKeyEntry keyEntry,
                                            final Map<Oid, byte[]> atrib,
                                            final Map<Oid, byte[]> uatrib) throws IOException, CertificateEncodingException, NoSuchAlgorithmException {

        this.cipherKey = Utils.initEnvelopedData(config, certDest);

        // 1. VERSION
        // la version se mete en el constructor del signedAndEnvelopedData y es
        // 1

        // 2. DIGESTALGORITM
        // buscamos que timo de algoritmo es y lo codificamos con su OID

        String signatureAlgorithm = null;
        String digestAlgorithm = null;
        AlgorithmId digestAlgorithmId = null;
        final ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
        String keyAlgorithm = null;

        try {
            signatureAlgorithm = parameters.getSignatureAlgorithm();
            digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
            digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
            keyAlgorithm = Utils.getKeyAlgorithm(signatureAlgorithm, digestAlgorithmId);

            final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());
            digestAlgs.add(digAlgId);
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e);
        }

        // LISTA DE CERTIFICADOS: obtenemos la lista de certificados
        final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();
        final ASN1Set certificates = Utils.fetchCertificatesList(signerCertificateChain);

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(parameters.getContent(), config, certDest, cipherKey);

        // 4. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        final TBSCertificateStructure tbs2 = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));

        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs2.getIssuer()), tbs2.getSerialNumber().getValue());

        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // AlgorithmIdentifier
        final AlgorithmIdentifier digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());

        // // ATRIBUTOS
        final ASN1Set signedAttr = generateSignerInfo(signerCertificateChain[0], digestAlgorithm, parameters.getContent(), dataType, atrib);

        ASN1Set unSignedAttr = null;
        unSignedAttr = generateUnsignerInfo(uatrib);

        // digEncryptionAlgorithm
        final AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;
        try {
            encAlgId = SigUtils.makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e);
        }

        ASN1OctetString sign2 = null;
        try {
            sign2 = Utils.firma(signatureAlgorithm, keyEntry, signedAttr2);
        }
        catch (final AOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
        ));

        final ASN1Set certrevlist = null;

        // construimos el Signed And Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(new DERSet(infos.getRecipientInfos()),
                                                                                                        new DERSet(digestAlgs),
                                                                                                        infos.getEncInfo(),
                                                                                                        certificates,
                                                                                                        certrevlist,
                                                                                                        new DERSet(signerInfos))).getDEREncoded();
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param datos
     *        Datos firmados.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param atrib
     *        Conjunto de atributos firmados.
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.security.cert.CertificateException
     * @throws java.io.IOException */
    private ASN1Set generateSignerInfo(final X509Certificate cert, final String digestAlgorithm, final byte[] datos, final Oid dataType, final Map<Oid, byte[]> atrib) throws NoSuchAlgorithmException {
        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = Utils.initContexExpecific(digestAlgorithm, datos, dataType, null);

        // Serial Number
        // comentar lo de abajo para version del rfc 3852
        ContexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        // agregamos la lista de atributos a mayores.
        if (atrib.size() != 0) {
            final Iterator<Map.Entry<Oid, byte[]>> it = atrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<Oid, byte[]> e = it.next();
                ContexExpecific.add(new Attribute(
                // el oid
                                                  new DERObjectIdentifier((e.getKey()).toString()),
                                                  // el array de bytes en formato string
                                                  new DERSet(new DERPrintableString(e.getValue()))));
            }
        }

        signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));

        return SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uatrib
     *        Lista de atributos no firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos no firmados de la firma. */
    private ASN1Set generateUnsignerInfo(final Map<Oid, byte[]> uatrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {
            final Iterator<Map.Entry<Oid, byte[]>> it = uatrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<Oid, byte[]> e = it.next();
                ContexExpecific.add(new Attribute(
                // el oid
                                                  new DERObjectIdentifier((e.getKey()).toString()),
                                                  // el array de bytes en formato string
                                                  new DERSet(new DERPrintableString(e.getValue()))));
            }
        }
        else {
            return null;
        }

        return SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));

    }
}
