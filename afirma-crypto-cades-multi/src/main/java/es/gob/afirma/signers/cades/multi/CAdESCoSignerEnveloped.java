/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.cades.multi;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.beans.AdESPolicy;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.cades.PKCS1ExternalizableSigner;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que implementa la cofirma digital CADES SignedAndEnvelopedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedAndEnvelopedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> pero con la
 * peculiaridad de que es una Cofirma.
 * Para ello, debe incluirse el atributo de pol&iacute;tica en la
 * identificaci&oacute;n del firmante de la siguiente manera:
 *
 * <pre>
 * <code>
 *
 * SignaturePolicyId ::= SEQUENCE {
 *  sigPolicyId           SigPolicyId,
 *  sigPolicyHash         SigPolicyHash,
 *  sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
 *                          SigPolicyQualifierInfo OPTIONAL}
 *
 *  SigPolicyId ::= OBJECT IDENTIFIER
 *
 *  OtherHashAlgAndValue ::= SEQUENCE {
 *     hashAlgorithm    AlgorithmIdentifier,
 *     hashValue        OCTET STRING }
 *
 *  SigPolicyQualifierInfo ::= SEQUENCE {
 *       SigPolicyQualifierId  SigPolicyQualifierId,
 *       SigQualifier          ANY DEFINED BY policyQualifierId }
 *
 *  SigPolicyQualifierId ::= OBJECT IDENTIFIER
 *
 *      id-spq-ets-uri OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-spq(5) 1 }
 *
 *     SPuri ::= IA5String
 *
 *      id-spq-ets-unotice OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-spq(5) 2 }
 *
 *     SPUserNotice ::= SEQUENCE {
 *          noticeRef        NoticeReference OPTIONAL,
 *          explicitText     DisplayText OPTIONAL
 *  }
 *
 *     NoticeReference ::= SEQUENCE {
 *          organization     DisplayText,
 *          noticeNumbers    SEQUENCE OF INTEGER
 *  }
 *
 *     DisplayText ::= CHOICE {
 *          visibleString    VisibleString  (SIZE (1..200)),
 *          bmpString        BMPString      (SIZE (1..200)),
 *          utf8String       UTF8String     (SIZE (1..200))
 *  }
 *
 * </code>
 * </pre> */
final class CAdESCoSignerEnveloped {

    private ASN1Set signedAttr2;

    /** Se crea una cofirma a partir de los datos del firmante, el archivo que se
     * firma y del archivo que contiene las firmas.
     * @param parameters
     *        par&aacute;metros necesarios que contienen tanto la firma del
     *        archivo a firmar como los datos del firmante.
     * @param sign
     *        Archivo que contiene las firmas.
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param keyEntry
     *        Clave privada usada para firmar.
     * @param messageDigest
     *        Hash espec&iacute;fico para una firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws java.security.cert.CertificateException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma. */
    byte[] coSigner(final P7ContentSignerParameters parameters,
                           final byte[] sign,
                           final AdESPolicy policy,
                           final boolean signingCertificateV2,
                           final PrivateKeyEntry keyEntry,
                           final byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

        final ASN1InputStream is = new ASN1InputStream(sign);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedAndEnvelopedData
        e.nextElement();
        // Contenido de SignedAndEnvelopedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido
                                                                        // del
                                                                        // SignedData

        final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        final ASN1Set certificatesSigned = sd.getCertificates();
        final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        final Enumeration<?> certs = certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()) {
            vCertsSig.add((DEREncodable) certs.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            final List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.fillRestCerts(ce, vCertsSig);
        }

        // buscamos que timo de algoritmo es y lo codificamos con su OID
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // Identificador del firmante ISSUER AND SERIAL-NUMBER
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS

        ASN1Set signedAttr = null;
        if (messageDigest == null) {
            final ASN1EncodableVector contextExpecific =
                CAdESUtils.generateSignerInfo(signerCertificateChain[0],
                     digestAlgorithm,
                     parameters.getContent(),
                     policy,
                     signingCertificateV2,
                     null,
                     new Date()
                 );

            this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
            signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
        }
        else {
            final ASN1EncodableVector contextExpecific =
                CAdESUtils.generateSignerInfo(signerCertificateChain[0],
                     digestAlgorithm,
                     null,
                     policy,
                     signingCertificateV2,
                     messageDigest,
                     new Date()
                );
            this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
            signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
        }

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del SignedAndEnvelopedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

        // introducimos los SignerInfos Existentes
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        // introducimos el nuevo SignerInfo del firmante actual.

        for (int i = 0; i < signerInfosSd.size(); i++) {
            final SignerInfo si = new SignerInfo((ASN1Sequence) signerInfosSd.getObjectAt(i));
            signerInfos.add(si);
        }

        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final AOException ex) {
            throw new IOException("Error al realizar la firma: " + ex); //$NON-NLS-1$
        }

        // Creamos los signerInfos del SignedAndEnvelopedData
        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, null // unsignedAttr
        ));

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                                                                        sd.getDigestAlgorithms(),
                                                                                                        sd.getEncryptedContentInfo(),
                                                                                                        certificates,
                                                                                                        null,
                                                                                                        new DERSet(signerInfos)// unsignedAttr
                               )).getDEREncoded();

    }

    /** Se crea una cofirma a partir de los datos del firmante y del archivo que
     * contiene las firmas.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param signerCertificateChain
     *        Cadena de certificados para la construccion de los parametros
     *        de firma.
     * @param data
     *        Archivo que contiene las firmas.
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param keyEntry
     *        Clave privada usada para firmar.
     * @param messageDigest
     *        Hash espec&iacute;fico para una firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws java.security.cert.CertificateException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma. */
    byte[] coSigner(final String signatureAlgorithm,
                           final X509Certificate[] signerCertificateChain,
                           final InputStream data,
                           final AdESPolicy policy,
                           final boolean signingCertificateV2,
                           final PrivateKeyEntry keyEntry,
                           byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

        final ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedAndEnvelopedData
        e.nextElement();
        // Contenido de SignedAndEnvelopedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido
                                                                        // del
                                                                        // SignedData

        final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        final ASN1Set certificatesSigned = sd.getCertificates();
        final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        final Enumeration<?> certs = certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()) {
            vCertsSig.add((DEREncodable) certs.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            final List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.fillRestCerts(ce, vCertsSig);
        }

        // buscamos que timo de algoritmo es y lo codificamos con su OID
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // Identificador del firmante ISSUER AND SERIAL-NUMBER
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // digEncryptionAlgorithm
        AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del SignedAndEnvelopedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

        // introducimos los SignerInfos Existentes
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        // introducimos el nuevo SignerInfo del firmante actual.

        for (int i = 0; i < signerInfosSd.size(); i++) {
            final SignerInfo si = new SignerInfo((ASN1Sequence) signerInfosSd.getObjectAt(i));
            final AlgorithmIdentifier algHash = si.getDigestAlgorithm();
            if (algHash.getAlgorithm().toString().equals(AOAlgorithmID.getOID(digestAlgorithm))) {
                final ASN1Set signedAttrib = si.getAuthenticatedAttributes();
                for (int s = 0; s < signedAttrib.size(); s++) {
                    final ASN1Sequence elemento = (ASN1Sequence) signedAttrib.getObjectAt(s);
                    final DERObjectIdentifier oids = (DERObjectIdentifier) elemento.getObjectAt(0);
                    if (CMSAttributes.messageDigest.getId().toString().equals(oids.toString())) {
                        final DERSet derSetHash = (DERSet) elemento.getObjectAt(1);
                        final DEROctetString derHash = (DEROctetString) derSetHash.getObjectAt(0);
                        messageDigest = derHash.getOctets();
                    }
                }
            }

            signerInfos.add(si);
        }

        // // ATRIBUTOS

        ASN1Set signedAttr = null;
        // atributos firmados
        if (messageDigest != null) {
            final ASN1EncodableVector contextExpecific =
                CAdESUtils.generateSignerInfo(signerCertificateChain[0],
                     digestAlgorithm,
                     null,
                     policy,
                     signingCertificateV2,
                     messageDigest,
                     new Date()
                );
            this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
            signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
        }
        else {
            throw new IllegalStateException("No se puede crear la firma ya que no se ha encontrado un message digest valido"); //$NON-NLS-1$
        }

        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final AOException ex) {
            throw new IOException("Error al realizar la firma: " + ex); //$NON-NLS-1$
        }

        // Creamos los signerInfos del SignedAndEnvelopedData
        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, null // unsignedAttr
        ));

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                                                                        sd.getDigestAlgorithms(),
                                                                                                        sd.getEncryptedContentInfo(),
                                                                                                        certificates,
                                                                                                        null,
                                                                                                        new DERSet(signerInfos)// unsignedAttr
                               )).getDEREncoded();

    }

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param keyEntry
     *        Clave para firmar.
     * @return Firma de los atributos.
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1OctetString firma(final String signatureAlgorithm, final PrivateKeyEntry keyEntry) throws AOException {

        final byte[] tmp;
        try {
            tmp = this.signedAttr2.getEncoded(ASN1Encodable.DER);
        }
        catch (final IOException ex) {
            throw new AOException("Error al obtener los datos a firmar", ex); //$NON-NLS-1$
        }

        return new DEROctetString(PKCS1ExternalizableSigner.sign(signatureAlgorithm, keyEntry, tmp));

    }
}
