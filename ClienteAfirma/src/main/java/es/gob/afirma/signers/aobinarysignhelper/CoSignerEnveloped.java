/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.fillRestCerts;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.getAttributeSet;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.makeAlgId;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

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
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x500.style.RFC4519Style;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;

/** Clase que implementa la cofirma digital PKCS#7/CMS SignedAndEnvelopedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedAndEnvelopedData pero con la peculiaridad de que es
 * una Cofirma. */
public final class CoSignerEnveloped {

    private ASN1Set signedAttr2;

    /** Constructor de la clase. Se crea una cofirma a partir de los datos del
     * firmante, el archivo que se firma y del archivo que contiene las firmas.
     * @param parameters
     *        par&aacute;metros necesarios que contienen tanto la firma del
     *        archivo a firmar como los datos del firmante.
     * @param sign
     *        Archivo que contiene las firmas.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param keyEntry
     *        Clave privada del firmante.
     * @param atrib
     *        Atributos firmados opcionales.
     * @param uatrib
     *        Atributos no autenticados firmados opcionales.
     * @param messageDigest
     *        Hash a aplicar en la firma.
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
    public byte[] coSigner(final P7ContentSignerParameters parameters,
                           final byte[] sign,
                           final Oid dataType,
                           final PrivateKeyEntry keyEntry,
                           final Map<Oid, byte[]> atrib,
                           final Map<Oid, byte[]> uatrib,
                           final byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

        final ASN1InputStream is = new ASN1InputStream(sign);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID signedAndEnvelopedData
        e.nextElement();
        // Contenido de signedAndEnvelopedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido
                                                                        // del
                                                                        // signedAndEnvelopedData

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
            // descomentar lo siguiente para version del rfc 3852
            final List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
            certificates = fillRestCerts(ce, vCertsSig);

            // y comentar esta parte de abajo
            // vCertsSig.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getEncoded())));
            // certificates = new BERSet(vCertsSig);

        }

        // buscamos que timo de algoritmo es y lo codificamos con su OID
        AlgorithmIdentifier digAlgId;
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        String digestAlgorithm = null;
        String keyAlgorithm = null;
        final int with = signatureAlgorithm.indexOf("with");
        if (with > 0) {
            digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(signatureAlgorithm);
            final int and = signatureAlgorithm.indexOf("and", with + 4);
            if (and > 0) {
                keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
            }
            else {
                keyAlgorithm = signatureAlgorithm.substring(with + 4);
            }
        }
        final AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
        digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());

        // Identificador del firmante ISSUER AND SERIAL-NUMBER
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS

        // atributos firmados
        ASN1Set signedAttr = null;
        if (messageDigest == null) {
            signedAttr =
                    generateSignerInfo(signerCertificateChain[0], digestAlgorithm, parameters.getContent(), dataType, atrib);
        }
        else {
            signedAttr = generateSignerInfoFromHash(signerCertificateChain[0], digestAlgorithm, messageDigest, dataType, atrib);
        }

        // atributos no firmados.
        ASN1Set unSignedAttr = null;
        unSignedAttr = generateUnsignerInfo(uatrib);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;
        encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del signedAndEnvelopedData
        ASN1Set signerInfosSd = null;
        signerInfosSd = sd.getSignerInfos();

        // introducimos los SignerInfos Existentes
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        // introducimos el nuevo SignerInfo del firmante actual.

        for (int i = 0; i < signerInfosSd.size(); i++) {
            final SignerInfo si = new SignerInfo((ASN1Sequence) signerInfosSd.getObjectAt(i));
            signerInfos.add(si);
        }

        ASN1OctetString sign2 = null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final Exception ex) {
            throw new IOException("Error al generar la firma: " + ex);
        }

        // Creamos los signerInfos del signedAndEnvelopedData
        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
        ));

        // CRLS no usado
        final ASN1Set certrevlist = null;

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                                                                        sd.getDigestAlgorithms(),
                                                                                                        sd.getEncryptedContentInfo(),
                                                                                                        certificates,
                                                                                                        certrevlist,
                                                                                                        new DERSet(signerInfos)// unsignedAttr
                               )).getDEREncoded();

    }

    /*
     * sd.getRecipientInfos(), sd.getDigestAlgorithms(),
     * sd.getEncryptedContentInfo(), certificates, certrevlist, new
     * DERSet(signerInfos))).getDEREncoded();
     */

    /** Constructor de la clase. Se crea una cofirma a partir de los datos del
     * firmante y el archivo que se firma.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param signerCertificateChain
     *        Cadena de certificados para la construccion de los parametros
     *        de firma.
     * @param sign
     *        Archivo que contiene las firmas.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param keyEntry
     *        Clave privada del firmante.
     * @param atrib
     *        Atributos firmados adicionales.
     * @param uatrib
     *        Atributos no firmados adicionales.
     * @param messageDigest
     *        Hash a aplicar en la firma.
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
    public byte[] coSigner(final String signatureAlgorithm,
                           final X509Certificate[] signerCertificateChain,
                           final byte[] sign,
                           final Oid dataType,
                           final PrivateKeyEntry keyEntry,
                           final Map<Oid, byte[]> atrib,
                           final Map<Oid, byte[]> uatrib,
                           byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

        final ASN1InputStream is = new ASN1InputStream(sign);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID signedAndEnvelopedData
        e.nextElement();
        // Contenido de signedAndEnvelopedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido
                                                                        // del
                                                                        // signedAndEnvelopedData

        final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        // X509Certificate[] signerCertificateChain =
        // parameters.getSignerCertificateChain();

        final ASN1Set certificatesSigned = sd.getCertificates();
        final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        final Enumeration<?> certs = certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()) {
            vCertsSig.add((DEREncodable) certs.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            // descomentar lo siguiente para version del rfc 3852
            final List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
            certificates = fillRestCerts(ce, vCertsSig);

            // y comentar esta parte de abajo
            // vCertsSig.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getEncoded())));
            // certificates = new BERSet(vCertsSig);

        }

        // buscamos que tipo de algoritmo es y lo codificamos con su OID
        AlgorithmIdentifier digAlgId;
        // String signatureAlgorithm = parameters.getSignatureAlgorithm();
        String digestAlgorithm = null;
        String keyAlgorithm = null;
        final int with = signatureAlgorithm.indexOf("with");
        if (with > 0) {
            digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(signatureAlgorithm);
            final int and = signatureAlgorithm.indexOf("and", with + 4);
            if (and > 0) {
                keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
            }
            else {
                keyAlgorithm = signatureAlgorithm.substring(with + 4);
            }
        }
        final AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
        digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());

        // Identificador del firmante ISSUER AND SERIAL-NUMBER
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS

        // atributos firmados
        ASN1Set signedAttr = null;

        // atributos no firmados.
        ASN1Set unSignedAttr = null;
        unSignedAttr = generateUnsignerInfo(uatrib);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;
        encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del signedAndEnvelopedData
        ASN1Set signerInfosSd = null;
        signerInfosSd = sd.getSignerInfos();

        // introducimos los SignerInfos Existentes
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        // introducimos el nuevo SignerInfo del firmante actual.

        // Secuencia:
        // 1.- Si cofirmamos sin datos en el mismo algoritmo de hash que la
        // firma
        // original sacamos el messagedigest de la firma previa.
        // 2.- Si no es el mismo algoritmo, miramos si nos ha llegado un
        // messagedigest
        // como parametro del metodo, que quiere decir que se ha calculado
        // externamente
        // (en el fondo sera que no se ha sobreescrito el parametro, con lo que
        // si llego
        // != null, seguira siendo != null)
        // 3.- Si no es ninguno de los dos casos, no podemos firmar
        for (int i = 0; i < signerInfosSd.size(); i++) {
            final SignerInfo si = new SignerInfo((ASN1Sequence) signerInfosSd.getObjectAt(i));
            final AlgorithmIdentifier algHash = si.getDigestAlgorithm();
            // Solo si coninciden los algos puedo sacar el hash de dentro
            if (algHash.getAlgorithm().toString().equals(digestAlgorithmId.getOID().toString())) {
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

        if (messageDigest != null) {
            signedAttr =
                    generateSignerInfoFromHash(signerCertificateChain[0], digestAlgorithm, messageDigest, dataType, atrib);
        }
        else {
            // En este caso no puedo usar un hash de fuera, ya que no me han
            // pasado datos ni
            // huellas digitales, solo un fichero de firma
            throw new IllegalStateException("No se puede crear la firma ya que no se ha encontrado un hash valido.");
        }

        ASN1OctetString sign2 = null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final Exception ex) {
            throw new IOException("Error al generar la firma: " + ex);
        }

        // Creamos los signerInfos del signedAndEnvelopedData
        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
        ));

        // CRLS no usado
        final ASN1Set certrevlist = null;

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                                                                        sd.getDigestAlgorithms(),
                                                                                                        sd.getEncryptedContentInfo(),
                                                                                                        certificates,
                                                                                                        certrevlist,
                                                                                                        new DERSet(signerInfos)// unsignedAttr
                               )).getDEREncoded();

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param cert
     *        Certificado necesario para la firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param datos
     *        Datos firmados.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param atrib
     *        Lista de atributos firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos firmados de la firma.
     * @throws java.security.NoSuchAlgorithmException */
    private ASN1Set generateSignerInfo(final X509Certificate cert, String digestAlgorithm, final byte[] datos, final Oid dataType, final Map<Oid, byte[]> atrib) throws NoSuchAlgorithmException {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));

        // fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // Los DigestAlgorithms con SHA-2 tienen un guion:
        if (digestAlgorithm.equals("SHA512")) {
            digestAlgorithm = "SHA-512";
        }
        else if (digestAlgorithm.equals("SHA384")) {
            digestAlgorithm = "SHA-384";
        }
        else if (digestAlgorithm.equals("SHA256")) {
            digestAlgorithm = "SHA-256";
        }

        // Si nos viene el hash de fuera no lo calculamos
        final byte[] md = MessageDigest.getInstance(digestAlgorithm).digest(datos);

        // MessageDigest
        ContexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString(md.clone()))));

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

        signedAttr2 = getAttributeSet(new AttributeTable(ContexExpecific));

        return getAttributeSet(new AttributeTable(ContexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * En este caso se introduce el hash directamente.
     * @param cert
     *        Certificado necesario para la firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param datos
     *        Datos firmados.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param atrib
     *        Lista de atributos firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos firmados de la firma. */
    private ASN1Set generateSignerInfoFromHash(final X509Certificate cert, final String digestAlgorithm, final byte[] datos, final Oid dataType, final Map<Oid, byte[]> atrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));

        // fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // MessageDigest
        ContexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString(datos))));

        // Serial Number
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

        signedAttr2 = getAttributeSet(new AttributeTable(ContexExpecific));

        return getAttributeSet(new AttributeTable(ContexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uatrib
     *        Conjunto de atributos no firmados que se insertar&aacute;n
     *        dentro del archivo de firma.
     * @return Los atributos no firmados de la firma */
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

        return getAttributeSet(new AttributeTable(ContexExpecific));

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
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e);
        }

        byte[] tmp = null;

        try {
            tmp = signedAttr2.getEncoded(ASN1Encodable.DER);
        }
        catch (final IOException ex) {
            Logger.getLogger(CoSignerEnveloped.class.getName()).log(Level.SEVERE, null, ex);
        }

        // Indicar clave privada para la firma
        try {
            sig.initSign(keyEntry.getPrivateKey());
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada", e);
        }

        // Actualizamos la configuracion de firma
        try {
            sig.update(tmp);
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar la informacion de firma", e);
        }

        // firmamos.
        byte[] realSig = null;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e);
        }

        final ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;

    }

}
