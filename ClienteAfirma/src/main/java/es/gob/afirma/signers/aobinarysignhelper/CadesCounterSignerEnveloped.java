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
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
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
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.Attribute;
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
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;

/** Clase que implementa la contrafirma digital CADES SignedAndEnvelopedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedAndEnvelopedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> pero con la
 * peculiaridad de que es una Contrafirma. */
public final class CadesCounterSignerEnveloped {

    /* Propiedades de la clase */
    private int actualIndex = 0;
    private Oid actualOid = null;
    private ASN1Set signedAttr2;

    private String globalPolicy = "";
    private Oid GlobalOidQualifier = null;
    private boolean GlobalsigningCertificateV2;

    Oid getActualOid() {
        return actualOid;
    }

    void setActualOid(final Oid actualOid) {
        this.actualOid = actualOid;
    }

    /** Obtiene el Oid del qualificador de pol&iacute;tica
     * @return Oid de calificador de pol&iacute;tica */
    Oid getGlobalOidQualifier() {
        return GlobalOidQualifier;
    }

    /** Establece el Oid del qualificador de pol&iacute;tica
     * @param globalOidQualifier
     *        Oid de calificador de pol&iacute;tica */
    void setGlobalOidQualifier(final Oid globalOidQualifier) {
        GlobalOidQualifier = globalOidQualifier;
    }

    /** Obtiene el tipo de atributo firmado signingCertificate o
     * signingCertificateV2
     * @return tipo de atributo firmado. */
    boolean isGlobalsigningCertificateV2() {
        return GlobalsigningCertificateV2;
    }

    /** Define si el atributo firmado es signingCertificate o
     * signingCertificateV2
     * @param globalsigningCertificateV2
     *        tipo de atributo */
    void setGlobalsigningCertificateV2(final boolean globalsigningCertificateV2) {
        GlobalsigningCertificateV2 = globalsigningCertificateV2;
    }

    /** Obtiene la pol&iacute;tica global.
     * @return politica de firma. */
    public String getGlobalPolicy() {
        return globalPolicy;
    }

    /** Establece la pol&iacute;tica de firma.
     * @param globalPolicy
     *        política de firma (URL). */
    public void setGlobalPolicy(final String globalPolicy) {
        this.globalPolicy = globalPolicy;
    }

    /** Constructor de la clase. Se crea una contrafirma a partir de los datos
     * del firmante, el archivo que se firma y del archivo que contiene las
     * firmas.<br>
     * @param parameters
     *        par&aacute;metros necesarios que contienen tanto la firma del
     *        archivo a firmar como los datos del firmante.
     * @param data
     *        Archivo que contiene las firmas.
     * @param targetType
     *        Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *        las hojas, un nodo determinado o unos determinados firmantes.
     * @param targets
     *        Nodos objetivos a firmar.
     * @param keyEntry
     *        Clave privada a usar para firmar.
     * @param policy
     *        URL de pol&iacute;tica.
     * @param qualifier
     *        OID de la pol&iacute;tica.
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException
     *         Excepci&oacute;n cuando se produce algun error con lectura
     *         escritura de ficheros.
     * @throws java.security.NoSuchAlgorithmException
     *         Excepci&oacute;n cuando no se encuentra el algoritmo de
     *         firma.
     * @throws java.security.cert.CertificateException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws AOException
     *         Cuando ocurre alguno error con contemplado por las otras
     *         excepciones declaradas */
    public byte[] counterSigner(final P7ContentSignerParameters parameters,
                                final byte[] data,
                                final CounterSignTarget targetType,
                                final int[] targets,
                                final PrivateKeyEntry keyEntry,
                                final String policy,
                                final Oid qualifier,
                                final boolean signingCertificateV2,
                                final Oid dataType) throws IOException, NoSuchAlgorithmException, CertificateException, AOException {

        // Inicializamos el Oid
        actualOid = dataType;

        // Introducimos la pol&iacute;tica en variable global por comodidad.
        // &Eacute;sta no var&iacute;a.
        this.setGlobalPolicy(policy);
        this.setGlobalOidQualifier(qualifier);
        this.setGlobalsigningCertificateV2(signingCertificateV2);

        final ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedAndEnvelopedData
        e.nextElement();
        // Contenido de SignedAndEnvelopedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();

        final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        // Obtenemos los signerInfos del SignedAndEnvelopedData
        ASN1Set signerInfosSd = null;
        signerInfosSd = sd.getSignerInfos();

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
        // e introducimos los del firmante actual.
        if (signerCertificateChain.length != 0) {
            final List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
            certificates = fillRestCerts(ce, vCertsSig);
        }

        // CRLS no usado
        final ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        // FIRMA EN ARBOL
        if (targetType.equals(CounterSignTarget.Tree)) {
            signerInfos = CounterTree(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        }
        // FIRMA DE LAS HOJAS
        else if (targetType.equals(CounterSignTarget.Leafs)) {
            signerInfos = CounterLeaf(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        }
        // FIRMA DE NODOS
        else if (targetType.equals(CounterSignTarget.Nodes)) {
            // Firma de Nodos
            SignedAndEnvelopedData sigDat;
            SignedAndEnvelopedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = CounterNode(aux, parameters, signerCertificateChain[0], keyEntry, nodo);
                sigDat =
                        new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                   sd.getDigestAlgorithms(),
                                                   sd.getEncryptedContentInfo(),
                                                   certificates,
                                                   certrevlist,
                                                   new DERSet(signerInfos));

                // Esto se realiza as&iacute; por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getDEREncoded());
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedAndEnvelopedData
                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, aux).getDEREncoded();
        }
        // FIRMA DE LOS SIGNERS
        else if (targetType.equals(CounterSignTarget.Signers)) {
            // Firma de Nodos
            SignedAndEnvelopedData sigDat;
            SignedAndEnvelopedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = CounterNode(aux, parameters, signerCertificateChain[0], keyEntry, nodo);
                sigDat =
                        new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                   sd.getDigestAlgorithms(),
                                                   sd.getEncryptedContentInfo(),
                                                   certificates,
                                                   certrevlist,
                                                   new DERSet(signerInfos));

                // Esto se realiza as&iacute; por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getDEREncoded());
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedAndEnvelopedData

                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, aux).getDEREncoded();
        }

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                                                                        sd.getDigestAlgorithms(),
                                                                                                        sd.getEncryptedContentInfo(),
                                                                                                        certificates,
                                                                                                        certrevlist,
                                                                                                        new DERSet(signerInfos))).getDEREncoded();

    }

    /** M&eacute;todo que contrafirma el arbol completo de forma recursiva, todos
     * los dodos creando un nuevo contraSigner.<br>
     * @param signerInfosRaiz
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo
     * @param cert
     *        Certificado de firma.
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1EncodableVector CounterTree(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final X509Certificate cert,
                                            final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1EncodableVector CounterSigners = new ASN1EncodableVector();

        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final ASN1Sequence atribute = (ASN1Sequence) signerInfosRaiz.getObjectAt(i);
            final SignerInfo si = new SignerInfo(atribute);

            final SignerInfo CounterSigner = getCounterUnsignedAtributes(si, parameters, cert, keyEntry);
            CounterSigners.add(CounterSigner);
        }

        return CounterSigners;
    }

    /** M&eacute;todo que contrafirma las hojas del arbol completo de forma
     * recursiva, todos los dodos creando un nuevo contraSigner.<br>
     * @param signerInfosRaiz
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param cert
     *        Certificado de firma.
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1EncodableVector CounterLeaf(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final X509Certificate cert,
                                            final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1EncodableVector CounterSigners = new ASN1EncodableVector();

        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final ASN1Sequence atribute = (ASN1Sequence) signerInfosRaiz.getObjectAt(i);
            final SignerInfo si = new SignerInfo(atribute);

            final SignerInfo CounterSigner = getCounterLeafUnsignedAtributes(si, parameters, cert, keyEntry);
            CounterSigners.add(CounterSigner);
        }

        return CounterSigners;
    }

    /** M&eacute;todo que contrafirma un nodo determinado del arbol buscandolo de
     * forma recursiva.<br>
     * @param sd
     *        SignedAndEnvelopedData que contiene el Nodo ra&iacute;z.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param cert
     *        Certificado de firma.
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param nodo
     *        Nodo signerInfo a firmar.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1EncodableVector CounterNode(final SignedAndEnvelopedData sd,
                                            final P7ContentSignerParameters parameters,
                                            final X509Certificate cert,
                                            final PrivateKeyEntry keyEntry,
                                            final int nodo) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1Set signerInfosRaiz = sd.getSignerInfos();

        final ASN1EncodableVector CounterSigners = new ASN1EncodableVector();
        ASN1Set auxSignerRaiz;

        auxSignerRaiz = signerInfosRaiz;
        actualIndex = 0;

        for (int i = 0; i < auxSignerRaiz.size(); i++) {
            final ASN1Sequence atribute = (ASN1Sequence) auxSignerRaiz.getObjectAt(i);
            final SignerInfo si = new SignerInfo(atribute);
            SignerInfo CounterSigner = null;
            if (actualIndex == nodo) {
                CounterSigner = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry);
            }
            else {
                if (actualIndex != nodo) {
                    CounterSigner = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry, nodo);
                }
            }
            actualIndex++;
            CounterSigners.add(CounterSigner);
        }

        return CounterSigners;

    }

    /** M&eacute;todo utilizado por la firma del &eacute;rbol para obtener la
     * contrafirma de los signerInfo de forma recursiva.<br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param cert
     *        Certificado de firma.
     * @param keyEntry
     *        Clave privada a usar para firmar.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private SignerInfo getCounterUnsignedAtributes(final SignerInfo signerInfo,
                                                   final P7ContentSignerParameters parameters,
                                                   final X509Certificate cert,
                                                   final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                            IOException,
                                                                            CertificateException,
                                                                            AOException {
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
                        final SignerInfo si = new SignerInfo(atrib);

                        final SignerInfo obtained = getCounterUnsignedAtributes(si, parameters, cert, keyEntry);
                        signerInfosU.add(obtained);

                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DEL NODO ACTUAL
            CounterSigner = UnsignedAtributte(parameters, cert, signerInfo, keyEntry);
            signerInfosU.add(CounterSigner);

            // FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        ContexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                CounterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       a1 // unsignedAttr
                        );

            }
            else {
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        ContexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.
                        signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                        final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
                        ContexExpecific.add(uAtrib);

                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                    CounterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           a1 // unsignedAttr
                            );
                }
                else {
                    // Esta sentencia se comenta para que no se firme el nodo
                    // actual cuando no sea hoja
                    // signerInfosU.add(UnsignedAtributte(parameters, cert,
                    // signerInfo, keyEntry));
                    final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                    CounterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           new DERSet(uAtrib) // unsignedAttr
                            );
                }
            }

        }
        else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            CounterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   new DERSet(uAtrib) // unsignedAttr
                    );

        }
        return CounterSigner;
    }

    /** M&eacute;todo utilizado por la firma de una hoja del &eacute;rbol para
     * obtener la contrafirma de los signerInfo de una determinada hoja de forma
     * recursiva.</br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param cert
     *        Certificado de firma.
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private SignerInfo getCounterLeafUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final X509Certificate cert,
                                                       final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                                IOException,
                                                                                CertificateException,
                                                                                AOException {

        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
                        final SignerInfo si = new SignerInfo(atrib);

                        final SignerInfo obtained = getCounterLeafUnsignedAtributes(si, parameters, cert, keyEntry);
                        signerInfosU.add(obtained);
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        ContexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                CounterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       a1 // unsignedAttr
                        );

            }
            else {
                final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                CounterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       new DERSet(uAtrib) // unsignedAttr
                        );

            }
        }
        else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            CounterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   new DERSet(uAtrib) // unsignedAttr
                    );

        }
        return CounterSigner;
    }

    /** M&eacute;todo utilizado por la firma de un nodo del &eacute;rbol para
     * obtener la contrafirma de los signerInfo Sin ser recursivo. Esto es por
     * el caso especial de que puede ser el nodo raiz el nodo a firmar, por lo
     * que no ser&iacute;a necesario usar la recursividad.</br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param cert
     *        Certificado de firma.
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException */
    private SignerInfo getCounterNodeUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final X509Certificate cert,
                                                       final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException, IOException, CertificateException {

        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
                        final SignerInfo si = new SignerInfo(atrib);
                        signerInfosU.add(si);
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DEL NODO ACTUAL
            signerInfosU.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));

            // FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        ContexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                CounterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       a1 // unsignedAttr
                        );

            }
            else {
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        ContexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.
                        signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                        final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
                        ContexExpecific.add(uAtrib);

                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                    CounterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           a1 // unsignedAttr
                            );
                }
                else {
                    // Esta sentencia se comenta para que no se firme el nodo
                    // actual cuando no sea hoja
                    // signerInfosU.add(UnsignedAtributte(parameters, cert,
                    // signerInfo, keyEntry));
                    final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                    CounterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           new DERSet(uAtrib) // unsignedAttr
                            );
                }
            }
        }
        else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            CounterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   new DERSet(uAtrib) // unsignedAttr
                    );
        }
        return CounterSigner;
    }

    /** M&eacute;todo utilizado por la firma de un nodo del &eacute;rbol para
     * obtener la contrafirma de los signerInfo buscando el nodo de forma
     * recursiva.</br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param cert
     *        Certificado de firma.
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param node
     *        Nodo espec&iacute;fico a firmar.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private SignerInfo getCounterNodeUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final X509Certificate cert,
                                                       final PrivateKeyEntry keyEntry,
                                                       final int node) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        SignerInfo CounterSigner2 = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
                        final SignerInfo si = new SignerInfo(atrib);
                        actualIndex++;
                        if (actualIndex != node) {
                            if (actualIndex < node) {
                                CounterSigner2 = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry, node);
                                signerInfosU.add(CounterSigner2);
                            }
                            else {
                                signerInfosU.add(si);
                            }
                        }
                        else {
                            final SignerInfo obtained = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry);
                            signerInfosU.add(obtained);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        ContexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                CounterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       a1 // unsignedAttr
                        );

            }
            else {
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        ContexExpecific.add(signerInfosU.get(0));

                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                    CounterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           a1 // unsignedAttr
                            );
                }
                else {
                    // Esta sentencia se comenta para que no se firme el nodo
                    // actual cuando no sea hoja
                    // signerInfosU.add(UnsignedAtributte(parameters, cert,
                    // signerInfo, keyEntry));
                    final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                    CounterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           new DERSet(uAtrib) // unsignedAttr
                            );
                }
            }
        }
        else {
            CounterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   null // unsignedAttr
                    );

        }
        return CounterSigner;
    }

    /** M&eacute;todo que genera un signerInfo espec&iacute;fico utilizando los
     * datos necesarios para crearlo. Se utiliza siempre que no se sabe cual es
     * el signerInfo que se debe firmar.</br>
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param cert
     *        Certificado de firma.
     * @param si
     *        SignerInfo del que se debe recoger la informaci&oacute;n para
     *        realizar la contrafirma espec&iacute;fica.
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @return El signerInfo contrafirmado.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException */
    private SignerInfo UnsignedAtributte(final P7ContentSignerParameters parameters, final X509Certificate cert, final SignerInfo si, final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                                                                                             IOException,
                                                                                                                                             CertificateException {
        // // UNAUTHENTICATEDATTRIBUTES
        ASN1Set unsignedAttr = null;

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

        // ATRIBUTOS FINALES
        final String politica = getGlobalPolicy();
        final boolean signingCertificateV2 = isGlobalsigningCertificateV2();
        final Oid qualifier = getGlobalOidQualifier();

        final ASN1EncodableVector contextExcepcific =
                Utils.generateSignerInfo(cert,
                                         digestAlgorithmId,
                                         digestAlgorithm,
                                         digAlgId,
                                         si.getEncryptedDigest().getOctets(),
                                         politica,
                                         qualifier,
                                         signingCertificateV2,
                                         null,
                                         null);
        signedAttr2 = getAttributeSet(new AttributeTable(contextExcepcific));
        unsignedAttr = getAttributeSet(new AttributeTable(contextExcepcific));

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // AlgorithmIdentifier
        digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;

        encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());

        // Firma del SignerInfo
        // ByteArrayInputStream signerToDigest = new
        // ByteArrayInputStream(si.getEncryptedDigest().getOctets());
        // byte[] signedInfo = signData(signerToDigest, signatureAlgorithm,
        // keyEntry);

        ASN1OctetString sign2 = null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final AOException ex) {
            Logger.getLogger(CadesCounterSignerEnveloped.class.getName()).log(Level.SEVERE, null, ex);
        }

        final SignerInfo uAtrib = new SignerInfo(identifier, digAlgId, unsignedAttr, encAlgId, sign2, null);

        return uAtrib;

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
            Logger.getLogger(CadesCounterSignerEnveloped.class.getName()).log(Level.SEVERE, null, ex);
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
