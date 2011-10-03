/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cms;

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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BERSet;
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

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que implementa la contrafirma digital PKCS#7/CMS signedAndEnvelopedData
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje SignedData pero con la peculiaridad de que es una
 * Contrafirma. */
final class CounterSignerEnveloped {

    private int actualIndex = 0;
    private ASN1Set signedAttr2;
    private Map<String, byte[]> atrib2 = new HashMap<String, byte[]>();
    private Map<String, byte[]> uatrib2 = new HashMap<String, byte[]>();

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
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param atri
     *        Atributo firmado que agregar a la firma.
     * @param uatri
     *        Atributo no firmado que agregar a la firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws java.security.cert.CertificateException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws AOException
     *         Cuando ocurre cualquier error no contemplado por el resto de
     *         las excepciones declaradas
     * @throws es.gob.afirma.exceptions.AOException
     *         Cuando ocurre un error durante el proceso de contrafirma
     *         (formato o clave incorrecto,...) */
    byte[] counterSignerEnveloped(final P7ContentSignerParameters parameters,
                                         final byte[] data,
                                         final CounterSignTarget targetType,
                                         final int[] targets,
                                         final PrivateKeyEntry keyEntry,
                                         final String dataType,
                                         final Map<String, byte[]> atri,
                                         final Map<String, byte[]> uatri) throws IOException, NoSuchAlgorithmException, CertificateException, AOException {

        // Inicializamos el Oid
        this.atrib2 = atri;
        this.uatrib2 = uatri;

        final ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        final Enumeration<?> e = ((ASN1Sequence) is.readObject()).getObjects();
        // Elementos que contienen los elementos OID signedAndEnvelopedData
        e.nextElement();
        // Contenido de signedAndEnvelopedData
        final ASN1Sequence contentSignedData = (ASN1Sequence) ((ASN1TaggedObject) e.nextElement()).getObject();

        final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        // Obtenemos los signerInfos del signedAndEnvelopedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

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
        // if (signerCertificateChain.length != 0) {
        // List<DEREncodable> ce = new ArrayList<DEREncodable>();
        // for (int i = 0; i < signerCertificateChain.length; i++) {
        // ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
        // }
        // certificates = FillRestCerts(ce, vCertsSig);
        // }
        if (signerCertificateChain.length != 0) {
            vCertsSig.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getEncoded())));
            certificates = new BERSet(vCertsSig);
        }

        // CRLS no usado
        final ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        // FIRMA EN ARBOL
        if (targetType.equals(CounterSignTarget.Tree)) {
            signerInfos = CounterTree(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        } // FIRMA DE LAS HOJAS
        else if (targetType.equals(CounterSignTarget.Leafs)) {
            signerInfos = CounterLeaf(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        } // FIRMA DE NODOS
        else if (targetType.equals(CounterSignTarget.Nodes)) {
            // Firma de Nodos
            SignedAndEnvelopedData sigDat;
            SignedAndEnvelopedData aux = sd;

            // int carry = 0;
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

                // Esto se realiza asi por problemas con los casting.
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) new ASN1InputStream(sigDat.getDEREncoded()).readObject(); // contenido del signedAndEnvelopedData
                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, aux).getDEREncoded();
        }
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
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del signedAndEnvelopedData

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

    /*
     * new SignedAndEnvelopedData( new DERSet(recipientInfos), new
     * DERSet(digestAlgs), encInfo, certificates, certrevlist, new
     * DERSet(signerInfos) )
     */

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
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.cert.CertificateException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws es.gob.afirma.exceptions.AOException
     *         Cuando ocurre un error durante el proceso de contrafirma
     *         (formato o clave incorrecto,...) */
    private ASN1EncodableVector CounterTree(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final X509Certificate cert,
                                            final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {
        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            counterSigners.add(getCounterUnsignedAtributes(new SignerInfo((ASN1Sequence) signerInfosRaiz.getObjectAt(i)), parameters, cert, keyEntry));
        }
        return counterSigners;
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

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            counterSigners.add(getCounterLeafUnsignedAtributes(new SignerInfo((ASN1Sequence) signerInfosRaiz.getObjectAt(i)), parameters, cert, keyEntry));
        }

        return counterSigners;
    }

    /** M&eacute;todo que contrafirma un nodo determinado del arbol buscandolo de
     * forma recursiva.<br>
     * @param sd
     *        signedAndEnvelopedData que contiene el Nodo ra&iacute;z.
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

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        final ASN1Set auxSignerRaiz = signerInfosRaiz;
        this.actualIndex = 0;

        for (int i = 0; i < auxSignerRaiz.size(); i++) {
            final SignerInfo si = new SignerInfo((ASN1Sequence) auxSignerRaiz.getObjectAt(i));
            SignerInfo counterSigner = null;
            if (this.actualIndex == nodo) {
                counterSigner = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry);
            }
            else {
                if (this.actualIndex != nodo) {
                    counterSigner = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry, nodo);
                }
            }
            this.actualIndex++;
            counterSigners.add(counterSigner);
        }

        return counterSigners;

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

        final List<Object> Atributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final ASN1Sequence atrib = (ASN1Sequence) obj;
                            final SignerInfo si = new SignerInfo(atrib);
                            final SignerInfo obtained = getCounterUnsignedAtributes(si, parameters, cert, keyEntry);
                            signerInfosU.add(obtained);
                        }
                        else {
                            Atributes.add(obj);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }
            }
            // FIRMA DEL NODO ACTUAL
            counterSigner = UnsignedAtributte(parameters, cert, signerInfo, keyEntry);
            signerInfosU.add(counterSigner);

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
                a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
                counterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       a1 // unsignedAttr
                        );

                // introducido este else pero es sospechoso que no estuviera
                // antes de este ultimo cambio.
            }
            else {
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        ContexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.
                        signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2)));

                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
                    counterSigner =
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
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
                            );
                }
            }
        }
        else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            counterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
                    );

        }
        return counterSigner;
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

        final List<Object> Atributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();

                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            signerInfosU.add(getCounterLeafUnsignedAtributes(new SignerInfo((ASN1Sequence) obj), parameters, cert, keyEntry));
                        }
                        else {
                            Atributes.add(obj);
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
                a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
                counterSigner =
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
                    a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
                    counterSigner =
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
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
                            );
                }

            }
        }
        else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            counterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   new DERSet(uAtrib) // unsignedAttr
                    );
        }
        return counterSigner;
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

        final List<Object> Atributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            signerInfosU.add(new SignerInfo((ASN1Sequence) obj));
                        }
                        else {
                            Atributes.add(obj);
                        }
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
                a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
                counterSigner =
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
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2)));

                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
                    counterSigner =
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
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
                            );
                }

            }
        }
        else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            counterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   new DERSet(uAtrib) // unsignedAttr
                    );
        }
        return counterSigner;
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

        final List<Object> Atributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        SignerInfo counterSigner2 = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final ASN1Sequence atrib = (ASN1Sequence) obj;
                            final SignerInfo si = new SignerInfo(atrib);
                            this.actualIndex++;
                            if (this.actualIndex != node) {
                                if (this.actualIndex < node) {
                                    counterSigner2 = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry, node);
                                    signerInfosU.add(counterSigner2);
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
                        else {
                            Atributes.add(obj);
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
                a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
                counterSigner =
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

                    }
                    else {
                        ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           a1 // unsignedAttr
                            );
                }

            }
        }
        else {

            counterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   null // unsignedAttr
                    );

        }
        return counterSigner;
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la
     * firma.</br>
     * @param cert
     *        Certificado necesario para la firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param datos
     *        Datos firmados.
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException */
    private ASN1Set generateSignerInfo(final X509Certificate cert, String digestAlgorithm, final byte[] datos) throws NoSuchAlgorithmException {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // Fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // MessageDigest
        ContexExpecific.add(
                new Attribute(
                        CMSAttributes.messageDigest,
                        new DERSet(new DEROctetString(
                                MessageDigest.getInstance(
                                        AOSignConstants.getDigestAlgorithmName(digestAlgorithm)).digest(datos))))
        );

        // Serial Number
        ContexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        // agregamos la lista de atributos a mayores.
        if (this.atrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.atrib2.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                ContexExpecific.add(new Attribute(
                        // el oid
                        new DERObjectIdentifier((e.getKey()).toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(e.getValue()))));
            }
        }

        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));

        return SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @return Los atributos no firmados de la firma. */
    private ASN1Set generateUnsignerInfo() {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (this.uatrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.uatrib2.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
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

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uAtrib
     *        Lista de atributos no firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos no firmados de la firma. */
    private ASN1Set generateUnsignerInfoFromCounter(final Attribute uAtrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (this.uatrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.uatrib2.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                ContexExpecific.add(new Attribute(
                // el oid
                                                  new DERObjectIdentifier((e.getKey()).toString()),
                                                  // el array de bytes en formato string
                                                  new DERSet(new DERPrintableString(e.getValue()))));
            }
        }
        ContexExpecific.add(uAtrib);

        return SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));

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
        ASN1Set signedAttr = null;

        // buscamos que timo de algoritmo es y lo codificamos con su OID

        
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // ATRIBUTOS FINALES
        signedAttr = generateSignerInfo(cert, digestAlgorithm, si.getEncryptedDigest().getOctets());
        unsignedAttr = generateUnsignerInfo();

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // Firma del SignerInfo
        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final AOException ex) {
            throw new IOException("Error en la firma electronica: " + ex); //$NON-NLS-1$
        }

        return new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unsignedAttr);

    }

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param keyEntry
     *        Clave para firmar.
     * @return Firma de los atributos.
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1OctetString firma(final String signatureAlgorithm, final PrivateKeyEntry keyEntry) throws AOException {

        final Signature sig;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        byte[] tmp;
        try {
            tmp = this.signedAttr2.getEncoded(ASN1Encodable.DER);
        }
        catch (final IOException ex) {
            throw new AOException("Error obteniendo los atributos firmados", ex); //$NON-NLS-1$
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
        final byte[] realSig;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

        return new DEROctetString(realSig);

    }

}
