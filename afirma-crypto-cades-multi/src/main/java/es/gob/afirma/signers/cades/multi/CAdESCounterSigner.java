/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.cades.multi;


import java.io.IOException;
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
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
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

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.beans.AdESPolicy;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.cades.PKCS1ExternalizableSigner;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Clase que implementa la contrafirma digital CADES SignedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> pero con la
 * peculiaridad de que es una Contrafirma. */
final class CAdESCounterSigner {

    /* Propiedades de la clase */
    private int actualIndex = 0;
    private ASN1Set signedAttr2;

    private AdESPolicy globalPolicy = null;
    private boolean globalSigningCertificateV2;


    private AdESPolicy getGlobalPolicy() {
        return this.globalPolicy;
    }
    
    private void setGlobalPolicy(final AdESPolicy pol) {
        this.globalPolicy = pol;
    }

    /** Obtiene el tipo de atributo firmado signingCertificate o
     * signingCertificateV2
     * @return tipo de atributo firmado. */
    private boolean isGlobalSigningCertificateV2() {
        return this.globalSigningCertificateV2;
    }

    /** Define si el atributo firmado es signingCertificate o
     * signingCertificateV2
     * @param globalsigningCertificateV2
     *        tipo de atributo */
    private void setGlobalsigningCertificateV2(final boolean globalsigningCertificateV2) {
        this.globalSigningCertificateV2 = globalsigningCertificateV2;
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
     * @param policy Pol&iacute;tica de firma
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
    byte[] counterSigner(final P7ContentSignerParameters parameters,
                                final byte[] data,
                                final CounterSignTarget targetType,
                                final int[] targets,
                                final PrivateKeyEntry keyEntry,
                                final AdESPolicy policy,
                                final boolean signingCertificateV2) throws IOException, NoSuchAlgorithmException, CertificateException, AOException {

        // Introducimos la pol&iacute;tica en variable global por comodidad.
        // &Eacute;sta no var&iacute;a.
        this.setGlobalPolicy(policy);
        this.setGlobalsigningCertificateV2(signingCertificateV2);

        final ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        e.nextElement();
        // Contenido de SignedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();

        final SignedData sd = new SignedData(contentSignedData);

        // Obtenemos los signerInfos del SignedData
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
        if (signerCertificateChain.length != 0) {
            final List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.fillRestCerts(ce, vCertsSig);
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
            SignedData sigDat;
            SignedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = CounterNode(aux, parameters, signerCertificateChain[0], keyEntry, nodo);
                sigDat = new SignedData(sd.getDigestAlgorithms(), sd.getEncapContentInfo(), certificates, certrevlist, new DERSet(signerInfos));

                // Esto se realiza as&iacute; por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getDEREncoded());
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedData
                aux = new SignedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedData, aux).getDEREncoded();
        }
        // FIRMA DE LOS SIGNERS
        else if (targetType.equals(CounterSignTarget.Signers)) {
            // Firma de Nodos
            SignedData sigDat;
            SignedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = CounterNode(aux, parameters, signerCertificateChain[0], keyEntry, nodo);
                sigDat = new SignedData(sd.getDigestAlgorithms(), sd.getEncapContentInfo(), certificates, certrevlist, new DERSet(signerInfos));

                // Esto se realiza as&iacute; por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getDEREncoded());
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedData

                aux = new SignedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedData, aux).getDEREncoded();
        }

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedData, new SignedData(sd.getDigestAlgorithms(),
                                                                                sd.getEncapContentInfo(),
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
     *        SignedData que contiene el Nodo ra&iacute;z.
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
    private ASN1EncodableVector CounterNode(final SignedData sd,
                                            final P7ContentSignerParameters parameters,
                                            final X509Certificate cert,
                                            final PrivateKeyEntry keyEntry,
                                            final int nodo) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1Set signerInfosRaiz = sd.getSignerInfos();

        final ASN1EncodableVector CounterSigners = new ASN1EncodableVector();
        ASN1Set auxSignerRaiz;

        auxSignerRaiz = signerInfosRaiz;
        this.actualIndex = 0;

        for (int i = 0; i < auxSignerRaiz.size(); i++) {
            final ASN1Sequence atribute = (ASN1Sequence) auxSignerRaiz.getObjectAt(i);
            final SignerInfo si = new SignerInfo(atribute);
            SignerInfo CounterSigner = null;
            if (this.actualIndex == nodo) {
                CounterSigner = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry);
            }
            else {
                if (this.actualIndex != nodo) {
                    CounterSigner = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry, nodo);
                }
            }
            this.actualIndex++;
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
                a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
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
                    a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
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
                a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
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
                    a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
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
                a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
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
                    a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
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
                        this.actualIndex++;
                        if (this.actualIndex != node) {
                            if (this.actualIndex < node) {
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
                a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
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
                    a1 = SigUtils.getAttributeSet(new AttributeTable(ContexExpecific));
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

        
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);

        // authenticatedAttributes
        final ASN1EncodableVector contextExcepcific =
                CAdESUtils.generateSignerInfo(cert,
                                         digestAlgorithm,
                                         si.getEncryptedDigest().getOctets(),
                                         getGlobalPolicy(),
                                         isGlobalSigningCertificateV2(),
                                         null,
                                         new Date()
                );
        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));
        unsignedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // AlgorithmIdentifier
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // Firma del SignerInfo
        // ByteArrayInputStream signerToDigest = new
        // ByteArrayInputStream(si.getEncryptedDigest().getOctets());
        // byte[] signedInfo = signData(signerToDigest, signatureAlgorithm,
        // keyEntry);

        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final AOException ex) {
            throw new IOException("Error al realizar la firma: " + ex); //$NON-NLS-1$
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
