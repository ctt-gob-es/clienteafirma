/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Encoding;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
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
import org.bouncycastle.asn1.x509.Certificate;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que implementa la contrafirma digital CADES SignedAndEnvelopedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedAndEnvelopedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> pero con la
 * peculiaridad de que es una Contrafirma. */
final class CAdESCounterSignerEnveloped {

    /* Propiedades de la clase */
    private int actualIndex = 0;

    private ASN1Set signedAttr2;

    private AdESPolicy globalPolicy;
    private boolean globalSigningCertificateV2;

    /** Establece la pol&iacute;tica de firma. */
    private void setGlobalPolicy(final AdESPolicy pol) {
        this.globalPolicy = pol;
    }

    private AdESPolicy getGlobalPolicy() {
        return this.globalPolicy;
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
     * @param key Clave privada a usar para firmar.
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
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
                                final PrivateKey key,
                                final java.security.cert.Certificate[] certChain,
                                final AdESPolicy policy,
                                final boolean signingCertificateV2,
                                final String contentType,
                                final String contentDescription) throws IOException, NoSuchAlgorithmException, CertificateException, AOException {

        // Introducimos la politica en variable global por comodidad.
        // Esta no varia.
        this.setGlobalPolicy(policy);
        this.setGlobalsigningCertificateV2(signingCertificateV2);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        final ASN1InputStream is = new ASN1InputStream(data);
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        is.close();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedAndEnvelopedData
        e.nextElement();
        // Contenido de SignedAndEnvelopedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();

        final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        // Obtenemos los signerInfos del SignedAndEnvelopedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;

        final ASN1Set certificatesSigned = sd.getCertificates();
        final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        final Enumeration<?> certs = certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()) {
            vCertsSig.add((ASN1Encodable) certs.nextElement());
        }
        // e introducimos los del firmante actual.
        if (certChain.length != 0) {
            final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
            for (final java.security.cert.Certificate element : certChain) {
                ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.fillRestCerts(ce, vCertsSig);
        }

        // CRLS no usado
        final ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        // FIRMA EN ARBOL
        if (targetType.equals(CounterSignTarget.TREE)) {
            signerInfos = counterTree(
        		signerInfosSd,
        		parameters,
        		key,
        		certChain,
                contentType,
                contentDescription
            );
        }
        // FIRMA DE LAS HOJAS
        else if (targetType.equals(CounterSignTarget.LEAFS)) {
            signerInfos = counterLeaf(
        		signerInfosSd,
        		parameters,
        		key,
        		certChain,
                contentType,
                contentDescription
            );
        }
        // FIRMA DE NODOS
        else if (targetType.equals(CounterSignTarget.NODES)) {
            // Firma de Nodos
            SignedAndEnvelopedData sigDat;
            SignedAndEnvelopedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = counterNode(
            		aux,
            		parameters,
            		key,
            		certChain,
                    contentType,
                    contentDescription,
                    nodo
                );
                sigDat =
                        new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                   sd.getDigestAlgorithms(),
                                                   sd.getEncryptedContentInfo(),
                                                   certificates,
                                                   certrevlist,
                                                   new DERSet(signerInfos));

                // Esto se realiza as&iacute; por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.toASN1Primitive().getEncoded(ASN1Encoding.DER));
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedAndEnvelopedData
                sd2.close();
                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, aux).getEncoded(ASN1Encoding.DER);
        }
        // FIRMA DE LOS SIGNERS
        else if (targetType.equals(CounterSignTarget.SIGNERS)) {
            // Firma de Nodos
            SignedAndEnvelopedData sigDat;
            SignedAndEnvelopedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = counterNode(
            		aux,
            		parameters,
            		key,
            		certChain,
                    contentType,
                    contentDescription,
                    nodo
                );
                sigDat =
                        new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                   sd.getDigestAlgorithms(),
                                                   sd.getEncryptedContentInfo(),
                                                   certificates,
                                                   certrevlist,
                                                   new DERSet(signerInfos));

                // Esto se realiza as&iacute; por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getEncoded(ASN1Encoding.DER));
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedAndEnvelopedData
                sd2.close();

                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, aux).getEncoded(ASN1Encoding.DER);
        }

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(sd.getRecipientInfos(),
                                                                                                        sd.getDigestAlgorithms(),
                                                                                                        sd.getEncryptedContentInfo(),
                                                                                                        certificates,
                                                                                                        certrevlist,
                                                                                                        new DERSet(signerInfos))).getEncoded(ASN1Encoding.DER);

    }

    /** M&eacute;todo que contrafirma el arbol completo de forma recursiva, todos
     * los dodos creando un nuevo contraSigner.<br>
     * @param signerInfosRaiz
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo
     * @param keyEntry
     *        Clave privada a usar para firmar
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1EncodableVector counterTree(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentType,
                                            final String contentDescription) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();

        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(signerInfosRaiz.getObjectAt(i));
            counterSigners.add(
        		getCounterSignerInfo(
            		si,
            		parameters,
            		key,
            		certChain,
            		contentType,
            		contentDescription
        		)
    		);
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
     * @param key Clave privada a usar para firmar
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1EncodableVector counterLeaf(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentType,
                                            final String contentDescription) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();

        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(signerInfosRaiz.getObjectAt(i));
            counterSigners.add(
        		getCounterLeafSignerInfo(
            		si,
            		parameters,
            		key,
            		certChain,
            		contentType,
            		contentDescription
        		)
    		);
        }

        return counterSigners;
    }

    /** M&eacute;todo que contrafirma un nodo determinado del arbol buscandolo de
     * forma recursiva.<br>
     * @param sd
     *        SignedAndEnvelopedData que contiene el Nodo ra&iacute;z.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
     * @param nodo
     *        Nodo signerInfo a firmar.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1EncodableVector counterNode(final SignedAndEnvelopedData sd,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentType,
                                            final String contentDescription,
                                            final int nodo) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1Set signerInfosRaiz = sd.getSignerInfos();

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        ASN1Set auxSignerRaiz;

        auxSignerRaiz = signerInfosRaiz;
        this.actualIndex = 0;

        for (int i = 0; i < auxSignerRaiz.size(); i++) {
            final ASN1Sequence atribute = (ASN1Sequence) auxSignerRaiz.getObjectAt(i);
            final SignerInfo si = SignerInfo.getInstance(atribute);
            SignerInfo counterSigner = null;
            if (this.actualIndex == nodo) {
                counterSigner = getCounterNodeSignerInfo(
            		si,
            		parameters,
            		key,
            		certChain,
            		contentType,
            		contentDescription
        		);
            }
            else {
                if (this.actualIndex != nodo) {
                    counterSigner = getCounterNodeSignerInfo(
                		si,
                		parameters,
                		key,
                		certChain,
                        contentType,
                        contentDescription,
                        nodo
                    );
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
     * @param key Clave privada a usar para firmar.
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private SignerInfo getCounterSignerInfo(final SignerInfo signerInfo,
                                                   final P7ContentSignerParameters parameters,
                                                   final PrivateKey key,
                                                   final java.security.cert.Certificate[] certChain,
                                                   final String contentType,
                                                   final String contentDescription) throws NoSuchAlgorithmException,
                                                                            IOException,
                                                                            CertificateException,
                                                                            AOException {
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final SignerInfo si = SignerInfo.getInstance(eAtributesData.nextElement());
                        signerInfosU.add(
                    		getCounterSignerInfo(
                        		si,
                        		parameters,
                        		key,
                        		certChain,
                                contentType,
                                contentDescription
                            )
                        );
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DEL NODO ACTUAL
            counterSigner = generateSignerInfo(
        		parameters,
        		signerInfo,
        		key,
        		certChain,
                contentType,
                contentDescription
            );
            signerInfosU.add(counterSigner);

            // FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        contexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                a1 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
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
                        contexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.
                        signerInfosU2.add(
                    		generateSignerInfo(
                        		parameters,
                        		signerInfo,
                        		key,
                        		certChain,
                                contentType,
                                contentDescription
                            )
                        );
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2)));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
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
                    final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                    counterSigner =
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
            signerInfosU2.add(
        		generateSignerInfo(
            		parameters,
            		signerInfo,
            		key,
            		certChain,
            		contentType,
            		contentDescription
        		)
    		);
            counterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   new DERSet(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2))) // unsignedAttr
                    );

        }
        return counterSigner;
    }

    /** M&eacute;todo utilizado por la firma de una hoja del &eacute;rbol para
     * obtener la contrafirma de los signerInfo de una determinada hoja de forma
     * recursiva.<br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private SignerInfo getCounterLeafSignerInfo(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain,
                                                       final String contentType,
                                                       final String contentDescription) throws NoSuchAlgorithmException,
                                                                                IOException,
                                                                                CertificateException,
                                                                                AOException {

        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAttributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAttributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAttributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final SignerInfo si = SignerInfo.getInstance(eAtributesData.nextElement());
                        signerInfosU.add(
                    		getCounterLeafSignerInfo(
                        		si,
                        		parameters,
                        		key,
                        		certChain,
                        		contentType,
                        		contentDescription
                    		)
                		);
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        contexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                a1 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
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
                counterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       new DERSet(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU))) // unsignedAttr
                        );

            }
        }
        else {
            signerInfosU2.add(
        		generateSignerInfo(
            		parameters,
            		signerInfo,
            		key,
            		certChain,
            		contentType,
            		contentDescription
        		)
    		);
            counterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   new DERSet(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2))) // unsignedAttr
                    );

        }
        return counterSigner;
    }

    /** M&eacute;todo utilizado por la firma de un nodo del &eacute;rbol para
     * obtener la contrafirma de los signerInfo Sin ser recursivo. Esto es por
     * el caso especial de que puede ser el nodo raiz el nodo a firmar, por lo
     * que no ser&iacute;a necesario usar la recursividad.<br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException */
    private SignerInfo getCounterNodeSignerInfo(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain,
                                                       final String contentType,
                                                       final String contentDescription) throws NoSuchAlgorithmException,
                                                                                              IOException,
                                                                                              CertificateException {

        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAttributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAttributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAttributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        signerInfosU.add(SignerInfo.getInstance(eAtributesData.nextElement()));
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DEL NODO ACTUAL
            signerInfosU.add(generateSignerInfo(
        		parameters,
        		signerInfo,
        		key,
        		certChain,
        		contentType,
        		contentDescription)
    		);

            // FIRMA DE CADA UNO DE LOS HIJOS
            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        contexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                counterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
                        );

            }
            else {
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        contexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.
                        signerInfosU2.add(
                    		generateSignerInfo(
                        		parameters,
                        		signerInfo,
                        		key,
                        		certChain,
                        		contentType,
                        		contentDescription
                    		)
                		);
                        final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
                        contexExpecific.add(uAtrib);

                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
                            );
                }
                else {
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           new DERSet(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU))) // unsignedAttr
                            );
                }
            }
        }
        else {
            signerInfosU2.add(generateSignerInfo(
        		parameters,
        		signerInfo,
        		key,
        		certChain,
        		contentType,
        		contentDescription)
    		);
            counterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   new DERSet(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2))) // unsignedAttr
                    );
        }
        return counterSigner;
    }

    /** M&eacute;todo utilizado por la firma de un nodo del &eacute;rbol para
     * obtener la contrafirma de los signerInfo buscando el nodo de forma
     * recursiva.<br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
     * @param node
     *        Nodo espec&iacute;fico a firmar.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private SignerInfo getCounterNodeSignerInfo(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain,
                                                       final String contentType,
                                                       final String contentDescription,
                                                       final int node) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final Enumeration<?> eAtributesData = data.getAttrValues().getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
                        final SignerInfo si = SignerInfo.getInstance(atrib);
                        this.actualIndex++;
                        if (this.actualIndex != node) {
                            if (this.actualIndex < node) {
                                signerInfosU.add(
                            		getCounterNodeSignerInfo(
                                		si,
                                		parameters,
                                		key,
                                		certChain,
                                        contentType,
                                        contentDescription,
                                        node
                                    )
                                );
                            }
                            else {
                                signerInfosU.add(si);
                            }
                        }
                        else {
                            signerInfosU.add(
                        		getCounterNodeSignerInfo(
                            		si,
                            		parameters,
                            		key,
                            		certChain,
                                    contentType,
                                    contentDescription
                                )
                            );
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DE CADA UNO DE LOS HIJOS
            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        contexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                counterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
                        );

            }
            else {
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        contexExpecific.add(signerInfosU.get(0));

                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
                            );
                }
                else {
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           new DERSet(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU))) // unsignedAttr
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

    /** M&eacute;todo que genera un signerInfo espec&iacute;fico utilizando los
     * datos necesarios para crearlo. Se utiliza siempre que no se sabe cual es
     * el signerInfo que se debe firmar.</br>
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param si
     *        SignerInfo del que se debe recoger la informaci&oacute;n para
     *        realizar la contrafirma espec&iacute;fica.
     * @param key Clave privada a usar para firmar
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription
     * 		  Descripci&oacute;n textual del tipo de contenido firmado.
     * @return El signerInfo contrafirmado.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException */
    private SignerInfo generateSignerInfo(final P7ContentSignerParameters parameters,
                                         final SignerInfo si,
                                         final PrivateKey key,
                                         final java.security.cert.Certificate[] certChain,
                                         final String contentType,
                                         final String contentDescription) throws NoSuchAlgorithmException,
                                                                                                                                             IOException,
                                                                                                                                             CertificateException {
        // // UNAUTHENTICATEDATTRIBUTES

        // buscamos que timo de algoritmo es y lo codificamos con su OID

        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);

        final ASN1EncodableVector contextExcepcific =
            CAdESUtils.generateSignerInfo(
        		 (X509Certificate) certChain[0],
                 digestAlgorithm,
                 si.getEncryptedDigest().getOctets(),
                 getGlobalPolicy(),
                 isGlobalSigningCertificateV2(),
                 null,
                 new Date(),
                 false,
                 contentType,
                 contentDescription
            );
        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));

        final ASN1Set unsignedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(
    		ASN1Primitive.fromByteArray(
				((X509Certificate)certChain[0]).getTBSCertificate()
			)
		);
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // AlgorithmIdentifier
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // Firma del SignerInfo
        // ByteArrayInputStream signerToDigest = new
        // ByteArrayInputStream(si.getEncryptedDigest().getOctets());
        // byte[] signedInfo = signData(signerToDigest, signatureAlgorithm,
        // keyEntry);

        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, key, certChain);
        }
        catch (final AOException ex) {
            throw new IOException("Error al realizar la firma electronica: " + ex, ex); //$NON-NLS-1$
        }

        return  new SignerInfo(identifier, digAlgId, unsignedAttr, encAlgId, sign2, null);

    }

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param key Clave para firmar.
     * @return Firma de los atributos.
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1OctetString firma(final String signatureAlgorithm,
    		                      final PrivateKey key,
    		                      final java.security.cert.Certificate[] certChain) throws AOException {

        final byte[] tmp;
        try {
            tmp = this.signedAttr2.getEncoded(ASN1Encoding.DER);
        }
        catch (final Exception ex) {
            throw new AOException("Error obteniendo los atributos firmados: " + ex); //$NON-NLS-1$
        }

        return new DEROctetString(new AOPkcs1Signer().sign(tmp, signatureAlgorithm, key, certChain, null));
    }
}
