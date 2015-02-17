/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateEncodingException;
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
import org.bouncycastle.asn1.ASN1Encoding;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BERSet;
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
import org.bouncycastle.asn1.x509.Certificate;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
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

    private static ASN1Set getCertificates(final SignedAndEnvelopedData sd, final X509Certificate[] signerCertificateChain) throws CertificateEncodingException, IOException {
        ASN1Set certificates = null;

        final ASN1Set certificatesSigned = sd.getCertificates();
        final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        final Enumeration<?> certs = certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()) {
            vCertsSig.add((ASN1Encodable) certs.nextElement());
        }
        if (signerCertificateChain.length != 0) {
            vCertsSig.add(Certificate.getInstance(ASN1Primitive.fromByteArray(signerCertificateChain[0].getEncoded())));
            certificates = new BERSet(vCertsSig);
        }

        return certificates;
    }

    private static SignedAndEnvelopedData readData(final byte[] data) throws IOException {
    	final ASN1InputStream is = new ASN1InputStream(data);
        final Enumeration<?> e = ((ASN1Sequence) is.readObject()).getObjects();
        is.close();

        // Elementos que contienen los elementos OID signedAndEnvelopedData
        e.nextElement();

        // Contenido de signedAndEnvelopedData
        final ASN1Sequence contentSignedData = (ASN1Sequence) ((ASN1TaggedObject) e.nextElement()).getObject();

        return new SignedAndEnvelopedData(contentSignedData);
    }

    /** Constructor de la clase. Se crea una contrafirma a partir de los datos
     * del firmante, el archivo que se firma y del archivo que contiene las
     * firmas.<br>
     * @param parameters par&aacute;metros necesarios que contienen tanto la firma del
     *                   archivo a firmar como los datos del firmante.
     * @param signerCertificateChain Cadena de certificados del firmante.
     * @param data Archivo que contiene las firmas.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
     * @param targets Nodos objetivos a firmar.
     * @param keyEntry Clave privada a usar para firmar.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param atri Atributo firmado que agregar a la firma.
     * @param uatri Atributo no firmado que agregar a la firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *                             datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
     *                                                digital.
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de
     *                                                 firma.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    byte[] counterSignerEnveloped(final P7ContentSignerParameters parameters,
    		                      final X509Certificate[] signerCertificateChain,
                                  final byte[] data,
                                  final CounterSignTarget targetType,
                                  final int[] targets,
                                  final PrivateKeyEntry keyEntry,
                                  final String dataType,
                                  final Map<String, byte[]> atri,
                                  final Map<String, byte[]> uatri) throws IOException,
                                                                                 NoSuchAlgorithmException,
                                                                                 CertificateException,
                                                                                 InvalidKeyException,
                                                                                 SignatureException {
        // Inicializamos el Oid
        this.atrib2 = atri;
        this.uatrib2 = uatri;

        final SignedAndEnvelopedData sd = readData(data);

        // Obtenemos los signerInfos del signedAndEnvelopedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados
        final ASN1Set certificates = getCertificates(sd, signerCertificateChain);

        // CRLS no usado
        final ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        // FIRMA EN ARBOL
        if (targetType.equals(CounterSignTarget.TREE)) {
            signerInfos = counterTree(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        } // FIRMA DE LAS HOJAS
        else if (targetType.equals(CounterSignTarget.LEAFS)) {
            signerInfos = counterLeaf(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        } // FIRMA DE NODOS
        else if (targetType.equals(CounterSignTarget.NODES)) {
            // Firma de Nodos
            SignedAndEnvelopedData sigDat;
            SignedAndEnvelopedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = counterNode(aux, parameters, signerCertificateChain[0], keyEntry, nodo);
                sigDat = new SignedAndEnvelopedData(
            		sd.getRecipientInfos(),
                    sd.getDigestAlgorithms(),
                    sd.getEncryptedContentInfo(),
                    certificates,
                    certrevlist,
                    new DERSet(signerInfos)
        		);

                // Esto se realiza asi por problemas con los casting.
                final ASN1InputStream asnIs = new ASN1InputStream(sigDat.getEncoded(ASN1Encoding.DER));
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) asnIs.readObject(); // contenido del signedAndEnvelopedData
                asnIs.close();
                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, aux).getEncoded(ASN1Encoding.DER);
        }
        else if (targetType.equals(CounterSignTarget.SIGNERS)) {
            // Firma de Nodos
            SignedAndEnvelopedData sigDat;
            SignedAndEnvelopedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = counterNode(aux, parameters, signerCertificateChain[0], keyEntry, nodo);
                sigDat = new SignedAndEnvelopedData(
            		sd.getRecipientInfos(),
                    sd.getDigestAlgorithms(),
                    sd.getEncryptedContentInfo(),
                    certificates,
                    certrevlist,
                    new DERSet(signerInfos)
        		);

                // Esto se realiza as&iacute; por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getEncoded(ASN1Encoding.DER));
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del signedAndEnvelopedData
                sd2.close();

                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, aux).getEncoded(ASN1Encoding.DER);
        }

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
    		PKCSObjectIdentifiers.signedAndEnvelopedData,
    		new SignedAndEnvelopedData(
    			sd.getRecipientInfos(),
    			sd.getDigestAlgorithms(),
    			sd.getEncryptedContentInfo(),
                certificates,
                certrevlist,
                new DERSet(signerInfos)
    		)
    	).getEncoded(ASN1Encoding.DER);

    }

    /** M&eacute;todo que contrafirma el arbol completo de forma recursiva, todos
     * los dodos creando un nuevo contraSigner.<br>
     * @param signerInfosRaiz Nodo ra&iacute; que contiene todos los signerInfos que se
     *                        deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   SignerInfo
     * @param cert Certificado de firma.
     * @param keyEntry Clave privada a usar para firmar
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
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    private ASN1EncodableVector counterTree(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final X509Certificate cert,
                                            final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                                   IOException,
                                                                                   CertificateException,
                                                                                   InvalidKeyException,
                                                                                   SignatureException {
        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            counterSigners.add(getCounterUnsignedAtributes(SignerInfo.getInstance(signerInfosRaiz.getObjectAt(i)), parameters, cert, keyEntry));
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
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws java.io.IOException Cuando hay problemas de entrada / salida.
     * @throws java.security.cert.CertificateException Cuando hay problemas relacionados con los certificados X.509.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    private ASN1EncodableVector counterLeaf(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final X509Certificate cert,
                                            final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                                   IOException,
                                                                                   CertificateException,
                                                                                   InvalidKeyException,
                                                                                   SignatureException {

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            counterSigners.add(
        		getCounterLeafUnsignedAtributes(
    				SignerInfo.getInstance(
						signerInfosRaiz.getObjectAt(i)
					),
    				parameters,
    				cert,
    				keyEntry
				)
    		);
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
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws java.io.IOException Cuando hay problemas de entrada / salida.
     * @throws java.security.cert.CertificateException Cuando hay problemas relacionados con los certificados X.509.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    private ASN1EncodableVector counterNode(final SignedAndEnvelopedData sd,
                                            final P7ContentSignerParameters parameters,
                                            final X509Certificate cert,
                                            final PrivateKeyEntry keyEntry,
                                            final int nodo) throws NoSuchAlgorithmException,
                                                                   IOException,
                                                                   CertificateException,
                                                                   InvalidKeyException,
                                                                   SignatureException {

        final ASN1Set signerInfosRaiz = sd.getSignerInfos();

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        final ASN1Set auxSignerRaiz = signerInfosRaiz;
        this.actualIndex = 0;

        for (int i = 0; i < auxSignerRaiz.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(auxSignerRaiz.getObjectAt(i));
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
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos que se
     *                   deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code> hoja.
     * @param cert Certificado de firma.
     * @param keyEntry Clave privada a usar para firmar.
     * @return El <code>SignerInfo</code> ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws java.io.IOException Cuando hay problemas de entrada / salida.
     * @throws CertificateException Caundo hay problemas relacionados con los certificados X.509.
     * @throws InvalidKeyException Cuando la clave proporcionada no es v&aacute;lida.
     * @throws SignatureException Cuando ocurren problando hay problemas de adecuaci&oacute;n de la clave. */
    private SignerInfo getCounterUnsignedAtributes(final SignerInfo signerInfo,
                                                   final P7ContentSignerParameters parameters,
                                                   final X509Certificate cert,
                                                   final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                                          IOException,
                                                                                          CertificateException,
                                                                                          InvalidKeyException,
                                                                                          SignatureException {
        final List<Object> attributes = new ArrayList<Object>();
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
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final ASN1Sequence atrib = (ASN1Sequence) obj;
                            final SignerInfo si = SignerInfo.getInstance(atrib);
                            final SignerInfo obtained = getCounterUnsignedAtributes(si, parameters, cert, keyEntry);
                            signerInfosU.add(obtained);
                        }
                        else {
                            attributes.add(obj);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }
            }
            // FIRMA DEL NODO ACTUAL
            counterSigner = unsignedAtributte(parameters, cert, signerInfo, keyEntry);
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

                // introducido este else pero es sospechoso que no estuviera
                // antes de este ultimo cambio.
            }
            else {
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        contexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.
                        signerInfosU2.add(unsignedAtributte(parameters, cert, signerInfo, keyEntry));
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
                                           generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
                            );
                }
            }
        }
        else {
            signerInfosU2.add(unsignedAtributte(parameters, cert, signerInfo, keyEntry));
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
     * recursiva.
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
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws java.io.IOException Cuando hay problemas de entrada / salida.
     * @throws java.security.cert.CertificateException Cuando hay problemas relacionados con los certificados X.509.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    private SignerInfo getCounterLeafUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final X509Certificate cert,
                                                       final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                                              IOException,
                                                                                              CertificateException,
                                                                                              InvalidKeyException,
                                                                                              SignatureException {

        final List<Object> attributes = new ArrayList<Object>();
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
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            signerInfosU.add(getCounterLeafUnsignedAtributes(SignerInfo.getInstance(obj), parameters, cert, keyEntry));
                        }
                        else {
                            attributes.add(obj);
                        }
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
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        contexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.
                        signerInfosU2.add(unsignedAtributte(parameters, cert, signerInfo, keyEntry));
                        final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
                        contexExpecific.add(uAtrib);

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
                                           generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
                            );
                }

            }
        }
        else {
            signerInfosU2.add(unsignedAtributte(parameters, cert, signerInfo, keyEntry));
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
     * que no ser&iacute;a necesario usar la recursividad.
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
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws java.io.IOException Cuando hay problemas de entrada / salida.
     * @throws java.security.cert.CertificateException Cuando hay problemas relacionados con los certificados X.509.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    private SignerInfo getCounterNodeUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final X509Certificate cert,
                                                       final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                                              IOException,
                                                                                              CertificateException,
                                                                                              InvalidKeyException,
                                                                                              SignatureException {

        final List<Object> attributes = new ArrayList<Object>();
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
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            signerInfosU.add(SignerInfo.getInstance(obj));
                        }
                        else {
                            attributes.add(obj);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }
            }
            // FIRMA DEL NODO ACTUAL
            signerInfosU.add(unsignedAtributte(parameters, cert, signerInfo, keyEntry));

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
                        signerInfosU2.add(unsignedAtributte(parameters, cert, signerInfo, keyEntry));
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
                                           generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
                            );
                }

            }
        }
        else {
            signerInfosU2.add(unsignedAtributte(parameters, cert, signerInfo, keyEntry));
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
     * recursiva.
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
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws java.io.IOException Cuando hay problemas de entrada / salida.
     * @throws java.security.cert.CertificateException Cuando hay problemas relacionados con los certificados X.509.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    private SignerInfo getCounterNodeUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final X509Certificate cert,
                                                       final PrivateKeyEntry keyEntry,
                                                       final int node) throws NoSuchAlgorithmException,
                                                                              IOException,
                                                                              CertificateException,
                                                                              InvalidKeyException,
                                                                              SignatureException {

        final List<Object> attributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        SignerInfo counterSigner2 = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final ASN1Sequence atrib = (ASN1Sequence) obj;
                            final SignerInfo si = SignerInfo.getInstance(atrib);
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
                            attributes.add(obj);
                        }
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
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        contexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.

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
     * firma.
     * @param cert
     *        Certificado necesario para la firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param datos
     *        Datos firmados.
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario. */
    private ASN1Set generateSignerInfo(final X509Certificate cert, final String digestAlgorithm, final byte[] datos) throws NoSuchAlgorithmException {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // Fecha de firma
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // MessageDigest
        contexExpecific.add(
                new Attribute(
                        CMSAttributes.messageDigest,
                        new DERSet(new DEROctetString(
                                MessageDigest.getInstance(
                                        AOSignConstants.getDigestAlgorithmName(digestAlgorithm)).digest(datos))))
        );

        // Serial Number
        contexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        // agregamos la lista de atributos a mayores.
        if (this.atrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.atrib2.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
                        // el oid
                        new ASN1ObjectIdentifier(e.getKey().toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(new String(e.getValue()))))
                );
            }
        }

        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @return Los atributos no firmados de la firma. */
    private ASN1Set generateUnsignerInfo() {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (this.uatrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.uatrib2.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
            		// el oid
                    new ASN1ObjectIdentifier(e.getKey().toString()),
                    // el array de bytes en formato string
                    new DERSet(new DERPrintableString(new String(e.getValue()))))
                );
            }
        }
        else {
            return null;
        }

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

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
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (this.uatrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.uatrib2.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
            		// el oid
                    new ASN1ObjectIdentifier(e.getKey().toString()),
                    // el array de bytes en formato string
                    new DERSet(new DERPrintableString(new String(e.getValue()))))
                );
            }
        }
        contexExpecific.add(uAtrib);

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }

    /** M&eacute;todo que genera un signerInfo espec&iacute;fico utilizando los
     * datos necesarios para crearlo. Se utiliza siempre que no se sabe cual es
     * el signerInfo que se debe firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param cert Certificado de firma.
     * @param si SignerInfo del que se debe recoger la informaci&oacute;n para
     *           realizar la contrafirma espec&iacute;fica.
     * @param keyEntry Clave privada a usar para firmar
     * @return El signerInfo contrafirmado.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws java.io.IOException Cuando hay errores de entrada / salida.
     * @throws java.security.cert.CertificateException Cuando hay problemas relacionados con los certificados X.509.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    private SignerInfo unsignedAtributte(final P7ContentSignerParameters parameters,
    		                             final X509Certificate cert,
    		                             final SignerInfo si,
    		                             final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
                                                                                                                                             IOException,
                                                                                                                                             CertificateException, InvalidKeyException, SignatureException {
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
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(cert.getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // Firma del SignerInfo
        final ASN1OctetString sign2 = firma(signatureAlgorithm, keyEntry);

        return new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unsignedAttr);

    }

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param keyEntry
     *        Clave para firmar.
     * @return Firma de los atributos.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws IOException Cuando hay problemas de entrada / salida.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1. */
    private ASN1OctetString firma(final String signatureAlgorithm,
    		                      final PrivateKeyEntry keyEntry) throws NoSuchAlgorithmException,
    		                                                             IOException,
    		                                                             InvalidKeyException,
    		                                                             SignatureException {

        final Signature sig = Signature.getInstance(signatureAlgorithm);

        // Indicar clave privada para la firma
        sig.initSign(keyEntry.getPrivateKey());

        // Actualizamos la configuracion de firma
        sig.update(this.signedAttr2.getEncoded(ASN1Encoding.DER));

        // firmamos y devolvemos
        return new DEROctetString(sig.sign());

    }

}
