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
import java.util.logging.Logger;

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
import org.bouncycastle.asn1.cms.SignedData;
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
import es.gob.afirma.core.signers.AOSimpleSigner;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.CAdESSignerMetadata;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.cades.CommitmentTypeIndicationBean;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Clase que implementa la contrafirma digital CADES SignedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> pero con la
 * peculiaridad de que es una Contrafirma. */
final class CAdESCounterSigner {

    private int actualIndex = 0;
    private AOSimpleSigner ss = new AOPkcs1Signer();
    private Date date = null;

    /** Crea una contrafirma a partir de los datos
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
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El archivo de firmas con la nueva firma.
     * @throws IOException Cuando se produce algun error con la lectura o escritura de datos.
     * @throws NoSuchAlgorithmException Excepci&oacute;n cuando no se encuentra el algoritmo de
     *                                  firma.
     * @throws CertificateException Si se produce alguna excepci&oacute;n con los certificados de
     *                              firma.
     * @throws AOException Cuando ocurre alguno error con contemplado por las otras
     *                     excepciones declaradas */
    byte[] counterSigner(final P7ContentSignerParameters parameters,
                                final byte[] data,
                                final CounterSignTarget targetType,
                                final int[] targets,
                                final PrivateKey key,
                                final java.security.cert.Certificate[] certChain,
                                final AdESPolicy policy,
                                final boolean signingCertificateV2,
                                final String contentDescription,
                                final List<CommitmentTypeIndicationBean> ctis,
                                final CAdESSignerMetadata csm) throws IOException,
                                                                      NoSuchAlgorithmException,
                                                                      CertificateException,
                                                                      AOException {

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
    	final ASN1InputStream is = new ASN1InputStream(data);
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        is.close();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        e.nextElement();
        // Contenido de SignedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();

        final SignedData sd = SignedData.getInstance(contentSignedData);

        // Obtenemos los signerInfos del SignedData
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
                contentDescription,
                policy,
                signingCertificateV2,
                ctis,
                csm
            );
        }
        // FIRMA DE LAS HOJAS
        else if (targetType.equals(CounterSignTarget.LEAFS)) {
            signerInfos = counterLeaf(
        		signerInfosSd,
        		parameters,
        		key,
        		certChain,
                contentDescription,
                policy,
                signingCertificateV2,
                ctis,
                csm
            );
        }
        // FIRMA DE NODOS
        else if (targetType.equals(CounterSignTarget.NODES)) {
            // Firma de Nodos
            SignedData sigDat;
            SignedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = counterNode(
            		aux,
            		parameters,
            		key,
            		certChain,
                    contentDescription,
                    nodo,
                    policy,
                    signingCertificateV2,
                    ctis,
                    csm
                );
                sigDat = new SignedData(
            		sd.getDigestAlgorithms(),
            		sd.getEncapContentInfo(),
            		certificates,
            		certrevlist,
            		new DERSet(signerInfos)
        		);

                // Esto se realiza asi por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getEncoded(ASN1Encoding.DER));
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedData
                sd2.close();
                aux = SignedData.getInstance(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedData, aux).getEncoded(ASN1Encoding.DER);
        }
        // FIRMA DE LOS SIGNERS
        else if (targetType.equals(CounterSignTarget.SIGNERS)) {
            // Firma de Nodos
            SignedData sigDat;
            SignedData aux = sd;

            int nodo = 0;
            for (int i = targets.length - 1; i >= 0; i--) {
                nodo = targets[i];
                signerInfos = counterNode(
            		aux,
            		parameters,
            		key,
            		certChain,
                    contentDescription,
                    nodo,
                    policy,
                    signingCertificateV2,
                    ctis,
                    csm
                );
                sigDat = new SignedData(sd.getDigestAlgorithms(), sd.getEncapContentInfo(), certificates, certrevlist, new DERSet(signerInfos));

                // Esto se realiza as&iacute; por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getEncoded(ASN1Encoding.DER));
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedData
                sd2.close();

                aux = SignedData.getInstance(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedData, aux).getEncoded(ASN1Encoding.DER);
        }

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedData, new SignedData(sd.getDigestAlgorithms(),
                                                                                sd.getEncapContentInfo(),
                                                                                certificates,
                                                                                certrevlist,
                                                                                new DERSet(signerInfos))).getEncoded(ASN1Encoding.DER);

    }

    /** Contrafirma el &aacute;rbol completo de forma recursiva, todos
     * los nodos creando un nuevo contraSigner.<br>
     * @param signerInfosRaiz
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo
     * @param key Clave privada a usar para firmar.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param certChain Cadena de certificados del firmante.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private ASN1EncodableVector counterTree(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentDescription,
                                            final AdESPolicy policy,
                                            final boolean signingCertificateV2,
                                            final List<CommitmentTypeIndicationBean> ctis,
                                            final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
                                                                                  IOException,
                                                                                  CertificateException,
                                                                                  AOException {
        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(signerInfosRaiz.getObjectAt(i));
            counterSigners.add(
        		getCounterSignerInfo(
        			si,
        			parameters,
        			key,
        			certChain,
                    contentDescription,
                    policy,
                    signingCertificateV2,
                    ctis,
                    csm
                )
            );
        }
        return counterSigners;
    }

    /** Contrafirma las hojas del &aacute;rbol completo de forma
     * recursiva, todos los dodos creando un nuevo contraSigner.<br>
     * @param signerInfosRaiz
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private ASN1EncodableVector counterLeaf(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentDescription,
                                            final AdESPolicy policy,
                                            final boolean signingCertificateV2,
                                            final List<CommitmentTypeIndicationBean> ctis,
                                            final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
                                                                                  IOException,
                                                                                  CertificateException,
                                                                                  AOException {
        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(signerInfosRaiz.getObjectAt(i));
            counterSigners.add(
        		getLeafSignerInfo(
    				si,
    				parameters,
    				key,
    				certChain,
    				contentDescription,
    				policy,
    				signingCertificateV2,
    				ctis,
    				csm
				)
			);
        }
        return counterSigners;
    }

    /** Contrafirma un nodo determinado del &aacute;rbol busc&aacute;ndolo de
     * forma recursiva.<br>
     * @param sd
     *        SignedData que contiene el Nodo ra&iacute;z.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param nodo Nodo signerInfo a firmar.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException Cuando ocurre cualquier tipo de error */
    private ASN1EncodableVector counterNode(final SignedData sd,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentDescription,
                                            final int nodo,
                                            final AdESPolicy policy,
                                            final boolean signingCertificateV2,
                                            final List<CommitmentTypeIndicationBean> ctis,
                                            final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
                                                                                  IOException,
                                                                                  CertificateException,
                                                                                  AOException {
        final ASN1Set signerInfosRaiz = sd.getSignerInfos();

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        ASN1Set auxSignerRaiz;

        auxSignerRaiz = signerInfosRaiz;
        this.actualIndex = 0;

        for (int i = 0; i < auxSignerRaiz.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(auxSignerRaiz.getObjectAt(i));
            SignerInfo counterSigner = null;
            if (this.actualIndex == nodo) {
                counterSigner = getNodeSignerInfo(
            		si,
            		parameters,
            		key,
            		certChain,
                    contentDescription,
                    policy,
                    signingCertificateV2,
                    ctis,
                    csm
                );
            }
            else {
                if (this.actualIndex != nodo) {
                    counterSigner = getNodeSignerInfo(
                		si,
                		parameters,
                		key,
                		certChain,
                        contentDescription,
                        nodo,
                        policy,
                        signingCertificateV2,
                        ctis,
                        csm
                    );
                }
            }
            this.actualIndex++;
            counterSigners.add(counterSigner);
        }

        return counterSigners;

    }

    /** Obtiene la contrafirma de los signerInfo de forma recursiva.<br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return <i>SignerInfo</i> ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private SignerInfo getCounterSignerInfo(final SignerInfo signerInfo,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentDescription,
                                            final AdESPolicy policy,
                                            final boolean signingCertificateV2,
                                            final List<CommitmentTypeIndicationBean> ctis,
                                            final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
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
                				contentDescription,
                				policy,
                				signingCertificateV2,
                				ctis,
                				csm
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
        		parameters.getSignatureAlgorithm(),
        		signerInfo,
        		key,
        		certChain,
                contentDescription,
                policy,
                signingCertificateV2,
                ctis,
                csm
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
                counterSigner = new SignerInfo(
            		signerInfo.getSID(),
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
                				parameters.getSignatureAlgorithm(),
                				signerInfo,
                				key,
                				certChain,
                				contentDescription,
                				policy,
                				signingCertificateV2,
                				ctis,
                				csm
            				)
        				);
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
                                           new DERSet(uAtrib) // unsignedAttr
                            );
                }
            }

        }
        else {
            signerInfosU2.add(
        		generateSignerInfo(
            		parameters.getSignatureAlgorithm(),
            		signerInfo,
            		key,
            		certChain,
            		contentDescription,
            		policy,
            		signingCertificateV2,
            		ctis,
            		csm
        		)
    		);
            final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            counterSigner = new SignerInfo(
        		signerInfo.getSID(),
                signerInfo.getDigestAlgorithm(),
                signerInfo.getAuthenticatedAttributes(),
                signerInfo.getDigestEncryptionAlgorithm(),
                signerInfo.getEncryptedDigest(),
                new DERSet(uAtrib) // unsignedAttr
            );

        }
        return counterSigner;
    }

    /** Obtiene la contrafirma de los signerInfo de una determinada hoja de forma
     * recursiva.<br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private SignerInfo getLeafSignerInfo(final SignerInfo signerInfo,
                                         final P7ContentSignerParameters parameters,
                                         final PrivateKey key,
                                         final java.security.cert.Certificate[] certChain,
                                         final String contentDescription,
                                         final AdESPolicy policy,
                                         final boolean signingCertificateV2,
                                         final List<CommitmentTypeIndicationBean> ctis,
                                         final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
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
                    		getLeafSignerInfo(
                        		si,
                        		parameters,
                        		key,
                        		certChain,
                        		contentDescription,
                        		policy,
                        		signingCertificateV2,
                        		ctis,
                        		csm
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
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        contexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.
                        signerInfosU2.add(
                    		generateSignerInfo(
                				parameters.getSignatureAlgorithm(),
                				signerInfo,
                				key,
                				certChain,
                                contentDescription,
                                policy,
                                signingCertificateV2,
                                ctis,
                                csm
                            )
                        );
                        final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
                        contexExpecific.add(uAtrib);

                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
                    counterSigner =new SignerInfo(
                		signerInfo.getSID(),
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
            		parameters.getSignatureAlgorithm(),
            		signerInfo,
            		key,
            		certChain,
            		contentDescription,
            		policy,
            		signingCertificateV2,
            		ctis,
            		csm
        		)
    		);
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

    /** Obtiene la contrafirma de los signerInfo sin ser recursivo. Esto es por
     * el caso especial de que puede ser el nodo raiz el nodo a firmar, por lo
     * que no ser&iacute;a necesario usar la recursividad.<br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException  Cuando hay problemas con los certificados proporcionados. */
    private SignerInfo getNodeSignerInfo(final SignerInfo signerInfo,
                                                final P7ContentSignerParameters parameters,
                                                final PrivateKey key,
                                                final java.security.cert.Certificate[] certChain,
                                                final String contentDescription,
                                                final AdESPolicy policy,
                                                final boolean signingCertificateV2,
                                                final List<CommitmentTypeIndicationBean> ctis,
                                                final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
                                                                                      IOException,
                                                                                      CertificateException {
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
                        signerInfosU.add(SignerInfo.getInstance(eAtributesData.nextElement()));
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DEL NODO ACTUAL
            signerInfosU.add(
        		generateSignerInfo(
            		parameters.getSignatureAlgorithm(),
            		signerInfo,
            		key,
            		certChain,
            		contentDescription,
            		policy,
            		signingCertificateV2,
            		ctis,
            		csm
        		)
    		);

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
                counterSigner = new SignerInfo(signerInfo.getSID(),
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
                        		parameters.getSignatureAlgorithm(),
                        		signerInfo,
                        		key,
                        		certChain,
                        		contentDescription,
                        		policy,
                        		signingCertificateV2,
                        		ctis,
                        		csm
                    		)
                		);
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
                    counterSigner = new SignerInfo(signerInfo.getSID(),
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
            		parameters.getSignatureAlgorithm(),
            		signerInfo,
            		key,
            		certChain,
            		contentDescription,
            		policy,
            		signingCertificateV2,
            		ctis,
            		csm
        		)
    		);
            final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            counterSigner = new SignerInfo(
        		signerInfo.getSID(),
                signerInfo.getDigestAlgorithm(),
                signerInfo.getAuthenticatedAttributes(),
                signerInfo.getDigestEncryptionAlgorithm(),
                signerInfo.getEncryptedDigest(),
                new DERSet(uAtrib) // unsignedAttr
            );
        }
        return counterSigner;
    }

    /** Obtiene la contrafirma de los signerInfo buscando el nodo de forma
     * recursiva.<br>
     * @param signerInfo
     *        Nodo ra&iacute; que contiene todos los signerInfos que se
     *        deben firmar.
     * @param parameters
     *        Par&aacute;metros necesarios para firmar un determinado
     *        SignerInfo hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param node Nodo espec&iacute;fico a firmar.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private SignerInfo getNodeSignerInfo(final SignerInfo signerInfo,
                                         final P7ContentSignerParameters parameters,
                                         final PrivateKey key,
                                         final java.security.cert.Certificate[] certChain,
                                         final String contentDescription,
                                         final int node,
                                         final AdESPolicy policy,
                                         final boolean signingCertificateV2,
                                         final List<CommitmentTypeIndicationBean> ctis,
                                         final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
                                                                               IOException,
                                                                               CertificateException,
                                                                               AOException {
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
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
                        this.actualIndex++;
                        if (this.actualIndex != node) {
                            if (this.actualIndex < node) {
                                signerInfosU.add(
                            		getNodeSignerInfo(
                                		si,
                                		parameters,
                                		key,
                                		certChain,
                                        contentDescription,
                                        node,
                                        policy,
                                        signingCertificateV2,
                                        ctis,
                                        csm
                                    )
                                );
                            }
                            else {
                                signerInfosU.add(si);
                            }
                        }
                        else {
                            final SignerInfo obtained = getNodeSignerInfo(
                        		si,
                        		parameters,
                        		key,
                        		certChain,
                        		contentDescription,
                        		policy,
                        		signingCertificateV2,
                        		ctis,
                        		csm
                    		);
                            signerInfosU.add(obtained);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DE CADA UNO DE LOS HIJOS
            final ASN1Set a1;
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

    /** Genera un signerInfo espec&iacute;fico utilizando los
     * datos necesarios para crearlo. Se utiliza siempre que no se sabe cual es
     * el signerInfo que se debe firmar.
     * @param signatureAlgorithm Algoritmo de firma a usar.
     * @param si SignerInfo del que se debe recoger la informaci&oacute;n para
     *           realizar la contrafirma espec&iacute;fica.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return <i>SignerInfo</i> contrafirmado.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados. */
    private SignerInfo generateSignerInfo(final String signatureAlgorithm,
                                                 final SignerInfo si,
                                                 final PrivateKey key,
                                                 final java.security.cert.Certificate[] certChain,
                                                 final String contentDescription,
                                                 final AdESPolicy policy,
                                                 final boolean signingCertificateV2,
                                                 final List<CommitmentTypeIndicationBean> ctis,
                                                 final CAdESSignerMetadata csm) throws NoSuchAlgorithmException,
                                                                                       IOException,
                                                                                       CertificateException {
        // buscamos que timo de algoritmo es y lo codificamos con su OID
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);

        // authenticatedAttributes
        final ASN1EncodableVector contextExcepcific = CAdESUtils.generateSignerInfo(
             certChain[0],
             digestAlgorithm,
             si.getEncryptedDigest().getOctets(),
             policy,
             signingCertificateV2,
             null,
             this.date != null ? this.date : new Date(), // Usamos fecha y hora actual nueva si no se nos ha indicado otra distinta
             false,
             null, // En contrafirma el ContentType no se pone
             contentDescription,
             ctis,
             csm
        );

        final ASN1Set signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));

        final ASN1OctetString sign2;
        try {
            sign2 = new DEROctetString(
        		pkcs1Sign(
    				signedAttr.getEncoded(ASN1Encoding.DER),
    				signatureAlgorithm,
    				key,
				    certChain
				)
    		);
        }
        catch (final AOException ex) {
            throw new IOException("Error al realizar la firma: " + ex, ex); //$NON-NLS-1$
        }

        // AlgorithmIdentifier
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(
    		ASN1Primitive.fromByteArray(((X509Certificate)certChain[0]).getTBSCertificate())
		);
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // UNAUTHENTICATEDATTRIBUTES
        final ASN1Set unsignedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));

        return new SignerInfo(identifier, digAlgId, unsignedAttr, encAlgId, sign2, null);


    }

    void setpkcs1Signer(final AOSimpleSigner p1Signer, final Date d) {
    	if (p1Signer == null) {
    		throw new IllegalArgumentException("El firmador PKCS#1 no puede ser nulo"); //$NON-NLS-1$
    	}
    	if (d == null) {
    		Logger.getLogger("es.gob.afirma").warning("Se ha establecido una fecha nula, se usara la actual"); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	this.ss = p1Signer;
    	this.date = d;
    }

    /** Realiza la firma usando los atributos del firmante.
     * @param data Datos a firmar.
     * @param signatureAlgorithm Algoritmo para la firma
     * @param key Clave para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @return Firma de los atributos.
     * @throws AOException En caso de cualquier otro tipo de error */
    private byte[] pkcs1Sign(final byte[] data,
    		                        final String signatureAlgorithm,
    		                        final PrivateKey key,
    		                        final java.security.cert.Certificate[] certChain) throws AOException {
    	try {
			return this.ss.sign(data, signatureAlgorithm, key, certChain, null);
		}
    	catch (final IOException e) {
			throw new AOException(
				"Error en la firma PKCS#1 de la contrafirma CAdES: " + e, e //$NON-NLS-1$
			);
		}
    }
}
