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
import java.util.Arrays;
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
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.Certificate;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
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
public final class CAdESTriPhaseCounterSigner {

	private static final int MAX_SUPPORTED_COUNTERSIGNS = 10;

	/** Resultado de un conjunto de PreContraFirmas CAdES. */
	public static final class CAdESPreCounterSignResult {

		private final List<byte[]> preSigns;
		private final byte[] sign;

		CAdESPreCounterSignResult(final byte[] s, final List<byte[]> sDatas) {
			this.preSigns = sDatas;
			this.sign = s.clone();
		}

		/**
		 * Recupera la firma con las contrafirmas que deseamos generada con un certificado fake.
		 * @return Firma CAdES con contrafirmas.
		 */
		public byte[] getSign() {
			return this.sign;
		}

		/**
		 * Recupera las prefirmas (SignedData) de cada contrafirma realizada listas para
		 * firmarse con el certificado correcto.
		 * @return Listado de prefirmas.
		 */
		public List<byte[]> getPreSigns() {
			return this.preSigns;
		}
	}

	/** Tama&ntilde;o de una firma PKCS#1. */
	public static final int PKCS1_DEFAULT_SIZE = 128;

    /** N&uacute;mero de contrafirma dentro de la firma actual. */
	private int counterIndex = 0;

	/** Lista de <i>SignedAttributes</i> de las contrafirmas de la firma ordenados por n&uacute;mero de contrafirma dentro de la firma. */
	private List<byte[]> signedDatas = new ArrayList<byte[]>();

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
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param contentType
     * 		  Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El archivo de firmas con la nueva firma.
     * @throws IOException Cuando se produce algun error con la lectura o escritura de datos.
     * @throws java.security.NoSuchAlgorithmException
     *         Excepci&oacute;n cuando no se encuentra el algoritmo de
     *         firma.
     * @throws java.security.cert.CertificateException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws AOException
     *         Cuando ocurre alguno error con contemplado por las otras
     *         excepciones declaradas */
    public CAdESPreCounterSignResult preCounterSign(final P7ContentSignerParameters parameters,
                                final byte[] data,
                                final CounterSignTarget targetType,
                                final PrivateKey key,
                                final java.security.cert.Certificate[] certChain,
                                final AdESPolicy policy,
                                final boolean signingCertificateV2,
                                final String contentType,
                                final String contentDescription,
                                final List<CommitmentTypeIndicationBean> ctis,
                                final CAdESSignerMetadata csm) throws IOException,
                                                                      NoSuchAlgorithmException,
                                                                      CertificateException,
                                                                      AOException {
    	// Inicializamos el contador global y la lista de SignedDatas
    	this.counterIndex = 0;
    	this.signedDatas = new ArrayList<byte[]>();

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
        if (CounterSignTarget.TREE.equals(targetType)) {
            signerInfos = counterTree(
        		signerInfosSd,
        		parameters,
        		key,
        		certChain,
                contentType,
                contentDescription,
                policy,
                signingCertificateV2,
                ctis,
                csm
            );
        }
        // FIRMA DE LAS HOJAS
        else if (CounterSignTarget.LEAFS.equals(targetType)) {
            signerInfos = counterLeaf(
        		signerInfosSd,
        		parameters,
        		key,
        		certChain,
                contentType,
                contentDescription,
                policy,
                signingCertificateV2,
                ctis,
                csm
            );
        }
        else {
        	throw new IllegalArgumentException("Modo de contrafirma no soportado: " + targetType); //$NON-NLS-1$
        }

        // construimos el Signed Data y lo devolvemos dentro del resultado
        return new CAdESPreCounterSignResult(
    		new ContentInfo(
				PKCSObjectIdentifiers.signedData,
				new SignedData(sd.getDigestAlgorithms(),
		        sd.getEncapContentInfo(),
		        certificates,
		        certrevlist,
		        new DERSet(signerInfos))
			).getEncoded(ASN1Encoding.DER),
			this.signedDatas
		);
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
     * @param certChain Cadena de certificados del firmante.
     * @param contentType Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws NoSuchAlgorithmException
     * @throws IOException
     * @throws CertificateException
     * @throws AOException En caso de cualquier otro tipo de error */
    private ASN1EncodableVector counterTree(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentType,
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
                    contentType,
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
     * @param key Clave privada a usar para firmar
     * @param contentType Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws AOException En caso de cualquier otro tipo de error */
    private ASN1EncodableVector counterLeaf(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentType,
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
    				contentType,
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

    /** Obtiene la contrafirma de los signerInfo de forma recursiva.<br>
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos que se deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado SignerInfo hoja.
     * @param key Clave privada a usar para firmar.
     * @param contentType Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws AOException En caso de cualquier otro tipo de error */
    private SignerInfo getCounterSignerInfo(final SignerInfo signerInfo,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final String contentType,
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
                				contentType,
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
        		certChain,
                contentType,
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
                				certChain,
                				contentType,
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
            		certChain,
            		contentType,
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
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos que se deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado <i>SignerInfo</i> hoja.
     * @param key Clave privada a usar para firmar
     * @param contentType Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return SignerInfo ra&iacute;z parcial con todos sus nodos contrafirmados.
     * @throws NoSuchAlgorithmException
     * @throws IOException
     * @throws CertificateException
     * @throws AOException En caso de cualquier otro tipo de error */
    private SignerInfo getLeafSignerInfo(final SignerInfo signerInfo,
                                         final P7ContentSignerParameters parameters,
                                         final PrivateKey key,
                                         final java.security.cert.Certificate[] certChain,
                                         final String contentType,
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
                        		contentType,
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
                        new SignerInfo(
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
                				certChain,
                                contentType,
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
                    counterSigner = new SignerInfo(
                		signerInfo.getSID(),
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
            		certChain,
            		contentType,
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

    /** Genera un signerInfo espec&iacute;fico utilizando los
     * datos necesarios para crearlo. Se utiliza siempre que no se sabe cual es
     * el signerInfo que se debe firmar.<br>
     * @param signatureAlgorithm Algoritmo de firma.
     * @param si SignerInfo del que se debe recoger la informaci&oacute;n para
     *           realizar la contrafirma espec&iacute;fica.
     * @param certChain Cadena de certificados del firmante
     * @param contentType Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param policy Pol&iacute;tica de firma
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return <code>SignerInfo</code> contrafirmado.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException En caso de errores de entrada / salida
     * @throws java.security.cert.CertificateException */
    private SignerInfo generateSignerInfo(final String signatureAlgorithm,
                                          final SignerInfo si,
                                          final java.security.cert.Certificate[] certChain,
                                          final String contentType,
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
             (X509Certificate) certChain[0],
             digestAlgorithm,
             si.getEncryptedDigest().getOctets(),
             policy,
             signingCertificateV2,
             null,
             new Date(),
             false,
             contentType,
             contentDescription,
             ctis,
             csm
        );

        final ASN1Set signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));

        // Anadimos los SignedAttributes a la lista en la posicion adecuada
        this.signedDatas.add(this.counterIndex, signedAttr.getEncoded(ASN1Encoding.DER));
        // Obtenemos el sustituto del PKCS#1, relleno con el numero de contrafirma
        final ASN1OctetString sign2 = new DEROctetString(firma());
        // Incrementamos el indice de contrafirmas
    	this.counterIndex = this.counterIndex + 1;

    	if (this.counterIndex >= MAX_SUPPORTED_COUNTERSIGNS) {
    		throw new UnsupportedOperationException("No se soportan mas de 10 contrafirmas en una misma firma"); //$NON-NLS-1$
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

    /** Simula una firma PKCS#1.
     * @return Array de octetos relleno con el ASCII del n&uacute;mero de contrafirma y de longitud igual a un PKCS#1 equivalente */
    private byte[] firma() {
    	final byte[] dummy = new byte[PKCS1_DEFAULT_SIZE];
    	Arrays.fill(dummy, (byte) Integer.toString(this.counterIndex).toCharArray()[0]);
        return dummy;
    }
}
