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

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.IssuerAndSerialNumber;
import org.spongycastle.asn1.cms.SignedData;
import org.spongycastle.asn1.cms.SignerIdentifier;
import org.spongycastle.asn1.cms.SignerInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSimpleSigner;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.CAdESSignerMetadata;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.cades.CommitmentTypeIndicationBean;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Contrafirma digital CADES SignedData.
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedData de SpongyCastle pero con la
 * peculiaridad de que es una Contrafirma.
 * <pre>
 *  SignedData ::= SEQUENCE {
 *       version CMSVersion,
 *       digestAlgorithms DigestAlgorithmIdentifiers,
 *       encapContentInfo EncapsulatedContentInfo,
 *       certificates [0] IMPLICIT CertificateSet OPTIONAL,
 *       crls [1] IMPLICIT RevocationInfoChoices OPTIONAL,
 *       signerInfos SignerInfos
 *  }
 *
 *  DigestAlgorithmIdentifiers ::= SET OF DigestAlgorithmIdentifier
 *
 *  SignerInfos ::= SET OF SignerInfo
 *
 *  EncapsulatedContentInfo ::= SEQUENCE {
 *       eContentType ContentType,
 *       eContent [0] EXPLICIT OCTET STRING OPTIONAL
 *  }
 *
 *  SignerInfo ::= SEQUENCE {
 *       version CMSVersion,
 *       sid SignerIdentifier,
 *       digestAlgorithm DigestAlgorithmIdentifier,
 *       signedAttrs [0] IMPLICIT SignedAttributes OPTIONAL,
 *       signatureAlgorithm SignatureAlgorithmIdentifier,
 *       signature SignatureValue,
 *       unsignedAttrs [1] IMPLICIT UnsignedAttributes OPTIONAL
 *  }
 *
 *  SignerIdentifier ::= CHOICE {
 *       issuerAndSerialNumber IssuerAndSerialNumber,
 *       subjectKeyIdentifier [0] SubjectKeyIdentifier
 *   }
 *
 *  SignedAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *  UnsignedAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *  Attribute ::= SEQUENCE {
 *       attrType OBJECT IDENTIFIER,
 *       attrValues SET OF AttributeValue
 *  }
 *
 *  AttributeValue ::= ANY
 *
 *  SignatureValue ::= OCTET STRING
 *
 *  ContentType ::= OBJECT IDENTIFIER
 * </pre> */
final class CAdESCounterSigner {

    private AOSimpleSigner ss = new AOPkcs1Signer();
    private Date date = null;


    void setPkcs1Signer(final AOSimpleSigner p1Signer, final Date d) {
    	if (p1Signer == null) {
    		throw new IllegalArgumentException("El firmador PKCS#1 no puede ser nulo"); //$NON-NLS-1$
    	}
    	if (d == null) {
    		Logger.getLogger("es.gob.afirma").warning("Se ha establecido una fecha nula, se usara la actual"); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	this.ss = p1Signer;
    	this.date = d;
    }

    /** Crea una contrafirma a partir de los datos del firmante, el archivo que se firma y
     * del archivo que contiene las firmas.
     * <p>El fichero de firmas tiene esta estructura:</p>
     * <pre>
     *  SEQUENCE {
     *  	OBJECT IDENTIFIER { 1.2.840.113549.1.7.2 }, // El OID de SignedData
     *  	ContextSpecific {
     *  		SignedData
     *  	}
     *  }
     * </pre>
     * <p>Y la estructura <i>SignedData</i> este esquema:</p>
     * <pre>
     * SignedData ::= SEQUENCE {
     * 		version Version,
     *		digestAlgorithms DigestAlgorithmIdentifiers,
     *		contentInfo ContentInfo,
     *		certificates [0] IMPLICIT ExtendedCertificatesAndCertificates OPTIONAL,
     *		crls [1] IMPLICIT CertificateRevocationLists OPTIONAL,
     *		signerInfos SignerInfos
     * }
     * </pre>
     * <p>
     *  Siguiendo con ls tipos de datos, <i>SignerInfos</i>, que es la estructura objetivo,
     *  tiene este esquema:
     * </p>
     * <pre>
     * SignerInfos ::= SET OF SignerInfo
     *
     * SignerInfo ::= SEQUENCE {
     *		version Version,
     *		issuerAndSerialNumber IssuerAndSerialNumber,
     *		digestAlgorithm DigestAlgorithmIdentifier,
     *		authenticatedAttributes [0] IMPLICIT Attributes OPTIONAL,
     *		digestEncryptionAlgorithm DigestEncryptionAlgorithmIdentifier,
     *		encryptedDigest EncryptedDigest,
     *		unauthenticatedAttributes [1] IMPLICIT Attributes OPTIONAL
     * }
     *
     * EncryptedDigest ::= OCTET STRING
     * </pre>
     * @param algorithm Algoritmo de firma.
     * @param signature Archivo que contiene las firmas.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2 <code>true</code> si se desea usar la versi&oacute;n 2 del
     *                             atributo <i>Signing Certificate</i> <code>false</code> para
     *                             usar la versi&oacute;n 1
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param includeSigningTimeAttribute <code>true</code> para incluir el atributo <i>SigningTime</i> de PKCS#9 (OID:1.2.840.113549.1.9.5),
     *                                    <code>false</code> para no incluirlo.
     * @param csm Metadatos sobre el firmante.
     * @param doNotIncludePolicyOnSigningCertificate Si se establece a <code>true</code> omite la inclusi&oacute;n de la
     *                                               pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>,
     *                                               si se establece a <code>false</code> se incluye siempre que el certificado
     *                                               la declare.
     * @return El archivo de firmas con la nueva firma.
     * @throws IOException Cuando se produce algun error con la lectura o escritura de datos.
     * @throws NoSuchAlgorithmException Cuando no se encuentra el algoritmo de firma.
     * @throws CertificateException Si se produce alguna excepci&oacute;n con los certificados de
     *                              firma.
     * @throws AOException Cuando ocurre alg&uacute;n error no contemplado por las otras
     *                     excepciones declaradas */
    byte[] counterSign(final String algorithm,
                       final byte[] signature,
                       final CounterSignTarget targetType,
                       final PrivateKey key,
                       final java.security.cert.Certificate[] certChain,
                       final AdESPolicy policy,
                       final boolean signingCertificateV2,
                       final List<CommitmentTypeIndicationBean> ctis,
                       final boolean includeSigningTimeAttribute,
                       final CAdESSignerMetadata csm,
                       final boolean doNotIncludePolicyOnSigningCertificate) throws IOException,
                                                                                    NoSuchAlgorithmException,
                                                                                    CertificateException,
                                                                                    AOException {
        // Leemos los datos originales (la firma que nos llega)
    	final ASN1InputStream is = new ASN1InputStream(signature);
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        is.close();
        final Enumeration<?> pkcs7RootSequenceElements = dsq.getObjects();

        // Pasamos el primer elemento de la secuencia original, que es el OID de SignedData
        final Object o = pkcs7RootSequenceElements.nextElement();
        if (!(o instanceof ASN1ObjectIdentifier) && ((ASN1ObjectIdentifier)o).equals(PKCSObjectIdentifiers.signedData)) {
			throw new AOFormatFileException("No se ha encontrado un SignedData en los datos a contrafirmar"); //$NON-NLS-1$
		}

        // Obtenemos el Context-Specific
        final ASN1TaggedObject pkcs7RootContextSpecificZero = (ASN1TaggedObject) pkcs7RootSequenceElements.nextElement();

        // Sacamos la secuencia de dentro Context-Specific, que es ya el SignedData ASN.1
        final SignedData signedData = SignedData.getInstance(pkcs7RootContextSpecificZero.getObject());

        // Obtenemos el SignerInfos (conjunto de SignerInfo) del SignedData
        final ASN1Set originalSignerInfosFromSignedData = signedData.getSignerInfos();

        // Anadimos los nuevos certificados a los ya existentes en el fichero de firmas
        // en un SET para anadirlo al SignedData final
        final ASN1Set certificates = CAdESMultiUtil.addCertificates(signedData, certChain);

        // Creamos el que sera el nuevo conjunto SignerInfos (SET de muchos SignerInfo), para lo que agregamos
        // a los actuales los nuevos
        final ASN1EncodableVector newSignerInfos = counterSignSignerInfos(
    		originalSignerInfosFromSignedData,
    		algorithm,
    		key,
    		certChain,
            policy,
            signingCertificateV2,
            ctis,
            includeSigningTimeAttribute,
            csm,
            targetType,
            doNotIncludePolicyOnSigningCertificate
        );

        // Construimos y devolvemos la nueva firma (atributo identificador del signedData mas el propio signedData).
        // Esta firma sera igual a la anterior pero con el conjunto de certificados actualizados con los nuevos y la
        // nueva estructura de SignerInfos. Tambien eliminamos las CRL ya que no estarian completas.
        return new ContentInfo(
    		PKCSObjectIdentifiers.signedData,
    		new SignedData(
				signedData.getDigestAlgorithms(),
                signedData.getEncapContentInfo(),
                certificates,
                null, // Lista de CRL
                new DERSet(newSignerInfos)
			)
		).getEncoded(ASN1Encoding.DER);

    }

    /** Contrafirma el &aacute;rbol completo de forma recursiva (todos los nodos).
     * @param signerInfosRaiz <i>SignerInfos</i> con los <i>SignerInfo</i> que se deben firmar.
     * @param algorithm Algoritmo de firma.
     * @param key Clave privada a usar para firmar.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *                             para usar <i>SigningCertificateV1</i>.
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param includeSigningTimeAttribute <code>true</code> para incluir el atributo <i>SigningTime</i> de PKCS#9 (OID:1.2.840.113549.1.9.5),
     *                                    <code>false</code> para no incluirlo.
     * @param csm Metadatos sobre el firmante.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo y s&oacute;lo los nodos hoja.
     * @param doNotIncludePolicyOnSigningCertificate Si se establece a <code>true</code> omite la inclusi&oacute;n de la
     *                                               pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>,
     *                                               si se establece a <code>false</code> se incluye siempre que el certificado
     *                                               la declare.
     * @return Conjunto de <i>SignerInfo</i> con todos los nodos, los anteriores y las contrafirmas de estos.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores en el tratamiento de datos.
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private ASN1EncodableVector counterSignSignerInfos(final ASN1Set signerInfosRaiz,
                                                       final String algorithm,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain,
                                                       final AdESPolicy policy,
                                                       final boolean signingCertificateV2,
                                                       final List<CommitmentTypeIndicationBean> ctis,
                                                       final boolean includeSigningTimeAttribute,
                                                       final CAdESSignerMetadata csm,
                                                       final CounterSignTarget targetType,
                                                       final boolean doNotIncludePolicyOnSigningCertificate) throws NoSuchAlgorithmException,
                                                                                                                    IOException,
                                                                                                                    CertificateException,
                                                                                                                    AOException {
        // Vector donde almacenaremos la nueva estructura de SignerInfo
    	final ASN1EncodableVector counterSigners = new ASN1EncodableVector();

    	// Recorremos todos los SignerInfo y llamamos a un metodo que los recorrera recursivamente cada uno
        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(
        		signerInfosRaiz.getObjectAt(i)
    		);
            counterSigners.add(
        		counterSignSignerInfo(
        			si,
        			algorithm,
        			key,
        			certChain,
                    policy,
                    signingCertificateV2,
                    ctis,
                    includeSigningTimeAttribute,
                    csm,
                    targetType,
                    doNotIncludePolicyOnSigningCertificate
                )
            );
        }
        return counterSigners;
    }

    /** Contrafirma una rama de <i>SignerInfo</i>. Como resultado se devuelve un SignerInfo igual
     * al procesado, con los mismos atributos no firmados mas el nuevo atributo de contrafirma si
     * tocaba firmar este SignerInfo. Cada nodo de contrafirma que ya existiese habra sido tambien
     * procesado para agregar contrafirmas. Si se encontrase un atributo no firmado no soportado,
     * se suspenderia todo el proceso de firma.
     * @param signerInfo <i>SignedInfo</i> ra&iacute;z.
     * @param algorithm Algoritmo de firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param includeSigningTimeAttribute <code>true</code> para incluir el atributo <i>SigningTime</i> de PKCS#9 (OID:1.2.840.113549.1.9.5),
     *                                    <code>false</code> para no incluirlo.
     * @param csm Metadatos sobre el firmante.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
     * @param doNotIncludePolicyOnSigningCertificate Si se establece a <code>true</code> omite la inclusi&oacute;n de la
     *                                               pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>,
     *                                               si se establece a <code>false</code> se incluye siempre que el certificado
     *                                               la declare.
     * @return <i>SignerInfo</i> ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private SignerInfo counterSignSignerInfo(final SignerInfo signerInfo,
                                             final String algorithm,
                                             final PrivateKey key,
                                             final java.security.cert.Certificate[] certChain,
                                             final AdESPolicy policy,
                                             final boolean signingCertificateV2,
                                             final List<CommitmentTypeIndicationBean> ctis,
                                             final boolean includeSigningTimeAttribute,
                                             final CAdESSignerMetadata csm,
                                             final CounterSignTarget targetType,
                                             final boolean doNotIncludePolicyOnSigningCertificate) throws NoSuchAlgorithmException,
                                                                                                          IOException,
                                                                                                          CertificateException,
                                                                                                          AOException {
    	// Base para el nuevo SET de SignerInfos
    	final List<Attribute> newUnauthenticatedAttributesList = new ArrayList<Attribute>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();

        // Es hoja?
        boolean isLeaf = true;

        // Comprobamos si tiene atributos no firmados y los recorremos
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> unauthenticatedAttributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (unauthenticatedAttributes.hasMoreElements()) {

            	final Attribute unauthenticatedAttribute = Attribute.getInstance(unauthenticatedAttributes.nextElement());

                // Si es un atributo no soportado, no podremos realizar la firma y se lanzara una excepcion
                CAdESMultiUtil.checkUnsupported(unauthenticatedAttribute.getAttrType());

                // Si es una contrafirma, la analizamos para saber si procesar su contenido y agregar nuevas contrafirmas
                if (CAdESMultiUtil.isCounterSignature(unauthenticatedAttribute.getAttrType())) {

                	isLeaf = false;

                	// Obtenemos el listado de SignerInfos y los procesamos recursivamente
                	final List<SignerInfo> signerInfos = getSignerInfoFromUnauthenticatedAttributes(unauthenticatedAttribute);
                    for (final SignerInfo si : signerInfos) {
                        signerInfosU.add(
                    		counterSignSignerInfo(
                				si,
                				algorithm,
                				key,
                				certChain,
                				policy,
                				signingCertificateV2,
                				ctis,
                				includeSigningTimeAttribute,
                				csm,
                				targetType,
                				doNotIncludePolicyOnSigningCertificate
            				)
                		);
                    }
                }
                // Si es cualquier otro atributo no firmado soportado, lo agregamos al listado de salida
                else {
                	newUnauthenticatedAttributesList.add(unauthenticatedAttribute);
                }

            }

        }

        // Vuelta de la recursividad, si toca firmar este nodo, creamos la contrafirma
        // del nodo y la agregamos a la estructura
        if (CounterSignTarget.TREE.equals(targetType) || CounterSignTarget.LEAFS.equals(targetType) && isLeaf) {
			signerInfosU.add(
				signSignerInfo(
					signerInfo,
					algorithm,
					key,
					certChain,
					policy,
					signingCertificateV2,
					ctis,
					includeSigningTimeAttribute,
					csm,
					doNotIncludePolicyOnSigningCertificate
				)
			);
        }

        // Agregamos la contrafirma encontrada (que incluira sus nodos internos)
        newUnauthenticatedAttributesList.add(
        		new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU)) // Se marca como contrafirma en sus atributos no firmados
        		);

        // Creamos el signer info con los nodos encontrados
		return new SignerInfo(
			signerInfo.getSID(),
		    signerInfo.getDigestAlgorithm(),
		    signerInfo.getAuthenticatedAttributes(),
		    signerInfo.getDigestEncryptionAlgorithm(),
		    signerInfo.getEncryptedDigest(),
		    new DERSet(
		    	newUnauthenticatedAttributesList.toArray(new ASN1Encodable[newUnauthenticatedAttributesList.size()])
		    )
		);

    }

    /** Realiza realmente la operaci&oacute;n criptogr&aacute;fica de firma para generar finalmente el <i>SignerInfo</i>.
     * @param si SignerInfo a firmar (se obtiene la huella digital almacenada en &eacute;l y se firma).
     * @param signatureAlgorithm Algoritmo de firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param includeSigningTimeAttribute <code>true</code> para incluir el atributo <i>SigningTime</i> de PKCS#9 (OID:1.2.840.113549.1.9.5),
     *                                    <code>false</code> para no incluirlo.
     * @param csm Metadatos sobre el firmante.
     * @param doNotIncludePolicyOnSigningCertificate Si se establece a <code>true</code> omite la inclusi&oacute;n de la
     *                                               pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>,
     *                                               si se establece a <code>false</code> se incluye siempre que el certificado
     *                                               la declare.
     * @return <i>SignerInfo</i> contrafirmado.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados. */
    private SignerInfo signSignerInfo(final SignerInfo si,
    								  final String signatureAlgorithm,
                                      final PrivateKey key,
                                      final java.security.cert.Certificate[] certChain,
                                      final AdESPolicy policy,
                                      final boolean signingCertificateV2,
                                      final List<CommitmentTypeIndicationBean> ctis,
                                      final boolean includeSigningTimeAttribute,
                                      final CAdESSignerMetadata csm,
                                      final boolean doNotIncludePolicyOnSigningCertificate) throws NoSuchAlgorithmException,
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
    		 includeSigningTimeAttribute,
             false,
             PKCSObjectIdentifiers.data.toString(), // El ContentType de las contrafirmas siempre sera id-data
			 null,	// No agregamos content-description
             ctis,
             csm,
             true,  // Es contrafirma
             doNotIncludePolicyOnSigningCertificate
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
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(
    		X500Name.getInstance(tbs.getIssuer()),
    		tbs.getSerialNumber().getValue()
		);
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // UNAUTHENTICATEDATTRIBUTES
        final ASN1Set unsignedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExcepcific));

        return new SignerInfo(identifier, digAlgId, unsignedAttr, encAlgId, sign2, null);

    }

    private static List<SignerInfo> getSignerInfoFromUnauthenticatedAttributes(final Attribute unauthenticatedAttribute) {

    	final ArrayList<SignerInfo> signerInfos = new ArrayList<SignerInfo>();

    	// El atributo tiene dentro un SignerInfos, que es un SET de SignerInfo
        final ASN1Set values = unauthenticatedAttribute.getAttrValues();

        // Recorremos los SignerInfo del SignerInfos de forma recursiva
        final Enumeration<?> eAtributesData = values.getObjects();
        while (eAtributesData.hasMoreElements()) {
        	try {
        		signerInfos.add(SignerInfo.getInstance(eAtributesData.nextElement()));
        	}
        	catch(final Exception e) {
        		// Ignoramos los objetos que no sea SignedInfo
        		continue;
        	}
        }

        return signerInfos;
    }

    /** Realiza la firma usando los atributos del firmante.
     * @param data Datos a firmar.
     * @param signatureAlgorithm Algoritmo de firma
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
