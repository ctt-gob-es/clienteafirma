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
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Encoding;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
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
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

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
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Contrafirma digital CADES SignedData.
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a> pero con la
 * peculiaridad de que es una Contrafirma. */
final class CAdESCounterSigner {

    private AOSimpleSigner ss = new AOPkcs1Signer();
    private Date date = null;

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
     * @param parameters Par&aacute;metros necesarios que contienen tanto la firma del
     *                   archivo a firmar como los datos del firmante.
     * @param data Archivo que contiene las firmas.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
     * @param targets Nodos objetivos a firmar.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2 <code>true</code> si se desea usar la versi&oacute;n 2 del
     *                             atributo <i>Signing Certificate</i> <code>false</code> para
     *                             usar la versi&oacute;n 1
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return El archivo de firmas con la nueva firma.
     * @throws IOException Cuando se produce algun error con la lectura o escritura de datos.
     * @throws NoSuchAlgorithmException Cuando no se encuentra el algoritmo de firma.
     * @throws CertificateException Si se produce alguna excepci&oacute;n con los certificados de
     *                              firma.
     * @throws AOException Cuando ocurre alg&uacute;n error no contemplado por las otras
     *                     excepciones declaradas */
    byte[] counterSign(final P7ContentSignerParameters parameters,
                       final byte[] data,
                       final CounterSignTarget targetType,
                       final int[] targets,
                       final PrivateKey key,
                       final java.security.cert.Certificate[] certChain,
                       final AdESPolicy policy,
                       final boolean signingCertificateV2,
                       final List<CommitmentTypeIndicationBean> ctis,
                       final CAdESSignerMetadata csm) throws IOException,
                                                             NoSuchAlgorithmException,
                                                             CertificateException,
                                                             AOException {
        // Leemos los datos originales
    	final ASN1InputStream is = new ASN1InputStream(data);
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        is.close();
        final Enumeration<?> e = dsq.getObjects();

        // Pasamos el primer elemento de la secuencia original, que es el OID de SignedData
        final Object o = e.nextElement();
        if (!(o instanceof ASN1ObjectIdentifier) && ((ASN1ObjectIdentifier)o).equals(PKCSObjectIdentifiers.signedData)) {
			throw new AOFormatFileException("No se ha encontrado un SignedData en los datos a contrafirmar"); //$NON-NLS-1$
		}

        // Obtenemos el Context-Specific
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

        // Sacamos la secuencia de dentro Context-Specific, que es ya el SignedData
        final SignedData signedData = SignedData.getInstance(doj.getObject());

        // Obtenemos el SignerInfos (conjunto de SignerInfo) del SignedData
        final ASN1Set originalSignerInfosFromSignedData = signedData.getSignerInfos();

        // Anadimos los nuevos certificados a los ya existentes en el fichero de firmas
        // en un SET para anadirlo al SignedData final
        final ASN1Set certificates = CAdESMultiUtil.addCertificates(signedData, certChain);

        // Nuevo conjunto de SignerInfo, que es la base para un el nuevo SignerInfos (SET de muchos SignerInfo),
        // los antiguos mas los nuevos
        final ASN1EncodableVector newSignerInfos = counterSignSignerInfos(
    		originalSignerInfosFromSignedData,
    		parameters,
    		key,
    		certChain,
            policy,
            signingCertificateV2,
            ctis,
            csm,
            targetType
        );

        // Construimos el Signed Data y lo devolvemos
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
     * @param parameters Par&aacute;metros necesarios para los <i>SignerInfo</i>.
     * @param key Clave privada a usar para firmar.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *                             para usar <i>SigningCertificateV1</i>.
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
     * @return Conjunto de <i>SignerInfo</i> con todos los nodos, los anteriores y las contrafirmas de estos.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores en el tratamiento de datos.
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private ASN1EncodableVector counterSignSignerInfos(final ASN1Set signerInfosRaiz,
                                                       final P7ContentSignerParameters parameters,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain,
                                                       final AdESPolicy policy,
                                                       final boolean signingCertificateV2,
                                                       final List<CommitmentTypeIndicationBean> ctis,
                                                       final CAdESSignerMetadata csm,
                                                       final CounterSignTarget targetType) throws NoSuchAlgorithmException,
                                                                                                  IOException,
                                                                                                  CertificateException,
                                                                                                  AOException {

        // Nuevo vector para los SignerInfo
    	final ASN1EncodableVector counterSigners = new ASN1EncodableVector();

    	// Recorremos todos los SignerInfo y llamamos a un metodo que los recorrera recursivamente cada uno
        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(
        		signerInfosRaiz.getObjectAt(i)
    		);
            counterSigners.add(
        		counterSignSignerInfo(
        			si,
        			parameters,
        			key,
        			certChain,
                    policy,
                    signingCertificateV2,
                    ctis,
                    csm,
                    targetType
                )
            );
        }
        return counterSigners;
    }

    /** Contrafirma <i>SignerInfo</i> de forma recursiva.
     * @param signerInfo <i>SignedInfo</i> ra&iacute;z.
     * @param parameters Par&aacute;metros necesarios para firmar los <i>SignerInfo</i>.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
     * @return <i>SignerInfo</i> ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private SignerInfo counterSignSignerInfo(final SignerInfo signerInfo,
                                             final P7ContentSignerParameters parameters,
                                             final PrivateKey key,
                                             final java.security.cert.Certificate[] certChain,
                                             final AdESPolicy policy,
                                             final boolean signingCertificateV2,
                                             final List<CommitmentTypeIndicationBean> ctis,
                                             final CAdESSignerMetadata csm,
                                             final CounterSignTarget targetType) throws NoSuchAlgorithmException,
                                                                                        IOException,
                                                                                        CertificateException,
                                                                                        AOException {
    	// Base para el nuevo SET de SignerInfos
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();

        // Es hoja?
        boolean isLeaf = true;

        // Recorremos los atributos no firmados buscando si es contrafirma
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> unauthenticatedAttributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (unauthenticatedAttributes.hasMoreElements()) {
                final Attribute unauthenticatedAttribute = Attribute.getInstance(unauthenticatedAttributes.nextElement());
                // Si es una contrafirma hacemos la llamada recursiva
                if (PKCSObjectIdentifiers.pkcs_9_at_counterSignature.equals(unauthenticatedAttribute.getAttrType())) {

                	isLeaf = false;

                	// El atributo tiene dentro un SignerInfos, que es un SET de SignerInfo
                    final ASN1Set signerInfos = unauthenticatedAttribute.getAttrValues();

                    // Recorremos los SignerInfo del SignerInfos de forma recursiva
                    final Enumeration<?> eAtributesData = signerInfos.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                    	final SignerInfo si;
                    	try {
                    		si = SignerInfo.getInstance(eAtributesData.nextElement());
                    	}
                    	catch(final Exception e) {
                    		// Podemos encontrar objetos que no sean un SignerInfo
                    		continue;
                    	}
                        signerInfosU.add(
                    		counterSignSignerInfo(
                				si,
                				parameters,
                				key,
                				certChain,
                				policy,
                				signingCertificateV2,
                				ctis,
                				csm,
                				targetType
            				)
                		);
                    }
                }

                // Puede ser un SignerInfo de tipo sello, que no se contrafirma
                else if (PKCSObjectIdentifiers.id_aa_signatureTimeStampToken.equals(unauthenticatedAttribute.getAttrType())) {
                    signerInfosU.add(unauthenticatedAttribute);
                }

            }

        }

        // Vuelta de la recursividad, anadimos la contrafirma
        if (CounterSignTarget.TREE.equals(targetType) || CounterSignTarget.LEAFS.equals(targetType) && isLeaf) {
			signerInfosU.add(
				signSignerInfo(
					parameters.getSignatureAlgorithm(),
					signerInfo,
					key,
					certChain,
					policy,
					signingCertificateV2,
					ctis,
					csm
				)
			);
        }

		return new SignerInfo(
			signerInfo.getSID(),
		    signerInfo.getDigestAlgorithm(),
		    signerInfo.getAuthenticatedAttributes(),
		    signerInfo.getDigestEncryptionAlgorithm(),
		    signerInfo.getEncryptedDigest(),
		    new DERSet(
	    		new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU)) // Se marca como contrafirma en sus atributos no firmados
    		)
		);

    }

    /** Realiza realmente la operaci&oacute;n criptogr&aacute;fica de firma para generar finalmente el <i>SignerInfo</i>.
     * @param signatureAlgorithm Algoritmo de firma a usar.
     * @param si SignerInfo a firmar (se obtiene la huella digital almacenada en &eacute;l y se firma).
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param policy Pol&iacute;tica de firma.
     * @param signingCertificateV2 <code>true</code> si se desea usar <i>SigningCertificateV2</i>, <code>false</code>
     *        para usar <i>SigningCertificateV1</i>.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @return <i>SignerInfo</i> contrafirmado.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados. */
    private SignerInfo signSignerInfo(final String signatureAlgorithm,
                                                 final SignerInfo si,
                                                 final PrivateKey key,
                                                 final java.security.cert.Certificate[] certChain,
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
             null, // En contrafirma el ContentDescription no se pone
             ctis,
             csm,
             true  // Es contrafirma
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
