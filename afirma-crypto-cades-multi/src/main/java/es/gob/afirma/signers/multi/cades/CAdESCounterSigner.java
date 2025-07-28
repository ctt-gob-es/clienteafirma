/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.multi.cades;


import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;

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
import org.spongycastle.asn1.x509.TBSCertificate;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.SigningLTSException;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSimpleSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.cades.CAdESParameters;
import es.gob.afirma.signers.cades.CAdESUtils;
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

    private final AOSimpleSigner ss;

    /**
     * Construye el firmador con un firmador PKCS#1 concreto.
     * @param p1Signer Firmador para la generaci&oacute;n del PKCS#1.
     */
    CAdESCounterSigner(final AOSimpleSigner p1Signer) {
    	this.ss = p1Signer != null ? p1Signer : new AOPkcs1Signer();
    }

    /** Crea una contrafirma a partir de los datos del firmante, el archivo que se firma y
     * del archivo que contiene las firmas.
     * @param algorithm Algoritmo de firma.
     * @param signature Archivo que contiene las firmas.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
	 * @param key Clave privada usada para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param config Configuraci&oacute;n de la firma a generar.
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
                       final CAdESParameters config) throws IOException,
                                                                                    NoSuchAlgorithmException,
                                                                                    CertificateException,
                                                                                    AOException {
        // Leemos los datos originales (la firma que nos llega)
    	final ASN1Sequence dsq;
    	try (
			final ASN1InputStream is = new ASN1InputStream(signature);
		) {
    		dsq = (ASN1Sequence) is.readObject();
    	}
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
            config,
            targetType
        );

        // Construimos y devolvemos la nueva firma (atributo identificador del signedData mas el propio signedData).
        // Esta firma sera igual a la anterior pero con el conjunto de certificados actualizados con los nuevos y la
        // nueva estructura de SignerInfos. Incluimos el listado de CRLs, aunque este puede no estar completo
        return new ContentInfo(
    		PKCSObjectIdentifiers.signedData,
    		new SignedData(
				signedData.getDigestAlgorithms(),
                signedData.getEncapContentInfo(),
                certificates,
                signedData.getCRLs(),
                new DERSet(newSignerInfos)
			)
		).getEncoded(ASN1Encoding.DER);

    }

    /** Contrafirma el &aacute;rbol completo de forma recursiva (todos los nodos).
     * @param signerInfosRaiz <i>SignerInfos</i> con los <i>SignerInfo</i> que se deben firmar.
     * @param algorithm Algoritmo de firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param config Configuraci&oacute;n con el detalle de la firma a generar.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo y s&oacute;lo los nodos hoja.
     * @return Conjunto de <i>SignerInfo</i> con todos los nodos, los anteriores y las contrafirmas de estos.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores en el tratamiento de datos.
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException En caso de cualquier otro tipo de error */
    private ASN1EncodableVector counterSignSignerInfos(final ASN1Set signerInfosRaiz,
                                                       final String algorithm,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain,
                                                       final CAdESParameters config,
                                                       final CounterSignTarget targetType) throws NoSuchAlgorithmException,
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
                    config,
                    targetType
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
     * @param config Configuraci&oacute;n con los detalles internos para la composici&oacute;n de la firma.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
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
                                             final CAdESParameters config,
                                             final CounterSignTarget targetType
                                             ) throws NoSuchAlgorithmException,
                                                                                                          IOException,
                                                                                                          CertificateException,
                                                                                                          AOException {
    	// Base para el nuevo SET de SignerInfos
    	final List<Attribute> newUnauthenticatedAttributesList = new ArrayList<>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();

        // Es hoja?
        boolean isLeaf = true;

        // Comprobamos si tiene atributos no firmados y los recorremos
        if (signerInfo.getUnauthenticatedAttributes() != null) {
        	final Properties xParams = config.getExtraParams();
            final Enumeration<?> unauthenticatedAttributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (unauthenticatedAttributes.hasMoreElements()) {

            	final Attribute unauthenticatedAttribute = Attribute.getInstance(unauthenticatedAttributes.nextElement());

                // Si es un atributo no soportado, no podremos realizar la firma y se lanzara una excepcion

            	final String allowSignLts = xParams != null ?
            			xParams.getProperty(CAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES) : null;
        		if (allowSignLts == null || !Boolean.parseBoolean(allowSignLts)) {
        			try {
        				CAdESMultiUtil.checkUnsupported(unauthenticatedAttribute.getAttrType());
        			}
        			catch (final SigningLTSException e) {
        				// Si se indico expresamente que no se debia permitir la cofirma de
        				// firmas de archivo, se lanza una excepcion bloqueando la ejecucion.
        				// Si no, se informa debidamente para que se consulte al usuario
        				if (allowSignLts != null) {
        					throw new AOException(e.getMessage());
        				}
        				throw e;
        			}
        		}

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
                				config,
                				targetType
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
					config
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

    /**
     * Realiza realmente la operaci&oacute;n criptogr&aacute;fica de firma para generar finalmente el <i>SignerInfo</i>.
     * @param si SignerInfo a firmar (se obtiene la huella digital almacenada en &eacute;l y se firma).
     * @param signatureAlgorithm Algoritmo de firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param config Configuraci&oacute;n con el detalle de la firma a generar.
     * @return <i>SignerInfo</i> contrafirmado.
     * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos necesarios.
     * @throws java.io.IOException Cuando hay errores de entrada / salida
     * @throws CertificateException Cuando hay problemas con los certificados proporcionados.
     * @throws AOException Cuando ocurre un error durante la generacion de PKCS#1 de la firma.
     */
    private SignerInfo signSignerInfo(
    		final SignerInfo si,
    		final String signatureAlgorithm,
    		final PrivateKey key,
    		final java.security.cert.Certificate[] certChain,
    		final CAdESParameters config
    		) throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

    	// Realizamos los cambios de configuracion que corresponden a las contrafirmas

        // La firma se realiza sobre una firma previa
        config.setContentData(si.getEncryptedDigest().getOctets());

        // Dejamos que el hash se calcule internamente en base a los datos que acabamos de introducir
        config.setDataDigest(null);

        // En las contrafirmas BES, el tipo declarado en el contentHint siempre
        // sera Data y no contendra descripcion
        if (AOSignConstants.SIGN_PROFILE_ADVANCED.equals(config.getProfileSet())) {
        	config.setContentTypeOid(PKCSObjectIdentifiers.data.toString());
        	config.setContentDescription(null);
        }
        // En las contrafirmas Baseline, las contrafirmas no deben incluir el
        // atributo ContentHint
        else if (AOSignConstants.SIGN_PROFILE_BASELINE.equals(config.getProfileSet())){
        	config.setContentTypeOid(null);
        	config.setContentDescription(null);
        }

        // authenticatedAttributes
        final ASN1EncodableVector signedAttributes = CAdESUtils.generateSignedAttributes(
             certChain[0],
             config,
             true);	// Es contrafirma

        final ASN1Set signedAttr = SigUtils.getAttributeSet(new AttributeTable(signedAttributes));

        final ASN1OctetString signValue = new DEROctetString(
        		pkcs1Sign(
        				signedAttr.getEncoded(ASN1Encoding.DER),
        				signatureAlgorithm,
        				key,
        				certChain,
        				config.getExtraParams()
        				)
        		);

        // Identificamos el algoritmo
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);

        // AlgorithmIdentifier
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        final String keyType = certChain[0].getPublicKey().getAlgorithm();

		final String algorithmName = AOSignConstants.composeSignatureAlgorithmName(digestAlgorithm, keyType);

		// digEncryptionAlgorithm
		final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(algorithmName));

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final TBSCertificate tbs = TBSCertificate.getInstance(
    		ASN1Primitive.fromByteArray(((X509Certificate)certChain[0]).getTBSCertificate())
		);
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(
    		X500Name.getInstance(tbs.getIssuer()),
    		tbs.getSerialNumber().getValue()
		);

        final SignerIdentifier identifier = new SignerIdentifier(encSid);


        return new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, signValue, null);

    }

    private static List<SignerInfo> getSignerInfoFromUnauthenticatedAttributes(final Attribute unauthenticatedAttribute) {

    	final ArrayList<SignerInfo> signerInfos = new ArrayList<>();

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
     * @param extraParams Par&aacute;metros de configuraci&oacute;n de firma.
     * @return Firma de los atributos.
     * @throws AOException En caso de cualquier otro tipo de error */
    private byte[] pkcs1Sign(final byte[] data,
    		                 final String signatureAlgorithm,
    		                 final PrivateKey key,
    		                 final java.security.cert.Certificate[] certChain,
    		                 final Properties extraParams) throws AOException {
    	try {
			return this.ss.sign(data, signatureAlgorithm, key, certChain, extraParams);
		}
    	catch (final IOException e) {
			throw new AOException(
				"Error en la firma PKCS#1 de la contrafirma CAdES: " + e, e //$NON-NLS-1$
			);
		}
    }
}
