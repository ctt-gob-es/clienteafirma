/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer.cades;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

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
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.cades.CAdESParameters;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.multi.cades.CAdESMultiUtil;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.SigUtils;
import es.gob.afirma.triphase.signer.processors.TriPhaseUtil;

/** Contrafirmador CAdES trif&aacute;sico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AOCAdESTriPhaseCounterSigner {

	/** Etiqueta de prefirma en el XML de sesi&oacute;n trif&aacute;sica. */
	private static final String TRIPHASE_PARAM_PRE = "PRE"; //$NON-NLS-1$

	/** Etiqueta que indica si la postfirma requiere la prefirma. */
	private static final String TRIPHASE_PARAM_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$

//	/** Etiqueta de identificador en el XML de sesi&oacute;n trif&aacute;sica. */
//	private static final String TRIPHASE_PARAM_ID = "ID"; //$NON-NLS-1$

	/** Etiqueta de firma PKCS#1 en el XML de sesi&oacute;n trif&aacute;sica. */
	private static final String TRIPHASE_PARAM_PKCS1 = "PK1"; //$NON-NLS-1$


	/** Algoritmo de huella digital que se utilizar&aacute; internamente. */
	private static final String DIGEST_ALGORITHM = "SHA-256"; //$NON-NLS-1$

	private static MessageDigest md = null;


	private AOCAdESTriPhaseCounterSigner() {
		// No permitimos instanciar
	}

	/**
	 * Realiza la pre-contrafirma trif&aacute;sica.
	 * B&aacute;sicamente, se realiza una contrafirma completa usando un firmador PKCS#1 <i>falso</i>,
	 * que almacena los octetos a firmar e introduce en vez de las firmas,
     * @param sign Flujo de lectura de los datos a firmar.
     * @param algorithm Algoritmo a usar para la firma (SHA1withRSA, SHA512withRSA, etc.).
     * @param targetType Tipo de objetivo de la contrafirma.
     * @param targets Informaci&oacute;n complementaria seg&uacute;n el tipo de objetivo de la contrafirma.
     * @param certChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales para la contrafirma.
	 * @param date Fecha de la contrafirma.
     * @return Prefirma en formato XML.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma.
     */
	public static TriphaseData preCountersign(final byte[] sign,
                                        final String algorithm,
                                        final CounterSignTarget targetType,
                                        final Object[] targets,
                                        final java.security.cert.Certificate[] certChain,
                                        final Properties xParams,
                                        final Date date) throws AOException,
                                                                IOException {
		final TriphaseData triphaseData = new TriphaseData();

		final SignedData signedData = loadSignature(sign);

        // Obtenemos el SignerInfos (conjunto de SignerInfo) del SignedData
        final ASN1Set signerInfos = signedData.getSignerInfos();


        final Set<SignerInfo> selectedSignerInfos = selectSignerInfosToSign(signerInfos, targetType);

        for (final SignerInfo signerInfo : selectedSignerInfos) {
        	final String id = buildSignerInfoId(signerInfo);
        	final String signatureId = TriPhaseUtil.getSignatureId(xParams);
        	final byte[] preSignature = generateSignedAttributes(signerInfo, algorithm, certChain[0], xParams, date);

        	// Componemos la operacion de firma trifasica individual
        	final Map<String, String> triParams = new HashMap<>();
        	triParams.put(TRIPHASE_PARAM_NEED_PRE, Boolean.TRUE.toString());
        	triParams.put(TRIPHASE_PARAM_PRE, Base64.encode(preSignature, false));

        	triphaseData.addSignOperation(new TriSign(triParams, id, signatureId));
        }

        // Devolvemos el conjunto de contrafirmas
		return triphaseData;
    }

	/**
	 * Obtiene la seleccion de firmas que se deben contrafirmar.
	 * @param signerInfos Firmas/cofirmas de la firma (puede que contrafirmas asociadas).
	 * @param targetType Identificador del tipo de firmas que se deben selecionar.
	 * @return Listado de firmas que se deben contrafirmar.
	 * @throws IllegalArgumentException Cuando se indique un par&aacute;metro
	 * {@code targetType} con un valor no soportado.
	 */
	private static Set<SignerInfo> selectSignerInfosToSign(final ASN1Set signerInfos, final CounterSignTarget targetType) {

		final Set<SignerInfo> selectedSignerInfos = new HashSet<>();
		if (targetType == CounterSignTarget.TREE) {
			selectSignerInfos(signerInfos, selectedSignerInfos, false);
		}
		else if (targetType == CounterSignTarget.LEAFS) {
			selectSignerInfos(signerInfos, selectedSignerInfos, true);
		}
		else {
			throw new IllegalArgumentException("No se soporta la contrafirma de la seleccion de firmas indificada"); //$NON-NLS-1$
		}

		return selectedSignerInfos;
	}

	/**
	 * Completa el listado de firmas seleccionadas seleccionando todas las del objeto proporcionado
	 * y las de sus contrafirmas o s&oacute;lo aquellas que no tengan contrafirmas (nodos hoja).
	 * @param signerInfos Conjunto de firmas del documento.
	 * @param selectedSignerInfos Listado de firmas ya seleccionadas y a las que agregar las del
	 * objeto proporcionado.
	 * @param onlyLeafs Indica si s&oacute;lo se deben seleccionar los nodos hoja ({@code true}) o
	 * si deben seleccionarse todas ({@code false}).
	 */
	private static void selectSignerInfos(final ASN1Set signerInfos, final Set<SignerInfo> selectedSignerInfos, final boolean onlyLeafs) {

		// Recorremos el conjunto de SignerInfo y agregamos cada elemento y las
		// contrafirmas de cada uno de ellos
        for (int i = 0; i < signerInfos.size(); i++) {
        	final SignerInfo si;
        	try {
        		si = SignerInfo.getInstance(signerInfos.getObjectAt(i));
        	}
        	catch (final Exception e) {
        		// Si el atributo no era un SignerInfo, pasamos al siguiente
        		continue;
        	}

        	// Valor bandera para marcar si la firma tenis contrafirmas
        	boolean hasCounterSigns = false;

            // Comprobamos si tiene atributos no firmados
            final ASN1Set unsignedAttributes = si.getUnauthenticatedAttributes();
            if (unsignedAttributes != null) {

            	// Recorremos los atributos no firmados buscando contrafirmas
            	final Enumeration<?> unauthenticatedAttributes = unsignedAttributes.getObjects();
                while (unauthenticatedAttributes.hasMoreElements()) {
                	// Si el atributo es una contrafirma, agregamos sus SignerInfo al listado
                	final Attribute unauthenticatedAttribute = Attribute.getInstance(unauthenticatedAttributes.nextElement());
                	if (CAdESMultiUtil.isCounterSignature(unauthenticatedAttribute.getAttrType())) {
                		hasCounterSigns = true;
                    	final ASN1Set counterSignSignersInfo = unauthenticatedAttribute.getAttrValues();
                    	selectSignerInfos(counterSignSignersInfo, selectedSignerInfos, onlyLeafs);
                    }
                }
            }

            // Agregamos la firma en cuestion si es una firma hoja (no tiene contrafirmas)
            // o si se seleccionan todas las firmas
            if (!hasCounterSigns || !onlyLeafs) {
            	selectedSignerInfos.add(si);
            }
        }
	}

	/**
	 * Obtiene un identificador asociado a una firma concreta.
	 * @param signerInfo Firma concreta del documento de firma.
	 * @return Identificador que se le va a asociar a la firma.
	 * @throws AOException Cuando no se puede generar el identificador.
	 */
	private static String buildSignerInfoId(final SignerInfo si) throws AOException {

		final byte[] siValue = si.getEncryptedDigest().getOctets();
		byte[] encodedId;
		try {
			encodedId = digest(siValue);
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido generar un identificador asociado a la firma", e); //$NON-NLS-1$
		}
		return Base64.encode(encodedId);
	}

	/**
	 * Calcula la huella digital de unos datos.
	 * @param data Datos de los que calcular la huella.
	 * @return Huella digital.
	 * @throws NoSuchAlgorithmException Si el algoritmo por defecto no existiese.
	 */
	private static byte[] digest(final byte[] data) throws NoSuchAlgorithmException {

		if (md == null) {
			md = MessageDigest.getInstance(DIGEST_ALGORITHM);
		}
		return md.digest(data);
	}

	private static SignedData loadSignature(final byte[] signature) throws IOException, AOFormatFileException {

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
        return SignedData.getInstance(pkcs7RootContextSpecificZero.getObject());
	}

	private static byte[] generateSignedAttributes(final SignerInfo si, final String algorithm,
			final Certificate signingCert, final Properties xParams, final Date date) throws AOException, IOException {


		// Los datos que se van a firmar son los de la firma que contrafirmamos
		final CAdESParameters config = CAdESParameters.load(
				null,
				algorithm,
				xParams);

		// Si se indico que debia generarse la contrafirma con una fecha
		// concreta, la usamos
		if (date != null) {
			config.setSigningTime(date);
		}

		// Configuramos los datos firmados (la firma que estamos firmando)
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

		// Generamos los signed attributtes de la firma
		ASN1EncodableVector signedAttributes;
		try {
			signedAttributes = CAdESUtils.generateSignedAttributes(
					signingCert,
					config,
					true  // Es contrafirma
					);
		} catch (final CertificateEncodingException e) {
			throw new AOException("No se ha podido codificar el certificado proporcionadoSe ha encontrado un algoritmo no valido", e); //$NON-NLS-1$
		} catch (final NoSuchAlgorithmException e) {
			throw new AOException("Se ha proporcionado un algoritmo de huella no soportado", e); //$NON-NLS-1$
		}

		// Devolvemos la prefirma codificada
		return SigUtils.getAttributeSet(new AttributeTable(signedAttributes)).getEncoded(ASN1Encoding.DER);
	}

	/**
	 * Realiza la post-contrafirma trif&aacute;sica.
	 * @param sign Flujo de lectura de los datos a firmar.
     * @param signAlgorithm Algoritmo a usar para la firma (SHA1withRSA, SHA512withRSA, etc.).
     * @param targetType Tipo de objetivo de la contrafirma.
     * @param targets Informaci&oacute;n complementaria seg&uacute;n el tipo de objetivo de la contrafirma.
     * @param certChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales para la contrafirma.
	 * @param triphaseData Estado intermedio de las firmas.
     * @return Prefirma en formato XML.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma.
     */
	public static byte[] postCountersign(final byte[] sign,
                                        final String signAlgorithm,
                                        final CounterSignTarget targetType,
                                        final Object[] targets,
                                        final X509Certificate[] certChain,
                                        final Properties xParams,
                                        final TriphaseData triphaseData) throws AOException,
                                                                IOException {

		if (triphaseData == null || triphaseData.getSignsCount() == 0) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos y deben contener firmas"); //$NON-NLS-1$
		}

		// Obtenemos el objeto de firma que estamos contrafirmando
		final SignedData signedData = getSignedData(sign);

        // Obtenemos el identificador del certificado firmante
		final SignerIdentifier signerIdentifier = getSignerIdentifier(certChain[0]);

		// Identificamos los algoritmos que vamos a utilizar
		final String keyType = certChain[0].getPublicKey().getAlgorithm();
		final String digestAlgorithmName = AOSignConstants.getDigestAlgorithmName(signAlgorithm);
		final String algorithmName = AOSignConstants.composeSignatureAlgorithmName(digestAlgorithmName, keyType);
		final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithmName));
		final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(algorithmName));

		// Obtenemos un conjunto de certificados que auna los existentes en la firma y los del
		// nuevo firmante
        final ASN1Set certificates = addCertificates(signedData, certChain);

        // Obtenemos las firmas existentes
        final ASN1Set signatureSignedInfos = signedData.getSignerInfos();

        // Creamos el que sera el nuevo conjunto de SignerInfo, que sera el mismo conjunto actual pero con
        // los SignerInfo ya contrafirmados
        final ASN1Set newSignerInfos = counterSignSignerInfos(
    		signatureSignedInfos,
    		signerIdentifier,
    		digAlgId,
    		encAlgId,
            targetType,
            triphaseData
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
                newSignerInfos
			)
		).getEncoded(ASN1Encoding.DER);
    }

	/**
	 * Agrega los certificados de la cadena de certificacion del firmante al conjunto de certificados de
	 * la firma.
	 * @param signedData Firma a la que agregar los certificados.
	 * @param certChain Cadena de certificaci&oacute;n del certificado firmante.
	 * @return Conjunto de certificados.
	 * @throws AOException Cuando no se pueden codificar los certificados del firmante.
	 */
	private static ASN1Set addCertificates(final SignedData signedData, final X509Certificate[] certChain) throws AOException {
		ASN1Set certificates;
		try {
			certificates = CAdESMultiUtil.addCertificates(signedData, certChain);
		} catch (final Exception e) {
			throw new AOException("Error en la codificacion de los certificados de firma", e); //$NON-NLS-1$
		}
		return certificates;
	}

	/**
	 * Obtiene el SignedData con la informaci&oacute;n de la firma.
	 * @param sign Firma de la que obtener la informaci&acute;n de la firma.
	 * @return SignedData con la informaci&oacute;n de la firma.
	 * @throws AOFormatFileException Cuando la firma no tiene un formato soportado.
	 */
	private static SignedData getSignedData(final byte[] sign) throws AOFormatFileException {
		final ASN1Sequence dsq = ASN1Sequence.getInstance(sign);
		final Enumeration<?> e = dsq.getObjects();
		// Elemento con el OID SignedData
		final Object o = e.nextElement();
        if (!(o instanceof ASN1ObjectIdentifier) && ((ASN1ObjectIdentifier)o).equals(PKCSObjectIdentifiers.signedData)) {
			throw new AOFormatFileException("No se ha encontrado un SignedData en los datos a contrafirmar"); //$NON-NLS-1$
		}
		// SignedData
		final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
		return SignedData.getInstance(doj.getObject());
	}

	/**
	 * Obtiene el identificador del firmante.
	 * @param signerCertificate Certificado firmante.
	 * @return Identificador del firmante.
	 * @throws CertificateEncodingException Cuando no se puede decodificar el certificado.
	 * @throws AOException Cuando no se pueda decodificar el certificado.
	 */
	private static SignerIdentifier getSignerIdentifier(final X509Certificate signerCertificate) throws AOException {

		// Identificador del firmante ISSUER AND SERIAL-NUMBER
		ASN1Primitive encodedCert;
		try {
			encodedCert = ASN1Primitive.fromByteArray(signerCertificate.getTBSCertificate());
		}
		catch (final Exception e) {
			throw new AOException("Error en la codificacion del certificado", e); //$NON-NLS-1$
		}
		final TBSCertificate tbs = TBSCertificate.getInstance(encodedCert);
		final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(
				X500Name.getInstance(tbs.getIssuer()),
				tbs.getSerialNumber().getValue());
		return new SignerIdentifier(encSid);
	}

	/**
	 * Obtiene el listado de SignerInfo de la nueva firma, compuesto por los de la firma
	 * original y los de las contrafirmas generadas.
	 * @param signatureSignedInfos Conjunto de SignerInfo de la firma original.
	 * @param signerIdentifier Identificador del firmate de las contrafirmas.
	 * @param digAlgId Identificador del algoritmo de huella.
	 * @param encAlgId Identificador del algoritmo de firma/cifrado.
	 * @param targetType Indicador de cuales son las firmas que se deben contrafirmas (todas o
	 * s&oacute;lo las de los nodos hoja).
	 * @param triphaseData
	 * @return Conjunto de SignerInfo contrafirmados.
	 * @throws AOException Cuando ocurre un error al generar las contrafirmas.
	 * @throws IOException Cuando ocurre un error al leer los datos.
	 */
	private static ASN1Set counterSignSignerInfos(final ASN1Set signatureSignedInfos,
			final SignerIdentifier signerIdentifier, final AlgorithmIdentifier digAlgId,
			final AlgorithmIdentifier encAlgId, final CounterSignTarget targetType,
			final TriphaseData triphaseData) throws IOException, AOException {

        // Vector donde almacenaremos la nueva estructura de SignerInfo
    	final ASN1EncodableVector counterSigners = new ASN1EncodableVector();

    	// Recorremos todos los SignerInfo y llamamos a un metodo que los contrafirmara
        for (int i = 0; i < signatureSignedInfos.size(); i++) {
        	final ASN1Encodable encodedSignedInfo = signatureSignedInfos.getObjectAt(i);
            final SignerInfo si = SignerInfo.getInstance(encodedSignedInfo);
            counterSigners.add(
        		counterSignSignerInfo(
        			si,
        			signerIdentifier,
        			digAlgId,
        			encAlgId,
                    targetType,
                    triphaseData
                )
            );
        }

		return new DERSet(counterSigners);
	}

    /**
     * Contrafirma una rama de <i>SignerInfo</i>. Como resultado se devuelve un SignerInfo igual
     * al procesado, con los mismos atributos no firmados mas el nuevo atributo de contrafirma si
     * tocaba firmar este SignerInfo. Cada nodo de contrafirma que ya existiese habra sido tambien
     * procesado para agregar contrafirmas. Si se encontrase un atributo no firmado no soportado,
     * se suspenderia todo el proceso de firma.
     * @param signerInfo <i>SignedInfo</i> ra&iacute;z.
     * @param signerIdentifier Identificador del firmate de las contrafirmas.
	 * @param digAlgId Identificador del algoritmo de huella.
	 * @param encAlgId Identificador del algoritmo de firma/cifrado.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
     * @param triphaseData
     * @return <i>SignerInfo</i> ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws IOException Cuando hay errores de entrada / salida
     * @throws AOException En caso de cualquier otro tipo de error
     */
	private static SignerInfo counterSignSignerInfo(final SignerInfo signerInfo,
			final SignerIdentifier signerIdentifier, final AlgorithmIdentifier digAlgId,
			final AlgorithmIdentifier encAlgId, final CounterSignTarget targetType,
			final TriphaseData triphaseData
										) throws IOException,
											AOException {
    	// Base para el nuevo SET de SignerInfos
    	final List<Attribute> newUnauthenticatedAttributesList = new ArrayList<>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();

        // Es hoja?
        boolean isLeaf = true;

        // Comprobamos si tiene atributos no firmados y los recorremos contrafirmando aquellas
        // contrafirmas que correspondan
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> unauthenticatedAttributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (unauthenticatedAttributes.hasMoreElements()) {

            	final Attribute unauthenticatedAttribute = Attribute.getInstance(unauthenticatedAttributes.nextElement());

                // Si es una contrafirma, la analizamos para saber si procesar su contenido y agregar nuevas contrafirmas
                if (CAdESMultiUtil.isCounterSignature(unauthenticatedAttribute.getAttrType())) {

                	isLeaf = false;

                	// Obtenemos el listado de SignerInfos y los procesamos recursivamente
                	final List<SignerInfo> signerInfos = getSignerInfoFromUnauthenticatedAttributes(unauthenticatedAttribute);
                    for (final SignerInfo si : signerInfos) {
                        signerInfosU.add(
                    		counterSignSignerInfo(
                				si,
                				signerIdentifier,
                    			digAlgId,
                    			encAlgId,
                				targetType,
                				triphaseData
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
					signerIdentifier,
        			digAlgId,
        			encAlgId,
					triphaseData
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
     * Obtenemos los SignerInfo de las contrafirmas encontradas en los atributos no firmados.
     * @param unauthenticatedAttribute Atributos no firmados de una firma.
     * @return Listado de SignedInfo de contrafirmas.
     */
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

    /**
     * Realiza realmente la operaci&oacute;n criptogr&aacute;fica de firma para generar finalmente el <i>SignerInfo</i>.
     * @param si SignerInfo a firmar (se obtiene la huella digital almacenada en &eacute;l y se firma).
     * @param signerIdentifier Identificador del firmate de las contrafirmas.
	 * @param digAlgId Identificador del algoritmo de huella.
	 * @param encAlgId Identificador del algoritmo de firma/cifrado.
     * @param certChain Cadena de certificados del firmante.
     * @param triphaseData
     * @return <i>SignerInfo</i> contrafirmado.
     * @throws IOException Cuando hay errores de entrada / salida
     * @throws AOException Cuando se produce un error en la contrafirma.
     */
    private static SignerInfo signSignerInfo(
    		final SignerInfo si,
    		final SignerIdentifier signerIdentifier,
    		final AlgorithmIdentifier digAlgId,
			final AlgorithmIdentifier encAlgId,
    		final TriphaseData triphaseData
                                      ) throws IOException, AOException {

        // Obtenemos el identificador de la contrafirma de esa firma
        final String presignId = buildSignerInfoId(si);

        // Recuperamos de la informacion de firma trifasica la prefirma de esa contrafirma
        final List<TriSign> triSigns = triphaseData.getTriSigns(presignId);
        if (triSigns == null || triSigns.isEmpty()) {
        	throw new AOException("No se ha encontrado la informacion de prefirma de una de las firmas"); //$NON-NLS-1$
        }

        final ASN1Set signedAttr = getSignedAttributes(triSigns);
        final ASN1OctetString pkcs1 = getPkcs1(triSigns);

        return new SignerInfo(signerIdentifier, digAlgId, signedAttr, encAlgId, pkcs1, null);
    }

    /**
     * Extrae el PKCS#1 de la nueva contrafirma de entre los datos de firma trif&aacute;sica
     * suministrados.
     * @param triSigns Datos de firma trif&aacute;sica.
     * @return PKCS#1 de firma.
     * @throws AOException Si no se encontraron los datos o no estaban correctamente codificados.
     */
    private static ASN1OctetString getPkcs1(final List<TriSign> triSigns) throws AOException {
		final String pkcs1B64 = triSigns.get(0).getProperty(TRIPHASE_PARAM_PKCS1);
        final ASN1OctetString signValue;
        try {
        	final byte[] pkcs1 = Base64.decode(pkcs1B64);
        	signValue = new DEROctetString(pkcs1);
        }
        catch (final Exception ex) {
        	throw new AOException("No se ha recibido el PKCS#1 de la contrafirma o se ha indicado un valor incorrectamente codificado", ex); //$NON-NLS-1$
        }
		return signValue;

	}

    /**
     * Extrae el conjunto de atributos firmados (prefirma) de la nueva contrafirma de entre los
     * datos de firma trif&aacute;sica suministrados.
     * @param triSigns Datos de firma trif&aacute;sica.
     * @return Atributos firmados de firma.
     * @throws AOException Si no se encontraron los datos o no estaban correctamente codificados.
     */
	private static ASN1Set getSignedAttributes(final List<TriSign> triSigns) throws AOException {
		final String preSignB64 = triSigns.get(0).getProperty(TRIPHASE_PARAM_PRE);
        ASN1Set signedAttr;
        try {
        	final byte[] preSign = Base64.decode(preSignB64);
        	signedAttr = (ASN1Set) ASN1Primitive.fromByteArray(preSign);
        }
        catch (final Exception ex) {
        	throw new AOException("No se ha recibido la prefirma de la contrafirma o se ha indicado un valor incorrectamente codificado", ex); //$NON-NLS-1$
        }
        return signedAttr;
	}
}
