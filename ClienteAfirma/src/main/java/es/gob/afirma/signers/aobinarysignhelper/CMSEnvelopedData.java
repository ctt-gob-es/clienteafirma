/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.Map;
import java.util.logging.Logger;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.Oid;

import es.gob.afirma.ciphers.AOCipherConfig;
import es.gob.afirma.misc.AOCryptoUtil;

/**
 * Clase que implementa firma digital PKCS#7/CMS EnvelopedData. La Estructura
 * del mensaje es la siguiente:<br>
 * 
 * <pre>
 * <code>
 * 
 *  EnvelopedData ::= SEQUENCE {
 *      version CMSVersion,
 *      originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
 *      recipientInfos RecipientInfos,
 *      encryptedContentInfo EncryptedContentInfo,
 *      unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL
 *  }
 * 
 * </code>
 * </pre>
 * 
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje Data de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */

public final class CMSEnvelopedData {

	/**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de
	 * mostrarla directamente al usuario.
	 */
	private SecretKey cipherKey;

	/**
	 * M&eacute;todo que genera la firma de tipo EnvelopedData.
	 * 
	 * @param parameters
	 *            Par&aacute;metros necesarios para la generaci&oacute;n de este
	 *            tipo.
	 * @param config
	 *            Configuraci&oacute;n del algoritmo para firmar
	 * @param certDest
	 *            Certificado del destino al cual va dirigido la firma.
	 * @param dataType
	 *            Identifica el tipo del contenido a firmar.
	 * @param uatrib
	 *            Conjunto de atributos no firmados.
	 * 
	 * @return la firma de tipo EnvelopedData.
	 * @throws java.io.IOException
	 *             Si ocurre alg&uacute;n problema leyendo o escribiendo los
	 *             datos
	 * @throws java.security.cert.CertificateEncodingException
	 *             Si se produce alguna excepci&oacute;n con los certificados de
	 *             firma.
	 * @throws java.security.NoSuchAlgorithmException
	 *             Si no se soporta alguno de los algoritmos de firma o huella
	 *             digital
	 */
	public byte[] genEnvelopedData(P7ContentSignerParameters parameters,
			AOCipherConfig config, X509Certificate[] certDest, Oid dataType,
			Map<Oid, byte[]> uatrib) throws IOException,
			CertificateEncodingException, NoSuchAlgorithmException {
		this.cipherKey = Utils.initEnvelopedData(config, certDest);

		// Datos previos &uacute;tiles
		String digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(parameters
				.getSignatureAlgorithm());

		// 1. ORIGINATORINFO
		// obtenemos la lista de certificados
		X509Certificate[] signerCertificateChain = parameters
				.getSignerCertificateChain();
		ASN1Set certificates = Utils
				.fetchCertificatesList(signerCertificateChain);
		ASN1Set certrevlist = null;

		OriginatorInfo origInfo = null;
		if (signerCertificateChain.length != 0) {
			origInfo = new OriginatorInfo(certificates, certrevlist);
		}

		// 2. RECIPIENTINFOS
		Info infos = Utils.initVariables(parameters.getContent(), config,
				certDest, cipherKey);

		// 4. ATRIBUTOS
		ASN1Set unprotectedAttrs = null;
		unprotectedAttrs = Utils.generateSignerInfo(digestAlgorithm,
				parameters.getContent(), dataType, uatrib);

		// construimos el Enveloped Data y lo devolvemos
		return new ContentInfo(PKCSObjectIdentifiers.envelopedData,
				new EnvelopedData(origInfo, new DERSet(
						infos.getRecipientInfos()), infos.getEncInfo(),
						unprotectedAttrs)).getDEREncoded();
	}

	/**
	 * M&eacute;todo que genera la firma de tipo EnvelopedData.
	 * 
	 * @param data
	 *            Datos binarios a firmar
	 * @param digestAlg
	 *            Algoritmo de hash
	 * @param config
	 *            Configuraci&oacute;n del algoritmo para firmar
	 * @param certDest
	 *            Certificado del destino al cual va dirigido la firma.
	 * @param dataType
	 *            Identifica el tipo del contenido a firmar.
	 * @param uatrib
	 *            Conjunto de atributos no firmados.
	 * 
	 * @return la firma de tipo EnvelopedData.
	 * @throws java.io.IOException
	 * @throws java.security.cert.CertificateEncodingException
	 * @throws java.security.NoSuchAlgorithmException
	 */
	public byte[] genEnvelopedData(byte[] data, String digestAlg,
			AOCipherConfig config, X509Certificate[] certDest, Oid dataType,
			Map<Oid, byte[]> uatrib) throws IOException,
			CertificateEncodingException, NoSuchAlgorithmException {

		// Comprobamos que el archivo a tratar no sea nulo.
		this.cipherKey = Utils.initEnvelopedData(config, certDest);

		// Datos previos utiles
		String digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(digestAlg);

		// 1. ORIGINATORINFO
		OriginatorInfo origInfo = null;

		// 2. RECIPIENTINFOS
		Info infos = Utils.initVariables(data, config, certDest, cipherKey);

		// 4. ATRIBUTOS
		ASN1Set unprotectedAttrs = null;
		unprotectedAttrs = Utils.generateSignerInfo(digestAlgorithm, data,
				dataType, uatrib);

		// construimos el Enveloped Data y lo devolvemos
		return new ContentInfo(PKCSObjectIdentifiers.envelopedData,
				new EnvelopedData(origInfo, new DERSet(
						infos.getRecipientInfos()), infos.getEncInfo(),
						unprotectedAttrs)).getDEREncoded();

	}

	/**
	 * M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre
	 * de tipo envelopedData.
	 * 
	 * @param data
	 *            Datos CMS que admiten multiples remitentes/firmantes.
	 * @param signerCertificateChain
	 *            Cadena de certificados a agregar.
	 * @return La nueva firma enveloped con los remitentes que ten&iacute;a (si
	 *         los tuviera) con la cadena de certificados nueva.
	 */
	public byte[] addOriginatorInfo(byte[] data,
			X509Certificate[] signerCertificateChain) {
		// boolean isValid = false;
		byte[] retorno = null;

		try {
			ASN1InputStream is = new ASN1InputStream(data);
			// LEEMOS EL FICHERO QUE NOS INTRODUCEN
			ASN1Sequence dsq = (ASN1Sequence) is.readObject();
			Enumeration<?> e = dsq.getObjects();
			// Elementos que contienen los elementos OID Data
			DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
			if (doi.equals(PKCSObjectIdentifiers.envelopedData)) {
				// Contenido de Data
				ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

				EnvelopedData ed = new EnvelopedData(
						(ASN1Sequence) doj.getObject());

				// Obtenemos los originatorInfo
				OriginatorInfo origInfo = ed.getOriginatorInfo();
				ASN1Set certs = null;
				if (origInfo != null) {
					certs = origInfo.getCertificates();
				}

				// Si no hay certificados, se deja como esta.
				origInfo = Utils.checkCertificates(signerCertificateChain,
						origInfo, certs);

				// Se crea un nuevo EnvelopedData a partir de los datos
				// anteriores con los nuevos originantes.
				retorno = new ContentInfo(PKCSObjectIdentifiers.envelopedData,
						new EnvelopedData(origInfo, ed.getRecipientInfos(),
								ed.getEncryptedContentInfo(),
								ed.getUnprotectedAttrs())).getDEREncoded();
			}
		} catch (Exception ex) {
			Logger.getLogger("es.gob.afirma").severe(
					"Error durante el proceso de insercion: " + ex);
		}

		return retorno;
	}
}