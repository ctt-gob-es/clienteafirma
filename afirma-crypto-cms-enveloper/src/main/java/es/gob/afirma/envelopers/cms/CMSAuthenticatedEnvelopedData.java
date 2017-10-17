/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERPrintableString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.DERUTCTime;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.AuthEnvelopedData;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.OriginatorInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;


/** Clase que implementa firma digital PKCS#7/CMS AuthenticatedEnvelopedData (RFC
 * 5083) La Estructura del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *
 *  id-ct-authEnvelopedData OBJECT IDENTIFIER ::= { iso(1)
 *         member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9)
 *         smime(16) ct(1) 23 }
 *
 *  The authenticated-data content type shall have ASN.1 type
 *  AuthenticatedEnvelopedData:
 *
 *      AuthEnvelopedData ::= SEQUENCE {
 *       version CMSVersion,
 *       originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
 *       recipientInfos RecipientInfos,
 *       authEncryptedContentInfo EncryptedContentInfo,
 *       authAttrs [1] IMPLICIT AuthAttributes OPTIONAL,
 *       mac MessageAuthenticationCode,
 *       unauthAttrs [2] IMPLICIT UnauthAttributes OPTIONAL }
 *
 *     AuthAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *     UnauthAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *     MessageAuthenticationCode ::= OCTET STRING
 *
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje AuthenticatedEnvelopedData de SpongyCastle. */

public final class CMSAuthenticatedEnvelopedData {

	private CMSAuthenticatedEnvelopedData() {
		// No permitimos la instanciacion
	}

    /** Genera una estructura PKCS#7 <code>AuthenticatedEnvelopedData</code>.
     * @param parameters Par&aacute;metros necesarios que contienen tanto la firma del
     *                   archivo a firmar como los datos del firmante.
     * @param signerCertificateChain Cadena de certificados del firmante.
     * @param autenticationAlgorithm Algoritmo de autenticacion
     * @param config Configuraci&oacute;n del algoritmo para firmar
     * @param certDest Certificado del destino al cual va dirigido la firma.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param applySigningTime Si se aplica la hora de firma o no.
     * @param atrib Atributos firmados opcionales.
     * @param uatrib Atributos no autenticados firmados opcionales.
     * @param keySize Tama&ntilde;o de la clave AES.
     * @return Firma de tipo AuthenticatedData.
     * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *                     datos
     * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de
     *                                      firma.
     * @throws NoSuchAlgorithmException Si no se encuentra un algoritmo v&aacute;lido.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario. */
    static byte[] genAuthenticatedEnvelopedData(final P7ContentSignerParameters parameters,
    		                                    final X509Certificate[] signerCertificateChain,
	                                        	final String autenticationAlgorithm,
	                                        	final AOCipherConfig config,
	                                        	final X509Certificate[] certDest,
	                                        	final String dataType,
	                                        	final boolean applySigningTime,
	                                        	final Map<String, byte[]> atrib,
	                                        	final Map<String, byte[]> uatrib,
	                                        	final Integer keySize) throws IOException,
	                                                                             CertificateEncodingException,
	                                                                             NoSuchAlgorithmException,
	                                                                             InvalidKeyException,
	                                                                             NoSuchPaddingException,
	                                                                             InvalidAlgorithmParameterException,
	                                                                             IllegalBlockSizeException,
	                                                                             BadPaddingException {
        final SecretKey cipherKey = Utils.initEnvelopedData(config, keySize);

        // Ya que el contenido puede ser grande, lo recuperamos solo una vez
        final byte[] content2 = parameters.getContent();

        // 1. ORIGINATORINFO
        // obtenemos la lista de certificados
        final ASN1Set certificates = Utils.fetchCertificatesList(signerCertificateChain);
        ASN1Set certrevlist = null;

        OriginatorInfo origInfo = null;
        if (signerCertificateChain.length != 0) {
            // introducimos una lista vacia en los CRL ya que no podemos
            // modificar el codigo de bc.
            certrevlist = EvelopUtils.createBerSetFromList(new ArrayList<ASN1Encodable>());
            origInfo = new OriginatorInfo(certificates, certrevlist);
        }

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(content2, config, certDest, cipherKey);

        // 4. ATRIBUTOS FIRMADOS
        final ASN1Set authAttr = generateSignedAtt(dataType, applySigningTime, atrib);

        // 5. MAC
        final byte[] mac = Utils.genMac(autenticationAlgorithm, genPack(authAttr.getEncoded(ASN1Encoding.DER), content2), cipherKey);

        // 6. ATRIBUTOS NO FIRMADOS.
        final ASN1Set unAuthAttr = Utils.generateUnsignedAtt(uatrib);

        // construimos el Authenticated data y lo devolvemos
        return new ContentInfo(
    		PKCSObjectIdentifiers.id_ct_authEnvelopedData,
    		new AuthEnvelopedData(origInfo, // originatorInfo,
            new DERSet(infos.getRecipientInfos()), // recipientInfos,
            infos.getEncInfo(), // authEncryptedContentInfo,
            authAttr, // authAttrs
            new DEROctetString(mac), // MAC
            unAuthAttr // unauthAttrs
            )
		).getEncoded(ASN1Encoding.DER);

    }

    private static byte[] genPack(final byte[] parte1, final byte[] parte2) {
        final byte[] pack = new byte[parte1.length + parte2.length];
        System.arraycopy(parte1, 0, pack, 0, parte1.length);
        System.arraycopy(parte2, 0, pack, parte1.length, parte2.length);
        return pack;
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param datatype
     *        Identifica el tipo del contenido a firmar.
     * @param signingTime
     *        Introducir la hora de firma (tomada del sistema)
     * @param atrib
     *        Lista de atributos firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos firmados de la firma. */
    private static ASN1Set generateSignedAtt(final String datatype,
    		                                 final boolean signingTime,
    		                                 final Map<String, byte[]> atrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        contexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new ASN1ObjectIdentifier(datatype))));

        // fecha de firma
        if (signingTime) {
            contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));
        }

        // agregamos la lista de atributos a mayores.
        if (atrib.size() != 0) {

            final Iterator<Map.Entry<String, byte[]>> it = atrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
                	  // el oid
	                  new ASN1ObjectIdentifier(e.getKey().toString()),
	                  // el array de bytes en formato string
	                  new DERSet(new DERPrintableString(new String(e.getValue())))
                ));
            }

        }

        return EvelopUtils.getAttributeSet(new AttributeTable(contexExpecific));
    }

    /*************************************************************************/
    /**************** Metodos auxiliares de cifrado **************************/
    /*************************************************************************/

    /** M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre
     * de tipo AuthenticatedEnvelopedData.
     * @param data
     *        Datos CMS de tipo AuthenticatedEnvelopedData.
     * @param signerCertificateChain
     *        Cadena de certificados a agregar.
     * @return La nueva firma AuthenticatedEnvelopedData con los remitentes que
     *         ten&iacute;a (si los tuviera) con la cadena de certificados
     *         nueva.
     * @throws IOException Cuando hay errores de lectura o escritura de datos
     * @throws CertificateEncodingException Si hay alg&uacute;n certificado inv&aacute;lido en la cadena */
    public static byte[] addOriginatorInfo(final byte[] data, final X509Certificate[] signerCertificateChain) throws IOException, CertificateEncodingException {

    	final ASN1Sequence dsq;
    	try (
			final ASN1InputStream is = new ASN1InputStream(data);
		) {
	        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
	        dsq = (ASN1Sequence) is.readObject();
    	}

        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID Data
        final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();

        if (doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)) {
            // Contenido de Data
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            final AuthEnvelopedData authEnv = AuthEnvelopedData.getInstance(doj.getObject());

            // Obtenemos los originatorInfo
            OriginatorInfo origInfo = authEnv.getOriginatorInfo();
            ASN1Set certs = null;
            if (origInfo != null) {
                certs = origInfo.getCertificates();
            }

            final OriginatorInfo origInfoChecked = Utils.checkCertificates(signerCertificateChain, certs);
            if (origInfoChecked != null) {
                origInfo = origInfoChecked;
            }


            // Se crea un nuevo AuthenticatedEnvelopedData a partir de los
            // datos anteriores con los nuevos originantes.
            return new ContentInfo(
        		PKCSObjectIdentifiers.id_ct_authEnvelopedData, new AuthEnvelopedData(origInfo, // OriginatorInfo
                                                                                                         authEnv.getRecipientInfos(), // ASN1Set
                                                                                                         authEnv.getAuthEncryptedContentInfo(),
                                                                                                         authEnv.getAuthAttrs(),
                                                                                                         authEnv.getMac(),
                                                                                                         authEnv.getUnauthAttrs())).getEncoded(ASN1Encoding.DER);
        }

        return null;
    }
}
