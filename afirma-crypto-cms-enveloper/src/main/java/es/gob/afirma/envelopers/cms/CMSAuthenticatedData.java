/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
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
import org.spongycastle.asn1.BEROctetString;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERPrintableString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.DERTaggedObject;
import org.spongycastle.asn1.DERUTCTime;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.AuthenticatedData;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.OriginatorInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x500.style.RFC4519Style;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.cms.CMSException;
import org.spongycastle.cms.CMSProcessable;
import org.spongycastle.cms.CMSProcessableByteArray;

import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Clase que implementa firma digital PKCS#7/CMS AuthenticatedData. La
 * Estructura del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *
 * id-ct-authData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
 *        us(840) rsadsi(113549) pkcs(1) pkcs-9(9) smime(16)
 *        ct(1) 2 }
 *
 *  The authenticated-data content type shall have ASN.1 type
 *  AuthenticatedData:
 *
 *     AuthenticatedData ::= SEQUENCE {
 *       version CMSVersion,
 *       originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
 *       recipientInfos RecipientInfos,
 *       macAlgorithm MessageAuthenticationCodeAlgorithm,
 *       digestAlgorithm [1] DigestAlgorithmIdentifier OPTIONAL,
 *       encapContentInfo EncapsulatedContentInfo,
 *       authAttrs [2] IMPLICIT AuthAttributes OPTIONAL,
 *       mac MessageAuthenticationCode,
 *       unauthAttrs [3] IMPLICIT UnauthAttributes OPTIONAL }
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
 * para crear un mensaje AuthenticatedData de SpongyCastle. */

 final class CMSAuthenticatedData {

	 private CMSAuthenticatedData() {
		 // No instanciable
	 }

    /** Genera una estructura PKCS#7 <code>AuthenticatedData</code>.
     * @param parameters Par&aacute;metros necesarios que contienen tanto la firma del
     *                    archivo a firmar como los datos del firmante.
     * @param signerCertChain Cadena de certificados del firmante.
     * @param autenticationAlgorithm Algoritmo para los codigos de autenticaci&oacute;n MAC
     * @param config Configuraci&oacute;n del algoritmo para firmar
     * @param certDest Certificado del destino al cual va dirigido la firma.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param applyTimestamp Si se aplica el Timestamp o no.
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
    static byte[] genAuthenticatedData(final P7ContentSignerParameters parameters,
    		                           final X509Certificate[] signerCertChain,
                                       final String autenticationAlgorithm,
                                       final AOCipherConfig config,
                                       final X509Certificate[] certDest,
                                       final String dataType,
                                       final boolean applyTimestamp,
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
        final ASN1Set certificates = Utils.fetchCertificatesList(signerCertChain);
        ASN1Set certrevlist = null;

        OriginatorInfo origInfo = null;
        if (signerCertChain.length != 0) {
            // introducimos una lista vacia en los CRL ya que no podemos
            // modificar el codigo de bc.
            final List<ASN1Encodable> crl = new ArrayList<>();
            certrevlist = EvelopUtils.createBerSetFromList(crl);
            origInfo = new OriginatorInfo(certificates, certrevlist);
        }

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(content2, config, certDest, cipherKey);

        // 3. MACALGORITHM
        final AlgorithmIdentifier macAlgorithm = EvelopUtils.makeAlgId(config.getAlgorithm().getOid());

        // 4. DIGESTALGORITMIDENTIFIER
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(parameters.getSignatureAlgorithm());
        final AlgorithmIdentifier digAlgId = EvelopUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));


        // 5. ENCAPSULATEDCONTENTINFO

        // si se introduce el contenido o no

        ContentInfo encInfo = null;
        final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(dataType);
        final ByteArrayOutputStream bOut = new ByteArrayOutputStream();
        final CMSProcessable msg = new CMSProcessableByteArray(content2);
        try {
            msg.write(bOut);
        }
        catch (final CMSException ex) {
            throw new IOException("Error en la escritura del procesable CMS: " + ex, ex); //$NON-NLS-1$
        }
        encInfo = new ContentInfo(contentTypeOID, new BEROctetString(bOut.toByteArray()));

        // 6. ATRIBUTOS FIRMADOS
        ASN1Set authAttr = null;
        authAttr = generateSignedAtt(signerCertChain[0], digestAlgorithm, content2, dataType, applyTimestamp, atrib);

        // 7. MAC
        final byte[] mac = Utils.genMac(autenticationAlgorithm, authAttr.getEncoded(ASN1Encoding.DER), cipherKey);

        // 8. ATRIBUTOS NO FIRMADOS.

        ASN1Set unAuthAttr = null;
        unAuthAttr = Utils.generateUnsignedAtt(uatrib);

        // construimos el Authenticated data y lo devolvemos
        return new ContentInfo(
    		PKCSObjectIdentifiers.id_ct_authData,
    		new AuthenticatedData(
				origInfo, // OriginatorInfo
                new DERSet(infos.getRecipientInfos()), // ASN1Set
                macAlgorithm, // macAlgorithm
                digAlgId, // AlgorithmIdentifier
                encInfo, // ContentInfo
                authAttr, // ASN1Set
                new DEROctetString(mac), // ASN1OctetString
                unAuthAttr // ASN1Set
			)
		).getEncoded(ASN1Encoding.DER);

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param cert
     *        Certificado necesario para la firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param datos
     *        Datos firmados.
     * @param datatype
     *        Identifica el tipo del contenido a firmar.
     * @param timestamp
     *        Introducir TimeStaming
     * @param atrib
     *        Lista de atributos firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos firmados de la firma.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se encuentra un algoritmo v&aacute;lido. */
    private static ASN1Set generateSignedAtt(final X509Certificate cert,
                                      final String digestAlgorithm,
                                      final byte[] datos,
                                      final String datatype,
                                      final boolean timestamp,
                                      final Map<String, byte[]> atrib) throws NoSuchAlgorithmException {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        contexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new ASN1ObjectIdentifier(datatype))));

        // fecha de firma
        if (timestamp) {
            contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));
        }

        // Si nos viene el hash de fuera no lo calculamos
        final byte[] md = MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(digestAlgorithm)).digest(datos);

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString(md.clone()))));

        // Serial Number
        // comentar lo de abajo para version del rfc 3852
        contexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

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
     * de tipo AuthenticatedData.
     * @param data
     *        fichero que tiene la firma.
     * @param signerCertificateChain
     *        Cadena de certificados a agregar.
     * @return La nueva firma AuthenticatedData con los remitentes que
     *         ten&iacute;a (si los tuviera) con la cadena de certificados
     *         nueva.
     * @throws IOException Si hay errores de lectura o escritura de datos
     * @throws CertificateEncodingException Si el certificado del remitente es invalido */
    static byte[] addOriginatorInfo(final InputStream data, final X509Certificate[] signerCertificateChain) throws IOException, CertificateEncodingException {

    	final ASN1Sequence dsq;
    	try (final ASN1InputStream is = new ASN1InputStream(data);) {
	        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
	        dsq = (ASN1Sequence) is.readObject();
    	}

        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID Data
        final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
        if (doi.equals(PKCSObjectIdentifiers.id_ct_authData)) {
            // Contenido de Data
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            final AuthenticatedData auth = AuthenticatedData.getInstance(doj.getObject());

            final AlgorithmIdentifier digAlg = extractAOIfromAuth((ASN1Sequence) doj.getObject());

            // Obtenemos los originatorInfo
            OriginatorInfo origInfo = auth.getOriginatorInfo();
            ASN1Set certs = null;
            if (origInfo != null) {
                certs = origInfo.getCertificates();
            }

            final OriginatorInfo origInfoChecked = Utils.checkCertificates(signerCertificateChain, certs);
            if (origInfoChecked != null) {
                origInfo = origInfoChecked;
            }

            // Se crea un nuevo AuthenticatedData a partir de los datos
            // anteriores con los nuevos originantes.
            return new ContentInfo(
        		PKCSObjectIdentifiers.id_ct_authData,
        		new AuthenticatedData(
    				origInfo, // OriginatorInfo
                    auth.getRecipientInfos(), // ASN1Set
                    auth.getMacAlgorithm(), // macAlgorithm
                    digAlg, // AlgorithmIdentifier se les ha olvidado a BC implementar el getDigestAlgorithm
                    auth.getEncapsulatedContentInfo(), // ContentInfo
                    auth.getAuthAttrs(), // ASN1Set
                    auth.getMac(), // ASN1OctetString
                    auth.getUnauthAttrs() // ASN1Set
                )
    		).getEncoded(ASN1Encoding.DER);
        }
        return null;
    }

    private static AlgorithmIdentifier extractAOIfromAuth(final ASN1Sequence auth) {

        final Enumeration<?> e = auth.getObjects();
        // Elemento 0 : version
        e.nextElement();
        // Elemento 1 : OriginatorInfo
        e.nextElement();
        // Elemento 2 : RecipientsInfo
        e.nextElement();
        // Elemento 3 : MAC Algorithm
        e.nextElement();

        // Elemento 4 : DigestAlgorithm
        final DERTaggedObject alg = (DERTaggedObject) e.nextElement();
        final ASN1Sequence content = (ASN1Sequence) alg.getObject();
        final AlgorithmIdentifier aoi = AlgorithmIdentifier.getInstance(content);

        return aoi;
    }
}
