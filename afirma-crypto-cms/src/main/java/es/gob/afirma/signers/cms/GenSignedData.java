/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cms;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Signature;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1UTCTime;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERPrintableString;
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
import org.spongycastle.asn1.x509.Certificate;
import org.spongycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Clase que implementa firma digital PKCS#7/CMS SignedData. La Estructura del
 * mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *  SignedData ::= SEQUENCE {
 *                      version           Version,
 *                      digestAlgorithms  DigestAlgorithmIdentifiers,
 *                      contentInfo       ContentInfo,
 *                      certificates      [0]  CertificateSet OPTIONAL,
 *                      crls              [1]  CertificateRevocationLists OPTIONAL,
 *                     signerInfos       SignerInfos
 *                   }
 *
 *  Donde signerInfo:
 *
 *  SignerInfo ::= SEQUENCE {
 *                      version                    Version,
 *                      signerIdentifier           SignerIdentifier,
 *                      digestAlgorithm            DigestAlgorithmIdentifier,
 *                      authenticatedAttributes    [0]  Attributes OPTIONAL,
 *                      digestEncryptionAlgorithm  DigestEncryptionAlgorithmIdentifier,
 *                      encryptedDigest            EncryptedDigest,
 *                      unauthenticatedAttributes  [1]  Attributes OPTIONAL
 *                    }
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje SignedData de SpongyCastle. */
final class GenSignedData {

    private ASN1Set signedAttr2;

    /** Genera una firma digital usando el sistema conocido como
     * <code>SignedData</code> y que podr&aacute; ser con el contenido del fichero codificado
     * o s&oacute;lo como referencia del fichero.
     * @param parameters Par&aacute;metros necesarios para obtener los datos de
     *                   <code>SignedData</code>.
     * @param omitContent Par&aacute;metro que indica si en la firma va el contenido del
     *                    fichero o s&oacute;lo va de forma referenciada.
     * @param applyTimestamp Si se aplica la marca de tiempo o no.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param key Clave privada del firmante.
     * @param certChain Cadena de certificados del firmante.
     * @param atrib Atributos firmados opcionales.
     * @param uatrib Atributos no autenticados firmados opcionales.
     * @param messageDigest Huella digital a aplicar en la firma.
     * @return La firma generada codificada.
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
     *                                                digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de
     *                                                 firma.
     * @throws java.io.IOException Cuando ocurre un error durante el proceso de descifrado
     *                             (formato o clave incorrecto,...)
     * @throws AOException Cuando ocurre un error durante el proceso de descifrado
     *                     (formato o clave incorrecto,...) */
    byte[] generateSignedData(final P7ContentSignerParameters parameters,
                                     final boolean omitContent,
                                     final boolean applyTimestamp,
                                     final String dataType,
                                     final PrivateKey key,
                                     final java.security.cert.Certificate[] certChain,
                                     final Map<String, byte[]> atrib,
                                     final Map<String, byte[]> uatrib,
                                     final byte[] messageDigest) throws NoSuchAlgorithmException,
                                                                        CertificateException,
                                                                        IOException,
                                                                        AOException {

        if (parameters == null) {
            throw new IllegalArgumentException("Los parametros no pueden ser nulos"); //$NON-NLS-1$
        }

        // 1. VERSION
        // la version se mete en el constructor del signedData y es 1

        // 2. DIGESTALGORITM
        // buscamos que timo de algoritmo es y lo codificamos con su OID

        final ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        digestAlgs.add(digAlgId);

        // 3. CONTENTINFO
        // si se introduce el contenido o no

        // Ya que el contenido puede ser grande, lo recuperamos solo una vez
        final byte[] content2 = parameters.getContent();

        final ContentInfo encInfo = CmsUtil.getContentInfo(content2, omitContent, dataType);

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados

        ASN1Set certificates = null;

        if (certChain.length != 0) {
            final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
            for (final java.security.cert.Certificate element : certChain) {
                ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.createBerSetFromList(ce);
        }

        final ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(((X509Certificate)certChain[0]).getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());

        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS

        // ATRIBUTOS FIRMADOS
        final ASN1Set signedAttr = generateSignedInfo(
    		digestAlgorithm,
            content2,
            dataType,
            applyTimestamp,
            atrib,
            messageDigest
        );

        // ATRIBUTOS NO FIRMADOS.

        final ASN1Set unSignedAttr = generateUnsignedInfo(uatrib);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId;
        try {
            encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e, e); //$NON-NLS-1$
        }

        final ASN1OctetString sign2 = firma(signatureAlgorithm, key);
        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
        ));

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedData, new SignedData(new DERSet(digestAlgs),
                                                                                encInfo,
                                                                                certificates,
                                                                                certrevlist,
                                                                                new DERSet(signerInfos))).getEncoded(ASN1Encoding.DER);

    }

    /** Genera los atributos firmados.
     * @param digestAlgorithm Algoritmo Firmado.
     * @param datos Datos firmados.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param timestamp Introducir el momento de la firma como atributo firmado (no confundir con un sello de tiempo reconocido)
     * @param atrib Lista de atributos firmados que se insertar&aacute;n dentro
     *              del archivo de firma.
     * @param messageDigest Huella digital.
     * @return Los atributos firmados de la firma.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario. */
    private ASN1Set generateSignedInfo(final String digestAlgorithm,
                                       final byte[] datos,
                                       final String dataType,
                                       final boolean timestamp,
                                       final Map<String, byte[]> atrib,
                                       final byte[] messageDigest) throws NoSuchAlgorithmException {
        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        contexExpecific.add(
    		new Attribute(
				CMSAttributes.contentType,
				new DERSet(
					new ASN1ObjectIdentifier(dataType)
				)
			)
		);

        // fecha de firma
        if (timestamp) {
            contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new ASN1UTCTime(new Date()))));
        }

        // Si nos viene el hash de fuera no lo calculamos
        final byte[] md;
        if (messageDigest == null || messageDigest.length < 1) {
            md = MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(digestAlgorithm)).digest(datos);
        }
        else {
            md = messageDigest;
        }

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString(md.clone()))));

        // agregamos la lista de atributos a mayores.
        if (atrib.size() != 0) {

            final Iterator<Map.Entry<String, byte[]>> it = atrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
                  new ASN1ObjectIdentifier(e.getKey()), // el oid
                  new DERSet(new DERPrintableString(new String(e.getValue()))) // el array de bytes en formato string
                ));
            }

        }

        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uatrib
     *        Lista de atributos no firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos no firmados de la firma. */
    private static ASN1Set generateUnsignedInfo(final Map<String, byte[]> uatrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = uatrib.entrySet().iterator();
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

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm Algoritmo para la firma.
     * @param key Clave para firmar.
     * @return Firma de los atributos.
     * @throws AOException Si ocurre cualquier problema durante el proceso */
    private ASN1OctetString firma(final String signatureAlgorithm, final PrivateKey key) throws AOException {

        final Signature sig;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        // Indicar clave privada para la firma
        try {
            sig.initSign(key);
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada", e); //$NON-NLS-1$
        }

        // Actualizamos la configuracion de firma
        try {
            sig.update(this.signedAttr2.getEncoded(ASN1Encoding.DER));
        }
        catch (final Exception e) {
            throw new AOException("Error al configurar la informacion de firma o al obtener los atributos a firmar", e); //$NON-NLS-1$
        }

        // firmamos.
        final byte[] realSig;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

        return new DEROctetString(realSig);

    }

}
