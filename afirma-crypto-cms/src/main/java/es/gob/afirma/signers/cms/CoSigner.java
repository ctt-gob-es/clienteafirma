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
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

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
import org.spongycastle.asn1.x500.style.RFC4519Style;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.Certificate;
import org.spongycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.ContainsNoDataException;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Clase que implementa la cofirma digital PKCS#7/CMS SignedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedData de SpongyCastle pero con la
 * peculiaridad de que es una Cofirma. */
final class CoSigner {

    private ASN1Set signedAttr2;

    /** Constructor de la clase. Se crea una cofirma a partir de los datos del
     * firmante, el archivo que se firma y del archivo que contiene las firmas.
     * @param parameters par&aacute;metros necesarios que contienen tanto la firma del
     *                   archivo a firmar como los datos del firmante.
     * @param sign Archivo que contiene las firmas.
     * @param omitContent Si se omite el contenido o no, es decir,si se hace de forma
     *                    Expl&iacute;cita o Impl&iacute;cita.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param key Clave privada del firmante.
     * @param certChain Cadena de certificados del firmante
     * @param atrib Atributos firmados opcionales.
     * @param uatrib Atributos no autenticados firmados opcionales.
     * @param messageDigest Hash a aplicar en la firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con
     *                                                 los certificados de firma. */
    byte[] coSigner(final P7ContentSignerParameters parameters,
                           final byte[] sign,
                           final boolean omitContent,
                           final String dataType,
                           final PrivateKey key,
                           final java.security.cert.Certificate[] certChain,
                           final Map<String, byte[]> atrib,
                           final Map<String, byte[]> uatrib,
                           final byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
    	final ASN1InputStream is = new ASN1InputStream(sign);
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        is.close();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        e.nextElement();
        // Contenido de SignedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido
                                                                        // del
                                                                        // SignedData

        final SignedData sd = SignedData.getInstance(contentSignedData);

        // 3. CONTENTINFO
        // si se introduce el contenido o no

        // Ya que el contenido puede ser grande, lo recuperamos solo una vez
        final byte[] content2 = parameters.getContent();

        final ContentInfo encInfo = CmsUtil.getContentInfo(content2, omitContent, dataType);

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

        if (certChain.length != 0) {
            final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
            for (final java.security.cert.Certificate element : certChain) {
                ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.fillRestCerts(ce, vCertsSig);
        }

        // buscamos que timo de algoritmo es y lo codificamos con su OID

        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // Identificador del firmante ISSUER AND SERIAL-NUMBER
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(
    		ASN1Primitive.fromByteArray(((X509Certificate)certChain[0]).getTBSCertificate())
		);
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS

        // atributos firmados
        ASN1Set signedAttr = null;
        if (messageDigest == null) {
            signedAttr =
            	generateSignerInfo(
            			digestAlgorithm,
            			content2 != null ? content2 : parameters.getContent(),
            			dataType,
            			atrib);
        }
        else {
            signedAttr = generateSignerInfoFromHash((X509Certificate) certChain[0], messageDigest, dataType, atrib);
        }

        // atributos no firmados.
        final ASN1Set unSignedAttr = generateUnsignerInfo(uatrib);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del SignedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

        // introducimos los SignerInfos Existentes
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        // introducimos el nuevo SignerInfo del firmante actual.

        for (int i = 0; i < signerInfosSd.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(signerInfosSd.getObjectAt(i));
            signerInfos.add(si);
        }

        final ASN1OctetString sign2;
        try {
            sign2 = CmsUtil.firma(signatureAlgorithm, key, this.signedAttr2);
        }
        catch (final Exception ex) {
            throw new IOException("Error al generar la firma: " + ex, ex); //$NON-NLS-1$
        }

        // Creamos los signerInfos del SignedData
        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
        ));


        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedData, new SignedData(sd.getDigestAlgorithms(),
                                                                                encInfo,
                                                                                certificates,
                                                                                null,
                                                                                new DERSet(signerInfos)// unsignedAttr
                               )).getEncoded(ASN1Encoding.DER);

    }

    /** Constructor de la clase. Se crea una cofirma a partir de los datos del
     * firmante y el archivo que se firma.
     * @param signatureAlgorithm Algoritmo para la firma
     * @param signerCertificateChain Cadena de certificados para la construccion de los parametros
     *                               de firma.
     * @param sign Archivo que contiene las firmas.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param key Clave privada del firmante.
     * @param atrib Atributos firmados adicionales.
     * @param uatrib Atributos no firmados adicionales.
     * @param digest Hash a aplicar en la firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de
     *                                                firma o huella digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con
     *                                                 los certificados de firma.
     * @throws ContainsNoDataException Cuando la firma no contiene los datos
     *                                 ni fue generada con el mismo algoritmo de firma. */
    byte[] coSigner(final String signatureAlgorithm,
                    final X509Certificate[] signerCertificateChain,
                    final byte[] sign,
                    final String dataType,
                    final PrivateKey key,
                    final Map<String, byte[]> atrib,
                    final Map<String, byte[]> uatrib,
                    final byte[] digest) throws IOException,
                                                NoSuchAlgorithmException,
                                                CertificateException,
                                                ContainsNoDataException {

        byte[] messageDigest = digest != null ? digest.clone() : null;

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        final ASN1InputStream is = new ASN1InputStream(sign);
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        is.close();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        e.nextElement();
        // Contenido de SignedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject(); // Contenido del SignedData

        final SignedData sd = SignedData.getInstance(contentSignedData);

        // 3. CONTENTINFO
        // si se introduce el contenido o no
        final ContentInfo encInfo = sd.getEncapContentInfo();

        final DEROctetString contenido = (DEROctetString) encInfo.getContent();
        byte[] contenidoDatos = null;
        if (contenido != null) {
            contenidoDatos = AOUtil.getDataFromInputStream(contenido.getOctetStream());
        }

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

        if (signerCertificateChain.length != 0) {
            final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.fillRestCerts(ce, vCertsSig);
        }

        // buscamos que tipo de algoritmo es y lo codificamos con su OID
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // Identificador del firmante ISSUER AND SERIAL-NUMBER
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS

        // atributos firmados
        ASN1Set signedAttr = null;

        // atributos no firmados.
        final ASN1Set unSignedAttr = generateUnsignerInfo(uatrib);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del SignedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

        // introducimos los SignerInfos Existentes
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        // introducimos el nuevo SignerInfo del firmante actual.

        // Secuencia:
        // 1.- Si cofirmamos sin datos en el mismo algoritmo de hash que la
        // firma
        // original sacamos el messagedigest de la firma previa.
        // 2.- Si no es el mismo algoritmo, miramos si nos ha llegado un
        // messagedigest
        // como parametro del metodo, que quiere decir que se ha calculado
        // externamente
        // (en el fondo sera que no se ha sobreescrito el parametro, con lo que
        // si llego
        // != null, seguira siendo != null)
        // 3.- Si no es ninguno de los dos casos, no podemos firmar
        for (int i = 0; i < signerInfosSd.size(); i++) {
            final SignerInfo si = SignerInfo.getInstance(signerInfosSd.getObjectAt(i));
            final AlgorithmIdentifier algHash = si.getDigestAlgorithm();
            // Solo si coninciden los algos puedo sacar el hash de dentro
            if (algHash.getAlgorithm().toString().equals(AOAlgorithmID.getOID(digestAlgorithm))) {
                final ASN1Set signedAttrib = si.getAuthenticatedAttributes();
                for (int s = 0; s < signedAttrib.size(); s++) {
                    final ASN1Sequence elemento = (ASN1Sequence) signedAttrib.getObjectAt(s);
                    final ASN1ObjectIdentifier oids = (ASN1ObjectIdentifier) elemento.getObjectAt(0);
                    if (CMSAttributes.messageDigest.getId().toString().equals(oids.toString())) {
                        final DERSet derSetHash = (DERSet) elemento.getObjectAt(1);
                        final DEROctetString derHash = (DEROctetString) derSetHash.getObjectAt(0);
                        messageDigest = derHash.getOctets();
                    }
                }
            }
            signerInfos.add(si);
        }

        // atributos firmados
        if (contenidoDatos != null) {
            signedAttr = generateSignerInfo(digestAlgorithm, contenidoDatos, dataType, atrib);
        }
        else if (messageDigest != null) {
            signedAttr =
                    generateSignerInfoFromHash(signerCertificateChain[0], messageDigest, dataType, atrib);
        }
        else {
            // En este caso no puedo usar un hash de fuera, ya que no me han
            // pasado datos ni
            // huellas digitales, solo un fichero de firma
            throw new ContainsNoDataException("No se puede crear la cofirma ya que no se han encontrado ni los datos firmados ni una huella digital compatible con el algoritmo de firma"); //$NON-NLS-1$
        }

        final ASN1OctetString sign2;
        try {
            sign2 = CmsUtil.firma(signatureAlgorithm, key, this.signedAttr2);
        }
        catch (final Exception ex) {
            throw new IOException("Error al generar la firma: " + ex, ex); //$NON-NLS-1$
        }

        // Creamos los signerInfos del SignedData
        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
        ));

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
    		PKCSObjectIdentifiers.signedData,
    		new SignedData(
				sd.getDigestAlgorithms(),
                encInfo,
                certificates,
                null,
                new DERSet(signerInfos)// unsignedAttr
            )
		).getEncoded(ASN1Encoding.DER);

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param digestAlgorithm Algoritmo de huella digital.
     * @param datos Datos firmados.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param atrib Lista de atributos firmados que se insertar&aacute;n dentro
     *              del archivo de firma.
     * @return Los atributos firmados de la firma.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n
     *                                                algoritmo necesario */
    private ASN1Set generateSignerInfo(final String digestAlgorithm,
    		                           final byte[] datos,
    		                           final String dataType,
    		                           final Map<String, byte[]> atrib) throws NoSuchAlgorithmException {

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
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new ASN1UTCTime(new Date()))));

        // Si nos viene el hash de fuera no lo calculamos
        final byte[] md = MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(digestAlgorithm)).digest(datos);

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString(md.clone()))));

        // agregamos la lista de atributos a mayores.
        if (atrib.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = atrib.entrySet().iterator();
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

        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * En este caso se introduce el hash directamente.
     * @param cert Certificado necesario para la firma.
     * @param datos Datos firmados.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param atrib Lista de atributos firmados que se insertar&aacute;n dentro
     *              del archivo de firma.
     * @return Atributos firmados de la firma. */
    private ASN1Set generateSignerInfoFromHash(final X509Certificate cert,
    		                                   final byte[] datos,
    		                                   final String dataType,
    		                                   final Map<String, byte[]> atrib) {

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
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new ASN1UTCTime(new Date()))));

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString(datos))));

        // Serial Number
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
                    new DERSet(new DERPrintableString(new String(e.getValue()))))
        		);
            }
        }

        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uatrib
     *        Conjunto de atributos no firmados que se insertar&aacute;n
     *        dentro del archivo de firma.
     * @return Los atributos no firmados de la firma */
    private static ASN1Set generateUnsignerInfo(final Map<String, byte[]> uatrib) {

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

}
