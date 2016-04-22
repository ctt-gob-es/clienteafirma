/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateEncodingException;
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
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERPrintableString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.DERUTCTime;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.IssuerAndSerialNumber;
import org.spongycastle.asn1.cms.SignerIdentifier;
import org.spongycastle.asn1.cms.SignerInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x500.style.RFC4519Style;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.Certificate;
import org.spongycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que implementa la cofirma digital PKCS#7/CMS SignedAndEnvelopedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedAndEnvelopedData pero con la peculiaridad de que es
 * una Cofirma. */
final class CoSignerEnveloped {

    private ASN1Set signedAttr2;

    private static ASN1Set getCertificates(final SignedAndEnvelopedData sd,
    		                               final X509Certificate[] signerCertificateChain) throws CertificateEncodingException,
    		                                                                                      IOException {
        ASN1Set certificates = null;

        final ASN1Set certificatesSigned = sd.getCertificates();
        final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        final Enumeration<?> certs = certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()) {
            vCertsSig.add((ASN1Encodable) certs.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            final List<ASN1Encodable> ce = new ArrayList<>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.fillRestCerts(ce, vCertsSig);
        }

        return certificates;
    }

    /** Constructor de la clase. Se crea una cofirma a partir de los datos del
     * firmante, el archivo que se firma y del archivo que contiene las firmas.
     * @param parameters par&aacute;metros necesarios que contienen tanto la firma del
     *                   archivo a firmar como los datos del firmante.
     * @param signerCertificateChain Cadena de certificados del firmante.
     * @param sign Archivo que contiene las firmas.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param keyEntry Clave privada del firmante.
     * @param atrib Atributos firmados opcion	ales.
     * @param uatrib Atributos no autenticados firmados opcionales.
     * @param messageDigest Huella digital a aplicar en la firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *                             datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
     *                                                digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de
     *                                                 firma. */
    byte[] coSigner(final P7ContentSignerParameters parameters,
    		        final X509Certificate[] signerCertificateChain,
                    final byte[] sign,
                    final String dataType,
                    final PrivateKeyEntry keyEntry,
                    final Map<String, byte[]> atrib,
                    final Map<String, byte[]> uatrib,
                    final byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

    	final ASN1Sequence dsq;
    	try (
			final ASN1InputStream is = new ASN1InputStream(sign);
		) {
	        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
	        dsq = (ASN1Sequence) is.readObject();
    	}

        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID signedAndEnvelopedData
        e.nextElement();
        // Contenido de signedAndEnvelopedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido
                                                                        // del
                                                                        // signedAndEnvelopedData

        final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados
        final ASN1Set certificates = getCertificates(sd, signerCertificateChain);

        // buscamos que timo de algoritmo es y lo codificamos con su OID
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // Identificador del firmante ISSUER AND SERIAL-NUMBER
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS

        // atributos firmados
        ASN1Set signedAttr = null;
        if (messageDigest == null) {
            signedAttr =
                    generateSignerInfo(digestAlgorithm, parameters.getContent(), dataType, atrib);
        }
        else {
            signedAttr = generateSignerInfoFromHash(signerCertificateChain[0], messageDigest, dataType, atrib);
        }

        // atributos no firmados.
        final ASN1Set unSignedAttr = EvelopUtils.generateUnsignedInfo(uatrib);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del signedAndEnvelopedData
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
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final Exception ex) {
            throw new IOException("Error al generar la firma: " + ex, ex); //$NON-NLS-1$
        }

        // Creamos los signerInfos del signedAndEnvelopedData
        signerInfos.add(
    		new SignerInfo(
				identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr
			)
		);

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
    		PKCSObjectIdentifiers.signedAndEnvelopedData,
    		new SignedAndEnvelopedData(
				sd.getRecipientInfos(),
				sd.getDigestAlgorithms(),
				sd.getEncryptedContentInfo(),
				certificates,
				null,
				new DERSet(signerInfos)// unsignedAttr
            )
		).getEncoded(ASN1Encoding.DER);

    }

    /** Constructor de la clase. Se crea una cofirma a partir de los datos del
     * firmante y el archivo que se firma.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param signerCertificateChain
     *        Cadena de certificados para la construccion de los parametros
     *        de firma.
     * @param sign
     *        Archivo que contiene las firmas.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param keyEntry
     *        Clave privada del firmante.
     * @param atrib
     *        Atributos firmados adicionales.
     * @param uatrib
     *        Atributos no firmados adicionales.
     * @param messageDigest
     *        Hash a aplicar en la firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.cert.CertificateException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma. */
    byte[] coSigner(final String signatureAlgorithm,
                           final X509Certificate[] signerCertificateChain,
                           final byte[] sign,
                           final String dataType,
                           final PrivateKeyEntry keyEntry,
                           final Map<String, byte[]> atrib,
                           final Map<String, byte[]> uatrib,
                           final byte[] messageDigest) throws IOException, CertificateException {

    	final ASN1Sequence dsq;
    	try (
			final ASN1InputStream is = new ASN1InputStream(sign);
		) {
	        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
	        dsq = (ASN1Sequence) is.readObject();
    	}

        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID signedAndEnvelopedData
        e.nextElement();
        // Contenido de signedAndEnvelopedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        // Contenido del signedAndEnvelopedData
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();

        final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        byte[] md = messageDigest != null ? messageDigest.clone() : null;

        // 4. CERTIFICADOS
        // obtenemos la lista de certificados
        final ASN1Set certificates = getCertificates(sd, signerCertificateChain);

        // buscamos que tipo de algoritmo es y lo codificamos con su OID
        final String digestAlgorithm =  AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId =
            SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // Identificador del firmante ISSUER AND SERIAL-NUMBER
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS

        // atributos firmados
        ASN1Set signedAttr = null;

        // atributos no firmados.
        final ASN1Set unSignedAttr = EvelopUtils.generateUnsignedInfo(uatrib);

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del signedAndEnvelopedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

        // introducimos los SignerInfos Existentes
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        // introducimos el nuevo SignerInfo del firmante actual.

        // Secuencia:
        // 1.- Si cofirmamos sin datos en el mismo algoritmo de hash que la firma
        //     original sacamos el messagedigest de la firma previa.
        // 2.- Si no es el mismo algoritmo, miramos si nos ha llegado un messagedigest
        //     como parametro del metodo, que quiere decir que se ha calculado externamente
        //     (en el fondo sera que no se ha sobreescrito el parametro, con lo que
        //     si llego != null, seguira siendo != null)
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
                        md = derHash.getOctets();
                    }
                }
            }
            signerInfos.add(si);
        }

        // En este caso no puedo usar un hash de fuera, ya que no me han
        // pasado datos ni huellas digitales, solo un fichero de firma
        if (md == null) {
            throw new IllegalStateException("No se puede crear la firma ya que no se ha encontrado un hash valido"); //$NON-NLS-1$
        }

        signedAttr =
            generateSignerInfoFromHash(signerCertificateChain[0], messageDigest, dataType, atrib);

        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final Exception ex) {
            throw new IOException("Error al generar la firma: " + ex, ex); //$NON-NLS-1$
        }

        // Creamos los signerInfos del signedAndEnvelopedData
        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
        ));

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
    		PKCSObjectIdentifiers.signedAndEnvelopedData,
    		new SignedAndEnvelopedData(
				sd.getRecipientInfos(),
                sd.getDigestAlgorithms(),
                sd.getEncryptedContentInfo(),
                certificates,
                null,
                new DERSet(signerInfos)// unsignedAttr
            )
		).getEncoded(ASN1Encoding.DER);

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param datos
     *        Datos firmados.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param atrib
     *        Lista de atributos firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos firmados de la firma.
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario */
    private ASN1Set generateSignerInfo(final String digestAlgorithm, final byte[] datos, final String dataType, final Map<String, byte[]> atrib) throws NoSuchAlgorithmException {

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
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // Si nos viene el hash de fuera no lo calculamos
        final byte[] md = MessageDigest.getInstance(
                AOSignConstants.getDigestAlgorithmName(digestAlgorithm)).digest(datos);

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
     * @return Los atributos firmados de la firma. */
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
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

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

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm Algoritmo para la firma.
     * @param keyEntry Clave para firmar.
     * @return Firma de los atributos.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws IOException Cuando hay problemas de entrada / salida.
     * @throws SignatureException  Cuando ocurren problemas en la firma PKCS#1 */
    private ASN1OctetString firma(final String signatureAlgorithm,
    		                      final PrivateKeyEntry keyEntry) throws InvalidKeyException,
    		                                                             NoSuchAlgorithmException,
    		                                                             IOException,
    		                                                             SignatureException {

        final Signature sig = Signature.getInstance(signatureAlgorithm);

        final byte[] tmp = this.signedAttr2.getEncoded(ASN1Encoding.DER);

        // Indicar clave privada para la firma
        sig.initSign(keyEntry.getPrivateKey());

        // Actualizamos la configuracion de firma
        sig.update(tmp);

        // firmamos y devolvemos.
        return new DEROctetString(sig.sign());

    }

}
