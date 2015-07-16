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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Encoding;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.ASN1UTCTime;
import org.bouncycastle.asn1.BERSet;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
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
import org.bouncycastle.asn1.x500.style.RFC4519Style;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.Certificate;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Contrafirma digital PKCS#7/CMS SignedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> pero con la
 * peculiaridad de que es una Contrafirma. */
final class CounterSigner {

    private int actualIndex = 0;
    private ASN1Set signedAttr2;
    private Map<String, byte[]> atrib2 = new HashMap<String, byte[]>();
    private Map<String, byte[]> uatrib2 = new HashMap<String, byte[]>();

    /** Constructor de la clase. Se crea una contrafirma a partir de los datos
     * del firmante, el archivo que se firma y del archivo que contiene las
     * firmas.<br>
     * @param parameters Par&aacute;metros necesarios que contienen tanto la firma del
     *                   archivo a firmar como los datos del firmante.
     * @param data Archivo que contiene las firmas.
     * @param targetType Lo que se quiere firmar. Puede ser el &aacute;rbol completo,
     *                   las hojas, un nodo determinado o unos determinados firmantes.
     * @param targets Nodos objetivos a firmar.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param atri Atributo firmado que agregar a la firma.
     * @param uatri Atributo no firmado que agregar a la firma.
     * @return El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *                             datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
     *                                                digital.
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de
     *                                                 firma.
     * @throws AOException Cuando ocurre cualquier error no contemplado por el resto de
     *                     las excepciones declaradas */
    byte[] counterSigner(final P7ContentSignerParameters parameters,
                                final byte[] data,
                                final CounterSignTarget targetType,
                                final int[] targets,
                                final PrivateKey key,
                                final java.security.cert.Certificate[] certChain,
                                final String dataType,
                                final Map<String, byte[]> atri,
                                final Map<String, byte[]> uatri) throws IOException, NoSuchAlgorithmException, CertificateException, AOException {

        this.atrib2 = atri;
        this.uatrib2 = uatri;

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        final ASN1InputStream is = new ASN1InputStream(data);
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        is.close();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        e.nextElement();
        // Contenido de SignedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();

        final SignedData sd = SignedData.getInstance(contentSignedData);

        // Obtenemos los signerInfos del SignedData
        final ASN1Set signerInfosSd = sd.getSignerInfos();

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
            vCertsSig.add(Certificate.getInstance(ASN1Primitive.fromByteArray(certChain[0].getEncoded())));
            certificates = new BERSet(vCertsSig);
        }

        // CRLS no usado
        final ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        // FIRMA EN ARBOL
        if (CounterSignTarget.TREE.equals(targetType)) {
            signerInfos = counterTree(signerInfosSd, parameters, key, certChain);
        }

        // FIRMA DE LAS HOJAS
        else if (CounterSignTarget.LEAFS.equals(targetType)) {
            signerInfos = counterLeaf(signerInfosSd, parameters, key, certChain);
        }

        // FIRMA DE NODOS O FIRMANTES CONCRETOS
        else if (CounterSignTarget.NODES.equals(targetType) || targetType.equals(CounterSignTarget.SIGNERS)) {

            SignedData aux = sd;

            for (int i = targets.length - 1; i >= 0; i--) {
                signerInfos = counterNode(
            		aux,
            		parameters,
            		key,
            		certChain,
            		targets[i]
        		);
                final SignedData sigDat = new SignedData(
            		sd.getDigestAlgorithms(),
            		sd.getEncapContentInfo(),
            		certificates,
            		certrevlist,
            		new DERSet(signerInfos)
        		);

                // Esto se realiza asi por problemas con los casting.
                final ASN1InputStream sd2 = new ASN1InputStream(sigDat.getEncoded(ASN1Encoding.DER));
                final ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedData
                sd2.close();

                aux = SignedData.getInstance(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(PKCSObjectIdentifiers.signedData, aux).getEncoded(ASN1Encoding.DER);
        }

        else {
        	throw new IllegalArgumentException(
    			"Tipo de objetivo para la contrafirma no soportado: " + targetType //$NON-NLS-1$
			);
        }

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
    		PKCSObjectIdentifiers.signedData,
    		new SignedData(
				sd.getDigestAlgorithms(),
				sd.getEncapContentInfo(),
				certificates,
				certrevlist,
				new DERSet(signerInfos)
			)
		).getEncoded(ASN1Encoding.DER);

    }

    /** M&eacute;todo que contrafirma el arbol completo de forma recursiva, todos
     * los dodos creando un nuevo contraSigner.<br>
     * @param signerInfosRaiz Nodo ra&iacute; que contiene todos los signerInfos que se
     *                        deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code>.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
     *                                                digital
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *                             datos
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de
     *                                                 firma.
     * @throws AOException Cuando ocurre un error durante el proceso de contrafirma
     *                     (formato o clave incorrecto,...) */
    private ASN1EncodableVector counterTree(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain) throws NoSuchAlgorithmException,
                                                                                                     IOException,
                                                                                                     CertificateException,
                                                                                                     AOException {

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();

        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final ASN1Sequence atribute = (ASN1Sequence) signerInfosRaiz.getObjectAt(i);
            final SignerInfo si = SignerInfo.getInstance(atribute);
            counterSigners.add(getCounterUnsignedAtributes(si, parameters, key, certChain));
        }

        return counterSigners;
    }

    /** M&eacute;todo que contrafirma las hojas del arbol completo de forma
     * recursiva, todos los dodos creando un nuevo contraSigner.<br>
     * @param signerInfosRaiz Nodo ra&iacute; que contiene todos los signerInfos que se
     *                        deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code> hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws java.io.IOException Si hay errores en la lectura de datos.
     * @throws java.security.cert.CertificateException Si hay problemas en el tratamiento de los certificados.
     * @throws AOException Si ocurre cualquier otro problema durante el proceso. */
    private ASN1EncodableVector counterLeaf(final ASN1Set signerInfosRaiz,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain) throws NoSuchAlgorithmException,
                                                                                                     IOException,
                                                                                                     CertificateException,
                                                                                                     AOException {

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();

        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            final ASN1Sequence atribute = (ASN1Sequence) signerInfosRaiz.getObjectAt(i);
            final SignerInfo si = SignerInfo.getInstance(atribute);
            counterSigners.add(getCounterLeafUnsignedAtributes(si, parameters, key, certChain));
        }

        return counterSigners;
    }

    /** Contrafirma un nodo determinado del arbol buscandolo de
     * forma recursiva.<br>
     * @param sd <code>SignedData</code> que contiene el Nodo ra&iacute;z.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code> hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param nodo Nodo signerInfo a firmar.
     * @return El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws java.io.IOException Si ocurren problemas durante la lectura de datos
     * @throws java.security.cert.CertificateException SI ocurren problemas en el tratamiento de los certificados
     * @throws AOException EN caso de cualquier otro error */
    private ASN1EncodableVector counterNode(final SignedData sd,
                                            final P7ContentSignerParameters parameters,
                                            final PrivateKey key,
                                            final java.security.cert.Certificate[] certChain,
                                            final int nodo) throws NoSuchAlgorithmException,
                                                                   IOException,
                                                                   CertificateException,
                                                                   AOException {

        final ASN1Set signerInfosRaiz = sd.getSignerInfos();

        final ASN1EncodableVector counterSigners = new ASN1EncodableVector();
        ASN1Set auxSignerRaiz;

        auxSignerRaiz = signerInfosRaiz;
        this.actualIndex = 0;

        for (int i = 0; i < auxSignerRaiz.size(); i++) {
            final ASN1Sequence atribute = (ASN1Sequence) auxSignerRaiz.getObjectAt(i);
            final SignerInfo si = SignerInfo.getInstance(atribute);
            SignerInfo counterSigner = null;
            if (this.actualIndex == nodo) {
                counterSigner = getCounterNodeUnsignedAtributes(si, parameters, key, certChain);
            }
            else {
                if (this.actualIndex != nodo) {
                    counterSigner = getCounterNodeUnsignedAtributes(si, parameters, key, certChain, nodo);
                }
            }
            this.actualIndex++;
            counterSigners.add(counterSigner);
        }

        return counterSigners;

    }

    /** M&eacute;todo utilizado por la firma del &eacute;rbol para obtener la
     * contrafirma de los signerInfo de forma recursiva.<br>
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos que se
     *                   deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code> hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos contrafirmados.
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws java.io.IOException SI hay problemas en la lectura de datos
     * @throws java.security.cert.CertificateException Si hay problemas en el tratamiento de los certificados
     * @throws AOException EN caso de cualquier otro error */
    private SignerInfo getCounterUnsignedAtributes(final SignerInfo signerInfo,
                                                   final P7ContentSignerParameters parameters,
                                                   final PrivateKey key,
                                                   final java.security.cert.Certificate[] certChain) throws NoSuchAlgorithmException,
                                                                            IOException,
                                                                            CertificateException,
                                                                            AOException {

        final List<Object> attributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();

        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final ASN1Sequence atrib = (ASN1Sequence) obj;
                            final SignerInfo si = SignerInfo.getInstance(atrib);

                            final SignerInfo obtained = getCounterUnsignedAtributes(si, parameters, key, certChain);
                            signerInfosU.add(obtained);
                        }
                        else {
                            attributes.add(obj);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }
            }
            // FIRMA DEL NODO ACTUAL
            signerInfosU.add(unsignedAtributte(parameters, signerInfo, key, certChain));

            // FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        contexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                a1 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
                return new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       a1 // unsignedAttr
                        );

                // introducido este else pero es sospechoso que no estuviera
                // antes de este ultimo cambio.
            }
			if (signerInfosU.size() == 1) {
			    if (signerInfosU.get(0) instanceof Attribute) {
			        // anadimos el que hay
			        contexExpecific.add(signerInfosU.get(0));
			        // creamos el de la contrafirma.
			        signerInfosU2.add(unsignedAtributte(parameters, signerInfo, key, certChain));
			        final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
			        contexExpecific.add(uAtrib);

			    }
			    else {
			        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
			    }

			    return new SignerInfo(signerInfo.getSID(),
			                           signerInfo.getDigestAlgorithm(),
			                           signerInfo.getAuthenticatedAttributes(),
			                           signerInfo.getDigestEncryptionAlgorithm(),
			                           signerInfo.getEncryptedDigest(),
			                           SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
			            );
			}

			final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
			return new SignerInfo(signerInfo.getSID(),
			                       signerInfo.getDigestAlgorithm(),
			                       signerInfo.getAuthenticatedAttributes(),
			                       signerInfo.getDigestEncryptionAlgorithm(),
			                       signerInfo.getEncryptedDigest(),
			                       generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
			        );
        }

		signerInfosU2.add(unsignedAtributte(parameters, signerInfo, key, certChain));
		final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
		return new SignerInfo(
		    	   signerInfo.getSID(),
		           signerInfo.getDigestAlgorithm(),
		           signerInfo.getAuthenticatedAttributes(),
		           signerInfo.getDigestEncryptionAlgorithm(),
		           signerInfo.getEncryptedDigest(),
		           generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
		        );
    }

    /** M&eacute;todo utilizado por la firma de una hoja del &eacute;rbol para
     * obtener la contrafirma de los signerInfo de una determinada hoja de forma
     * recursiva.
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos que se
     *                   deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code> hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws java.io.IOException Si hay errores en la lectura de datos.
     * @throws java.security.cert.CertificateException Si hay problemas en el tratamiento de los certificados.
     * @throws AOException Si ocurre cualquier otro problema durante el proceso. */
    private SignerInfo getCounterLeafUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain) throws NoSuchAlgorithmException,
                                                                                                                IOException,
                                                                                                                CertificateException,
                                                                                                                AOException {

        final List<Object> attributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();

        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();

                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final SignerInfo si = SignerInfo.getInstance(obj);
                            final SignerInfo obtained = getCounterLeafUnsignedAtributes(si, parameters, key, certChain);
                            signerInfosU.add(obtained);
                        }
                        else {
                            attributes.add(obj);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }

            // FIRMA DE CADA UNO DE LOS HIJOS

            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        contexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }

                return new SignerInfo(
            		signerInfo.getSID(),
                    signerInfo.getDigestAlgorithm(),
                    signerInfo.getAuthenticatedAttributes(),
                    signerInfo.getDigestEncryptionAlgorithm(),
                    signerInfo.getEncryptedDigest(),
                    SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
                );

            }
			if (signerInfosU.size() == 1) {
			    if (signerInfosU.get(0) instanceof Attribute) {
			        // anadimos el que hay
			        contexExpecific.add(signerInfosU.get(0));
			        // creamos el de la contrafirma.
			        signerInfosU2.add(unsignedAtributte(parameters, signerInfo, key, certChain));
			        final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
			        contexExpecific.add(uAtrib);

			    }
			    else {
			        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
			    }

			    return new SignerInfo(signerInfo.getSID(),
			                           signerInfo.getDigestAlgorithm(),
			                           signerInfo.getAuthenticatedAttributes(),
			                           signerInfo.getDigestEncryptionAlgorithm(),
			                           signerInfo.getEncryptedDigest(),
			                           SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
			            );
			}
			final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
			return new SignerInfo(signerInfo.getSID(),
			                       signerInfo.getDigestAlgorithm(),
			                       signerInfo.getAuthenticatedAttributes(),
			                       signerInfo.getDigestEncryptionAlgorithm(),
			                       signerInfo.getEncryptedDigest(),
			                       generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
			        );
        }
		signerInfosU2.add(unsignedAtributte(parameters, signerInfo, key, certChain));
		final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
		return new SignerInfo(signerInfo.getSID(),
		                       signerInfo.getDigestAlgorithm(),
		                       signerInfo.getAuthenticatedAttributes(),
		                       signerInfo.getDigestEncryptionAlgorithm(),
		                       signerInfo.getEncryptedDigest(),
		                       new DERSet(uAtrib) // unsignedAttr
		        );
    }

    /** M&eacute;todo utilizado por la firma de un nodo del &eacute;rbol para
     * obtener la contrafirma de los signerInfo sin ser recursivo. Esto es por
     * el caso especial de que puede ser el nodo ra&iacute;z el nodo a firmar, por lo
     * que no ser&iacute;a necesario usar la recursividad.
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos que se
     *                   deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code> hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @return El SignerInfo ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws java.io.IOException Si hay errores en la lectura de datos.
     * @throws java.security.cert.CertificateException Si hay problemas en el tratamiento de los certificados. */
    private SignerInfo getCounterNodeUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain) throws NoSuchAlgorithmException,
                                                                                                                IOException,
                                                                                                                CertificateException {
        final List<Object> attributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        final ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();

        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            signerInfosU.add(SignerInfo.getInstance(obj));
                        }
                        else {
                            attributes.add(obj);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }
            }
            // FIRMA DEL NODO ACTUAL
            signerInfosU.add(unsignedAtributte(parameters, signerInfo, key, certChain));

            // FIRMA DE CADA UNO DE LOS HIJOS

            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        contexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }

                return new SignerInfo(signerInfo.getSID(),
                   signerInfo.getDigestAlgorithm(),
                   signerInfo.getAuthenticatedAttributes(),
                   signerInfo.getDigestEncryptionAlgorithm(),
                   signerInfo.getEncryptedDigest(),
                   SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
                );

            }
			if (signerInfosU.size() == 1) {
			    if (signerInfosU.get(0) instanceof Attribute) {
			        // anadimos el que hay
			        contexExpecific.add(signerInfosU.get(0));
			        // creamos el de la contrafirma.
			        signerInfosU2.add(unsignedAtributte(parameters, signerInfo, key, certChain));
			        final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
			        contexExpecific.add(uAtrib);

			    }
			    else {
			        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
			    }

			    return new SignerInfo(signerInfo.getSID(),
			       signerInfo.getDigestAlgorithm(),
			       signerInfo.getAuthenticatedAttributes(),
			       signerInfo.getDigestEncryptionAlgorithm(),
			       signerInfo.getEncryptedDigest(),
			       SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
			    );
			}
			final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
			return new SignerInfo(
			   signerInfo.getSID(),
			   signerInfo.getDigestAlgorithm(),
			   signerInfo.getAuthenticatedAttributes(),
			   signerInfo.getDigestEncryptionAlgorithm(),
			   signerInfo.getEncryptedDigest(),
			   generateUnsignerInfoFromCounter(uAtrib) // unsignedAttr
			);
        }
		signerInfosU2.add(unsignedAtributte(parameters, signerInfo, key, certChain));
		final Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
		return new SignerInfo(
			signerInfo.getSID(),
		    signerInfo.getDigestAlgorithm(),
		    signerInfo.getAuthenticatedAttributes(),
		    signerInfo.getDigestEncryptionAlgorithm(),
		    signerInfo.getEncryptedDigest(),
		    new DERSet(uAtrib) // unsignedAttr
		);
    }

    /** M&eacute;todo utilizado por la firma de un nodo del &eacute;rbol para
     * obtener la contrafirma de los signerInfo buscando el nodo de forma
     * recursiva.
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos que se
     *                   deben firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code> hoja.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param node Nodo espec&iacute;fico a firmar.
     * @return El <code>SignerInfo</code> ra&iacute;z parcial con todos sus nodos
     *         Contrafirmados.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws java.io.IOException Si hay errores en la lectura de datos.
     * @throws java.security.cert.CertificateException Si hay problemas en el tratamiento de los certificados.
     * @throws AOException Si ocurre cualquier otro problema durante el proceso. */
    private SignerInfo getCounterNodeUnsignedAtributes(final SignerInfo signerInfo,
                                                       final P7ContentSignerParameters parameters,
                                                       final PrivateKey key,
                                                       final java.security.cert.Certificate[] certChain,
                                                       final int node) throws NoSuchAlgorithmException,
                                                                              IOException,
                                                                              CertificateException,
                                                                              AOException {

        final List<Object> attributes = new ArrayList<Object>();
        final ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        SignerInfo counterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = Attribute.getInstance(eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final ASN1Sequence atrib = (ASN1Sequence) obj;
                            final SignerInfo si = SignerInfo.getInstance(atrib);
                            this.actualIndex++;
                            if (this.actualIndex != node) {
                                if (this.actualIndex < node) {
                                    signerInfosU.add(getCounterNodeUnsignedAtributes(si, parameters, key, certChain, node));
                                }
                                else {
                                    signerInfosU.add(si);
                                }
                            }
                            else {
                                signerInfosU.add(getCounterNodeUnsignedAtributes(si, parameters, key, certChain));
                            }
                        }
                        else {
                            attributes.add(obj);
                        }
                    }
                }
                else {
                    signerInfosU.add(data);
                }

            }
            // FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                    if (signerInfosU.get(i) instanceof Attribute) {
                        contexExpecific.add(signerInfosU.get(i));
                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                    }
                }
                a1 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
                counterSigner =
                        new SignerInfo(signerInfo.getSID(),
                                       signerInfo.getDigestAlgorithm(),
                                       signerInfo.getAuthenticatedAttributes(),
                                       signerInfo.getDigestEncryptionAlgorithm(),
                                       signerInfo.getEncryptedDigest(),
                                       a1 // unsignedAttr
                        );

            }
            else {
                if (signerInfosU.size() == 1) {
                    if (signerInfosU.get(0) instanceof Attribute) {
                        // anadimos el que hay
                        contexExpecific.add(signerInfosU.get(0));
                        // creamos el de la contrafirma.

                    }
                    else {
                        contexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                    }
                    a1 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
                    counterSigner =
                            new SignerInfo(signerInfo.getSID(),
                                           signerInfo.getDigestAlgorithm(),
                                           signerInfo.getAuthenticatedAttributes(),
                                           signerInfo.getDigestEncryptionAlgorithm(),
                                           signerInfo.getEncryptedDigest(),
                                           a1 // unsignedAttr
                            );
                }

            }
        }
        else {

            counterSigner =
                    new SignerInfo(signerInfo.getSID(),
                                   signerInfo.getDigestAlgorithm(),
                                   signerInfo.getAuthenticatedAttributes(),
                                   signerInfo.getDigestEncryptionAlgorithm(),
                                   signerInfo.getEncryptedDigest(),
                                   null // unsignedAttr
                    );

        }
        return counterSigner;
    }

    /** Genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la
     * firma.
     * @param cert Certificado necesario para la firma.
     * @param digestAlgorithm Algoritmo Firmado.
     * @param datos Datos firmados.
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario. */
    private ASN1Set generateSignerInfo(final X509Certificate cert,
    		                           final String digestAlgorithm,
    		                           final byte[] datos) throws NoSuchAlgorithmException {
        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // Las Contrafirmas CMS no tienen ContentType

        // fecha de firma
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new ASN1UTCTime(new Date()))));

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest,
                                          new DERSet(new DEROctetString(MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(digestAlgorithm)).digest(datos)))));

        // Serial Number
        contexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        // agregamos la lista de atributos a mayores.
        if (this.atrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.atrib2.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
                  new ASN1ObjectIdentifier(e.getKey().toString()), // el oid
                  new DERSet(new DERPrintableString(new String(e.getValue()))))); // el array de bytes en formato string
            }
        }

        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @return Los atributos no firmados de la firma. */
    private ASN1Set generateUnsignerInfo() {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (this.uatrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.uatrib2.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(
                    new Attribute(
                          // el oid
                          new ASN1ObjectIdentifier(e.getKey().toString()),
                          // el array de bytes en formato string
                          new DERSet(new DERPrintableString(new String(e.getValue())))
                    )
                );
            }
        }
        else {
            return null;
        }

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uAtrib
     *        Lista de atributos no firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos no firmados de la firma. */
    private ASN1Set generateUnsignerInfoFromCounter(final Attribute uAtrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (this.uatrib2.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = this.uatrib2.entrySet().iterator();
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
        contexExpecific.add(uAtrib);

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }

    /** M&eacute;todo que genera un signerInfo espec&iacute;fico utilizando los
     * datos necesarios para crearlo. Se utiliza siempre que no se sabe cual es
     * el signerInfo que se debe firmar.
     * @param parameters Par&aacute;metros necesarios para firmar un determinado
     *                   <code>SignerInfo</code> hoja.
     * @param si <code>SignerInfo</code> del que se debe recoger la informaci&oacute;n para
     *           realizar la contrafirma espec&iacute;fica.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @return <code>SignerInfo</code> contrafirmado.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws java.io.IOException Si hay errores en la lectura de datos.
     * @throws java.security.cert.CertificateException Si hay problemas en el tratamiento de los certificados. */
    private SignerInfo unsignedAtributte(final P7ContentSignerParameters parameters,
                                         final SignerInfo si,
                                         final PrivateKey key,
                                         final java.security.cert.Certificate[] certChain) throws NoSuchAlgorithmException,
                                                                                                  IOException,
                                                                                                  CertificateException {
        // // UNAUTHENTICATEDATTRIBUTES

        // buscamos que timo de algoritmo es y lo codificamos con su OID
        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);

        // ATRIBUTOS FINALES

        final ASN1Set signedAttr = generateSignerInfo((X509Certificate) certChain[0], digestAlgorithm, si.getEncryptedDigest().getOctets());
        final ASN1Set unsignedAttr = generateUnsignerInfo();

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(((X509Certificate)certChain[0]).getTBSCertificate()));
        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // AlgorithmIdentifier
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        // // FIN ATRIBUTOS

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        final ASN1OctetString sign2;
        try {
            sign2 = CmsUtil.firma(signatureAlgorithm, key, this.signedAttr2);
        }
        catch (final Exception ex) {
            throw new IOException("Error realizando la firma: " + ex, ex); //$NON-NLS-1$
        }

        return new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unsignedAttr);

    }

}
