/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.getAttributeSet;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.makeAlgId;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BERSet;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.asn1.x509.X509Name;
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;

/**
 * Clase que implementa la contrafirma digital PKCS#7/CMS signedAndEnvelopedData
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje SignedData pero con la peculiaridad de que es una Contrafirma.
 */
public final class CounterSignerEnveloped  {

    int actualIndex = 0;
    Oid actualOid=null;
    ASN1Set signedAttr2;
    private Map<Oid, byte[]> atrib2 = new HashMap<Oid, byte[]>();
    private Map<Oid, byte[]> uatrib2 = new HashMap<Oid, byte[]>();
    
    /**
     * Constructor de la clase.
     * Se crea una contrafirma a partir de los datos del firmante, el archivo que se firma y
     * del archivo que contiene las firmas.<br>
     *
     * @param parameters    par&aacute;metros necesarios que contienen tanto la firma
     *                      del archivo a firmar como los datos del firmante.
     * @param data          Archivo que contiene las firmas.
     * @param targetType    Lo que se quiere firmar. Puede ser el &aacute;rbol completo, las hojas,
     *                      un nodo determinado o unos determinados firmantes.
     * @param targets       Nodos objetivos a firmar.
     * @param keyEntry      Clave privada a usar para firmar.
     * @param dataType      Identifica el tipo del contenido a firmar.
     * @param atri          Atributo firmado que agregar a la firma.
     * @param uatri         Atributo no firmado que agregar a la firma.
     * 
     * @return              El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
     * @throws AOException Cuando ocurre cualquier error no contemplado por el resto de las excepciones declaradas
     * @throws es.gob.afirma.exceptions.AOException Cuando ocurre un error durante el proceso de contrafirma (formato o clave incorrecto,...)
     */
    public byte[] counterSignerEnveloped(P7ContentSignerParameters parameters, 
    		                             byte[] data, 
    		                             CounterSignTarget targetType, 
    		                             int[] targets, 
    		                             PrivateKeyEntry keyEntry, 
    		                             Oid dataType, 
    		                             Map<Oid, byte[]> atri, 
    		                             Map<Oid, byte[]> uatri) throws IOException, 
    		                                                            NoSuchAlgorithmException, 
    		                                                            CertificateException, 
    		                                                            AOException {

        // Inicializamos el Oid
        actualOid= dataType;
        this.atrib2=atri;
        this.uatrib2=uatri;

        ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq = (ASN1Sequence) is.readObject();
        Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID signedAndEnvelopedData
        e.nextElement();
        // Contenido de signedAndEnvelopedData
        ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();

        SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        //Obtenemos los signerInfos del signedAndEnvelopedData
        ASN1Set signerInfosSd = null;
        signerInfosSd = sd.getSignerInfos();

        // 4.    CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        ASN1Set certificatesSigned = sd.getCertificates();
        ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        Enumeration<?> certs = certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()) {
            vCertsSig.add((DEREncodable) certs.nextElement());
        }
        // e introducimos los del firmante actual.
//        if (signerCertificateChain.length != 0) {
//            List<DEREncodable> ce = new ArrayList<DEREncodable>();
//            for (int i = 0; i < signerCertificateChain.length; i++) {
//                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
//            }
//            certificates = FillRestCerts(ce, vCertsSig);
//        }
        if (signerCertificateChain.length != 0) {
            
            vCertsSig.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getEncoded())));
            certificates = new BERSet(vCertsSig);

        }

        // CRLS no usado
        ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        //FIRMA EN ARBOL
        if (targetType.equals(CounterSignTarget.Tree)) {
            signerInfos = CounterTree(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        } //FIRMA DE LAS HOJAS
        else if (targetType.equals(CounterSignTarget.Leafs)) {
            signerInfos = CounterLeaf(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        } //FIRMA DE NODOS
        else if (targetType.equals(CounterSignTarget.Nodes)) {
            //Firma de Nodos
        	SignedAndEnvelopedData sigDat;
        	SignedAndEnvelopedData aux = sd;

            //int carry = 0;
            int nodo = 0;
             for (int i = targets.length-1; i >=0; i--) {
                nodo = targets[i];
                signerInfos = CounterNode(aux, parameters, signerCertificateChain[0], keyEntry, nodo);
                sigDat = new SignedAndEnvelopedData(
                		 sd.getRecipientInfos(),
                         sd.getDigestAlgorithms(),
                         sd.getEncryptedContentInfo(),
                         certificates,
                         certrevlist,
                        new DERSet(signerInfos));

                //Esto se realiza as&iacute; por problemas con los casting.
                ASN1InputStream sd2 = new ASN1InputStream(sigDat.getDEREncoded());
                ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del signedAndEnvelopedData

                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(
                    PKCSObjectIdentifiers.signedAndEnvelopedData,
                    aux).getDEREncoded();
        }
        else if (targetType.equals(CounterSignTarget.Signers)) {
            //Firma de Nodos
        	SignedAndEnvelopedData sigDat;
        	SignedAndEnvelopedData aux = sd;

            int nodo = 0;
            for (int i = targets.length-1; i >=0; i--) {
                nodo = targets[i];
                signerInfos = CounterNode(aux, parameters, signerCertificateChain[0], keyEntry, nodo);
                sigDat = new SignedAndEnvelopedData(
                		 sd.getRecipientInfos(),
                         sd.getDigestAlgorithms(),
                         sd.getEncryptedContentInfo(),
                         certificates,
                         certrevlist,
                        new DERSet(signerInfos));

                //Esto se realiza as&iacute; por problemas con los casting.
                ASN1InputStream sd2 = new ASN1InputStream(sigDat.getDEREncoded());
                ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del signedAndEnvelopedData

                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(
                    PKCSObjectIdentifiers.signedAndEnvelopedData,
                    aux).getDEREncoded();
        }

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
        		PKCSObjectIdentifiers.signedAndEnvelopedData,
                new SignedAndEnvelopedData(
                sd.getRecipientInfos(),
                sd.getDigestAlgorithms(),
                sd.getEncryptedContentInfo(),
                certificates,
                certrevlist,
                new DERSet(signerInfos))).getDEREncoded();

    }
    
    /*
     * new SignedAndEnvelopedData(
                new DERSet(recipientInfos),
                new DERSet(digestAlgs),
                encInfo,
                certificates,
                certrevlist,
                new DERSet(signerInfos)
            )
     */

    
    /**
     * M&eacute;todo que contrafirma el arbol completo de forma recursiva,
     * todos los dodos creando un nuevo contraSigner.<br>
     *
     * @param signerInfosRaiz Nodo ra&iacute; que contiene todos los signerInfos
     *                        que se deben firmar.
     * @param parameters      Par&aacute;metros necesarios para firmar un determinado
     *                        SignerInfo
     * @param cert            Certificado de firma.
     * @param keyEntry        Clave privada a usar para firmar
     * @return                El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     *
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
     * @throws es.gob.afirma.exceptions.AOException Cuando ocurre un error durante el proceso de contrafirma (formato o clave incorrecto,...)
     */
    private ASN1EncodableVector CounterTree(ASN1Set signerInfosRaiz,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        ASN1EncodableVector CounterSigners = new ASN1EncodableVector();

        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            ASN1Sequence atribute = (ASN1Sequence) signerInfosRaiz.getObjectAt(i);
            SignerInfo si = new SignerInfo(atribute);

            SignerInfo CounterSigner = getCounterUnsignedAtributes(si, parameters, cert, keyEntry);
            CounterSigners.add(CounterSigner);
        }

        return CounterSigners;
    }

    /**
     * M&eacute;todo que contrafirma las hojas del arbol completo de forma recursiva,
     * todos los dodos creando un nuevo contraSigner.<br>
     *
     * @param signerInfosRaiz Nodo ra&iacute; que contiene todos los signerInfos
     *                        que se deben firmar.
     * @param parameters      Par&aacute;metros necesarios para firmar un determinado
     *                        SignerInfo hoja.
     * @param cert            Certificado de firma.
     * @param keyEntry        Clave privada a usar para firmar
     * @return                El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException
     */
    private ASN1EncodableVector CounterLeaf(ASN1Set signerInfosRaiz,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        ASN1EncodableVector CounterSigners = new ASN1EncodableVector();

        for (int i = 0; i < signerInfosRaiz.size(); i++) {
            ASN1Sequence atribute = (ASN1Sequence) signerInfosRaiz.getObjectAt(i);
            SignerInfo si = new SignerInfo(atribute);

            SignerInfo CounterSigner = getCounterLeafUnsignedAtributes(si, parameters, cert, keyEntry);
            CounterSigners.add(CounterSigner);
        }
        
        return CounterSigners;
    }

    /**
     * M&eacute;todo que contrafirma un nodo determinado del arbol buscandolo
     * de forma recursiva.<br>
     *
     * @param sd              signedAndEnvelopedData que contiene el Nodo ra&iacute;z.
     * @param parameters      Par&aacute;metros necesarios para firmar un determinado
     *                        SignerInfo hoja.
     * @param cert            Certificado de firma.
     * @param keyEntry        Clave privada a usar para firmar
     * @param nodo            Nodo signerInfo a firmar.
     * @return                El SignerInfo ra&iacute;z con todos sus nodos Contrafirmados.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException
     */
    private ASN1EncodableVector CounterNode(SignedAndEnvelopedData sd,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry,
            int nodo)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        ASN1Set signerInfosRaiz = sd.getSignerInfos();

        ASN1EncodableVector CounterSigners = new ASN1EncodableVector();
        ASN1Set auxSignerRaiz;

        auxSignerRaiz = signerInfosRaiz;
        actualIndex = 0;

        for (int i = 0; i < auxSignerRaiz.size(); i++) {
            ASN1Sequence atribute = (ASN1Sequence) auxSignerRaiz.getObjectAt(i);
            SignerInfo si = new SignerInfo(atribute);
            SignerInfo CounterSigner = null;
            if (actualIndex == nodo) {
                CounterSigner = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry);
            } else {
                if (actualIndex != nodo) {
                    CounterSigner = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry, nodo);
                }
            }
            actualIndex++;
            CounterSigners.add(CounterSigner);
        }

        return CounterSigners;

    }

    /**
     * M&eacute;todo utilizado por la firma del &eacute;rbol para obtener la
     * contrafirma de los signerInfo de forma recursiva.<br>
     *
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos
     *                        que se deben firmar.
     * @param parameters      Par&aacute;metros necesarios para firmar un determinado
     *                        SignerInfo hoja.
     * @param cert            Certificado de firma.
     * @param keyEntry        Clave privada a usar para firmar.
     * @return                El SignerInfo ra&iacute;z parcial con todos sus nodos Contrafirmados.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException
     */
    private SignerInfo getCounterUnsignedAtributes(SignerInfo signerInfo,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        List<Object> Atributes = new ArrayList<Object>();
        ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)){
	                ASN1Set setInto = data.getAttrValues();
	                Enumeration<?> eAtributesData = setInto.getObjects();
	                while (eAtributesData.hasMoreElements()) {
	                    Object obj =eAtributesData.nextElement();
	                    if (obj instanceof ASN1Sequence){
	                        ASN1Sequence atrib = (ASN1Sequence) obj;
	                        SignerInfo si = new SignerInfo(atrib);
	
	                        SignerInfo obtained = getCounterUnsignedAtributes(si, parameters, cert, keyEntry);
	                        signerInfosU.add(obtained);
	                    }
	                    else{
	                        Atributes.add(obj);
	                    }
	                }
                }
                else{           	
                  signerInfosU.add(data);
                }
            }
            //FIRMA DEL NODO ACTUAL
            CounterSigner = UnsignedAtributte(parameters, cert, signerInfo, keyEntry);
            signerInfosU.add(CounterSigner);

            //FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                	if (signerInfosU.get(i) instanceof Attribute){
                		ContexExpecific.add(signerInfosU.get(i));
                	}
                	else{
                		ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                	}
                }
                a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                CounterSigner = new SignerInfo(
                        signerInfo.getSID(),
                        signerInfo.getDigestAlgorithm(),
                        signerInfo.getAuthenticatedAttributes(),
                        signerInfo.getDigestEncryptionAlgorithm(),
                        signerInfo.getEncryptedDigest(),
                        a1 //unsignedAttr
                        );

            // introducido este else pero es sospechoso que no estuviera antes de este ultimo cambio.
            } else {
            	if(signerInfosU.size() == 1){
            		if (signerInfosU.get(0) instanceof Attribute){
            			//anadimos el que hay
                		ContexExpecific.add(signerInfosU.get(0));
                		//creamos el de la contrafirma.
                		signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                        Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
                        ContexExpecific.add(uAtrib);
                        
                	}
                	else{
                		ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                	}
            		a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                    CounterSigner = new SignerInfo(
                            signerInfo.getSID(),
                            signerInfo.getDigestAlgorithm(),
                            signerInfo.getAuthenticatedAttributes(),
                            signerInfo.getDigestEncryptionAlgorithm(),
                            signerInfo.getEncryptedDigest(),
                            a1 //unsignedAttr
                            );
            	}else{
            		// Esta sentencia se comenta para que no se firme el nodo actual cuando no sea hoja 
                	// signerInfosU.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                    Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                    CounterSigner = new SignerInfo(
                            signerInfo.getSID(),
                            signerInfo.getDigestAlgorithm(),
                            signerInfo.getAuthenticatedAttributes(),
                            signerInfo.getDigestEncryptionAlgorithm(),
                            signerInfo.getEncryptedDigest(),
                            generateUnsignerInfoFromCounter(uAtrib) //unsignedAttr
                            );
            	}
            }
        } else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            CounterSigner = new SignerInfo(
                    signerInfo.getSID(),
                    signerInfo.getDigestAlgorithm(),
                    signerInfo.getAuthenticatedAttributes(),
                    signerInfo.getDigestEncryptionAlgorithm(),
                    signerInfo.getEncryptedDigest(),
                    generateUnsignerInfoFromCounter(uAtrib) //unsignedAttr
                    );


        }
        return CounterSigner;
    }

    /**
     * M&eacute;todo utilizado por la firma de una hoja del &eacute;rbol para obtener la
     * contrafirma de los signerInfo de una determinada hoja de forma recursiva.</br>
     *
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos
     *                        que se deben firmar.
     * @param parameters      Par&aacute;metros necesarios para firmar un determinado
     *                        SignerInfo hoja.
     * @param cert            Certificado de firma.
     * @param keyEntry        Clave privada a usar para firmar
     * @return                El SignerInfo ra&iacute;z parcial con todos sus nodos Contrafirmados.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException
     */
    private SignerInfo getCounterLeafUnsignedAtributes(SignerInfo signerInfo,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {
        
        List<Object> Atributes = new ArrayList<Object>();
        ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)){
	                ASN1Set setInto = data.getAttrValues();
	                Enumeration<?> eAtributesData = setInto.getObjects();
	                
	                while (eAtributesData.hasMoreElements()) {
	                    Object obj =eAtributesData.nextElement();
	                    if (obj instanceof ASN1Sequence) {
	                    	ASN1Sequence atrib = (ASN1Sequence) obj;
	                    	SignerInfo si = new SignerInfo(atrib);
	
	                    	SignerInfo obtained = getCounterLeafUnsignedAtributes(si, parameters, cert, keyEntry);
	                    	signerInfosU.add(obtained);
	                    }
	                    else{
	                        Atributes.add(obj);
	                    }
	                }
                }
                else{
                	signerInfosU.add(data);
                }

            }
            //FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {            	
                for (int i = 0; i < signerInfosU.size(); i++) {
                	if (signerInfosU.get(i) instanceof Attribute){
                		ContexExpecific.add(signerInfosU.get(i));
                	}
                	else{
                		ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                	}
                }
                a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                CounterSigner = new SignerInfo(
                        signerInfo.getSID(),
                        signerInfo.getDigestAlgorithm(),
                        signerInfo.getAuthenticatedAttributes(),
                        signerInfo.getDigestEncryptionAlgorithm(),
                        signerInfo.getEncryptedDigest(),
                        a1 //unsignedAttr
                        );

            }
            else {
            	if(signerInfosU.size() == 1){
            		if (signerInfosU.get(0) instanceof Attribute){
            			//anadimos el que hay
                		ContexExpecific.add(signerInfosU.get(0));
                		//creamos el de la contrafirma.
                		signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                        Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
                        ContexExpecific.add(uAtrib);
                        
                	}
                	else{
                		ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                	}
            		a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                    CounterSigner = new SignerInfo(
                            signerInfo.getSID(),
                            signerInfo.getDigestAlgorithm(),
                            signerInfo.getAuthenticatedAttributes(),
                            signerInfo.getDigestEncryptionAlgorithm(),
                            signerInfo.getEncryptedDigest(),
                            a1 //unsignedAttr
                            );
            	}else{
            		// Esta sentencia se comenta para que no se firme el nodo actual cuando no sea hoja 
                	// signerInfosU.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                    Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                    CounterSigner = new SignerInfo(
                            signerInfo.getSID(),
                            signerInfo.getDigestAlgorithm(),
                            signerInfo.getAuthenticatedAttributes(),
                            signerInfo.getDigestEncryptionAlgorithm(),
                            signerInfo.getEncryptedDigest(),
                            generateUnsignerInfoFromCounter(uAtrib) //unsignedAttr
                            );
            	}
            	
                
            }
        } else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            CounterSigner = new SignerInfo(
                    signerInfo.getSID(),
                    signerInfo.getDigestAlgorithm(),
                    signerInfo.getAuthenticatedAttributes(),
                    signerInfo.getDigestEncryptionAlgorithm(),
                    signerInfo.getEncryptedDigest(),
                    new DERSet(uAtrib) //unsignedAttr
                    );
        }
        return CounterSigner;
    }
    
    
    /**
     * M&eacute;todo utilizado por la firma de un nodo del &eacute;rbol para obtener la
     * contrafirma de los signerInfo Sin ser recursivo. Esto es por el caso especial
     * de que puede ser el nodo raiz el nodo a firmar, por lo que no ser&iacute;a necesario
     * usar la recursividad.</br>
     *
     * @param signerInfo Nodo ra&iacute; que contiene todos los signerInfos
     *                        que se deben firmar.
     * @param parameters      Par&aacute;metros necesarios para firmar un determinado
     *                        SignerInfo hoja.
     * @param cert            Certificado de firma.
     * @param keyEntry        Clave privada a usar para firmar
     * @return                El SignerInfo ra&iacute;z parcial con todos sus nodos Contrafirmados.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     */
    private SignerInfo getCounterNodeUnsignedAtributes(SignerInfo signerInfo,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException {

        List<Object> Atributes = new ArrayList<Object>();
        ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)){
                	ASN1Set setInto = data.getAttrValues();
                    Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        Object obj =eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence){
                        	                        	
                        		ASN1Sequence atrib = (ASN1Sequence) obj;
                                SignerInfo si = new SignerInfo(atrib);
                                signerInfosU.add(si);
                        	}
                        else{
                            Atributes.add(obj);
                        }
                    }
                }
                else{                	
                    signerInfosU.add(data);
                }
            }
            //FIRMA DEL NODO ACTUAL
            signerInfosU.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));

            //FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                	if (signerInfosU.get(i) instanceof Attribute){
                		ContexExpecific.add(signerInfosU.get(i));
                	}
                	else{
                		ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                	}
                }
                a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                CounterSigner = new SignerInfo(
                        signerInfo.getSID(),
                        signerInfo.getDigestAlgorithm(),
                        signerInfo.getAuthenticatedAttributes(),
                        signerInfo.getDigestEncryptionAlgorithm(),
                        signerInfo.getEncryptedDigest(),
                        a1 //unsignedAttr
                        );

            } 
            else {
            	if(signerInfosU.size() == 1){
            		if (signerInfosU.get(0) instanceof Attribute){
            			//anadimos el que hay
                		ContexExpecific.add(signerInfosU.get(0));
                		//creamos el de la contrafirma.
                		signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                        Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
                        ContexExpecific.add(uAtrib);
                        
                	}
                	else{
                		ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                	}
            		a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                    CounterSigner = new SignerInfo(
                            signerInfo.getSID(),
                            signerInfo.getDigestAlgorithm(),
                            signerInfo.getAuthenticatedAttributes(),
                            signerInfo.getDigestEncryptionAlgorithm(),
                            signerInfo.getEncryptedDigest(),
                            a1 //unsignedAttr
                            );
            	}else{
            		// Esta sentencia se comenta para que no se firme el nodo actual cuando no sea hoja 
                	// signerInfosU.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
                    Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                    CounterSigner = new SignerInfo(
                            signerInfo.getSID(),
                            signerInfo.getDigestAlgorithm(),
                            signerInfo.getAuthenticatedAttributes(),
                            signerInfo.getDigestEncryptionAlgorithm(),
                            signerInfo.getEncryptedDigest(),
                            generateUnsignerInfoFromCounter(uAtrib) //unsignedAttr
                            );
            	}
            	
                
            }
        }
        else {
            signerInfosU2.add(UnsignedAtributte(parameters, cert, signerInfo, keyEntry));
            Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU2));
            CounterSigner = new SignerInfo(
                    signerInfo.getSID(),
                    signerInfo.getDigestAlgorithm(),
                    signerInfo.getAuthenticatedAttributes(),
                    signerInfo.getDigestEncryptionAlgorithm(),
                    signerInfo.getEncryptedDigest(),
                    new DERSet(uAtrib) //unsignedAttr
                    );
        }
        return CounterSigner;
    }

    /**
     * M&eacute;todo utilizado por la firma de un nodo del &eacute;rbol para obtener la
     * contrafirma de los signerInfo buscando el nodo de forma recursiva.</br>
     * 
     * @param signerInfo	  Nodo ra&iacute; que contiene todos los signerInfos
     *                        que se deben firmar.
     * @param parameters      Par&aacute;metros necesarios para firmar un determinado
     *                        SignerInfo hoja.
     * @param cert            Certificado de firma.
     * @param keyEntry        Clave privada a usar para firmar
     * @param node            Nodo espec&iacute;fico a firmar.
     * @return                El SignerInfo ra&iacute;z parcial con todos sus nodos Contrafirmados.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException
     */
    private SignerInfo getCounterNodeUnsignedAtributes(SignerInfo signerInfo,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry,
            int node)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        List<Object> Atributes = new ArrayList<Object>();
        ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        SignerInfo CounterSigner2 = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            Enumeration<?> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)){
	                ASN1Set setInto = data.getAttrValues();
	                Enumeration<?> eAtributesData = setInto.getObjects();
	                while (eAtributesData.hasMoreElements()) {
	                   Object obj =eAtributesData.nextElement();
	                    if (obj instanceof ASN1Sequence){
	                        ASN1Sequence atrib = (ASN1Sequence) obj;
	                        SignerInfo si = new SignerInfo(atrib);
	                        actualIndex++;
	                        if (actualIndex != node) {
	                            if (actualIndex < node) {
	                                CounterSigner2 = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry, node);
	                                signerInfosU.add(CounterSigner2);
	                            } else {
	                                signerInfosU.add(si);
	                            }
	                        } else {
	                            SignerInfo obtained = getCounterNodeUnsignedAtributes(si, parameters, cert, keyEntry);
	                            signerInfosU.add(obtained);
	                        }
	                    }else{
	                       Atributes.add(obj);
	                    }
	                }
                }
                else{                	
                    signerInfosU.add(data);
                }

            }
            //FIRMA DE CADA UNO DE LOS HIJOS
            ASN1Set a1;
            ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
            if (signerInfosU.size() > 1) {
                for (int i = 0; i < signerInfosU.size(); i++) {
                	if (signerInfosU.get(i) instanceof Attribute){
                		ContexExpecific.add(signerInfosU.get(i));
                	}
                	else{
                		ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(i))));
                	}
                }
                a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                CounterSigner = new SignerInfo(
                        signerInfo.getSID(),
                        signerInfo.getDigestAlgorithm(),
                        signerInfo.getAuthenticatedAttributes(),
                        signerInfo.getDigestEncryptionAlgorithm(),
                        signerInfo.getEncryptedDigest(),
                        a1 //unsignedAttr
                        );

            } else {
            	if(signerInfosU.size() == 1){
            		if (signerInfosU.get(0) instanceof Attribute){
            			//anadimos el que hay
                		ContexExpecific.add(signerInfosU.get(0));
                		//creamos el de la contrafirma.
                        
                	}
                	else{
                		ContexExpecific.add(new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU.get(0))));
                	}
            		a1 = getAttributeSet(new AttributeTable(ContexExpecific));
                    CounterSigner = new SignerInfo(
                            signerInfo.getSID(),
                            signerInfo.getDigestAlgorithm(),
                            signerInfo.getAuthenticatedAttributes(),
                            signerInfo.getDigestEncryptionAlgorithm(),
                            signerInfo.getEncryptedDigest(),
                            a1 //unsignedAttr
                            );
            	}           	
                
            }
        } else {

            CounterSigner = new SignerInfo(
                    signerInfo.getSID(),
                    signerInfo.getDigestAlgorithm(),
                    signerInfo.getAuthenticatedAttributes(),
                    signerInfo.getDigestEncryptionAlgorithm(),
                    signerInfo.getEncryptedDigest(),
                    null //unsignedAttr
                    );


        }
        return CounterSigner;
    }

    /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
     *  Se generan los atributos que se necesitan para generar la firma.</br>
     *
     * @param cert              Certificado necesario para la firma.
     * @param digestAlgorithm   Algoritmo Firmado.
     * @param datos             Datos firmados.
     *
     * @return      Los datos necesarios para generar la firma referente a los
     *              datos del usuario.
     *
     * @throws java.security.NoSuchAlgorithmException
     */
    private ASN1Set generateSignerInfo(X509Certificate cert,
            String digestAlgorithm,
            byte[] datos)
            throws NoSuchAlgorithmException {

       
        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

// Las Contrafirmas CMS no tienen ContentType
//        //tipo de contenido
//        ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(actualOid.toString()))));

        //fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

     // Los DigestAlgorithms con SHA-2 tienen un guion:
        if (digestAlgorithm.equals("SHA512")) digestAlgorithm = "SHA-512";
        else if (digestAlgorithm.equals("SHA384")) digestAlgorithm = "SHA-384";
        else if (digestAlgorithm.equals("SHA256")) digestAlgorithm = "SHA-256";
        
        
        //MessageDigest
        ContexExpecific.add(
                new Attribute(
                CMSAttributes.messageDigest,
                new DERSet(
                new DEROctetString(
                MessageDigest.getInstance(digestAlgorithm.toString()).digest(datos)))));

        //Serial Number
        ContexExpecific.add(
                new Attribute(
                X509Name.SERIALNUMBER,
                new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        //agregamos la lista de atributos a mayores.
        if (atrib2.size()!=0){
        	Iterator<Map.Entry<Oid, byte[]>> it = atrib2.entrySet().iterator();
        	while (it.hasNext()) {
        	    Map.Entry<Oid, byte[]> e = it.next();
        	ContexExpecific.add(
                    new Attribute(
                        // el oid
                        new DERObjectIdentifier((e.getKey()).toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(e.getValue()))
                    )
                );
        	}
        }

        signedAttr2 = getAttributeSet(new AttributeTable(ContexExpecific));

        return getAttributeSet(new AttributeTable(ContexExpecific));

    }

    /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
     *  Se generan los atributos no firmados.

     * @return      Los atributos no firmados de la firma.
     */
    private ASN1Set generateUnsignerInfo(){

        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();


        //agregamos la lista de atributos a mayores.
        if (uatrib2.size()!=0){
        	Iterator<Map.Entry<Oid, byte[]>> it = uatrib2.entrySet().iterator();
        	while (it.hasNext()) {
        	    Map.Entry<Oid, byte[]> e = it.next();
        	ContexExpecific.add(
                    new Attribute(
                        // el oid
                        new DERObjectIdentifier((e.getKey()).toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(e.getValue()))
                    )
                );
        	}
        }
        else{
            return null;
        }

        return getAttributeSet(new AttributeTable(ContexExpecific));

    }

    /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
     *  Se generan los atributos no firmados.
     *
     * @param uAtrib    Lista de atributos no firmados que se insertar&aacute;n dentro del archivo de firma.
     *
     * @return      Los atributos no firmados de la firma.
     */
    private ASN1Set generateUnsignerInfoFromCounter(Attribute uAtrib){

        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();


        //agregamos la lista de atributos a mayores.
        if (uatrib2.size()!=0){
        	Iterator<Map.Entry<Oid, byte[]>> it = uatrib2.entrySet().iterator();
        	while (it.hasNext()) {
        	    Map.Entry<Oid, byte[]> e = it.next();
        	ContexExpecific.add(
                    new Attribute(
                        // el oid
                        new DERObjectIdentifier((e.getKey()).toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(e.getValue()))
                    )
                );
        	}
        }
        ContexExpecific.add(uAtrib);

        return getAttributeSet(new AttributeTable(ContexExpecific));

    }

    /**
     * M&eacute;todo que genera un signerInfo espec&iacute;fico utilizando los datos necesarios
     * para crearlo. Se utiliza siempre que no se sabe cual es el signerInfo que se
     * debe firmar.</br>
     *
     * @param parameters      Par&aacute;metros necesarios para firmar un determinado
     *                        SignerInfo hoja.
     * @param cert            Certificado de firma.
     * @param si              SignerInfo del que se debe recoger la informaci&oacute;n para
     *                        realizar la contrafirma espec&iacute;fica.
     * @param keyEntry        Clave privada a usar para firmar
     * @return                El signerInfo contrafirmado.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     */
    private SignerInfo UnsignedAtributte(P7ContentSignerParameters parameters,
            X509Certificate cert,
            SignerInfo si,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException {
        //// UNAUTHENTICATEDATTRIBUTES
        ASN1Set unsignedAttr = null;
        ASN1Set signedAttr = null;

        // buscamos que timo de algoritmo es y lo codificamos con su OID

        AlgorithmIdentifier digAlgId;
        String signatureAlgorithm = parameters.getSignatureAlgorithm();
        String digestAlgorithm = null;
        String keyAlgorithm = null;
        int with = signatureAlgorithm.indexOf("with");
        if (with > 0) {
            digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(signatureAlgorithm);
            int and = signatureAlgorithm.indexOf("and", with + 4);
            if (and > 0) {
                keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
            } else {
                keyAlgorithm = signatureAlgorithm.substring(with + 4);
            }
        }
                
        AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
        digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());

        //ATRIBUTOS FINALES
        //ByteArrayInputStream signerToDigest = new ByteArrayInputStream(si.getEncryptedDigest().getOctets());
        signedAttr = generateSignerInfo(cert, digestAlgorithm, si.getEncryptedDigest().getOctets());
        unsignedAttr = generateUnsignerInfo();

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
        IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(tbs.getIssuer(), tbs.getSerialNumber().getValue());
        SignerIdentifier identifier = new SignerIdentifier(encSid);

        //AlgorithmIdentifier
        digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());


        ////  FIN ATRIBUTOS

        //digEncryptionAlgorithm
        AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;

        encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());

        // Firma del SignerInfo

        //ByteArrayInputStream signerToDigest = new ByteArrayInputStream(si.getEncryptedDigest().getOctets());

        //byte[] signedInfo = signData(signerToDigest, signatureAlgorithm, keyEntry);

        ASN1OctetString sign2= null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        } catch (AOException ex) {
            Logger.getLogger(CounterSignerEnveloped.class.getName()).log(Level.SEVERE, null, ex);
        }

        SignerInfo uAtrib = new SignerInfo(
                identifier,
                digAlgId,
                signedAttr,
                encAlgId,
                sign2,
                unsignedAttr//null
                );

        return uAtrib;

    }

    /**
     * Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm    Algoritmo para la firma
     * @param keyEntry              Clave para firmar.
     * @return                      Firma de los atributos.
     * @throws es.map.es.map.afirma.exceptions.AOException
     */
    private ASN1OctetString firma (String signatureAlgorithm, PrivateKeyEntry keyEntry) throws AOException{

        Signature sig = null;
		try {
			sig = Signature.getInstance(signatureAlgorithm);
		} 
		catch (final Throwable e) {
            throw new AOException(
        		"Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e
    		);
		}

        byte[] tmp= null;

        try {
            tmp = signedAttr2.getEncoded(ASN1Encodable.DER);
        } catch (IOException ex) {
            Logger.getLogger(CounterSignerEnveloped.class.getName()).log(Level.SEVERE, null, ex);
        }

        //Indicar clave privada para la firma
		try {
			sig.initSign(keyEntry.getPrivateKey());
		} 
		catch (final Throwable e) {
			throw new AOException(
				"Error al inicializar la firma con la clave privada", e
			);
		}



        // Actualizamos la configuracion de firma
		try {
			sig.update(tmp);
		} 
		catch (final SignatureException e) {
			throw new AOException(
				"Error al configurar la informacion de firma", e);
		}


        //firmamos.
        byte[] realSig=null;
        try {
			realSig = sig.sign();
		} 
        catch (final Throwable e) {
			throw new AOException("Error durante el proceso de firma", e);
		}

        ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;


    }

}
