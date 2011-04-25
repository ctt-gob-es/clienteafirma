/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.fillRestCerts;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.getAttributeSet;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.makeAlgId;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BERConstructedOctetString;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.cms.CMSException;
import org.bouncycastle.cms.CMSProcessable;
import org.bouncycastle.cms.CMSProcessableByteArray;
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOUtil;

/**
 * Clase que implementa la cofirma digital CADES SignedData
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje SignedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 * pero con la peculiaridad de que es una Cofirma.
 *
 * Para ello, debe incluirse el atributo de pol&iacute;tica en la identificaci&oacute;n del
 * firmante de la siguiente manera:
 * <pre><code>
 * 
 * SignaturePolicyId ::= SEQUENCE {
 *  sigPolicyId           SigPolicyId,
 *  sigPolicyHash         SigPolicyHash,
 *  sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
 *                          SigPolicyQualifierInfo OPTIONAL}
 *
 *  SigPolicyId ::= OBJECT IDENTIFIER
 *
 *  OtherHashAlgAndValue ::= SEQUENCE {
 *     hashAlgorithm    AlgorithmIdentifier,
 *     hashValue        OCTET STRING }
 *
 *  SigPolicyQualifierInfo ::= SEQUENCE {
 *       SigPolicyQualifierId  SigPolicyQualifierId,
 *       SigQualifier          ANY DEFINED BY policyQualifierId }
 *
 *  SigPolicyQualifierId ::= OBJECT IDENTIFIER
 *
 *      id-spq-ets-uri OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-spq(5) 1 }
 *
 *     SPuri ::= IA5String
 *
 *      id-spq-ets-unotice OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-spq(5) 2 }
 *
 *     SPUserNotice ::= SEQUENCE {
 *          noticeRef        NoticeReference OPTIONAL,
 *          explicitText     DisplayText OPTIONAL
 *  }
 *
 *     NoticeReference ::= SEQUENCE {
 *          organization     DisplayText,
 *          noticeNumbers    SEQUENCE OF INTEGER
 *  }
 *
 *     DisplayText ::= CHOICE {
 *          visibleString    VisibleString  (SIZE (1..200)),
 *          bmpString        BMPString      (SIZE (1..200)),
 *          utf8String       UTF8String     (SIZE (1..200))
 *  }
 *
 * </code></pre>
 *
 */
public final class CadesCoSigner {

    ASN1Set signedAttr2;
    /**
     * Constructor de la clase.
     * Se crea una cofirma a partir de los datos del firmante, el archivo que se firma y
     * del archivo que contiene las firmas.
     *
     * @param parameters    par&aacute;metros necesarios que contienen tanto la firma
     *                      del archivo a firmar como los datos del firmante.
     * @param sign          Archivo que contiene las firmas.
     * @param omitContent   Si se omite el contenido o no, es decir,si se hace de
     *                      forma Expl&iacute;cita o Impl&iacute;cita.
     * @param policy        Url de la Politica aplicada.
     * @param qualifier		OID de la pol&iacute;tica.
     * @param signingCertificateV2 <code>true</code> si se desea usar la versi&oacute;n 2 del atributo <i>Signing Certificate</i>
     *                             <code>false</code> para usar la versi&oacute;n 1
     * @param dataType      Identifica el tipo del contenido a firmar.
     * @param keyEntry      Clave privada usada para firmar.
     * @param messageDigest Hash espec&iacute;fico para una firma.
     * 
     * @return              El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
     * @throws javax.security.cert.CertificateException 
     */
	public byte[] coSigner(P7ContentSignerParameters parameters, byte[] sign, boolean omitContent, String policy, Oid qualifier, boolean signingCertificateV2, Oid dataType, PrivateKeyEntry keyEntry, byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

        ASN1InputStream is = new ASN1InputStream(sign);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq=(ASN1Sequence)is.readObject();
        Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        e.nextElement();
        // Contenido de SignedData
        ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
        ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido del SignedData

        SignedData sd = new SignedData(contentSignedData);

         // 3. CONTENTINFO
        // si se introduce el contenido o no
        ContentInfo encInfo = null;
        ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(dataType.toString());

        if (omitContent == false) {
            ByteArrayOutputStream bOut = new ByteArrayOutputStream();
            byte[] content2 = parameters.getContent();
            CMSProcessable msg = new CMSProcessableByteArray(content2);
            try {
            	msg.write(bOut);
            }
            catch (final CMSException ex) {
                throw new IOException("Error en la escritura del procesable CMS: " + ex);
            }
            encInfo = new ContentInfo(contentTypeOID, new BERConstructedOctetString(bOut.toByteArray()));
        }
        else{
            encInfo = new ContentInfo(contentTypeOID, null);
        }

        // 4.    CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        ASN1Set certificatesSigned = sd.getCertificates();
        ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        Enumeration<?> certs =certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()){
             vCertsSig.add((DEREncodable) certs.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i=0; i<signerCertificateChain.length;i++)
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            certificates = fillRestCerts(ce,vCertsSig);
        }

        // buscamos que timo de algoritmo es y lo codificamos con su OID
        AlgorithmIdentifier digAlgId;
        String signatureAlgorithm = parameters.getSignatureAlgorithm();
        String digestAlgorithm = null;
        String keyAlgorithm = null;
        int with = signatureAlgorithm.indexOf("with");
        if (with > 0) {
            digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(signatureAlgorithm);
            int and = signatureAlgorithm.indexOf("and", with + 4);
            if (and > 0) keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
            else keyAlgorithm = signatureAlgorithm.substring(with + 4);
        }
        AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
        digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());

        //Identificador del firmante ISSUER AND SERIAL-NUMBER
        TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(tbs.getIssuer(), tbs.getSerialNumber().getValue());
        SignerIdentifier identifier = new SignerIdentifier(encSid);

        //// ATRIBUTOS

        ASN1Set signedAttr = null;
        if (messageDigest == null) {
        	ASN1EncodableVector contextExpecific = Utils.generateSignerInfo(
            		signerCertificateChain[0],           
                    digestAlgorithmId,
                    digestAlgorithm,
                    digAlgId,
                    parameters.getContent(),
                    policy,
                    qualifier,
                    signingCertificateV2,
                    dataType,
                    null
                );
        	signedAttr2 = getAttributeSet(new AttributeTable(contextExpecific));
        	signedAttr = getAttributeSet(new AttributeTable(contextExpecific));
        } else {
        	ASN1EncodableVector contextExpecific = Utils.generateSignerInfo(
            		signerCertificateChain[0],           
                    digestAlgorithmId,
                    digestAlgorithm,
                    digAlgId,
                    null,
                    policy,
                    qualifier,
                    signingCertificateV2,
                    dataType,
                    null
                );
        	signedAttr2 = getAttributeSet(new AttributeTable(contextExpecific));
        	signedAttr = getAttributeSet(new AttributeTable(contextExpecific));
        }

        //digEncryptionAlgorithm
        AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;
        encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());


        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        //Obtenemos los signerInfos del SignedData
        ASN1Set signerInfosSd = null;
        signerInfosSd = sd.getSignerInfos();
        

        //introducimos los SignerInfos Existentes
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        //introducimos el nuevo SignerInfo del firmante actual.

        for(int i =0; i< signerInfosSd.size(); i++){
            SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));
            signerInfos.add(si);
        }

        ASN1OctetString sign2= null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        } catch (AOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        // Creamos los signerInfos del SignedData
    	signerInfos.add(
    		new SignerInfo(
    	        	identifier,
    	        	digAlgId,
    	        	signedAttr,
    	        	encAlgId,
    	        	sign2,
    	        	null //unsignedAttr
	        )
        );
    
        // CRLS no usado
        ASN1Set certrevlist = null;

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
        	PKCSObjectIdentifiers.signedData,
        	new SignedData(
                sd.getDigestAlgorithms(),
                encInfo,
                certificates,
                certrevlist,
                new DERSet(signerInfos)//unsignedAttr
            )
        ).getDEREncoded();

    }

    /**
     * Constructor de la clase.
     * Se crea una cofirma a partir de los datos del firmante y
     * del archivo que contiene las firmas.
     *
     * @param signatureAlgorithm Algoritmo para la firma
     * @param signerCertificateChain Cadena de certificados para la construccion de los
     * 						parametros de firma.
     * @param data          Archivo que contiene las firmas.
     * @param policy        Url de la Politica aplicada.
     * @param qualifier		OID de la pol&iacute;tica.
     * @param signingCertificateV2 <code>true</code> si se desea usar la versi&oacute;n 2 del atributo <i>Signing Certificate</i>
     *                             <code>false</code> para usar la versi&oacute;n 1
     * @param dataType      Identifica el tipo del contenido a firmar.
     * @param keyEntry      Clave privada usada para firmar.
     * @param messageDigest Hash espec&iacute;fico para una firma.
     * 
     * @return              El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
     * @throws javax.security.cert.CertificateException 
     */
	public byte[] coSigner(String signatureAlgorithm, X509Certificate[] signerCertificateChain, InputStream data, String policy, Oid qualifier, boolean signingCertificateV2, Oid dataType, PrivateKeyEntry keyEntry, byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

        ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq=(ASN1Sequence)is.readObject();
        Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        e.nextElement();
        // Contenido de SignedData
        ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
        ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido del SignedData

        SignedData sd = new SignedData(contentSignedData);

        // 3. CONTENTINFO
        // si se introduce el contenido o no
        ContentInfo encInfo = null;
        //DERObjectIdentifier contentTypeOID = new DERObjectIdentifier(dataType.toString());

        encInfo = sd.getEncapContentInfo();
        
        DEROctetString contenido = (DEROctetString)encInfo.getContent();
        byte[] contenidoDatos = null;
        if (contenido != null){
        	contenidoDatos = AOUtil.getDataFromInputStream(contenido.getOctetStream());
        }

        // 4.    CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        ASN1Set certificatesSigned = sd.getCertificates();
        ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        Enumeration<?> certs =certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()){
             vCertsSig.add((DEREncodable) certs.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i=0; i<signerCertificateChain.length;i++)
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            certificates = fillRestCerts(ce,vCertsSig);
        }

        // buscamos que timo de algoritmo es y lo codificamos con su OID
        AlgorithmIdentifier digAlgId;
        String digestAlgorithm = null;
        String keyAlgorithm = null;
        int with = signatureAlgorithm.indexOf("with");
        if (with > 0) {
            digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(signatureAlgorithm);
            int and = signatureAlgorithm.indexOf("and", with + 4);
            if (and > 0) keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
            else keyAlgorithm = signatureAlgorithm.substring(with + 4);
        }
        AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
        digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());

        //Identificador del firmante ISSUER AND SERIAL-NUMBER
        TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(tbs.getIssuer(), tbs.getSerialNumber().getValue());
        SignerIdentifier identifier = new SignerIdentifier(encSid);
        
        //digEncryptionAlgorithm
        AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;
        encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        //Obtenemos los signerInfos del SignedData
        ASN1Set signerInfosSd = null;
        signerInfosSd = sd.getSignerInfos();
        

        //introducimos los SignerInfos Existentes
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();
        //introducimos el nuevo SignerInfo del firmante actual.

        for(int i =0; i< signerInfosSd.size(); i++){
            SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));
            AlgorithmIdentifier algHash = si.getDigestAlgorithm();
            if (algHash.getAlgorithm().toString().equals(digestAlgorithmId.getOID().toString())){
            	ASN1Set signedAttrib = si.getAuthenticatedAttributes();
            	 for (int s=0; s<signedAttrib.size();s++){
            		 ASN1Sequence elemento =(ASN1Sequence) signedAttrib.getObjectAt(s);
            		 DERObjectIdentifier oids = (DERObjectIdentifier)elemento.getObjectAt(0);
            		 if (CMSAttributes.messageDigest.getId().toString().equals(oids.toString())){
            			 DERSet derSetHash = (DERSet) elemento.getObjectAt(1);
            			 DEROctetString derHash = (DEROctetString) derSetHash.getObjectAt(0);
            			 messageDigest = derHash.getOctets();
            		 }
            	 }
            }
            
            signerInfos.add(si);
        }
        
   //// ATRIBUTOS

        ASN1Set signedAttr = null;       
      //atributos firmados
        if (contenidoDatos != null){
        	ASN1EncodableVector contextExpecific = Utils.generateSignerInfo(
            		signerCertificateChain[0],           
                    digestAlgorithmId,
                    digestAlgorithm,
                    digAlgId,
                    contenidoDatos,
                    policy,
                    qualifier,
                    signingCertificateV2,
                    dataType,
                    null
                );
        	signedAttr2 = getAttributeSet(new AttributeTable(contextExpecific));
        	signedAttr = getAttributeSet(new AttributeTable(contextExpecific));
        }
        else if (messageDigest != null) {
        	ASN1EncodableVector contextExpecific = Utils.generateSignerInfo(
        			signerCertificateChain[0],
        			digestAlgorithmId,
        			digestAlgorithm,
        			digAlgId,
        			null,
        			policy,
        			qualifier,
        			signingCertificateV2,
        			dataType,
        			messageDigest
        	);
        	signedAttr2 = getAttributeSet(new AttributeTable(contextExpecific));
        	signedAttr = getAttributeSet(new AttributeTable(contextExpecific));
        }
        else{
        	throw new NullPointerException(
        		"No se puede crear la firma ya que no se ha encontrado un message digest valido"
			);
        }

        ASN1OctetString sign2= null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        } catch (AOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        // Creamos los signerInfos del SignedData
    	signerInfos.add(
    		new SignerInfo(
    	        	identifier,
    	        	digAlgId,
    	        	signedAttr,
    	        	encAlgId,
    	        	sign2,
    	        	null //unsignedAttr
	        )
        );
    
        // CRLS no usado
        ASN1Set certrevlist = null;

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
        	PKCSObjectIdentifiers.signedData,
        	new SignedData(
                sd.getDigestAlgorithms(),
                encInfo,
                certificates,
                certrevlist,
                new DERSet(signerInfos)//unsignedAttr
            )
        ).getDEREncoded();

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
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
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
		} catch (SignatureException e) {
			throw new AOException(
					"Error al configurar la informacion de firma", e);
		}


        //firmamos.
        byte[] realSig=null;
        try {
			realSig = sig.sign();
		} catch (Exception e) {
			throw new AOException("Error durante el proceso de firma", e);
		}

        ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;
    }
}

