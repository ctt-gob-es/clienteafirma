/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers.aobinarysignhelper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.BERConstructedOctetString;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.ess.ESSCertID;
import org.bouncycastle.asn1.ess.ESSCertIDv2;
import org.bouncycastle.asn1.ess.SigningCertificate;
import org.bouncycastle.asn1.ess.SigningCertificateV2;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.DigestInfo;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.IssuerSerial;
import org.bouncycastle.asn1.x509.PolicyInformation;
import org.bouncycastle.asn1.x509.PolicyQualifierInfo;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.asn1.x509.X509Name;
import org.bouncycastle.cms.CMSProcessable;
import org.bouncycastle.cms.CMSProcessableByteArray;
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;

/**
 * Clase que implementa firma digital CMS Advanced Electronic Signatures (CAdES).
 *
 * La implementaci&oacute;n es la misma que para el Signed Data de CMS, salvo que
 * en los atributos del SignerInfo en vez de ir el n&uacute;mero de serie (SerialNumber),
 * va la firma del certificado.<br>
 *
 * La Estructura del mensaje es la siguiente (Se omite la parte correspondiente a CMS):<br>
 * <pre><code>
 * id-aa-ets-sigPolicyId OBJECT IDENTIFIER ::= { iso(1)
 *     member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *     smime(16) id-aa(2) 15 }
 *
 *     SignaturePolicyIdentifier ::= CHOICE {
 *          signaturePolicyId          SignaturePolicyId,
 *          signaturePolicyImplied     SignaturePolicyImplied
 *                                     -- not used in this version
 *  }
 *
 *     SignaturePolicyId ::= SEQUENCE {
 *          sigPolicyId           SigPolicyId,
 *          sigPolicyHash         SigPolicyHash,
 *          sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
 *                                  SigPolicyQualifierInfo OPTIONAL}
 *</code></pre>
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje SignedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */

public final class GenCadesEPESSignedData extends SigUtils{


     ASN1Set signedAttr2;

    /**
     * M&eacute;odo que genera una firma digital usando el sitema conocido como
     * SignedData y que podr&aacute; ser con el contenido del fichero codificado
     * o s&oacute;lo como referencia del fichero.
     *
     * @param parameters    Par&aacute;metros necesarios para obtener los datos
     *                      de SignedData.
     * @param omitContent   Par&aacute;metro qeu indica si en la firma va el
     *                      contenido del fichero o s&oacute;lo va de forma
     *                      referenciada.
     * @param policy        Url de la Politica aplicada.
     * @param qualifier		OID de la pol&iacute;tica.
     * @param signingCertificateV2 <code>true</code> si se desea usar la versi&oacute;n 2 del atributo <i>Signing Certificate</i>
     *                             <code>false</code> para usar la versi&oacute;n 1
     * @param dataType      Identifica el tipo del contenido a firmar.
     * @param keyEntry      Entrada a la clave privada para firma.
     * @param messageDigest	Hash a aplicar en la firma.
     *
     * @return  La firma generada codificada.
     *
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws AOException Cuando ocurre un error durante el proceso de descifrado (formato o clave incorrecto,...)
     */
    public byte[] generateSignedData(
            final P7ContentSignerParameters parameters,
            final boolean omitContent,
            final String policy,
            final Oid qualifier,
            final boolean signingCertificateV2,
            final Oid dataType,
            final PrivateKeyEntry keyEntry,
            final byte[] messageDigest)
            throws NoSuchAlgorithmException, CertificateException, IOException, AOException {

        if (parameters == null) throw new NullPointerException(
        	"Los parametros no pueden ser nulos"
        );

        // 1. VERSION
        // la version se mete en el constructor del signedData y es 1

        // 2. DIGESTALGORITM
        // buscamos que timo de algoritmo es y lo codificamos con su OID

        final ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
        AlgorithmIdentifier digAlgId;

        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        String digestAlgorithm = null;
        String keyAlgorithm = null;
        final int with = signatureAlgorithm.indexOf("with");
        if (with > 0) {
            digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(signatureAlgorithm);
            int and = signatureAlgorithm.indexOf("and", with + 4);
            if (and > 0) keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
            else keyAlgorithm = signatureAlgorithm.substring(with + 4);
        }

        
        final AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);

        try {
            digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());
        }
        catch (final Throwable e) {
        	//e.printStackTrace();
            throw new IOException("Error de codificacion: " + e);
        }

        digestAlgs.add(digAlgId);

        // 3. CONTENTINFO
        // si se introduce el contenido o no

        ContentInfo encInfo = null;
        DERObjectIdentifier contentTypeOID = new DERObjectIdentifier(dataType.toString());

        if (omitContent == false) {
            ByteArrayOutputStream bOut = new ByteArrayOutputStream();
            byte[] content2 = parameters.getContent();
            CMSProcessable msg = new CMSProcessableByteArray(content2);
            try {
            	msg.write(bOut);
            }
            catch (Throwable ex) {
                throw new IOException("Error en la escritura del procesable CMS: " + ex);
            }
            encInfo = new ContentInfo(contentTypeOID, new BERConstructedOctetString(bOut.toByteArray()));
        }
        else encInfo = new ContentInfo(contentTypeOID, null);

        // 4.    CERTIFICADOS
        // obtenemos la lista de certificados

        ASN1Set certificates = null;
        X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        if (signerCertificateChain.length != 0) {
            List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i=0; i<signerCertificateChain.length;i++)
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            certificates = createBerSetFromList(ce);
        }

        ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(tbs.getIssuer(), tbs.getSerialNumber().getValue());

        SignerIdentifier identifier = new SignerIdentifier(encSid);

        //AlgorithmIdentifier
        digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());

        //// ATRIBUTOS

        ASN1Set signedAttr = null;
         signedAttr = generateSignerInfo(
        	signerCertificateChain[0],
            digestAlgorithmId,
            digAlgId,
            digestAlgorithm,
            parameters.getContent(),
            policy,
            qualifier,
            signingCertificateV2,
            dataType,
            messageDigest
        );
        

        ////  FIN ATRIBUTOS

        //digEncryptionAlgorithm
        AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;
        try {
            encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());
        }
        catch (Exception e) {
            throw new IOException("Error de codificacion: " + e);
        }

        ASN1OctetString sign2= null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        } catch (AOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, ex.getMessage(), ex);
            throw ex;
        }

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

        // construimos el Signed Data y lo devolvemos
        return new ContentInfo(
        	PKCSObjectIdentifiers.signedData,
        	new SignedData(
                new DERSet(digestAlgs),
                encInfo,
                certificates,
                certrevlist,
                new DERSet(signerInfos)
            )
        ).getDEREncoded();

    }

    /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario
     *  junto con las pol&iacute;ticas del certificado. Se genera la firma de certificado
     *  usando el estandar SigningCertificateV2 (RFC 5035).
     *
     * @param cert              Certificado necesario para la firma.
     * @param digestAlgorithmId Identifica el algoritmo utilizado firmado.
     * @param digestAlgorithm   Algoritmo Firmado.
     * @param datos             Datos firmados.
     * @param dataType          Identifica el tipo del contenido a firmar.
     * 
     * @return      Los datos necesarios para generar la firma referente a los
     *              datos del usuario.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.security.cert.CertificateException
     * @throws java.io.IOException
     */
    private ASN1Set generateSignerInfo(
            X509Certificate cert,
            AlgorithmId digestAlgorithmId,
            AlgorithmIdentifier digAlgId,
            String digestAlgorithm,
            byte[] datos,
            String politica,
            Oid qualifier,
            boolean signingCertificateV2,
            Oid dataType,
            byte[] messageDigest) throws NoSuchAlgorithmException, CertificateException, IOException{
    	
        //AlgorithmIdentifier
        //digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());

        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        //tipo de contenido
        ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));

        //fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        //MessageDigest
        // Los DigestAlgorithms con SHA-2 tienen un guion:
        if (digestAlgorithm.equals("SHA512")) digestAlgorithm = "SHA-512";
        else if (digestAlgorithm.equals("SHA384")) digestAlgorithm = "SHA-384";
        else if (digestAlgorithm.equals("SHA256")) digestAlgorithm = "SHA-256";
        
        // Si el hash nos viene de fuera lo reutilizamos
        if (messageDigest == null) {
            messageDigest = MessageDigest.getInstance(digestAlgorithm).digest(datos);
        }
        
        ContexExpecific.add(
            new Attribute(
            	CMSAttributes.messageDigest,
                new DERSet(
                	new DEROctetString(
                		messageDigest
                	)
                )
            )
        );

        //Serial Number
        // comentar lo de abajo para version del rfc 3852
        ContexExpecific.add(
    		new Attribute(
				X509Name.SERIALNUMBER,
                new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))
			)
        );

        if (signingCertificateV2){
        	
		/********************************************/
		/*  La Nueva operatividad esta comentada    */
		/********************************************/
        //INICIO SINGING CERTIFICATE-V2

        /**
         * IssuerSerial ::= SEQUENCE {
         *   issuer                   GeneralNames,
         *   serialNumber             CertificateSerialNumber
         *
         */

        TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
        GeneralName gn = new GeneralName(tbs.getIssuer());
        GeneralNames gns = new GeneralNames(gn);

        IssuerSerial isuerSerial = new IssuerSerial(gns,tbs.getSerialNumber());


        /**
         * ESSCertIDv2 ::=  SEQUENCE {
         *       hashAlgorithm           AlgorithmIdentifier  DEFAULT {algorithm id-sha256},
         *       certHash                 Hash,
         *       issuerSerial             IssuerSerial OPTIONAL
         *   }
         *
         *   Hash ::= OCTET STRING
         */

        MessageDigest md = MessageDigest.getInstance(AOCryptoUtil.getDigestAlgorithmName(digestAlgorithmId.getName()));
        byte [] certHash = md.digest(cert.getEncoded());
        ESSCertIDv2[] essCertIDv2 = {new ESSCertIDv2(digAlgId,certHash,isuerSerial)};


        /**
         * PolicyInformation ::= SEQUENCE {
         *           policyIdentifier   CertPolicyId,
         *           policyQualifiers   SEQUENCE SIZE (1..MAX) OF
         *                                  PolicyQualifierInfo OPTIONAL }
         *
         *      CertPolicyId ::= OBJECT IDENTIFIER
         *
         *      PolicyQualifierInfo ::= SEQUENCE {
         *           policyQualifierId  PolicyQualifierId,
         *           qualifier          ANY DEFINED BY policyQualifierId }
         *
         */
               
        PolicyInformation[] pI ; 
        SigningCertificateV2 scv2 = null;
        if (qualifier!=null){
        	
	        DERObjectIdentifier oidQualifier = new DERObjectIdentifier(qualifier.toString());
	        if(politica.equals("")){
	        	pI = new PolicyInformation[]{new PolicyInformation(oidQualifier)};
	        }else{
	        	PolicyQualifierInfo pqInfo = new PolicyQualifierInfo(politica);
	        	pI = new PolicyInformation[]{new PolicyInformation(oidQualifier, new DERSequence(pqInfo))};
	        }
	        
	        /**
	         * SigningCertificateV2 ::=  SEQUENCE {
	         *          certs        SEQUENCE OF ESSCertIDv2,
	         *          policies     SEQUENCE OF PolicyInformation OPTIONAL
	         *      }
	         *
	         */ 
	        scv2 = new SigningCertificateV2(essCertIDv2,pI); // con politica
        }
        else{
        	scv2 = new SigningCertificateV2(essCertIDv2);	// Sin politica
        }

        //Secuencia con singningCertificate
        ContexExpecific.add(
                new Attribute(
            	PKCSObjectIdentifiers.id_aa_signingCertificateV2,
                new DERSet(
                	scv2
                )
            )
                );


        //FIN SINGING CERTIFICATE-V2
    
        }else{
        	
        //INICIO SINGNING CERTIFICATE
        	
			/**
			 *	IssuerSerial ::= SEQUENCE {
			 *	     issuer                   GeneralNames,
			 *	     serialNumber             CertificateSerialNumber
			 *	}
        	 */
        	
        	 
        TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
        GeneralName gn = new GeneralName(tbs.getIssuer());
        GeneralNames gns = new GeneralNames(gn);

        IssuerSerial isuerSerial = new IssuerSerial(gns,tbs.getSerialNumber());

        /**
         *	ESSCertID ::=  SEQUENCE {
         *   certHash                 Hash,
         *   issuerSerial             IssuerSerial OPTIONAL
       	 *	}
		 * 
       	 *	Hash ::= OCTET STRING -- SHA1 hash of entire certificate
         */
        //MessageDigest
        // El getName() del AlgorithmId devuelve los nombres del SHA2 sin guion y este es necesario
        // para el algoritmo de hash.
        String digestAlgorithmName = AOCryptoUtil.getDigestAlgorithmName(digestAlgorithmId.getName());
        MessageDigest md = MessageDigest.getInstance(digestAlgorithmName);
        byte [] certHash = md.digest(cert.getEncoded());
        ESSCertID essCertID = new ESSCertID(certHash,isuerSerial);
        
        /**
         * PolicyInformation ::= SEQUENCE {
         *           policyIdentifier   CertPolicyId,
         *           policyQualifiers   SEQUENCE SIZE (1..MAX) OF
         *                                  PolicyQualifierInfo OPTIONAL }
         *
         *      CertPolicyId ::= OBJECT IDENTIFIER
         *
         *      PolicyQualifierInfo ::= SEQUENCE {
         *           policyQualifierId  PolicyQualifierId,
         *           qualifier          ANY DEFINED BY policyQualifierId }
         *
         */
        
        PolicyInformation[] pI ; 
        SigningCertificate scv = null;
        if (qualifier!=null){
        	
	        DERObjectIdentifier oidQualifier = new DERObjectIdentifier(qualifier.toString());
	        if(politica.equals("")){
	        	pI = new PolicyInformation[]{new PolicyInformation(oidQualifier)};
	        }else{
	        	PolicyQualifierInfo pqInfo = new PolicyQualifierInfo(politica);
	        	pI = new PolicyInformation[]{new PolicyInformation(oidQualifier, new DERSequence(pqInfo))};
	        }
	        
	        /**
	         * SigningCertificateV2 ::=  SEQUENCE {
	         *          certs        SEQUENCE OF ESSCertIDv2,
	         *          policies     SEQUENCE OF PolicyInformation OPTIONAL
	         *      }
	         *
	         */ 
	        /* HAY QUE HACER UN SEQUENCE, YA QUE EL CONSTRUCTOR DE BOUNCY CASTLE
	         * NO TIENE DICHO CONSTRUCTOR.
	         */
	        ASN1EncodableVector v = new ASN1EncodableVector();
	        v.add(new DERSequence(essCertID));
	        v.add(new DERSequence(pI));
	        scv = new SigningCertificate(new DERSequence(v)); //con politica
        }
        else{
        	scv = new SigningCertificate(essCertID);	// Sin politica
        }
        
        /**
         * id-aa-signingCertificate OBJECT IDENTIFIER ::= { iso(1)
		 *   member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
		 *   smime(16) id-aa(2) 12 }
         */
        //Secuencia con singningCertificate
        ContexExpecific.add(
                new Attribute(
            	PKCSObjectIdentifiers.id_aa_signingCertificate,
                new DERSet(
                	scv
                )
            )
                );
        }    
        
        // INICIO SIGPOLICYID ATTRIBUTE

        if (qualifier!=null){
	        /*
	         *  SigPolicyId ::= OBJECT IDENTIFIER
	         *  Politica de firma.
	         */
	        DERObjectIdentifier DOISigPolicyId = new DERObjectIdentifier(qualifier.toString());
	
	        /*
	         *   OtherHashAlgAndValue ::= SEQUENCE {
	         *     hashAlgorithm    AlgorithmIdentifier,
	         *     hashValue        OCTET STRING }
	         *
	         */
	         MessageDigest mdgest = MessageDigest.getInstance(digestAlgorithm);
	         byte[] hashed = mdgest.digest(politica.getBytes());
	         DigestInfo OtherHashAlgAndValue =new DigestInfo(digAlgId, hashed);
	
	          /*
	         *   SigPolicyQualifierInfo ::= SEQUENCE {
	         *       SigPolicyQualifierId  SigPolicyQualifierId,
	         *       SigQualifier          ANY DEFINED BY policyQualifierId }
	         */
	
	        SigPolicyQualifierInfo spqInfo = new SigPolicyQualifierInfo(politica);
	
	        /*
	         * SignaturePolicyId ::= SEQUENCE {
	         *  sigPolicyId           SigPolicyId,
	         *  sigPolicyHash         SigPolicyHash,
	         *  sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
	         *                          SigPolicyQualifierInfo OPTIONAL}
	         *
	         */
	        ASN1EncodableVector v = new ASN1EncodableVector();
	        //sigPolicyId
	        v.add(DOISigPolicyId);
	        //sigPolicyHash
	        v.add(OtherHashAlgAndValue.toASN1Object()); // como sequence
	        //sigPolicyQualifiers
	        v.add(spqInfo.toASN1Object());
	
	        DERSequence ds = new DERSequence(v);
	
	
	        //Secuencia con singningCertificate
	        ContexExpecific.add(
	                new Attribute(
	            	PKCSObjectIdentifiers.id_aa_ets_sigPolicyId,
	                    new DERSet(
	                        ds.toASN1Object()
	                    )
	                )
	                );
	        // FIN SIGPOLICYID ATTRIBUTE
        }

        signedAttr2 = getAttributeSet(new AttributeTable(ContexExpecific));

        return getAttributeSet(new AttributeTable(ContexExpecific));

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
		} catch (Exception e) {
		    throw new AOException("No ha instalado soporte para el algoritmo de firma '" + signatureAlgorithm + "'", e);
		}

        byte[] tmp= null;

        try {
            tmp = signedAttr2.getEncoded(ASN1Encodable.DER);
        } catch (IOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, ex.toString(), ex);
            throw new AOException("Error al detectar la codificacion de los datos ASN.1", ex);
        }

        //Indicar clave privada para la firma
		try {
			sig.initSign(keyEntry.getPrivateKey());
		} catch (Exception e) {
			throw new AOException(
					"Error al obtener la clave de firma para el algoritmo '" + signatureAlgorithm + "': " + e);
		}

        // Actualizamos la configuracion de firma
		try {
			sig.update(tmp);
		} catch (SignatureException e) {
			throw new AOException(
					"Error al configurar la informacion de firma: " + e);
		}


        //firmamos.
        byte[] realSig=null;
        try {
			realSig = sig.sign();
		} catch (Exception e) {
			throw new AOException("Error durante el proceso de firma: " + e);
		}

        ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;


    }


}
