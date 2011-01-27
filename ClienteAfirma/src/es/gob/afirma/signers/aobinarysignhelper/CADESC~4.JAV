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
import java.util.Enumeration;
import java.util.List;
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
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;

/**
 * Clase que implementa la contrafirma digital CADES SignedAndEnvelopedData
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje SignedAndEnvelopedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 * pero con la peculiaridad de que es una Contrafirma.
 */
public final class CadesCounterSignerEnveloped extends SigUtils {

    /* Propiedades de la clase */
    private int actualIndex = 0;
    private Oid actualOid=null;
    private ASN1Set signedAttr2;
    
    private String globalPolicy="";
    private Oid GlobalOidQualifier=null;
    private boolean GlobalsigningCertificateV2;
    

    
    Oid getActualOid() {
		return actualOid;
	}

	void setActualOid(Oid actualOid) {
		this.actualOid = actualOid;
	}

	/**
	 * Obtiene el Oid del qualificador de pol&iacute;tica
	 * @return Oid de cualificador de pol&iacute;tica
	 */
	Oid getGlobalOidQualifier() {
		return GlobalOidQualifier;
	}

	/**
	 * Establece el Oid del qualificador de pol&iacute;tica
	 * @param globalOidQualifier	Oid de cualificador de pol&iacute;tica
	 */
	void setGlobalOidQualifier(Oid globalOidQualifier) {
		GlobalOidQualifier = globalOidQualifier;
	}

	/**
	 * Obtiene el tipo de atributo firmado signingCertificate o signingCertificateV2
	 * @return tipo de atributo firmado.
	 */
	boolean isGlobalsigningCertificateV2() {
		return GlobalsigningCertificateV2;
	}

	/**
	 * Define si el atributo firmado es signingCertificate o signingCertificateV2
	 * @param globalsigningCertificateV2 tipo de atributo
	 */
	void setGlobalsigningCertificateV2(boolean globalsigningCertificateV2) {
		GlobalsigningCertificateV2 = globalsigningCertificateV2;
	}

	/**
     * Obtiene la pol&iacute;tica global.
     * @return politica de firma.
     */
    public String getGlobalPolicy() {
        return globalPolicy;
    }

    /**
     * Establece la pol&iacute;tica de firma.
     * @param globalPolicy polÃ­tica de firma (URL).
     */
    public void setGlobalPolicy(String globalPolicy) {
        this.globalPolicy = globalPolicy;
    }

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
     * @param policy 		URL de pol&iacute;tica.
     * @param qualifier		OID de la pol&iacute;tica.
     * @param signingCertificateV2 <code>true</code> si se desea usar la versi&oacute;n 2 del atributo <i>Signing Certificate</i>
     *                             <code>false</code> para usar la versi&oacute;n 1
     * @param dataType      Identifica el tipo del contenido a firmar.
     * 
     * @return              El archivo de firmas con la nueva firma.
     * @throws java.io.IOException	Excepci&oacute;n cuando se produce algun error con lectura escritura de ficheros.
     * @throws java.security.NoSuchAlgorithmException Excepci&oacute;n cuando no se encuentra el algoritmo de firma.
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
     * @throws AOException Cuando ocurre alguno error con contemplado por las otras excepciones declaradas
     */
    @SuppressWarnings("unchecked")
	public byte[] counterSigner(P7ContentSignerParameters parameters, byte[] data, CounterSignTarget targetType, int[] targets, PrivateKeyEntry keyEntry, String policy, Oid qualifier, boolean signingCertificateV2, Oid dataType) throws IOException, NoSuchAlgorithmException, CertificateException, AOException {

        // Inicializamos el Oid
        actualOid= dataType;

        //Introducimos la pol&iacute;tica en variable global por comodidad. &Eacute;sta no var&iacute;a.
        this.setGlobalPolicy(policy);
        this.setGlobalOidQualifier(qualifier);
        this.setGlobalsigningCertificateV2(signingCertificateV2);
        
        ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq = (ASN1Sequence) is.readObject();
        Enumeration<Object> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedAndEnvelopedData
        e.nextElement();
        // Contenido de SignedAndEnvelopedData
        ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();

        SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);

        //Obtenemos los signerInfos del SignedAndEnvelopedData
        ASN1Set signerInfosSd = null;
        signerInfosSd = sd.getSignerInfos();

        // 4.    CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        ASN1Set certificatesSigned = sd.getCertificates();
        ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        Enumeration<Object> certs = certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()) {
            vCertsSig.add((DEREncodable) certs.nextElement());
        }
        // e introducimos los del firmante actual.
        if (signerCertificateChain.length != 0) {
            List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i = 0; i < signerCertificateChain.length; i++) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            }
            certificates = FillRestCerts(ce, vCertsSig);
        }

        // CRLS no usado
        ASN1Set certrevlist = null;

        // 5. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        //FIRMA EN ARBOL
        if (targetType.equals(CounterSignTarget.Tree)) {
            signerInfos = CounterTree(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        }
        //FIRMA DE LAS HOJAS
        else if (targetType.equals(CounterSignTarget.Leafs)) {
            signerInfos = CounterLeaf(signerInfosSd, parameters, signerCertificateChain[0], keyEntry);
        }
        //FIRMA DE NODOS
        else if (targetType.equals(CounterSignTarget.Nodes)) {
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
                ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedAndEnvelopedData
                aux = new SignedAndEnvelopedData(contentSignedData2);
            }

            // construimos el Signed Data y lo devolvemos
            return new ContentInfo(
                    PKCSObjectIdentifiers.signedAndEnvelopedData,
                    aux).getDEREncoded();
        }
        //FIRMA DE LOS SIGNERS
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
                ASN1Sequence contentSignedData2 = (ASN1Sequence) sd2.readObject();// contenido del SignedAndEnvelopedData

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
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateException
     * @throws es.map.es.map.afirma.exceptions.AOException
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
     * @param sd              SignedAndEnvelopedData que contiene el Nodo ra&iacute;z.
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
     * @param signerInfo	  Nodo ra&iacute; que contiene todos los signerInfos
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
    @SuppressWarnings("unchecked")
	private SignerInfo getCounterUnsignedAtributes(SignerInfo signerInfo,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {
        ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            Enumeration<Object> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)){
	                ASN1Set setInto = data.getAttrValues();
	                Enumeration<Object> eAtributesData = setInto.getObjects();
	                while (eAtributesData.hasMoreElements()) {
	                    ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
	                    SignerInfo si = new SignerInfo(atrib);
	
	                    SignerInfo obtained = getCounterUnsignedAtributes(si, parameters, cert, keyEntry);
	                    signerInfosU.add(obtained);
	
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

            } else {
            	if(signerInfosU.size() == 1){
            		if (signerInfosU.get(0) instanceof Attribute){
            			//añadimos el que hay
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
                            new DERSet(uAtrib) //unsignedAttr
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
     * M&eacute;todo utilizado por la firma de una hoja del &eacute;rbol para obtener la
     * contrafirma de los signerInfo de una determinada hoja de forma recursiva.</br>
     *
     * @param signerInfo	  Nodo ra&iacute; que contiene todos los signerInfos
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
    @SuppressWarnings("unchecked")
	private SignerInfo getCounterLeafUnsignedAtributes(SignerInfo signerInfo,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            Enumeration<Object> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();

            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)){
	                ASN1Set setInto = data.getAttrValues();
	                Enumeration<Object> eAtributesData = setInto.getObjects();
	                while (eAtributesData.hasMoreElements()) {
	                    ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
	                    SignerInfo si = new SignerInfo(atrib);
	
	                    SignerInfo obtained = getCounterLeafUnsignedAtributes(si, parameters, cert, keyEntry);
	                    signerInfosU.add(obtained);
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
                Attribute uAtrib = new Attribute(CMSAttributes.counterSignature, new DERSet(signerInfosU));
                CounterSigner = new SignerInfo(
                        signerInfo.getSID(),
                        signerInfo.getDigestAlgorithm(),
                        signerInfo.getAuthenticatedAttributes(),
                        signerInfo.getDigestEncryptionAlgorithm(),
                        signerInfo.getEncryptedDigest(),
                        new DERSet(uAtrib) //unsignedAttr
                        );

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
     * @param signerInfo      Nodo ra&iacute; que contiene todos los signerInfos
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
    @SuppressWarnings("unchecked")
	private SignerInfo getCounterNodeUnsignedAtributes(SignerInfo signerInfo,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry)
            throws NoSuchAlgorithmException, IOException, CertificateException {

        ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        ASN1EncodableVector signerInfosU2 = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            Enumeration<Object> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)){
	                ASN1Set setInto = data.getAttrValues();
	                Enumeration<Object> eAtributesData = setInto.getObjects();
	                while (eAtributesData.hasMoreElements()) {
	                    ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
	                    SignerInfo si = new SignerInfo(atrib);
	                    signerInfosU.add(si);
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

            } else {
            	if(signerInfosU.size() == 1){
            		if (signerInfosU.get(0) instanceof Attribute){
            			//añadimos el que hay
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
                            new DERSet(uAtrib) //unsignedAttr
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
    @SuppressWarnings("unchecked")
	private SignerInfo getCounterNodeUnsignedAtributes(SignerInfo signerInfo,
            P7ContentSignerParameters parameters,
            X509Certificate cert,
            PrivateKeyEntry keyEntry,
            int node)
            throws NoSuchAlgorithmException, IOException, CertificateException, AOException {

        ASN1EncodableVector signerInfosU = new ASN1EncodableVector();
        SignerInfo CounterSigner = null;
        SignerInfo CounterSigner2 = null;
        if (signerInfo.getUnauthenticatedAttributes() != null) {
            Enumeration<Object> eAtributes = signerInfo.getUnauthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)){
	                ASN1Set setInto = data.getAttrValues();
	                Enumeration<Object> eAtributesData = setInto.getObjects();
	                while (eAtributesData.hasMoreElements()) {
	                    ASN1Sequence atrib = (ASN1Sequence) eAtributesData.nextElement();
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
            			//añadimos el que hay
                		ContexExpecific.add(signerInfosU.get(0));
                        
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
                            new DERSet(uAtrib) //unsignedAttr
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
        unsignedAttr = generateSignerInfo(cert,
                                            digestAlgorithmId,
                                            digAlgId,
                                            digestAlgorithm,
                                            si.getEncryptedDigest().getOctets());

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
            Logger.getLogger(CadesCounterSignerEnveloped.class.getName()).log(Level.SEVERE, null, ex);
        }

        SignerInfo uAtrib = new SignerInfo(
                identifier,
                digAlgId,
                unsignedAttr,
                encAlgId,
                sign2,
                null);

        return uAtrib;

    }
    
    /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
     *  Se generan los atributos que se necesitan para generar la firma.
     *
     * @param digestAlgorithmId Identificador del algoritmo de firma.
     * @param digestAlgorithm   Algoritmo Firmado.
     * @param data             Datos firmados.
     *
     * @return      Los datos necesarios para generar la firma referente a los
     *              datos del usuario.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.security.cert.CertificateException
     * @throws java.io.IOException
     */
     private ASN1Set generateSignerInfo(X509Certificate cert,
                            AlgorithmId digestAlgorithmId,
                            AlgorithmIdentifier digAlgId,
                            String digestAlgorithm,
                            byte[] data)
                        throws NoSuchAlgorithmException, CertificateException, IOException{
    	 
    	 //recuperamos las variables globales
    	 String politica = getGlobalPolicy();
    	 boolean signingCertificateV2 = isGlobalsigningCertificateV2();
    	 Oid qualifier = getGlobalOidQualifier();

        //AlgorithmIdentifier
        //digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());

        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

// Las contrafirmas CAdES no tienen COntentType
//        //tipo de contenido
//        ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(actualOid.toString()))));

        //fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        //MessageDigest
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
                		MessageDigest.getInstance(digestAlgorithm).digest(data)
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
        // Los DigestAlgorithms con SHA-2 tienen un guion:
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
            e.printStackTrace();
		}

        byte[] tmp= null;

        try {
            tmp = signedAttr2.getEncoded(ASN1Encodable.DER);
        } catch (IOException ex) {
            Logger.getLogger(CadesCounterSignerEnveloped.class.getName()).log(Level.SEVERE, null, ex);
        }

        //Indicar clave privada para la firma
		try {
			sig.initSign(keyEntry.getPrivateKey());
		} catch (final Throwable e) {
			throw new AOException(
					"Error al obtener la clave de firma para el algoritmo '" + signatureAlgorithm + "'", e);
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
