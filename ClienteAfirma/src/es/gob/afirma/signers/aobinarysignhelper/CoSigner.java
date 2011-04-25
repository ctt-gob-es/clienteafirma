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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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
import org.bouncycastle.asn1.DERPrintableString;
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
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.asn1.x509.X509Name;
import org.bouncycastle.cms.CMSProcessable;
import org.bouncycastle.cms.CMSProcessableByteArray;
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOUtil;

/**
 * Clase que implementa la cofirma digital PKCS#7/CMS SignedData
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje SignedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 * pero con la peculiaridad de que es una Cofirma.
 */
public final class CoSigner  {

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
     * @param dataType      Identifica el tipo del contenido a firmar.
     * @param keyEntry      Clave privada del firmante.
     * @param atrib 		Atributos firmados opcionales. 
	 * @param uatrib		Atributos no autenticados firmados opcionales.
	 * @param messageDigest	Hash a aplicar en la firma.
	 * 
     * @return              El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
     */
	public byte[] coSigner(final P7ContentSignerParameters parameters, 
			               final byte[] sign, 
			               final boolean omitContent,
			               final Oid dataType, 
			               final PrivateKeyEntry keyEntry, 
			               final Map<Oid, byte[]> atrib, 
			               final Map<Oid, byte[]> uatrib, 
			               final byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {
    	 
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
        ASN1ObjectIdentifier contentTypeOID =  new ASN1ObjectIdentifier(dataType.toString());

        if (omitContent == false) {
            ByteArrayOutputStream bOut = new ByteArrayOutputStream();
            byte[] content2 = parameters.getContent();
            CMSProcessable msg = new CMSProcessableByteArray(content2);
            try {
            	msg.write(bOut);
            }
            catch (final Throwable ex) {
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
        while (certs.hasMoreElements()) vCertsSig.add((DEREncodable) certs.nextElement());

        if (signerCertificateChain.length != 0) {
            // descomentar lo siguiente para version del rfc 3852
             List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i=0; i<signerCertificateChain.length;i++)
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            certificates = fillRestCerts(ce,vCertsSig);

            //y comentar esta parte de abajo
//            vCertsSig.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getEncoded())));
//            certificates = new BERSet(vCertsSig);

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

        //atributos firmados
        ASN1Set signedAttr = null;
        if (messageDigest == null) signedAttr = generateSignerInfo(
    		signerCertificateChain[0], 
    		digestAlgorithm, 
    		parameters.getContent(), 
    		dataType,
    		atrib
		);
        else signedAttr = generateSignerInfoFromHash(
        	signerCertificateChain[0],
        	digestAlgorithm, 
        	messageDigest, 
        	dataType, 
        	atrib
    	);

        //atributos no firmados.
         ASN1Set unSignedAttr = null;
        unSignedAttr = generateUnsignerInfo(uatrib);

        //// FIN ATRIBUTOS

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
        } 
        catch (final Throwable ex) {
            throw new IOException("Error al generar la firma: " + ex);
        }


        // Creamos los signerInfos del SignedData
    	signerInfos.add(
    		new SignerInfo(
    	        	identifier,
    	        	digAlgId,
    	        	signedAttr,
    	        	encAlgId,
    	        	sign2,
    	        	unSignedAttr//null //unsignedAttr
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
     * Se crea una cofirma a partir de los datos del firmante y el archivo que se firma.
     *
     * @param signatureAlgorithm Algoritmo para la firma
     * @param signerCertificateChain Cadena de certificados para la construccion de los
     * 						parametros de firma.
     * @param sign          Archivo que contiene las firmas.
     * @param dataType      Identifica el tipo del contenido a firmar.
     * @param keyEntry      Clave privada del firmante.
     * @param atrib         Atributos firmados adicionales.
     * @param uatrib        Atributos no firmados adicionales.
     * @param messageDigest	Hash a aplicar en la firma.
     * 
     * @return              El archivo de firmas con la nueva firma.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
     * @throws java.security.cert.CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
     */
	public byte[] coSigner(
			String signatureAlgorithm, 
			X509Certificate[] signerCertificateChain, 
			byte[] sign, 
			Oid dataType, 
			PrivateKeyEntry keyEntry, 
			Map<Oid, byte[]> atrib, 
			Map<Oid, byte[]> uatrib, 
			byte[] messageDigest) throws IOException, NoSuchAlgorithmException, CertificateException {

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
        //DERObjectIdentifier contentTypeOID =  new DERObjectIdentifier(dataType.toString());

        encInfo = sd.getEncapContentInfo();

        DEROctetString contenido = (DEROctetString)encInfo.getContent();
        byte[] contenidoDatos = null;
        if (contenido != null){
        	contenidoDatos = AOUtil.getDataFromInputStream(contenido.getOctetStream());
        }
        
        // 4.    CERTIFICADOS
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        //X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        ASN1Set certificatesSigned = sd.getCertificates();
        ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
        Enumeration<?> certs =certificatesSigned.getObjects();

        // COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
        while (certs.hasMoreElements()){
             vCertsSig.add((DEREncodable) certs.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            // descomentar lo siguiente para version del rfc 3852
             List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i=0; i<signerCertificateChain.length;i++)
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            certificates = fillRestCerts(ce,vCertsSig);

            //y comentar esta parte de abajo
//            vCertsSig.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getEncoded())));
//            certificates = new BERSet(vCertsSig);

        }

        // buscamos que tipo de algoritmo es y lo codificamos con su OID
        AlgorithmIdentifier digAlgId;
//        String signatureAlgorithm = parameters.getSignatureAlgorithm();
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

        //atributos firmados
        ASN1Set signedAttr = null;
        
        //atributos no firmados.
         ASN1Set unSignedAttr = null;
        unSignedAttr = generateUnsignerInfo(uatrib);

        //// FIN ATRIBUTOS

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

        // Secuencia:
        // 1.- Si cofirmamos sin datos en el mismo algoritmo de hash que la firma
        //     original sacamos el messagedigest de la firma previa.
        // 2.- Si no es el mismo algoritmo, miramos si nos ha llegado un messagedigest
        //     como parametro del metodo, que quiere decir que se ha calculado externamente
        //     (en el fondo sera que no se ha sobreescrito el parametro, con lo que si llego
        //     != null, seguira siendo != null)
        // 3.- Si no es ninguno de los dos casos, no podemos firmar
        for(int i =0; i< signerInfosSd.size(); i++){
            SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));
            AlgorithmIdentifier algHash = si.getDigestAlgorithm();
            // Solo si coninciden los algos puedo sacar el hash de dentro
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
        
        
        //atributos firmados
        if(contenidoDatos != null){
        	signedAttr = generateSignerInfo(signerCertificateChain[0], digestAlgorithm, contenidoDatos, dataType, atrib);
        }
        else if (messageDigest!=null)
        	signedAttr = generateSignerInfoFromHash(signerCertificateChain[0], digestAlgorithm, messageDigest, dataType,atrib);
        else {
        	// En este caso no puedo usar un hash de fuera, ya que no me han pasado datos ni
        	// huellas digitales, solo un fichero de firma
        	throw new NullPointerException(
              "No se puede crear la firma ya que no se ha encontrado un hash valido."
			);
        }
        
        
        ASN1OctetString sign2 = null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        } 
        catch (final Throwable ex) {
            throw new IOException("Error al generar la firma: " + ex);
        }


        // Creamos los signerInfos del SignedData
    	signerInfos.add(
    		new SignerInfo(
    	        	identifier,
    	        	digAlgId,
    	        	signedAttr,
    	        	encAlgId,
    	        	sign2,
    	        	unSignedAttr//null //unsignedAttr
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
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
     *  Se generan los atributos que se necesitan para generar la firma.
     *
     * @param cert              Certificado necesario para la firma.
     * @param digestAlgorithm   Algoritmo Firmado.
     * @param datos             Datos firmados.
     * @param dataType      Identifica el tipo del contenido a firmar.
     * @param atrib    Lista de atributos firmados que se insertar&aacute;n dentro del archivo de firma.
     *
     * @return      Los atributos firmados de la firma.
     *
     * @throws java.security.NoSuchAlgorithmException
     */
    private ASN1Set generateSignerInfo(X509Certificate cert,
                            String digestAlgorithm,
                            byte[] datos,
                            Oid dataType,
                            Map<Oid, byte[]> atrib)
                        throws NoSuchAlgorithmException {

        
        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        //tipo de contenido
        ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));

        //fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // Los DigestAlgorithms con SHA-2 tienen un guion:
        if (digestAlgorithm.equals("SHA512")) digestAlgorithm = "SHA-512";
        else if (digestAlgorithm.equals("SHA384")) digestAlgorithm = "SHA-384";
        else if (digestAlgorithm.equals("SHA256")) digestAlgorithm = "SHA-256";
        
        // Si nos viene el hash de fuera no lo calculamos
        byte[] md = MessageDigest.getInstance(digestAlgorithm).digest(datos);
        
        //MessageDigest
        ContexExpecific.add(
            new Attribute(
            	CMSAttributes.messageDigest,
                new DERSet(new DEROctetString(md.clone()))
            )
        );
        
        //agregamos la lista de atributos a mayores.
        if (atrib.size()!=0){
        	Iterator<Map.Entry<Oid, byte[]>> it = atrib.entrySet().iterator();
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
         *  Se generan los atributos que se necesitan para generar la firma.
         *  En este caso se introduce el hash directamente.
         *
         * @param cert              Certificado necesario para la firma.
         * @param digestAlgorithm   Algoritmo Firmado.
         * @param datos             Datos firmados.
         * @param dataType      Identifica el tipo del contenido a firmar.
         * @param atrib    Lista de atributos firmados que se insertar&aacute;n dentro del archivo de firma.
         *
         * @return      Los atributos firmados de la firma.
         */
        private ASN1Set generateSignerInfoFromHash(X509Certificate cert,
                                String digestAlgorithm,
                                byte[] datos,
                                Oid dataType,
                                Map<Oid, byte[]> atrib) {
            
            //// ATRIBUTOS

            //authenticatedAttributes
            ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

            //tipo de contenido
            ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));

            //fecha de firma
            ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

            //MessageDigest
            ContexExpecific.add(
                new Attribute(
                	CMSAttributes.messageDigest,
                    new DERSet(
                    	new DEROctetString(
                    			datos
                    	)
                    )
                )
            );

        //Serial Number
        ContexExpecific.add(
    		new Attribute(
				X509Name.SERIALNUMBER,
                new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))
			)
        );

        //agregamos la lista de atributos a mayores.
        if (atrib.size()!=0){
        	Iterator<Map.Entry<Oid, byte[]>> it = atrib.entrySet().iterator();
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
     *
     * @param uatrib    Conjunto de atributos no firmados que se insertar&aacute;n dentro del archivo de firma.
     *
     * @return      Los atributos no firmados de la firma
     */
    private ASN1Set generateUnsignerInfo(Map<Oid, byte[]> uatrib){

        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();


        //agregamos la lista de atributos a mayores.
        if (uatrib.size()!=0){
        	Iterator<Map.Entry<Oid, byte[]>> it = uatrib.entrySet().iterator();
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

