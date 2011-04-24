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

import java.io.IOException;
import java.util.Enumeration;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

/**
 * 
 * Clase que obtiene el tipo de datos declarado en una firma mediante su Mime Type. 
 * Para ello, hacemos uso de la estructura SignedData.
 * Es cuesti&oacute;n de obtener el "contentInfo" y obtener el OID del tipo.
 * 
 * SignedData ::= SEQUENCE {
 *                     version           Version,
 *                     digestAlgorithms  DigestAlgorithmIdentifiers,
 *                     contentInfo       ContentInfo,
 *                     certificates      [0]  CertificateSet OPTIONAL,
 *                     crls              [1]  CertificateRevocationLists OPTIONAL,
 *                    signerInfos       SignerInfos
 *                  } 
 * 
 * ContentInfo ::= SEQUENCE {
     *          contentType ContentType,
     *          content
     *          [0] EXPLICIT ANY DEFINED BY contentType OPTIONAL }
 *
 */
public final class ExtractMimeType {
	
	/**
	 * Extrae el OID del MimeType asociado de una firma.
	 * 
	 * @param data	Fichero que contiene la firma.	
	 * 
	 * @return		OID del MimeType
	 */
	public String extractMimeType(byte[] data){
		String objectIdentifier = "";
		try {
			ASN1InputStream is = new ASN1InputStream(data);
	        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
	        ASN1Sequence dsq = null;
	        
	        dsq=(ASN1Sequence)is.readObject();
			
	        Enumeration<?> e = dsq.getObjects();
	        // Elementos que contienen los elementos OID Data
	        DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
	        if (doi.equals(PKCSObjectIdentifiers.signedData)){
	        	// Contenido de SignedData
	            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
	            ASN1Sequence datos = (ASN1Sequence) doj.getObject();
	            SignedData sd = new SignedData(datos);
	            
	            // El contentype es el que nos dice el tipo de contenido que es. 
	            // Consultar la cabecera de la clase 
	            ContentInfo type = sd.getEncapContentInfo();
	            DERObjectIdentifier doid = type.getContentType();
	            
	            objectIdentifier = doid.getId();
	            	
	            return objectIdentifier;	            
	        }
		} catch (IOException e1) {
			e1.printStackTrace();
		}
        return objectIdentifier;
		
	}
		

}
