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

import java.io.InputStream;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AuthEnvelopedData;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.cms.CompressedData;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;


/**
 * Clase que verifica los distintos tipos de firma para CMS a partir de un fichero
 * pasado por par&aacute;metro.
 *
 * La verificaci&oacute; es para los tipo:
 *
 * <ul>
 * <li>Data</li>
 * <li>Signed Data</li>
 * <li>Digested Data</li>
 * <li>Encrypted Data</li>
 * <li>Enveloped Data</li>
 * <li>Signed and Enveloped Data</li>
 * </ul>
 */
public final class ValidateCMS {

    /**
     * M&eacute;todo que verifica que es una firma de tipo "data"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSData(InputStream data){
        boolean isValid = true;
        try {
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq=(ASN1Sequence)is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.data)){
                isValid=false;
            }
            // Contenido de Data
            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();

            /* Estas variables no se usan, solo es para verificar que
               la conversion ha sido correcta. De no ser asi, se pasaria
               al manejo de la excepcion.*/
            new DEROctetString(doj.getObject());

        } catch (Exception ex) {
            //Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion " + ex);
            isValid= false;
        }

      return isValid;
    }

    /**
     * M&eacute;todo que verifica que es una firma de tipo "Signed data"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSSignedData(InputStream data){
        //boolean isValid = false;
    	boolean isValid = true;
        try {
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq=(ASN1Sequence)is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.signedData)){
                isValid=true;
            }
            // Contenido de SignedData
            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
            ASN1Sequence datos = (ASN1Sequence) doj.getObject();
            SignedData sd = new SignedData(datos);

            ASN1Set signerInfosSd = null;
            signerInfosSd = sd.getSignerInfos();

            for(int i =0; i< signerInfosSd.size(); i++){
                SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));
                isValid=verifySignerInfo(si);
            }

        } catch (Exception ex) {
            //Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion " + ex);
            isValid= false;
        }
      return isValid;
    }

    /**
     * M&eacute;todo que verifica que los SignerInfos tenga el par&aacute;metro que
     * identifica que es de tipo cades.
     *
     * @param si    SignerInfo para la verificaci&oacute;n del p&aacute;rametro adecuado.
     * @return      si contiene el par&aacute;metro.
     */
    @SuppressWarnings("unchecked")
	private boolean verifySignerInfo(SignerInfo si){
        boolean isSignerValid= true;
        ASN1Set attrib = si.getAuthenticatedAttributes();
        Enumeration<Object> e = attrib.getObjects();
        Attribute atribute;
        while (e.hasMoreElements()){
            atribute = new Attribute((ASN1Sequence) e.nextElement());
            // si tiene la pol&iacute;tica es CADES.
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)){
                isSignerValid=true;
                Logger.getLogger("es.gob.afirma").warning("El signerInfo no es del tipo CMS, es del tipo CADES.");
            }
        }
        return isSignerValid;
    }

    /**
     * M&eacute;todo que verifica que es una firma de tipo "Digested data"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSDigestedData(InputStream data){
        //boolean isValid = false;
    	boolean isValid = true;
        try {
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq=(ASN1Sequence)is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.digestedData)){
                isValid=true;
            }
            // Contenido de Data
            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();

            /* Estas variables no se usan, solo es para verificar que
               la conversion ha sido correcta. De no ser asi, se pasaria
               al manejo de la excepcion.*/
            new DigestedData((ASN1Sequence)doj.getObject());

        } catch (Exception ex) {
            //Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion " + ex);
            isValid= false;
        }

      return isValid;
    }

    /**
     * M&eacute;todo que verifica que es una firma de tipo "Encrypted data"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSEncryptedData(InputStream data){
        //boolean isValid = false;
    	boolean isValid = true;
        try {
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq=(ASN1Sequence)is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.encryptedData)){
                isValid=true;
            }
            // Contenido de Data
            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();

            ASN1Sequence asq = (ASN1Sequence)doj.getObject();

            /* Estas variables no se usan, solo es para verificar que
               la conversion ha sido correcta. De no ser asi, se pasaria
               al manejo de la excepcion.*/
            /*DERInteger version = */DERInteger.getInstance(asq.getObjectAt(0));
            /*EncryptedContentInfo encryptedContentInfo = */EncryptedContentInfo.getInstance(asq.getObjectAt(1));
            //ASN1TaggedObject unprotectedAttrs;
            if (asq.size() == 3)
            {
                /*unprotectedAttrs = (ASN1TaggedObject)*/ asq.getObjectAt(2);
            }

        } catch (Exception ex) {
            //Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion " + ex);
            isValid= false;
        }

      return isValid;
    }

    /**
     * M&eacute;todo que verifica que es una firma de tipo "Enveloped data"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSEnvelopedData(InputStream data){
        //boolean isValid = false;
    	boolean isValid = true;
        try {
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq=(ASN1Sequence)is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.envelopedData)){
                isValid=true;
            }
            // Contenido de Data
            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();

            /* Estas variables no se usan, solo es para verificar que
               la conversion ha sido correcta. De no ser asi, se pasaria
               al manejo de la excepcion.*/
            /*EnvelopedData sd = */new EnvelopedData((ASN1Sequence)doj.getObject());


        } catch (Exception ex) {
            //Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion " + ex);
            isValid= false;
        }

      return isValid;
    }

    /**
     * M&eacute;todo que verifica que es una firma de tipo "Signed and Enveloped data"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSSignedAndEnvelopedData(InputStream data){
        //boolean isValid = false;
    	boolean isValid = true;
        try {
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq=(ASN1Sequence)is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.signedData)){
                isValid=true;
            }
            // Contenido de SignedData
            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
            ASN1Sequence datos = (ASN1Sequence) doj.getObject();
            SignedAndEnvelopedData sd = new SignedAndEnvelopedData(datos);

            ASN1Set signerInfosSd = null;
            signerInfosSd = sd.getSignerInfos();

            for(int i =0; i< signerInfosSd.size(); i++){
                SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));
                isValid=verifySignerInfo(si);
            }

        } catch (Exception ex) {
            //Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion " + ex);
            isValid= false;
        }
      return isValid;
    }
    
    /**
     * M&eacute;todo que verifica que es una firma de tipo "AuthenticatedData"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSAuthenticatedData(InputStream data){
        //boolean isValid = false;
    	boolean isValid = true;
		// Leemos el fichero que contiene la firma.
        ASN1InputStream is = new ASN1InputStream(data);

        try{
            // Comenzamos a obtener los datos.
            ASN1Sequence dsq = null;
            dsq = (ASN1Sequence) is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID AuthenticatedData.
            e.nextElement();
            // Contenido de AuthenticatedData
            ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
            ASN1Sequence authenticatedData = (ASN1Sequence) doj.getObject();

            AuthenticatedData.getInstance(authenticatedData);
        }catch(Exception ex){
            Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo AuthenticatedData:  " + ex);
            isValid= false;
        }
      return isValid;
    }
    
    /**
     * M&eacute;todo que verifica que es una firma de tipo "AuthenticatedEnvelopedData"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSAuthenticatedEnvelopedData(InputStream data){
        //boolean isValid = false;
    	boolean isValid = true;
		// Leemos el fichero que contiene la firma.
        ASN1InputStream is = new ASN1InputStream(data);

        try{
            // Comenzamos a obtener los datos.
            ASN1Sequence dsq = null;
            dsq = (ASN1Sequence) is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID AuthenticatedEnvelopedData.
            e.nextElement();
            // Contenido de AuthenticatedEnvelopedData
            ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
            ASN1Sequence authenticatedEnvelopedData = (ASN1Sequence) doj.getObject();

            AuthEnvelopedData.getInstance(authenticatedEnvelopedData);
        }catch(Exception ex){
            Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo AuthenticatedEnvelopedData:  " + ex);
            isValid= false;
        }
      return isValid;
    }
    
    /**
     * M&eacute;todo que verifica que es una firma de tipo "CompressedData"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCMSCompressedData(InputStream data){
        //boolean isValid = false;
    	boolean isValid = true;
		// Leemos el fichero que contiene la firma.
        ASN1InputStream is = new ASN1InputStream(data);

        try{
            // Comenzamos a obtener los datos.
            ASN1Sequence dsq = null;
            dsq = (ASN1Sequence) is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID CompressedData.
            e.nextElement();
            // Contenido de CompressedData
            ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
            ASN1Sequence compressedData = (ASN1Sequence) doj.getObject();

            CompressedData.getInstance(compressedData);
        }catch(Exception ex){
            Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo CompressedData:  " + ex);
            isValid= false;
        }
      return isValid;
    }

}


