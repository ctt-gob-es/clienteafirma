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

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;


/**
 * Clase que verifica los distintos tipos de firma para CADES a partir de un fichero
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

public final class ValidateCADES {

    /**
     * M&eacute;todo que verifica que es una firma de tipo "data"
     *
     * @param   data fichero que tiene la firma.
     * @return  si es de este tipo.
     */
    @SuppressWarnings("unchecked")
	public boolean isCADESData(InputStream data){
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
            /*DEROctetString sd = */new DEROctetString(doj.getObject());

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
	public boolean isCADESSignedData(InputStream data){
        boolean isValid = false;
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
        boolean isSignerValid= false;
        ASN1Set attrib = si.getAuthenticatedAttributes();
        Enumeration<Object> e = attrib.getObjects();
        Attribute atribute;
        while (e.hasMoreElements()){
            atribute = new Attribute((ASN1Sequence) e.nextElement());
            // si tiene la pol&iacute;tica es CADES.
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signingCertificate)){
                isSignerValid=true;
            }
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signingCertificateV2)){
                isSignerValid=true;
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
	public boolean isCADESDigestedData(InputStream data){
        boolean isValid = false;
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
            /*DigestedData sd = */new DigestedData((ASN1Sequence)doj.getObject());

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
	public boolean isCADESEncryptedData(InputStream data){
        boolean isValid = false;
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
            if (asq.size() == 3)
            {
            	/*ASN1TaggedObject unprotectedAttrs =(ASN1TaggedObject)*/ asq.getObjectAt(2);
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
	public boolean isCADESEnvelopedData(InputStream data){
        boolean isValid = false;
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
	public boolean isCADESSignedAndEnvelopedData(InputStream data){
        boolean isValid = false;
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

}


