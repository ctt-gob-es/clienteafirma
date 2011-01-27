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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.RecipientIdentifier;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;

/**
 * Clase que obtiene la informaci&oacute;n de los distintos tipos de firma para CADES a partir de un fichero
 * pasado por par&aacute;metro.
 *
 * La informaci&oacute;n es para los tipo:
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
public final class CADESInformation {

    /**
     * M&eacute;todo principal que obtiene la informaci&oacute;n a partir de un fichero firmado
     * de tipo CADES.
     * @param data Fichero firmado
     * @return Informaci&oacute;n del fichero firmado
     */
    @SuppressWarnings("unchecked")
	public String getInformation(InputStream data){
        String datos="";

        try{
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq=(ASN1Sequence)is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            // Contenido a obtener informacion
            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.data)){
                datos = getFromData();
            }
            else if(doi.equals(PKCSObjectIdentifiers.digestedData)){
                datos = getFromDigestedData(doj);
            }
            else if(doi.equals(PKCSObjectIdentifiers.encryptedData)){
                datos = getFromEncryptedData(doj);
            }
            else if(doi.equals(PKCSObjectIdentifiers.signedData)){
                datos = getFromSignedData(doj);
            }
            else if(doi.equals(PKCSObjectIdentifiers.envelopedData)){
                datos = getFromEnvelopedData(doj);
            }
            else if(doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)){
                datos = getFromSignedAndEnvelopedData(doj);
            }
            else{
               Logger.getLogger("es.gob.afirma").warning("No se reconoce el tipo");
            }
        }catch(Exception e){
            e.printStackTrace();
        }
        return datos;
    }


    /**
     * Obtiene la informaci&oacute;n de un tipo Data.
     * @return  Representaci&oacute;n de los datos.
     */
    private String getFromData() {
        String detalle = "";
        detalle = detalle + "Tipo: Data\n";
        return detalle;
    }

   /**
     * Obtiene la informaci&oacute;n de un tipo Digested Data.
     * @return  Representaci&oacute;n de los datos.
     */
    private String getFromDigestedData(ASN1TaggedObject doj) {
        String detalle = "";
        detalle = detalle + "Tipo: DigestedData\n";

        //obtenemos el digestedData
        DigestedData dd = new DigestedData((ASN1Sequence)doj.getObject());

        //obtenemos la version
        detalle = detalle + "Version: "+dd.getVersion()+"\n";

        //obtenemos el algoritmo
        AlgorithmIdentifier ai = dd.getDigestAlgorithm();
        detalle = detalle + "Algoritmo de firma: "+ai.getObjectId()+"\n";

        //obtenemos el tipo de contenido
        detalle = detalle + "Tipo de Contenido: "+dd.getContentInfo().getContentType()+"\n";

        return detalle;
    }

    /**
     * Obtiene la informaci&oacute;n de un tipo Encrypted Data.
     * @return  Representaci&oacute;n de los datos.
     */
    private String getFromEncryptedData(ASN1TaggedObject doj) {
        String detalle = "";
        detalle = detalle + "Tipo: Encrypted\n";

        //obtenemos el digestedData
         ASN1Sequence asq = (ASN1Sequence)doj.getObject();

        DERInteger version = DERInteger.getInstance(asq.getObjectAt(0));
        EncryptedContentInfo encryptedContentInfo = EncryptedContentInfo.getInstance(asq.getObjectAt(1));


        ASN1TaggedObject unprotectedAttrs=null;
        if (asq.size() == 3)
        {
            unprotectedAttrs = (ASN1TaggedObject) asq.getObjectAt(2);
        }

        //obtenemos la version
        detalle = detalle + "Version: "+version+"\n";

        //obtenemos datos de los datos cifrados.
        detalle = detalle + "Informacion de los datos cifrados:\n";
        detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);
        //obtenemos lo atributos opcionales
        if (unprotectedAttrs == null){
            detalle = detalle + "Atributos : No tiene atributos opcionales\n";
        }
        else{
            String atributos = getUnsignedAttributes(unprotectedAttrs);
            detalle = detalle + "Atributos : \n";
            detalle = detalle + atributos;
        }
        return detalle;
    }

    /**
     * Obtiene la informaci&oacute;n de un tipo Signed Data.
     * @return  Representaci&oacute;n de los datos.
     */
    private String getFromSignedData(ASN1TaggedObject doj) {
        String detalle = "";
        detalle = detalle + "Tipo: SignedData\n";

        SignedData sd = new SignedData((ASN1Sequence)doj.getObject());

        //obtenemos la version
        detalle = detalle + "Version: "+sd.getVersion()+"\n";

        //algoritmo de firma
        ASN1Set da = sd.getDigestAlgorithms();
        AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObjectAt(0));
        detalle = detalle + "OID del Algoritmo de firma: "+dai.getObjectId()+"\n";

        ContentInfo ci =sd.getEncapContentInfo();
        detalle = detalle + "OID del tipo de contenido: "+ci.getContentType()+"\n";

        //obtenemos el(los) firmate(s)
        ASN1Set signerInfosSd = null;
        signerInfosSd = sd.getSignerInfos();

        if (signerInfosSd.size()>0){
            detalle = detalle + "Firmantes:\n";
        }
        for(int i =0; i< signerInfosSd.size(); i++){
            SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));

            detalle = detalle + "- firmante "+(i+1)+" :\n";
            // version
            detalle = detalle + "\tversion: "+si.getVersion()+"\n";
            //signerIdentifier
            SignerIdentifier sident = si.getSID();
            IssuerAndSerialNumber iss = IssuerAndSerialNumber.getInstance(sident.getId());
            detalle = detalle + "\tIssuer: "+iss.getName().toString()+"\n";
            detalle = detalle + "\tNumero de serie: "+iss.getSerialNumber()+"\n";

            //digestAlgorithm
            AlgorithmIdentifier algId = si.getDigestAlgorithm();
            detalle = detalle + "\tOID del algoritmo de firma de este firmante: "+algId.getObjectId()+"\n";

            //obtenemos lo atributos obligatorios
            ASN1Set sa =si.getAuthenticatedAttributes();
            String satributes="";
            if (sa != null){
                satributes = getsignedAttributes(sa);
            }
            detalle = detalle + "\tAtributos obligatorios : \n";
            detalle = detalle + satributes;

        }
        return detalle;
    }

    /**
     * Obtiene la informaci&oacute;n de un tipo Enveloped Data.
     * @return  Representaci&oacute;n de los datos.
     */
    private String getFromEnvelopedData(ASN1TaggedObject doj) {
        String detalle = "";
        detalle = detalle + "Tipo: EnvelopedData\n";
        EnvelopedData ed = new EnvelopedData((ASN1Sequence)doj.getObject());

        //obtenemos la version
        detalle = detalle + "Version: "+ed.getVersion()+"\n";

        //recipientInfo
        ASN1Set rins = ed.getRecipientInfos();
        if (rins.size()>0){
            detalle = detalle + "Destinatarios: \n";
        }
        for (int i=0; i<rins.size();i++){
            RecipientInfo rin = RecipientInfo.getInstance(rins.getObjectAt(i));
            KeyTransRecipientInfo kti = KeyTransRecipientInfo.getInstance(rin.getInfo());
            detalle = detalle + " - Informacion de destino de firma "+(i+1)+":\n";
            AlgorithmIdentifier diAlg= kti.getKeyEncryptionAlgorithm();

            //issuer y serial
            RecipientIdentifier rid = kti.getRecipientIdentifier();
            SignerIdentifier sident = SignerIdentifier.getInstance(rid.getId());
            IssuerAndSerialNumber iss = (IssuerAndSerialNumber) sident.getId();
            detalle = detalle + "\tIssuer: "+iss.getName().toString()+"\n";
            detalle = detalle + "\tNumero de serie: "+iss.getSerialNumber()+"\n";

            // el algoritmo de cifrado de los datos
            AOCipherAlgorithm algorithm = null;
            AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();

            // obtenemos el algoritmo usado para cifrar la pass
            for (int j=0;j<algos.length;j++){
                if (algos[j].getOid().equals(diAlg.getObjectId().toString())){
                    algorithm = algos[j];
                }
            }
            if (algorithm != null){
                detalle = detalle + "\tAlgoritmo de cifrado:"+algorithm.getName()+"\n";
            }else{
                detalle = detalle + "\tOID del algoritmo de cifrado:"+diAlg.getObjectId()+"\n";
            }
        }

         //obtenemos datos de los datos cifrados.
        EncryptedContentInfo encryptedContentInfo = ed.getEncryptedContentInfo();
        detalle = detalle + "Informacion de los datos cifrados:\n";
        detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);

        //obtenemos lo atributos opcionales
        ASN1Set unprotectedAttrs=ed.getUnprotectedAttrs();
        if (unprotectedAttrs == null){
            detalle = detalle + "Atributos : No tiene atributos opcionales\n";
        }
        else{
            String atributos = getUnSignedAttributes(unprotectedAttrs);
            detalle = detalle + "Atributos : \n";
            detalle = detalle + atributos;
        }
        return detalle;
    }

    /**
     * Obtiene la informaci&oacute;n de un tipo Signed and enveloped Data.
     * @return  Representaci&oacute;n de los datos.
     */
    private String getFromSignedAndEnvelopedData(ASN1TaggedObject doj) {
        String detalle = "";
        detalle = detalle + "Tipo: SignedAndEnvelopedData\n";
        SignedAndEnvelopedData sed = new SignedAndEnvelopedData((ASN1Sequence)doj.getObject());

        //obtenemos la version
        detalle = detalle + "Version: "+sed.getVersion()+"\n";

        //recipientInfo
        ASN1Set rins = sed.getRecipientInfos();
        if (rins.size()>0){
            detalle = detalle + "Destinatarios: \n";
        }
        for (int i=0; i<rins.size();i++){
            RecipientInfo rin = RecipientInfo.getInstance(rins.getObjectAt(i));
            KeyTransRecipientInfo kti = KeyTransRecipientInfo.getInstance(rin.getInfo());
            detalle = detalle + " - Informacion de destino de firma "+(i+1)+":\n";
            AlgorithmIdentifier diAlg= kti.getKeyEncryptionAlgorithm();

            //issuer y serial
            RecipientIdentifier rid = kti.getRecipientIdentifier();
            SignerIdentifier sident = SignerIdentifier.getInstance(rid.getId());
            IssuerAndSerialNumber iss = (IssuerAndSerialNumber) sident.getId();
            detalle = detalle + "\tIssuer: "+iss.getName().toString()+"\n";
            detalle = detalle + "\tNumero de serie: "+iss.getSerialNumber()+"\n";

            // el algoritmo de cifrado de los datos
            AOCipherAlgorithm algorithm = null;
            AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();

            // obtenemos el algoritmo usado para cifrar la pass
            for (int j=0;j<algos.length;j++){
                if (algos[j].getOid().equals(diAlg.getObjectId().toString())){
                    algorithm = algos[j];
                }
            }
            if (algorithm != null){
                detalle = detalle + "\tAlgoritmo de cifrado:"+algorithm.getName()+"\n";
            }else{
                detalle = detalle + "\tOID del algoritmo de cifrado:"+diAlg.getObjectId()+"\n";

            }
        }

        //algoritmo de firma
        ASN1Sequence seq =(ASN1Sequence)doj.getObject();
        ASN1Set da = (ASN1Set)seq.getObjectAt(2);
        AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObjectAt(0));
        detalle = detalle + "OID del Algoritmo de firma: "+dai.getObjectId()+"\n";

         //obtenemos datos de los datos cifrados.
        EncryptedContentInfo encryptedContentInfo = sed.getEncryptedContentInfo();
        detalle = detalle + "Informacion de los datos cifrados:\n";
        detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);

        //obtenemos el(los) firmate(s)
        ASN1Set signerInfosSd = null;
        signerInfosSd = sed.getSignerInfos();

        if (signerInfosSd.size()>0){
            detalle = detalle + "Firmantes:\n";
        }
        for(int i =0; i< signerInfosSd.size(); i++){
            SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));

            detalle = detalle + "- firmante "+(i+1)+" :\n";
            // version
            detalle = detalle + "\tversion: "+si.getVersion()+"\n";
            //signerIdentifier
            SignerIdentifier sident = si.getSID();
            IssuerAndSerialNumber iss = IssuerAndSerialNumber.getInstance(sident.getId());
            detalle = detalle + "\tIssuer: "+iss.getName().toString()+"\n";
            detalle = detalle + "\tNumero de serie: "+iss.getSerialNumber()+"\n";

            //digestAlgorithm
            AlgorithmIdentifier algId = si.getDigestAlgorithm();
            detalle = detalle + "\tOID del algoritmo de firma de este firmante: "+algId.getObjectId()+"\n";

            //obtenemos lo atributos obligatorios
            ASN1Set sa =si.getAuthenticatedAttributes();
            String satributes="";
            if (sa != null){
                satributes = getsignedAttributes(sa);
            }
            detalle = detalle + "\tAtributos obligatorios : \n";
            detalle = detalle + satributes;

        }

        return detalle;
    }

    /**
     * Obtiene los atributos obligatorios de una firma.
     *
     * @param attributes    Grupo de atributos opcionales
     * @return              lista de atributos concatenados.
     */
    @SuppressWarnings("unchecked")
	private String getsignedAttributes(ASN1Set attributes){
        String attributos="";

        Enumeration<Object> e = attributes.getObjects();

       while (e.hasMoreElements()){
            ASN1Sequence a = (ASN1Sequence)e.nextElement();
            DERObjectIdentifier derIden = (DERObjectIdentifier)a.getObjectAt(0);
            // tipo de contenido de la firma.
            if (derIden.equals(CMSAttributes.contentType)){
                attributos = attributos + "\t\tOID del tipo de contenido: "+ a.getObjectAt(1) +"\n";
            }
            //Message digest de  la firma
            if (derIden.equals(CMSAttributes.messageDigest)){
                attributos = attributos + "\t\tContiene el atributo \"MessageDigest\"\n";
            }
            //la fecha de firma. obtenemos y casteamos a algo legible.
            if (derIden.equals(CMSAttributes.signingTime)){
                ASN1Set time = (ASN1Set)a.getObjectAt(1);
                DERUTCTime d = (DERUTCTime)time.getObjectAt(0);
                Date date = null;
                try {
                    date = d.getDate();
                } catch (ParseException ex) {
                   Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha");
                }
                SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss");
                String ds = formatter.format(date);

                attributos = attributos + "\t\tContiene fecha de firma: "+ ds +"\n";
            }
            //atributo signing certificate v2
            if (derIden.equals(PKCSObjectIdentifiers.id_aa_signingCertificateV2)){
                attributos = attributos + "\t\tContiene el atributo \"Signing Certificate V2\" \n";
            }
            //Politica de firma.
            if (derIden.equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)){                
                attributos = attributos + "\t\tContiene la politica de la firma \n";
            }
        }
        return attributos;
    }

    /**
     * Obtiene los atributos opcionales de una firma cualquiera.
     * En caso de ser EncryptedData, usar el otro metodo, ya que por construccion
     * no es posible utilizar este.
     *
     * @param attributes    Grupo de atributos opcionales
     * @return              lista de atributos concatenados.
     */
    @SuppressWarnings("unchecked")
	private String getUnSignedAttributes(ASN1Set attributes){
        String attributos="";

        Enumeration<Object> e = attributes.getObjects();

       while (e.hasMoreElements()){
            ASN1Sequence a = (ASN1Sequence)e.nextElement();
            DERObjectIdentifier derIden = (DERObjectIdentifier)a.getObjectAt(0);
            // tipo de contenido de la firma.
            if (derIden.equals(CMSAttributes.contentType)){
                attributos = attributos + "\tOID del tipo de contenido: "+ a.getObjectAt(1) +"\n";
            }
            //Message digest de  la firma
            if (derIden.equals(CMSAttributes.messageDigest)){
                attributos = attributos + "\tContiene el atributo \"MessageDigest\"\n";
            }
            //la fecha de firma. obtenemos y casteamos a algo legible.
            if (derIden.equals(CMSAttributes.signingTime)){
                ASN1Set time = (ASN1Set)a.getObjectAt(1);
                DERUTCTime d = (DERUTCTime)time.getObjectAt(0);
                Date date = null;
                try {
                    date = d.getDate();
                } catch (ParseException ex) {
                   Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha");
                }
                SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss");
                String ds = formatter.format(date);

                attributos = attributos + "\tContiene fecha de firma: "+ ds +"\n";
            }
            //contrafirma de la firma.
            if (derIden.equals(CMSAttributes.counterSignature)){
                attributos = attributos + "\tContiene la contrafirma de la firma \n";
            }

        }
        return attributos;
    }


    /**
     * Obtiene los atributos opcionales de una firma para el tipo especial Encrypted Data.
     *
     * @param attributes    Grupo de atributos opcionales
     * @return              lista de atributos concatenados.
     */
    @SuppressWarnings("unchecked")
	private String getUnsignedAttributes(ASN1TaggedObject attributes){
        String attributos="";

        ASN1Sequence atrib = (ASN1Sequence) attributes.getObject();
        Enumeration<Object> e = atrib.getObjects();
        while (e.hasMoreElements()){
            ASN1Sequence a = (ASN1Sequence)e.nextElement();
            DERObjectIdentifier derIden = (DERObjectIdentifier)a.getObjectAt(0);
            // tipo de contenido de la firma.
            if (derIden.equals(CMSAttributes.contentType)){
                attributos = attributos + "\tOID del tipo de contenido: "+ a.getObjectAt(1) +"\n";
            }
            //Message digest de  la firma
            if (derIden.equals(CMSAttributes.messageDigest)){
                attributos = attributos + "\tContiene el atributo \"MessageDigest\"\n";
            }
            //la fecha de firma. obtenemos y casteamos a algo legible.
            if (derIden.equals(CMSAttributes.signingTime)){
                ASN1Set time = (ASN1Set)a.getObjectAt(1);
                DERUTCTime d = (DERUTCTime)time.getObjectAt(0);
                Date date = null;
                try {
                    date = d.getDate();
                } catch (ParseException ex) {
                   Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha");
                }
                SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss");
                String ds = formatter.format(date);

                attributos = attributos + "\tContiene fecha de firma: "+ ds +"\n";
            }
            //contrafirma de la firma.
            if (derIden.equals(CMSAttributes.counterSignature)){
                attributos = attributos + "\tContiene la contrafirma de la firma \n";
            }

        }
        return attributos;
    }

    /**
     * Obtiene los datos de cifrado usados.
     *
     * @param datos     informacion de los datos cifrados sin formatear.
     * @return          informacion de los datos cifrados.
     */
    private String getEncryptedContentInfo(EncryptedContentInfo datos){
        String info = "";

        //especificamos el tipo de contenido
        if (datos.getContentType().equals(PKCSObjectIdentifiers.encryptedData)){
            info = info +"\tTipo: EncryptedData\n";
        }
        else{
            info = info +"\tTipo: "+datos.getContentType()+"\n";
        }

        // el algoritmo de cifrado de los datos
        AlgorithmIdentifier ai =datos.getContentEncryptionAlgorithm();
        AOCipherAlgorithm algorithm = null;
        AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();

        // obtenemos el algoritmo usado para cifrar la pass
        for (int i=0;i<algos.length;i++){
            if (algos[i].getOid().equals(ai.getObjectId().toString())){
                algorithm = algos[i];
            }
        }

        if (algorithm != null){
            info = info +"\tAlgoritmo de cifrado: "+algorithm.getName()+"\n";
        }
        else{
            info = info +"\tOID del Algoritmo de cifrado: "+ai.getObjectId().toString()+"\n";
        }

        return info;
    }


//    /**
//     * M&eacute;todo principal de pruebas
//     * @param args
//     */
//    public static void main(String[] args) {
//        String[] files = {//C:\\firmas\\data.p7s",
//                          //"C:\\firmas\\digestedData.p7s",
//                          //"C:\\firmas\\encryptedData.p7s",
//                          //"C:\\firmas\\signedData.p7s",
//                          //"C:\\firmas\\envelopedData.p7s",
//                          //"C:\\firmas\\signedAndEnveloped.p7s",
//                          "C:\\firmas\\cades signed.p7s",
//                          //"C:\\firmas\\CADESsignedAndEnveloped.p7s",
//                          ""};
//         // firmados
//        for (int i = 0; (files[i] != "") ; i++)
//		{
//           System.out.println("=========================");
//           System.out.println(files[i]);
//           System.out.println();
//            try {
//
//                File firma = new File(files[i]);
//                //File firma = new File("c:\\resultado.jpg");
//                FileInputStream fis = new FileInputStream(firma);
//
//                String a = new CADESInformation().getInformation(fis);
//
//                System.out.println(a);
//
//            }catch(Exception e){
//                e.printStackTrace();
//            }
//        }
//    }

}