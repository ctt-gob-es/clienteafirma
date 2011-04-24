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

import java.io.InputStream;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

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
	public String getInformation(InputStream data){
        String datos="";

        try{
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = (ASN1Sequence)is.readObject();
            Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            // Contenido a obtener informacion
            ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.data)){
                datos = Utils.getFromData();
            }
            else if(doi.equals(PKCSObjectIdentifiers.digestedData)){
                datos = getFromDigestedData(doj);
            }
            else if(doi.equals(PKCSObjectIdentifiers.encryptedData)){
                datos = Utils.extractData(doj, "5", "Tipo: Encrypted\n", "CADES");
            }
            else if(doi.equals(PKCSObjectIdentifiers.signedData)){
                datos = Utils.extractData(doj, "4", "Tipo: SignedData\n", "CADES");
            }
            else if(doi.equals(PKCSObjectIdentifiers.envelopedData)){
                datos = Utils.extractData(doj, "0", "Tipo: EnvelopedData\n", "CADES");
            }
            else if(doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)){
                datos = Utils.extractData(doj, "3", "Tipo: SignedAndEnvelopedData\n", "CADES");
            }
            else{
               Logger.getLogger("es.gob.afirma").warning("No se reconoce el tipo");
            }
        } catch(final Throwable e){
            Logger.getLogger("es.gob.afirma").warning(
        		"Error intentando reconocer el tipo: " + e
    		);
        }
        return datos;
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
        final AlgorithmIdentifier ai = dd.getDigestAlgorithm();
        detalle = detalle + "Algoritmo de firma: " + ai.getAlgorithm() + "\n";

        //obtenemos el tipo de contenido
        detalle = detalle + "Tipo de Contenido: "+dd.getContentInfo().getContentType()+"\n";

        return detalle;
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