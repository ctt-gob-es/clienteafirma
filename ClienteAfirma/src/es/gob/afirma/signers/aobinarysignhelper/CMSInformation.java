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
import org.bouncycastle.asn1.cms.CMSObjectIdentifiers;
import org.bouncycastle.asn1.cms.CompressedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.exceptions.AOInvalidFormatException;

/**
 * Clase que obtiene la informaci&oacute;n de los distintos tipos de firma para CMS a partir de un fichero
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
 * <li>Authenticated Data</li>
 * <li>Authenticated and Enveloped Data</li>
 * </ul>
 */
public final class CMSInformation {

	/**
	 * M&eacute;todo principal que obtiene la informaci&oacute;n a partir de un fichero firmado
	 * de tipo CMS.
	 * @param data Objeto CMS.
	 * @return Texto descriptivo del objeto CMS.
	 * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws AOInvalidFormatException Error de formato no v&aacute;lido.
	 */
	public String getInformation(byte[] data) throws IOException, AOInvalidFormatException {
		String datos="";

		ASN1InputStream is = new ASN1InputStream(data);
		// LEEMOS EL FICHERO QUE NOS INTRODUCEN
		ASN1Sequence dsq = null;
		dsq=(ASN1Sequence)is.readObject();
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
			datos = Utils.extractData(doj, "5", "Tipo: Encrypted\n", "CMS");
		}
		else if(doi.equals(PKCSObjectIdentifiers.signedData)){
			datos = Utils.extractData(doj, "4", "Tipo: SignedData\n", "CMS");
		}
		else if(doi.equals(PKCSObjectIdentifiers.envelopedData)){
			datos = Utils.extractData(doj, "0", "Tipo: EnvelopedData\n", "CMS");
		}
		else if(doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)){
			datos = Utils.extractData(doj, "3", "Tipo: SignedAndEnvelopedData\n", "CMS");
		}
		else if(doi.equals(PKCSObjectIdentifiers.id_ct_authData)){
			datos = Utils.extractData(doj, "1", "Tipo: AuthenticatedData\n", "CMS");
		}
		else if(doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)){
			datos = Utils.extractData(doj, "2", "Tipo: AuthenticatedEnvelopedData\n", "CMS");
		}
		else if(doi.equals(CMSObjectIdentifiers.compressedData)){
			datos = getFromCompressedData(doj);
		}
		else {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado");
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
		AlgorithmIdentifier ai = dd.getDigestAlgorithm();
		detalle = detalle + "Algoritmo de firma: " + ai.getAlgorithm() + "\n";

		//obtenemos el tipo de contenido
		detalle = detalle + "Tipo de Contenido: "+dd.getContentInfo().getContentType()+"\n";

		return detalle;
	}

	/**
	 * Obtiene la informaci&oacute;n de un tipo Compressed Data.
	 * @return  Representaci&oacute;n de los datos.
	 */
	private String getFromCompressedData(ASN1TaggedObject doj) {
		String detalle = "";
		detalle = detalle + "Tipo: CompressedData\n";
		CompressedData ed = new CompressedData((ASN1Sequence)doj.getObject());

		//obtenemos la version
		detalle = detalle + "Version: "+ed.getVersion()+"\n";

		final AlgorithmIdentifier aid = ed.getCompressionAlgorithmIdentifier();
		if (aid.getAlgorithm().toString().equals("1.2.840.113549.1.9.16.3.8")){
			detalle = detalle + "OID del Algoritmo de firma: ZLIB\n";
		}
		else{
			detalle = detalle + "OID del Algoritmo de firma: " + aid.getAlgorithm() + "\n";
		}

		return detalle;
	}

	//    public static void main(String[] args) {
	//        String[] files = {"C:\\firmas\\authenticatedData.csig",
	//                          "C:\\firmas\\authenticatedEnvelopedData.csig",
	//                          "C:\\firmas\\comprimido.csig",
	////                          "C:\\firmas\\signedData.p7s",
	////                          "C:\\firmas\\envelopedData.p7s",
	////                          "C:\\firmas\\signedAndEnveloped.p7s",
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
	//                String a = new CMSInformation().getInformation(fis);
	//
	//                System.out.println(a);
	//
	//            }catch(Exception e){
	//                e.printStackTrace();
	//            }
	//        }
	//    }

}