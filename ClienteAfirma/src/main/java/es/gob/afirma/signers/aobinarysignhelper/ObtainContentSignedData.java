/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

/** Clase que obtiene el contenido de un fichero en formato SignedData. de CMS o
 * CADES. */
public final class ObtainContentSignedData {

    /** M&eacute;todo que obtiene el contenido firmado de un tipo Signed Data
     * tanto en CADES como en CMS. Si la firma no contiene los datos, devuelve <code>null</code>.
     * @param data
     *        datos que contienen la firma.
     * @return el contenido firmado. */
    public byte[] obtainData(final byte[] data) {
        byte[] contenido = null;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            // Contenido a obtener informacion
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            // buscamos si es signedData
            if (doi.equals(PKCSObjectIdentifiers.signedData)) {
                // obtenemos el signed Data
                final SignedData sd = new SignedData((ASN1Sequence) doj.getObject());
                final ContentInfo ci = sd.getEncapContentInfo();
                // obtenemos el contenido si lo tiene.
                if (ci.getContent() != null) {
                    final DEROctetString os = (DEROctetString) ci.getContent();
                    contenido = os.getOctets();
                }
                else {
                    Logger.getLogger("es.gob.afirma").warning("No existe contenido en esta firma.");
                }
            }
            else {
                Logger.getLogger("es.gob.afirma").warning("No se puede obtener el contenido de esta firma.");
            }

        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se pudieron recuperar los datos contenidos en la firma: " + e);
            contenido = null;
        }

        return contenido;
    }

    // /**
    // * M&eacute;todo principal de pruebas
    // * @param args
    // */
    // public static void main(String[] args) {
    //
    // es.gob.afirma.signers.AOCMSSigner signer = new
    // es.gob.afirma.signers.AOCMSSigner();
    //
    // try {
    // File firma = new File("C:\\firmacms.csig");
    // FileInputStream fis = new FileInputStream(firma);
    //
    // byte[] a = signer.getData(fis);
    //
    // System.out.println(new String(a));
    // FileOutputStream fo = new FileOutputStream("c:\\salida.jpg");
    // fo.write(a);
    // fo.close();
    //
    // }catch(Exception e){
    // e.printStackTrace();
    // }
    // }

}
