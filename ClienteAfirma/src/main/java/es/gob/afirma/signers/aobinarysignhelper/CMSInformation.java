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

/** Clase que obtiene la informaci&oacute;n de los distintos tipos de firma para
 * CMS a partir de un fichero pasado por par&aacute;metro.
 * La informaci&oacute;n es para los tipo:
 * <ul>
 * <li>Data</li>
 * <li>Signed Data</li>
 * <li>Digested Data</li>
 * <li>Encrypted Data</li>
 * <li>Enveloped Data</li>
 * <li>Signed and Enveloped Data</li>
 * <li>Authenticated Data</li>
 * <li>Authenticated and Enveloped Data</li>
 * </ul> */
public final class CMSInformation {

    /** M&eacute;todo principal que obtiene la informaci&oacute;n a partir de un
     * fichero firmado de tipo CMS.
     * @param data
     *        Objeto CMS.
     * @return Texto descriptivo del objeto CMS.
     * @throws IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws AOInvalidFormatException
     *         Error de formato no v&aacute;lido. */
    public String getInformation(final byte[] data) throws IOException, AOInvalidFormatException {
        String datos = "";

        final ASN1InputStream is = new ASN1InputStream(data);
        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID Data
        final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
        // Contenido a obtener informacion
        final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        if (doi.equals(PKCSObjectIdentifiers.data)) {
            datos = Utils.getFromData();
        }
        else if (doi.equals(PKCSObjectIdentifiers.digestedData)) {
            datos = getFromDigestedData(doj);
        }
        else if (doi.equals(PKCSObjectIdentifiers.encryptedData)) {
            datos = Utils.extractData(doj, "5", "Tipo: Encrypted\n", "CMS");
        }
        else if (doi.equals(PKCSObjectIdentifiers.signedData)) {
            datos = Utils.extractData(doj, "4", "Tipo: SignedData\n", "CMS");
        }
        else if (doi.equals(PKCSObjectIdentifiers.envelopedData)) {
            datos = Utils.extractData(doj, "0", "Tipo: EnvelopedData\n", "CMS");
        }
        else if (doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)) {
            datos = Utils.extractData(doj, "3", "Tipo: SignedAndEnvelopedData\n", "CMS");
        }
        else if (doi.equals(PKCSObjectIdentifiers.id_ct_authData)) {
            datos = Utils.extractData(doj, "1", "Tipo: AuthenticatedData\n", "CMS");
        }
        else if (doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)) {
            datos = Utils.extractData(doj, "2", "Tipo: AuthenticatedEnvelopedData\n", "CMS");
        }
        else if (doi.equals(CMSObjectIdentifiers.compressedData)) {
            datos = getFromCompressedData(doj);
        }
        else {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado");
        }

        return datos;
    }

    /** Obtiene la informaci&oacute;n de un tipo Digested Data.
     * @return Representaci&oacute;n de los datos. */
    private String getFromDigestedData(final ASN1TaggedObject doj) {
        String detalle = "";
        detalle = detalle + "Tipo: DigestedData\n";

        // obtenemos el digestedData
        final DigestedData dd = new DigestedData((ASN1Sequence) doj.getObject());

        // obtenemos la version
        detalle = detalle + "Version: " + dd.getVersion() + "\n";

        // obtenemos el algoritmo
        final AlgorithmIdentifier ai = dd.getDigestAlgorithm();
        detalle = detalle + "Algoritmo de firma: " + ai.getAlgorithm() + "\n";

        // obtenemos el tipo de contenido
        detalle = detalle + "Tipo de Contenido: " + dd.getContentInfo().getContentType() + "\n";

        return detalle;
    }

    /** Obtiene la informaci&oacute;n de un tipo Compressed Data.
     * @return Representaci&oacute;n de los datos. */
    private String getFromCompressedData(final ASN1TaggedObject doj) {
        String detalle = "";
        detalle = detalle + "Tipo: CompressedData\n";
        final CompressedData ed = new CompressedData((ASN1Sequence) doj.getObject());

        // obtenemos la version
        detalle = detalle + "Version: " + ed.getVersion() + "\n";

        final AlgorithmIdentifier aid = ed.getCompressionAlgorithmIdentifier();
        if (aid.getAlgorithm().toString().equals("1.2.840.113549.1.9.16.3.8")) {
            detalle = detalle + "OID del Algoritmo de firma: ZLIB\n";
        }
        else {
            detalle = detalle + "OID del Algoritmo de firma: " + aid.getAlgorithm() + "\n";
        }

        return detalle;
    }

}
