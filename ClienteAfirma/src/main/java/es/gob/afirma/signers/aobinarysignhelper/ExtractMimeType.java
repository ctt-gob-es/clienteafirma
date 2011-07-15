/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.io.IOException;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

/** Clase que obtiene el tipo de datos declarado en una firma mediante su Mime
 * Type. Para ello, hacemos uso de la estructura SignedData. Es cuesti&oacute;n
 * de obtener el "contentInfo" y obtener el OID del tipo.
 * SignedData ::= SEQUENCE { version Version, digestAlgorithms
 * DigestAlgorithmIdentifiers, contentInfo ContentInfo, certificates [0]
 * CertificateSet OPTIONAL, crls [1] CertificateRevocationLists OPTIONAL,
 * signerInfos SignerInfos }
 * ContentInfo ::= SEQUENCE { contentType ContentType, content [0] EXPLICIT ANY
 * DEFINED BY contentType OPTIONAL } */
public final class ExtractMimeType {

    /** Extrae el OID del MimeType asociado de una firma.
     * @param data
     *        Fichero que contiene la firma.
     * @return OID del MimeType */
    public String extractMimeType(final byte[] data) {
        try {
            final ASN1InputStream is = new ASN1InputStream(data);

            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final Enumeration<?> e = ((ASN1Sequence) is.readObject()).getObjects();

            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.signedData)) {
                // Contenido de SignedData
                final SignedData sd = new SignedData((ASN1Sequence) ((ASN1TaggedObject) e.nextElement()).getObject());

                // El contentype es el que nos dice el tipo de contenido que es.
                // Consultar la cabecera de la clase
                return sd.getEncapContentInfo().getContentType().getId();

            }
        }
        catch (final IOException e1) {
            Logger.getLogger("es.gob.afirma").warning("Error intentando obtener el MIME-Type, se devolvera una cadena vacia: " + e1);
        }
        return "";

    }

}
