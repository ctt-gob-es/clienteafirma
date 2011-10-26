/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.BERSet;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

/** Clase que contiene una serie de m&eacute;todos utilizados por GenSignedData,
 * GenCadesSignedData, CoSigner y CounterSigner. */
final class SigUtils {
    
    private SigUtils() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que devuelve el Identificador del algoritmo.
     * @param oid
     *        OID del algoritmo a idenfiticar
     * @return El identificador del algoritmo formateado y listo para introducir
     *         en el cms.
     * @throws java.io.IOException */
    static AlgorithmIdentifier makeAlgId(final String oid) throws IOException {
        return new AlgorithmIdentifier(new DERObjectIdentifier(oid), new DERNull());
    }

    /** Genera un estructura de tipo SET de formato ASN1.
     * @param derObjects
     *        Una lista con los objetos a obtener el tipo SET
     * @return Un SET de ASN1 con los elementos de la lista introducida. */
    static ASN1Set createBerSetFromList(final List<DEREncodable> derObjects) {
        final ASN1EncodableVector v = new ASN1EncodableVector();
        for (final DEREncodable d : derObjects) {
            v.add(d);
        }
        return new BERSet(v);
    }

    /** Genera un atributo de un SET en formato DER
     * @param attr
     *        Atributo a formatear.
     * @return SET en formato DER del atributo. */
    static ASN1Set getAttributeSet(final AttributeTable attr) {
        if (attr != null) {
            return new DERSet(attr.toASN1EncodableVector());
        }
        Logger.getLogger("es.gob.afirma").warning("Los atributos eran nulos, se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
    }
}
