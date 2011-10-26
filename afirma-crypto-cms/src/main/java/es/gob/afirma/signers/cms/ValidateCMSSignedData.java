/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.cms;

import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

/** Clase que permite verificar si unos datos se corresponden con una firma CMS. */
final class ValidateCMSSignedData {
    
    private ValidateCMSSignedData() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Signed data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    public static boolean isCMSSignedData(final byte[] data) {
        boolean isValid = true;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.signedData)) {
                isValid = false;
            }
            else {
                // Contenido de SignedData
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
                final ASN1Sequence datos = (ASN1Sequence) doj.getObject();
                final SignedData sd = new SignedData(datos);
                final ASN1Set signerInfosSd = sd.getSignerInfos();

                for (int i = 0; isValid && i < signerInfosSd.size(); i++) {
                    final SignerInfo si = new SignerInfo((ASN1Sequence) signerInfosSd.getObjectAt(i));
                    isValid = verifySignerInfo(si);
                }
            }
        }
        catch (final Exception ex) {
            // Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }
        return isValid;
    }

    /** M&eacute;todo que verifica que los SignerInfos tenga el par&aacute;metro
     * que identifica que es de tipo cades.
     * @param si
     *        SignerInfo para la verificaci&oacute;n del p&aacute;rametro
     *        adecuado.
     * @return si contiene el par&aacute;metro. */
    private static boolean verifySignerInfo(final SignerInfo si) {
        boolean isSignerValid = true;
        final ASN1Set attrib = si.getAuthenticatedAttributes();
        final Enumeration<?> e = attrib.getObjects();
        Attribute atribute;
        while (isSignerValid && e.hasMoreElements()) {
            atribute = new Attribute((ASN1Sequence) e.nextElement());
            // si tiene la pol&iacute;tica es CADES.
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)) {
                isSignerValid = false;
                Logger.getLogger("es.gob.afirma").warning("El signerInfo no es del tipo CMS, es del tipo CADES"); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        return isSignerValid;
    }
}
