/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.cades;

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

import es.gob.afirma.signers.pkcs7.DigestedData;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que verifica los distintos tipos de firma para CADES a partir de un
 * fichero pasado por par&aacute;metro.
 * La verificaci&oacute; es para los tipo:
 * <ul>
 * <li>Data</li>
 * <li>Signed Data</li>
 * <li>Digested Data</li>
 * <li>Encrypted Data</li>
 * <li>Enveloped Data</li>
 * <li>Signed and Enveloped Data</li>
 * </ul> */

public final class ValidateCADES {
    
    /** M&eacute;todo que verifica que es una firma de tipo "data"
     * @param data
     *        El envoltorio.
     * @return si es de este tipo. */
    @SuppressWarnings("unused")
    boolean isCADESData(final byte[] data) {
        boolean isValid = true;
        try {
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final Enumeration<?> e = ((ASN1Sequence) new ASN1InputStream(data).readObject()).getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.data)) {
                isValid = false;
            }
            // Contenido de Data
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            /*
             * Los valores de retorno no se usan, solo es para verificar que la
             * conversion ha sido correcta. De no ser asi, se pasaria al manejo
             * de la excepcion.
             */
            new DEROctetString(doj.getObject());

        }
        catch (final Exception ex) {
            // LOGGER.severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Signed data"
     * @param data
     *        El envoltorio.
     * @return si es de este tipo. */
    public boolean isCADESSignedData(final byte[] data) {
        boolean isValid = false;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.signedData)) {
                isValid = true;
            }
            // Contenido de SignedData
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
            final ASN1Sequence datos = (ASN1Sequence) doj.getObject();
            final SignedData sd = new SignedData(datos);

            final ASN1Set signerInfosSd = sd.getSignerInfos();

            for (int i = 0; i < signerInfosSd.size(); i++) {
                final SignerInfo si = new SignerInfo((ASN1Sequence) signerInfosSd.getObjectAt(i));
                isValid = verifySignerInfo(si);
            }

        }
        catch (final Exception ex) {
            // LOGGER.severe("Error durante el proceso de conversion "
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
    private boolean verifySignerInfo(final SignerInfo si) {
        boolean isSignerValid = false;
        final ASN1Set attrib = si.getAuthenticatedAttributes();
        final Enumeration<?> e = attrib.getObjects();
        Attribute atribute;
        while (e.hasMoreElements()) {
            atribute = new Attribute((ASN1Sequence) e.nextElement());
            // si tiene la pol&iacute;tica es CADES.
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signingCertificate)) {
                isSignerValid = true;
            }
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signingCertificateV2)) {
                isSignerValid = true;
            }
        }
        return isSignerValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Digested data"
     * @param data
     *        El envoltorio.
     * @return si es de este tipo. */
    @SuppressWarnings("unused")
    boolean isCADESDigestedData(final byte[] data) {
        boolean isValid = false;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.digestedData)) {
                isValid = true;
            }
            // Contenido de Data
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            /*
             * Los resultados no se usan, solo es para verificar que la
             * conversion ha sido correcta. De no ser asi, se pasaria al manejo
             * de la excepcion.
             */
            new DigestedData((ASN1Sequence) doj.getObject());

        }
        catch (final Exception ex) {
            // LOGGER.severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Encrypted data"
     * @param data
     *        El envoltorio.
     * @return si es de este tipo. */
    boolean isCADESEncryptedData(final byte[] data) {
        boolean isValid = false;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.encryptedData)) {
                isValid = true;
            }
            // Contenido de Data
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            final ASN1Sequence asq = (ASN1Sequence) doj.getObject();

            /*
             * Estas variables no se usan, solo es para verificar que la
             * conversion ha sido correcta. De no ser asi, se pasaria al manejo
             * de la excepcion.
             */
            /* DERInteger version = */DERInteger.getInstance(asq.getObjectAt(0));
            /* EncryptedContentInfo encryptedContentInfo = */EncryptedContentInfo.getInstance(asq.getObjectAt(1));
            if (asq.size() == 3) {
                /* ASN1TaggedObject unprotectedAttrs =(ASN1TaggedObject) */asq.getObjectAt(2);
            }

        }
        catch (final Exception ex) {
            // LOGGER.severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Enveloped data"
     * @param data
     *        El envoltorio.
     * @return si es de este tipo. */
    @SuppressWarnings("unused")
    boolean isCADESEnvelopedData(final byte[] data) {
        boolean isValid = false;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.envelopedData)) {
                isValid = true;
            }
            // Contenido de Data
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            /*
             * los retornos no se usan, solo es para verificar que la conversion
             * ha sido correcta. De no ser asi, se pasaria al manejo de la
             * excepcion.
             */
            new EnvelopedData((ASN1Sequence) doj.getObject());

        }
        catch (final Exception ex) {
            // LOGGER.severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo
     * "Signed and Enveloped data"
     * @param data
     *        El envoltorio.
     * @return si es de este tipo. */
    boolean isCADESSignedAndEnvelopedData(final byte[] data) {
        boolean isValid = false;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.signedData)) {
                isValid = true;
            }
            // Contenido de SignedData
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
            final ASN1Sequence datos = (ASN1Sequence) doj.getObject();
            final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(datos);

            final ASN1Set signerInfosSd = sd.getSignerInfos();

            for (int i = 0; i < signerInfosSd.size(); i++) {
                final SignerInfo si = new SignerInfo((ASN1Sequence) signerInfosSd.getObjectAt(i));
                isValid = verifySignerInfo(si);
            }

        }
        catch (final Exception ex) {
            // LOGGER.severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }
        return isValid;
    }

}
