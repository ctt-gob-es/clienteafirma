/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.cms;

import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AuthEnvelopedData;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.cms.CMSObjectIdentifiers;
import org.bouncycastle.asn1.cms.CompressedData;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.signers.pkcs7.DigestedData;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que verifica los distintos tipos de firma para CMS a partir de un
 * fichero pasado por par&aacute;metro.
 * La verificaci&oacute; es para los tipo:
 * <ul>
 * <li>Data</li>
 * <li>Signed Data</li>
 * <li>Digested Data</li>
 * <li>Encrypted Data</li>
 * <li>Enveloped Data</li>
 * <li>Signed and Enveloped Data</li>
 * <li>Compressed Data</li>
 * <li>Authenticated Data</li>
 * <li>Authenticated and Enveloped Data</li>
 * </ul> */
final class ValidateCMS {

    /** M&eacute;todo que verifica que es una firma de tipo "data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSData(final byte[] data) {
        boolean isValid = true;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.data)) {
                isValid = false;
            }
            else {
                // Contenido de Data
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

                /*
                 * Si no es un objeto de tipo Dara se pasa al manejo de la
                 * excepcion
                 */
                new DEROctetString(doj.getObject());
            }
        }
        catch (final Exception ex) {
            // Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Signed data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSSignedData(final byte[] data) {
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
    private boolean verifySignerInfo(final SignerInfo si) {
        boolean isSignerValid = true;
        final ASN1Set attrib = si.getAuthenticatedAttributes();
        final Enumeration<?> e = attrib.getObjects();
        Attribute atribute;
        while (isSignerValid && e.hasMoreElements()) {
            atribute = new Attribute((ASN1Sequence) e.nextElement());
            // si tiene la pol&iacute;tica es CADES.
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)) {
                isSignerValid = false;
                Logger.getLogger("es.gob.afirma").warning("El signerInfo no es del tipo CMS, es del tipo CADES.");
            }
        }
        return isSignerValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Digested data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSDigestedData(final byte[] data) {
        boolean isValid = true;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.digestedData)) {
                isValid = false;
            }
            else {
                // Contenido de Data
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

                /*
                 * Estas variables no se usan, solo es para verificar que la
                 * conversion ha sido correcta. De no ser asi, se pasaria al
                 * manejo de la excepcion.
                 */
                new DigestedData((ASN1Sequence) doj.getObject());
            }
        }
        catch (final Exception ex) {
            // Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Encrypted data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSEncryptedData(final byte[] data) {
        boolean isValid = true;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.encryptedData)) {
                isValid = false;
            }
            else {
                // Contenido de Data
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
                final ASN1Sequence asq = (ASN1Sequence) doj.getObject();

                /*
                 * Si no es de tipo EncryptedData se pasa al manejo de la
                 * excepcion
                 */
                DERInteger.getInstance(asq.getObjectAt(0));
                EncryptedContentInfo.getInstance(asq.getObjectAt(1));
            }
        }
        catch (final Exception ex) {
            // Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Enveloped data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSEnvelopedData(final byte[] data) {
        boolean isValid = true;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.envelopedData)) {
                isValid = false;
            }
            else {
                // Contenido de Data
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
                /*
                 * Si no se construye el objeto correctamente, se pasa al manejo
                 * de la excepcion
                 */
                new EnvelopedData((ASN1Sequence) doj.getObject());
            }

        }
        catch (final Exception ex) {
            // Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de conversion "
            // + ex);
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo
     * "Signed and Enveloped data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSSignedAndEnvelopedData(final byte[] data) {
        boolean isValid = true;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)) {
                isValid = false;
            }
            else {
                // Contenido de SignedData
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
                final ASN1Sequence datos = (ASN1Sequence) doj.getObject();
                final SignedAndEnvelopedData sd = new SignedAndEnvelopedData(datos);
                final ASN1Set signerInfosSd = sd.getSignerInfos();

                for (int i = 0; i < signerInfosSd.size() && isValid; i++) {
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

    /** M&eacute;todo que verifica que es una firma de tipo "AuthenticatedData"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSAuthenticatedData(final byte[] data) {
        boolean isValid = true;
        // Leemos el fichero que contiene la firma.
        final ASN1InputStream is = new ASN1InputStream(data);

        try {
            // Comenzamos a obtener los datos.
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID AuthenticatedData.
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.id_ct_authData)) {
                isValid = false;
            }
            else {
                // Contenido de AuthenticatedData
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
                final ASN1Sequence authenticatedData = (ASN1Sequence) doj.getObject();
                AuthenticatedData.getInstance(authenticatedData);
            }
        }
        catch (final Exception ex) {
            // Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo AuthenticatedData:  "
            // + ex);
            isValid = false;
        }
        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo
     * "AuthenticatedEnvelopedData"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSAuthenticatedEnvelopedData(final byte[] data) {
        boolean isValid = true;
        // Leemos el fichero que contiene la firma.
        final ASN1InputStream is = new ASN1InputStream(data);

        try {
            // Comenzamos a obtener los datos.
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID
            // AuthenticatedEnvelopedData.
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)) {
                isValid = false;
            }
            else {
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
                final ASN1Sequence authenticatedEnvelopedData = (ASN1Sequence) doj.getObject();
                AuthEnvelopedData.getInstance(authenticatedEnvelopedData);
            }

        }
        catch (final Exception ex) {
            // Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo AuthenticatedEnvelopedData:  "
            // + ex);
            isValid = false;
        }
        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "CompressedData"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    boolean isCMSCompressedData(final byte[] data) {
        boolean isValid = true;
        // Leemos el fichero que contiene la firma.
        final ASN1InputStream is = new ASN1InputStream(data);
        try {
            // Comenzamos a obtener los datos.
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID CompressedData.
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (!doi.equals(CMSObjectIdentifiers.compressedData)) {
                isValid = false;
            }
            else {
                // Contenido de CompressedData
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
                final ASN1Sequence compressedData = (ASN1Sequence) doj.getObject();
                CompressedData.getInstance(compressedData);
            }
        }
        catch (final Exception ex) {
            // Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo CompressedData:  "
            // + ex);
            isValid = false;
        }
        return isValid;
    }
}
