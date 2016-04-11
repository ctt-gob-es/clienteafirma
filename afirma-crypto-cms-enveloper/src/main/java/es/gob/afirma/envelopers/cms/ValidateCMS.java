/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cms;

import java.util.Enumeration;
import java.util.logging.Logger;

import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1Integer;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AuthEnvelopedData;
import org.spongycastle.asn1.cms.AuthenticatedData;
import org.spongycastle.asn1.cms.CMSObjectIdentifiers;
import org.spongycastle.asn1.cms.CompressedData;
import org.spongycastle.asn1.cms.EncryptedContentInfo;
import org.spongycastle.asn1.cms.EnvelopedData;
import org.spongycastle.asn1.cms.SignedData;
import org.spongycastle.asn1.cms.SignerInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;

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

	private ValidateCMS() {
		// No permitimos la instanciacion
	}

    /** M&eacute;todo que verifica que es una firma de tipo "data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    @SuppressWarnings("unused")
	static
    boolean isCMSData(final byte[] data) {
        boolean isValid = true;
        try (
    		final ASN1InputStream is = new ASN1InputStream(data);
		) {

            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
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
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Signed data"
     * @param data Datos CMS.
     * @return si es de este tipo. */
    static boolean isCMSSignedData(final byte[] data) {
        boolean isValid = true;
        try (
    		final ASN1InputStream is = new ASN1InputStream(data);
		) {
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.signedData)) {
                isValid = false;
            }
            else {
                // Contenido de SignedData
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
                final ASN1Sequence datos = (ASN1Sequence) doj.getObject();
                final SignedData sd = SignedData.getInstance(datos);
                final ASN1Set signerInfosSd = sd.getSignerInfos();

                for (int i = 0; isValid && i < signerInfosSd.size(); i++) {
                    final SignerInfo si = SignerInfo.getInstance(signerInfosSd.getObjectAt(i));
                    isValid = verifySignerInfo(si);
                }
            }
        }
        catch (final Exception ex) {
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
            atribute = Attribute.getInstance(e.nextElement());
            // si tiene la politica es CADES.
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)) {
                isSignerValid = false;
                Logger.getLogger("es.gob.afirma").warning("El signerInfo no es del tipo CMS, es del tipo CADES"); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        return isSignerValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Digested data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    @SuppressWarnings("unused")
	static
    boolean isCMSDigestedData(final byte[] data) {
        boolean isValid = true;
        try (
    		final ASN1InputStream is = new ASN1InputStream(data);
		) {
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
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
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Encrypted data"
     * @param data Datos CMS.
     * @return si es de este tipo. */
    static boolean isCMSEncryptedData(final byte[] data) {
        boolean isValid = true;
        try (
    		final ASN1InputStream is = new ASN1InputStream(data);
		) {
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
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
                ASN1Integer.getInstance(asq.getObjectAt(0));
                EncryptedContentInfo.getInstance(asq.getObjectAt(1));
            }
        }
        catch (final Exception ex) {
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Enveloped data"
     * @param data Datos CMS.
     * @return si es de este tipo. */
	static
    boolean isCMSEnvelopedData(final byte[] data) {
        boolean isValid = true;
        try (
    		final ASN1InputStream is = new ASN1InputStream(data);
		) {
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
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
                EnvelopedData.getInstance(doj.getObject());
            }

        }
        catch (final Exception ex) {
            isValid = false;
        }

        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo
     * "Signed and Enveloped data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    static boolean isCMSSignedAndEnvelopedData(final byte[] data) {
        boolean isValid = true;
        try (
    		final ASN1InputStream is = new ASN1InputStream(data);
		) {
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
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
                    final SignerInfo si = SignerInfo.getInstance(signerInfosSd.getObjectAt(i));
                    isValid = verifySignerInfo(si);
                }
            }

        }
        catch (final Exception ex) {
            isValid = false;
        }
        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "AuthenticatedData"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    static boolean isCMSAuthenticatedData(final byte[] data) {
        boolean isValid = true;

        try (
    		// Leemos el fichero que contiene la firma.
            final ASN1InputStream is = new ASN1InputStream(data);
		) {
            // Comenzamos a obtener los datos.
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID AuthenticatedData.
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
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
            isValid = false;
        }
        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo
     * "AuthenticatedEnvelopedData"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    static boolean isCMSAuthenticatedEnvelopedData(final byte[] data) {
        boolean isValid = true;

        try (
    		// Leemos el fichero que contiene la firma.
            final ASN1InputStream is = new ASN1InputStream(data);
		) {
            // Comenzamos a obtener los datos.
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID
            // AuthenticatedEnvelopedData.
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
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
            isValid = false;
        }
        return isValid;
    }

    /** M&eacute;todo que verifica que es una firma de tipo "CompressedData"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo. */
    static boolean isCMSCompressedData(final byte[] data) {
        boolean isValid = true;
        try (
    		// Leemos el fichero que contiene la firma.
            final ASN1InputStream is = new ASN1InputStream(data);
		) {
            // Comenzamos a obtener los datos.
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            is.close();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID CompressedData.
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
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
            isValid = false;
        }
        return isValid;
    }
}
