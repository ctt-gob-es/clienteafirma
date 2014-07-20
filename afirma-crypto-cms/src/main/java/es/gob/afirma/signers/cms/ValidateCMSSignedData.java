/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cms;

import java.io.IOException;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.signers.pkcs7.BCChecker;

/** Clase que permite verificar si unos datos se corresponden con una firma CMS. */
final class ValidateCMSSignedData {

    private ValidateCMSSignedData() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Signed data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo.
     * @throws IOException Si ocurren errores durante la lectura de los datos */
    public static boolean isCMSSignedData(final byte[] data) throws IOException {
    	new BCChecker().checkBouncyCastle();
        boolean isValid = true;
        ASN1InputStream is = null;
        try {
            is = new ASN1InputStream(data);
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
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
        finally {
        	if (is != null) {
        		is.close();
        	}
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
            // si tiene la pol&iacute;tica es CADES.
            if (atribute.getAttrType().equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)) {
                isSignerValid = false;
                Logger.getLogger("es.gob.afirma").warning("El signerInfo no es del tipo CMS, es del tipo CADES"); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        return isSignerValid;
    }
}
