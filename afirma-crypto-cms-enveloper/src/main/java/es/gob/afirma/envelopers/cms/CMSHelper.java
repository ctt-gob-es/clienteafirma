/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import java.util.logging.Logger;


/** Utilidades para la firma CMS. */
public final class CMSHelper {

    private CMSHelper() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que comprueba que un archivo cumple la estructura deseada.
     * Se realiza la verificaci&oacute;n sobre los los siguientes tipos de PKCS#7
     * reconocidos:
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
     * @param data
     *        Fichero (como array de octetos) que deseamos comprobar.
     * @return La validez del archivo cumpliendo la estructura. */
    public static boolean isCMSValid(final byte[] data) {
        // si se lee en el CMSDATA, el inputstream ya esta leido y en los demas
        // siempre sera nulo
        if (data == null) {
            Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");  //$NON-NLS-1$//$NON-NLS-2$
            return false;
        }

        boolean valid = ValidateCMS.isCMSData(data); // Comprobamos DATA
        if (!valid)
         {
            valid = ValidateCMS.isCMSSignedData(data); // Comprobamos SIGNEDDATA
        }
        if (!valid)
         {
            valid = ValidateCMS.isCMSDigestedData(data); // Comprobamos DIGESTDATA
        }
        if (!valid)
         {
            valid = ValidateCMS.isCMSEncryptedData(data); // Comprobamos
        }
                                                                // ENCRYPTEDDATA
        if (!valid)
         {
            valid = ValidateCMS.isCMSEnvelopedData(data); // Comprobamos
        }
                                                                // ENVELOPEDDATA
        if (!valid)
         {
            valid = ValidateCMS.isCMSSignedAndEnvelopedData(data); // Comprobamos
        }
                                                                         // SIGNEDANDENVELOPED
        if (!valid)
         {
            valid = ValidateCMS.isCMSAuthenticatedData(data); // Comprobamos
        }
                                                                    // AUTHENTICATED
        if (!valid)
         {
            valid = ValidateCMS.isCMSAuthenticatedEnvelopedData(data); // Comprobamos
        }
                                                                             // AUTHENTICATEDENVELOPEDDATA
        if (!valid)
         {
            valid = ValidateCMS.isCMSCompressedData(data); // Comprobamos
                                                                     // COMPRESSEDDATA
        }

        return valid;
    }

    /** Comprueba que un fichero sea un tipo de dato envuelto CMS.
     * @param data
     *        Datos que queremos comprobar.
     * @param type
     *        Tipo de dato que queremos.
     * @return Indica si el fichero es una envoltura CMS con el tipo de
     *         contenido indicado. */
    public static boolean isCMSValid(final byte[] data, final String type) {
        if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_DATA)) {
			return ValidateCMS.isCMSData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_SIGNEDDATA)) {
			return ValidateCMS.isCMSSignedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_DIGESTEDDATA)) {
			return ValidateCMS.isCMSDigestedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_ENCRYPTEDDATA)) {
			return ValidateCMS.isCMSEncryptedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_ENVELOPEDDATA)) {
			return ValidateCMS.isCMSEnvelopedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
			return ValidateCMS.isCMSSignedAndEnvelopedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_AUTHENTICATEDDATA)) {
			return ValidateCMS.isCMSAuthenticatedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
			return ValidateCMS.isCMSAuthenticatedEnvelopedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_COMPRESSEDDATA)) {
			return ValidateCMS.isCMSCompressedData(data);
        }
        Logger.getLogger("es.gob.afirma").warning("Tipo de contenido CMS no reconocido");  //$NON-NLS-1$//$NON-NLS-2$
        return false;
    }
}
