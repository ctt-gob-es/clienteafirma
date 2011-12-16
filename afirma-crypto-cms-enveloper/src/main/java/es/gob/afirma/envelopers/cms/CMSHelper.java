/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cms;

import java.util.logging.Logger;


/** Utilidades para la firma CMS. */
final class CMSHelper {
    
    private CMSHelper() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que comprueba que un archivo cumple la estructura deseada.
     * Se realiza la verificaci&ocute;n sobre los los siguientes tipos de PKCS#7
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
    static boolean isCMSValid(final byte[] data) {
        // si se lee en el CMSDATA, el inputstream ya esta leido y en los demas
        // siempre sera nulo
        if (data == null) {
            Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");  //$NON-NLS-1$//$NON-NLS-2$
            return false;
        }

        final ValidateCMS validator = new ValidateCMS();
        boolean valid = validator.isCMSData(data); // Comprobamos DATA
        if (!valid)
         {
            valid = validator.isCMSSignedData(data); // Comprobamos SIGNEDDATA
        }
        if (!valid)
         {
            valid = validator.isCMSDigestedData(data); // Comprobamos DIGESTDATA
        }
        if (!valid)
         {
            valid = validator.isCMSEncryptedData(data); // Comprobamos
        }
                                                                // ENCRYPTEDDATA
        if (!valid)
         {
            valid = validator.isCMSEnvelopedData(data); // Comprobamos
        }
                                                                // ENVELOPEDDATA
        if (!valid)
         {
            valid = validator.isCMSSignedAndEnvelopedData(data); // Comprobamos
        }
                                                                         // SIGNEDANDENVELOPED
        if (!valid)
         {
            valid = validator.isCMSAuthenticatedData(data); // Comprobamos
        }
                                                                    // AUTHENTICATED
        if (!valid)
         {
            valid = validator.isCMSAuthenticatedEnvelopedData(data); // Comprobamos
        }
                                                                             // AUTHENTICATEDENVELOPEDDATA
        if (!valid)
         {
            valid = validator.isCMSCompressedData(data); // Comprobamos
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
    static boolean isCMSValid(final byte[] data, final String type) {
        if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_DATA)) {
            return new ValidateCMS().isCMSData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_SIGNEDDATA)) {
            return new ValidateCMS().isCMSSignedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_DIGESTEDDATA)) {
            return new ValidateCMS().isCMSDigestedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_ENCRYPTEDDATA)) {
            return new ValidateCMS().isCMSEncryptedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_ENVELOPEDDATA)) {
            return new ValidateCMS().isCMSEnvelopedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
            return new ValidateCMS().isCMSSignedAndEnvelopedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_AUTHENTICATEDDATA)) {
            return new ValidateCMS().isCMSAuthenticatedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
            return new ValidateCMS().isCMSAuthenticatedEnvelopedData(data);
        }
        else if (type.equals(AOCMSEnveloper.CMS_CONTENTTYPE_COMPRESSEDDATA)) {
            return new ValidateCMS().isCMSCompressedData(data);
        }
        Logger.getLogger("es.gob.afirma").warning("Tipo de contenido CMS no reconocido");  //$NON-NLS-1$//$NON-NLS-2$
        return false;
    }
}
