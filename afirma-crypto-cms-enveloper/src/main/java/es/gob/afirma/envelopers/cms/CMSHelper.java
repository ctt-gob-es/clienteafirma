/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.envelopers.cms;

import java.util.logging.Logger;


/** Utilidades para la firma CMS. */
public final class CMSHelper {

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
    public static boolean isCMSValid(final byte[] data) {
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
    public static boolean isCMSValid(final byte[] data, final String type) {
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
