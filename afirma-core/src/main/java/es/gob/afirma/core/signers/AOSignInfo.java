/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

/** Objeto que contiene la informacion general de un objeto de firma. Ya que un
 * objeto de firma puede contener muchas firmas, se considera informaci&oacute;n
 * general la que aplica a todo el objeto. Esto es:
 * <ul>
 * <li>Formato de firma: Formato general de la firma (p.e. CAdES, XAdES,...)</li>
 * <li>Variante: Variante del formato de firma (p.e. Enveloped, Detached,...)</li>
 * <li>URL de firma: URL desde donde descargar el fichero de firma.  Esta
 * informaci&oacute;n puede haberse insertado en alg&uacute;n campo no estandarizado.</li>
 * <li>URL de datos: URL desde donde descargar el fichero de datos. Esta
 * informaci&oacute;n puede haberse insertado en alg&uacute;n campo no estandarizado.</li>
 * <li>C&oacute;digo de verificaci&oacute;n: C&oacute;digo en base64 para la
 * verificaci&oacute;n de la firma.</li>
 * </ul>
 * Todos los campos, salvo el "Formato de firma" son opcionales. */
public final class AOSignInfo {

    /** Formato de firma. */
    private String format = null;

    /** Variante del formato de firma. */
    private String variant = null;

    /** URL desde la que descargar el objeto de firma. */
    private String urlSignObject = null;

    /** URL desde la que descargar el objeto de datos. */
    private String urlSignedData = null;

    /** C&oacute;digo de verificaci&oacute;n de la firma en Base64. */
    private String b64VerificationCode = null;

    /** Construye un objeto de informaci&oacute;n de firma. Si no se especifica
     * un formato de firma se establece el formato "Desconocido"
     * @param signFormat
     *        Formato general de firma. */
    public AOSignInfo(final String signFormat) {
        this.format = signFormat != null ? signFormat : "Desconocido"; //$NON-NLS-1$
    }

    /** Recupera la variante de formato a la que pertene el objeto de firma.
     * @return Nombre de la variante */
    public String getVariant() {
        return this.variant;
    }

    /** Establece la variante de formato a la que pertene el objeto de firma.
     * @param variant Nombre de la variante */
    public void setVariant(final String variant) {
        this.variant = variant;
    }

    /**
     * Recupera la URL en la que se puede encontrar la firma.
     * Si no se conoce o no se ha podido obtener esta URL, se
     * devolver&aacute;a {@code null}.
     * @return URL de la firma.
     */
    public String getUrlSignObject() {
        return this.urlSignObject;
    }

    /**
     * /**
     * Establece la URL en la que se puede encontrar la firma.
     * @param urlSignObject URL de la firma.
     */
    public void setUrlSignObject(final String urlSignObject) {
        this.urlSignObject = urlSignObject;
    }

    /**
     * Recupera la URL en la que se pueden encontrar los datos que se firmaron.
     * Si no se conoce o no se ha podido obtener esta URL, se
     * devolver&aacute;a {@code null}.
     * @return URL de los datos que se han firmado.
     */
    public String getUrlSignedData() {
        return this.urlSignedData;
    }

    /**
     * Establece la URL en la que se pueden encontrar los datos que se firmaron.
     * @param urlSignedData URL de los datos que se han firmado.
     */
    public void setUrlSignedData(final String urlSignedData) {
        this.urlSignedData = urlSignedData;
    }

    /**
     * Recupera el c&oacute;digo de verificaci&oacute;n de la firma en base 64.
     * Si no se conoce o no se ha podido obtener este c&oacute;digo, se
     * devolver&aacute;a {@code null}.
     * @return C&oacute;digo de verificaci&oacute;n de la firma.
     */
    public String getB64VerificationCode() {
        return this.b64VerificationCode;
    }

    /**
     * Establece el c&oacute;digo de verificaci&oacute;n de la firma en base 64.
     * @param b64VerificationCode C&oacute;digo de verificaci&oacute;n de la firma
     * en base 64.
     */
    public void setB64VerificationCode(final String b64VerificationCode) {
        this.b64VerificationCode = b64VerificationCode;
    }

    /** Obtiene el formato de la firma.
     * @return Formato de la firma */
    public String getFormat() {
        return this.format;
    }
}
