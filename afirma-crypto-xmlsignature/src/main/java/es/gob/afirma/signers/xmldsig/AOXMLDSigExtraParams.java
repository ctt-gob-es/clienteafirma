/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xmldsig;

final class AOXMLDSigExtraParams {

    /** URL en la que se encuentra el documento a firmar, necesario en el caso del formato <i>Externally Detached</i>. */
    static final String URI ="uri"; //$NON-NLS-1$

    /** Indica si se debe evitar la inclusi&oacute;n de la transformaci&oacute;n
     * XPATH2 que normalmente se a&ntilde;ade para posibilitar las cofirmas y
     * que elimina todas las firmas del documento para dejar &uacute;nicamente
     * el contenido. Por defecto, se encuentra a <code>false</code>.
     * &Uacute;nicamente aplica a firmas <i>enveloped</i>. */
    static final String AVOID_XPATH_EXTRA_TRANSFORMS_ON_ENVELOPED = "avoidXpathExtraTransformsOnEnveloped";//$NON-NLS-1$

    /** Modo de firma. */
    static final String MODE = "mode"; //$NON-NLS-1$

    /** Formato de firma. */
    static final String FORMAT = "format"; //$NON-NLS-1$

    /** Algoritmo de huella digital a usar en las referencias XML (referencesDigestMethod). */
    static final String REFERENCES_DIGEST_METHOD = "referencesDigestMethod";//$NON-NLS-1$

    /** Algoritmo de canonicalizaci&oacute;n. */
    static final String CANONICALIZATION_ALGORITHM = "canonicalizationAlgorithm";//$NON-NLS-1$

    /** Ignora las hojas de estilo externas de los XML (no las firma) si se establece a <code>true</code>,
     * si se establece a false act&uacute;a normalmente (s&iacute; las firma)
     * (no aplica a contrafirmas). */
    static final String IGNORE_STYLE_SHEETS = "ignoreStyleSheets";//$NON-NLS-1$

    /** No declara transformaciones Base64 incluso si son necesarias si se establece a true,
     * si se establece a false act&uacute;a normalmente (s&iacute; las declara)
     * (no aplica a contrafirmas). */
    static final String AVOID_BASE64_TRANSFORMS = "avoidBase64Transforms";//$NON-NLS-1$

    /** Evita cualquier interacci&oacute;n con el usuario si se establece a true, si se establece a false act&uacute;a normalmente
     * (puede mostrar di&aacute;logos, por ejemplo, para la dereferenciaci&oacute;n de hojas de estilo enlazadas con rutas relativas).
     * &uacute;til para los procesos desatendidos y por lotes. */
    static final String HEADLESS = "headless";//$NON-NLS-1$

    /** MIME-Type de los datos a firmar. Si no se indica se realiza una auto-detecci&oacute;n cuyo resultado puede ser inexacto
     * (no aplica a contrafirmas). */
    static final String MIME_TYPE = "mimeType";//$NON-NLS-1$

    /** Codificaci&oacute;n de los datos a firmar. */
    static final String ENCODING = "encoding";//$NON-NLS-1$

    /** Algoritmo utilizado para el c&aacute;lculo de la huella digital cuando se proporciona esta en vez de los datos a firmar.
     * Cuando se proporcione una huella en vez de datos deben tenerse en cuenta los siguientes aspectos:
     * La huella digital debe indicarse en lugar de los datos, en el mismo par&aacute;metro del m&eacute;todo de firma.
     * Solo puede indicarse una huella cuando no se incluyan los datos dentro de la propia firma, es decir,
     * en firmas externally detached, siendo conveniente adem&aacute;s hacer uso de un Manifest. */
    static final String PRECALCULATED_HASH_ALGORITHM = "precalculatedHashAlgorithm";//$NON-NLS-1$

    /** Indica, mediante un <code>true</code> o <code>false</code>, que debe incluirse en la firma
     * &uacute;nicamente el certificado utilizado para firmar y no su cadena de certificaci&oacute;n
     * completa. Por defecto, se incluir&aacute; toda la cadena de certificaci&oacute;n
     * (propiedad compartida con CAdES y PAdES). */
    static final String INCLUDE_ONLY_SIGNNING_CERTIFICATE = "includeOnlySignningCertificate";//$NON-NLS-1$

    /** Prefijo XMLDSig. */
    static final String XML_SIGNATURE_PREFIX = "xmlSignaturePrefix";//$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private AOXMLDSigExtraParams(){
        // No instanciable
    }
}
