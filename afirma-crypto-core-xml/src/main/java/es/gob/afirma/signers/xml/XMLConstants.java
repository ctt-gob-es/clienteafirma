/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xml;

import java.util.HashMap;
import java.util.Map;

import es.gob.afirma.core.signers.AOSignConstants;

/** Clase con las constantes comunes compartidas por los distintos formatos de
 * firma XML. */
public final class XMLConstants {

    private XMLConstants() {
        // No permitimos la instanciacion
    }

    /** URI que define el NameSpace de firma XMLdSig (Compatible XAdES). */
    public static final String DSIGNNS = "http://www.w3.org/2000/09/xmldsig#"; //$NON-NLS-1$

    private static final String URL_SHA1_RSA    = "http://www.w3.org/2000/09/xmldsig#rsa-sha1"; //$NON-NLS-1$
    private static final String URL_SHA256_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"; //$NON-NLS-1$
    private static final String URL_SHA384_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384"; //$NON-NLS-1$
    private static final String URL_SHA512_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512"; //$NON-NLS-1$
    private static final String URL_SHA1        = "http://www.w3.org/2000/09/xmldsig#sha1"; //$NON-NLS-1$
    private static final String URL_SHA256      = "http://www.w3.org/2001/04/xmlenc#sha256"; //$NON-NLS-1$
    private static final String URL_SHA384      = "http://www.w3.org/2001/04/xmldsig-more#sha384"; //$NON-NLS-1$
    private static final String URL_SHA512      = "http://www.w3.org/2001/04/xmlenc#sha512"; //$NON-NLS-1$

    /** URIs de los algoritmos de firma */
    public static final Map<String, String> SIGN_ALGOS_URI;
    static {
    	SIGN_ALGOS_URI = new HashMap<String, String>();

    	SIGN_ALGOS_URI.put(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, URL_SHA1_RSA);
    	// Introducimos variantes para hacerlo mas robusto
    	SIGN_ALGOS_URI.put("RSA", URL_SHA1_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA-1withRSA", URL_SHA1_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA1withRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA-1withRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHAwithRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHAwithRSA", URL_SHA1_RSA); //$NON-NLS-1$

    	SIGN_ALGOS_URI.put(AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, URL_SHA256_RSA);
    	// Introducimos variantes para hacerlo mas robusto
    	SIGN_ALGOS_URI.put("SHA-256withRSA", URL_SHA256_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA256withRSAEncryption", URL_SHA256_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA-256withRSAEncryption", URL_SHA256_RSA); //$NON-NLS-1$

    	SIGN_ALGOS_URI.put(AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA, URL_SHA384_RSA);
    	// Introducimos variantes para hacerlo mas robusto
    	SIGN_ALGOS_URI.put("SHA-384withRSA", URL_SHA384_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA384withRSAEncryption", URL_SHA384_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA-384withRSAEncryption", URL_SHA384_RSA); //$NON-NLS-1$

    	SIGN_ALGOS_URI.put(AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA, URL_SHA512_RSA);
    	// Introducimos variantes para hacerlo mas robusto
    	SIGN_ALGOS_URI.put("SHA-512withRSA", URL_SHA512_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA512withRSAEncryption", URL_SHA512_RSA); //$NON-NLS-1$
    	SIGN_ALGOS_URI.put("SHA-512withRSAEncryption", URL_SHA512_RSA); //$NON-NLS-1$
    }

    /** Codificaci&oacute;n Base64 para firmas XMLDSig y XAdES. */
    public static final String BASE64_ENCODING = "http://www.w3.org/2000/09/xmldsig#base64"; //$NON-NLS-1$

    /** URIs de los algoritmos de hash. Las claves se encuentran en
     * min&uacute;sculas. */
    public static final Map<String, String> MESSAGEDIGEST_ALGOS_URI;
    static {
    	MESSAGEDIGEST_ALGOS_URI = new HashMap<String, String>();

    	// Introducimos variantes para hacerlo mas robusto

    	// SHA1
    	MESSAGEDIGEST_ALGOS_URI.put("sha1", URL_SHA1); //$NON-NLS-1$
    	MESSAGEDIGEST_ALGOS_URI.put("sha-1", URL_SHA1); //$NON-NLS-1$

    	// SHA256
    	MESSAGEDIGEST_ALGOS_URI.put("sha256", URL_SHA256); //$NON-NLS-1$
    	MESSAGEDIGEST_ALGOS_URI.put("sha-256", URL_SHA256); //$NON-NLS-1$

    	// SHA384
    	MESSAGEDIGEST_ALGOS_URI.put("sha384", URL_SHA384); //$NON-NLS-1$
    	MESSAGEDIGEST_ALGOS_URI.put("sha-384", URL_SHA384); //$NON-NLS-1$

    	// SHA512
    	MESSAGEDIGEST_ALGOS_URI.put("sha512", URL_SHA512); //$NON-NLS-1$
    	MESSAGEDIGEST_ALGOS_URI.put("sha-512", URL_SHA512); //$NON-NLS-1$
    }

    /** MimeType por defecto para los datos firmados. */
    public static final String DEFAULT_MIMETYPE = "application/octet-stream"; //$NON-NLS-1$

    /** URI que define una referencia de tipo OBJECT. */
    public static final String OBJURI = "http://www.w3.org/2000/09/xmldsig#Object"; //$NON-NLS-1$
}
