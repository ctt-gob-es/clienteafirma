/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.xml;

import java.util.HashMap;
import java.util.Map;

import es.gob.afirma.core.signers.AOSignConstants;

/** Clase con las constantes comunes compartidas por los distintos formatos de
 * firma XML. */
public final class XMLConstants {

    /** URI que define el NameSpace de firma XMLdSig (Compatible XAdES). */
    public static final String DSIGNNS = "http://www.w3.org/2000/09/xmldsig#"; //$NON-NLS-1$
    
    /** URIs de los algoritmos de firma */
    public static final Map<String, String> SIGN_ALGOS_URI = new HashMap<String, String>() {
        private static final long serialVersionUID = 1897588397257599853L;
        {
            put(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, "http://www.w3.org/2000/09/xmldsig#rsa-sha1"); //$NON-NLS-1$
            // Introducimos variantes para hacerlo mas robusto
            put("RSA", "http://www.w3.org/2000/09/xmldsig#rsa-sha1"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA-1withRSA", "http://www.w3.org/2000/09/xmldsig#rsa-sha1"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA1withRSAEncryption", "http://www.w3.org/2000/09/xmldsig#rsa-sha1"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA-1withRSAEncryption", "http://www.w3.org/2000/09/xmldsig#rsa-sha1"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHAwithRSAEncryption", "http://www.w3.org/2000/09/xmldsig#rsa-sha1"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHAwithRSA", "http://www.w3.org/2000/09/xmldsig#rsa-sha1"); //$NON-NLS-1$ //$NON-NLS-2$

            put(AOSignConstants.SIGN_ALGORITHM_MD5WITHRSA, "http://www.w3.org/2001/04/xmldsig-more#rsa-md5"); //$NON-NLS-1$

            put(AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"); //$NON-NLS-1$
            // Introducimos variantes para hacerlo mas robusto
            put("SHA-256withRSA", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA256withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA-256withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"); //$NON-NLS-1$ //$NON-NLS-2$

            put(AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA, "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384"); //$NON-NLS-1$
            // Introducimos variantes para hacerlo mas robusto
            put("SHA-384withRSA", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA384withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA-384withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384"); //$NON-NLS-1$ //$NON-NLS-2$

            put(AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA, "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512"); //$NON-NLS-1$
            // Introducimos variantes para hacerlo mas robusto
            put("SHA-512withRSA", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA512withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512"); //$NON-NLS-1$ //$NON-NLS-2$
            put("SHA-512withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512"); //$NON-NLS-1$ //$NON-NLS-2$

        }
    };
    
    /** Codificaci&oacute;n Base64 para firmas XMLDSig y XAdES. */
    public static final String BASE64_ENCODING = "http://www.w3.org/2000/09/xmldsig#base64"; //$NON-NLS-1$

    /** URIs de los algoritmos de hash. Las claves se encuentran en
     * min&uacute;sculas. */
    public static final Map<String, String> MESSAGEDIGEST_ALGOS_URI = new HashMap<String, String>() {
        private static final long serialVersionUID = 7994196143222514908L;
        {
            // Introducimos variantes para hacerlo mas robusto

            // SHA1
            put("sha1", "http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$ //$NON-NLS-2$
            put("sha-1", "http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$ //$NON-NLS-2$

            // MD5
            put("md5", "http://www.w3.org/2001/04/xmldsig-more#md5"); //$NON-NLS-1$ //$NON-NLS-2$

            // SHA256
            put("sha256", "http://www.w3.org/2001/04/xmlenc#sha256"); //$NON-NLS-1$ //$NON-NLS-2$
            put("sha-256", "http://www.w3.org/2001/04/xmlenc#sha256"); //$NON-NLS-1$ //$NON-NLS-2$

            // SHA384
            put("sha384", "http://www.w3.org/2001/04/xmldsig-more#sha384"); //$NON-NLS-1$ //$NON-NLS-2$
            put("sha-384", "http://www.w3.org/2001/04/xmldsig-more#sha384"); //$NON-NLS-1$ //$NON-NLS-2$

            // SHA512
            put("sha512", "http://www.w3.org/2001/04/xmlenc#sha512"); //$NON-NLS-1$ //$NON-NLS-2$
            put("sha-512", "http://www.w3.org/2001/04/xmlenc#sha512"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    };
    
    /** MimeType por defecto para los datos firmados. */
    public static final String DEFAULT_MIMETYPE = "application/octet-stream"; //$NON-NLS-1$


}
