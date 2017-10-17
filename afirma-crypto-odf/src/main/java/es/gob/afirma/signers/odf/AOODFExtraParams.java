/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.odf;

final class AOODFExtraParams {

    /** Algoritmo de huella digital a usar en las referencias XML (referencesDigestMethod).
     * Debe indicarse como una URL, acept&aacute;ndose los siguientes valores:
     * <ul>
     *  <li><b>http://www.w3.org/2000/09/xmldsig#sha1</b> (SHA-1)</li>
     *  <li><b>http://www.w3.org/2001/04/xmlenc#sha256</b> (SHA-256, valor recomendado)</li>
     *  <li><b>http://www.w3.org/2001/04/xmlenc#sha512</b> (SHA-512)</li>
     * </ul> */
    static final String REFERENCES_DIGEST_METHOD = "referencesDigestMethod";//$NON-NLS-1$

    /** Un valor <code>true</code> fuerza la generaci&oacute;n de firmas en formato OpenOffice.org 3.1. Las firmas en formato
     * OpenOffice.org 3.1 no son compatibles ni con versiones anteriores ni con posteriores, incluyendo LibreOffice. */
    static final String USE_OPEN_OFFICE_31_MODE = "useOpenOffice31Mode";//$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private AOODFExtraParams(){
        // No instanciable
    }
}
