/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

import java.util.logging.Logger;

import es.gob.afirma.core.signers.AOSignConstants;


/** Clase auxiliar que prepara los par&aacute;metros para cada una de las formas
 * de firma. */
public final class P7ContentSignerParameters {

    private final byte[] data;
    private final String sigAlgo;
    private final byte[] signature;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** M&eacute;todo que asigna los datos pasados por par&aacute;metro a las
     * propiedades de la clase.
     * @param data2
     *        Archivo de firmas.
     * @param signatureAlgorithm Tipo de datos. */
    public P7ContentSignerParameters(final byte[] data2, final String signatureAlgorithm) {

        this.data = data2 != null ? data2.clone() : null;

        if (signatureAlgorithm == null || signatureAlgorithm.length() < 1) {
            LOGGER.warning("No se especifico algoritmo para la firma CADES, se utilizara " + AOSignConstants.DEFAULT_SIGN_ALGO); //$NON-NLS-1$
            this.sigAlgo = AOSignConstants.DEFAULT_SIGN_ALGO;
        }
        else {
            this.sigAlgo = signatureAlgorithm;
        }
        this.signature = new byte[0]; // la firma se realizara despues

    }

    /** M&eacute;todo que devuelve el contenido
     * @return el contenido */
    public byte[] getContent() {
        return this.data != null ? this.data.clone() : null;
    }

    /** M&eacute;todo que devuelve la firma.
     * @return la firma. */
    public byte[] getSignature() {
        return this.signature.clone();
    }

    /** M&eacute;todo que devuelve el tipo
     * @return el tipo */
    public String getSignatureAlgorithm() {
        return this.sigAlgo;
    }

}
