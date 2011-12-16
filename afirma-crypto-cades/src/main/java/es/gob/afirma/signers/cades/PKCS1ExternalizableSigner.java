/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cades;

import java.security.PrivateKey;
import java.security.Signature;
import java.security.SignatureException;

import es.gob.afirma.core.AOException;

/** Realizaci&oacute;n de firmas simples PKCS#1 V1.5 como parte externalizable de las firmas CAdES.
 * La exposici&oacute;n de esta clase es necesaria para la realizaci&oacute;n de firmas CAdES en tres fases.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PKCS1ExternalizableSigner {
    
    private PKCS1ExternalizableSigner() {
     // No permitimos la instanciacion
    }
    
    /** Realiza una firma electr&oacute;nica PKCS#1 v1.5.
     * @param signatureAlgorithm Algoritmo de firma a utilizar
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>signatureAlgorithm</code>:</p>
     * <ul>
     *  <li><i>SHA1withRSA</i></li>
     *  <li><i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li><i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li><i>SHA256withRSA</i></li>
     *  <li><i>SHA384withRSA</i></li>
     *  <li><i>SHA512withRSA</i></li>
     * </ul>
     * @param privateKey Clave privada a usar para la firma
     * @param data Datos a firmar
     * @return Firma PKCS#1 en binario puro no tratado
     * @throws AOException en caso de cualquier problema durante la firma
     */
    public static byte[] sign(final String signatureAlgorithm, final PrivateKey privateKey, final byte[] data) throws AOException {
        
        final Signature sig;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        try {
            sig.initSign(privateKey);
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada", e); //$NON-NLS-1$
        }

        try {
            sig.update(data);
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar los datos a firmar", e); //$NON-NLS-1$
        }
        
        try {
            return sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

    }

}
