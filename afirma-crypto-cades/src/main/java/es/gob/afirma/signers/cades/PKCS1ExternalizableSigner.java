/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.cades;

import java.security.Signature;
import java.security.SignatureException;
import java.security.KeyStore.PrivateKeyEntry;

import es.gob.afirma.core.AOException;

/** Realizaci&oacute;n de firmas simples PKCS#1 V1.5 como parte externalizable de las firmas CAdES.
 * La exposici&oacute;n de esta clase es necesaria para la realizaci&oacute; de firmas CAdES en tres fases.
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
     * @param keyEntry Entrada que apunta a la clave de privada a usar para la firma
     * @param data Datos a firmar
     * @return Firma PKCS#1 en binario puro no tratado
     * @throws AOException en caso de cualquier problema durante la firma
     */
    public static byte[] sign(final String signatureAlgorithm, final PrivateKeyEntry keyEntry, final byte[] data) throws AOException {
        
        final Signature sig;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        try {
            sig.initSign(keyEntry.getPrivateKey());
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
