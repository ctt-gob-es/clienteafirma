package es.gob.afirma.signers.cades;

import java.security.Signature;
import java.security.SignatureException;
import java.security.KeyStore.PrivateKeyEntry;

import es.gob.afirma.core.AOException;

/** Clase que realiza la firma simple PKCS#1 V1.5 como parte externalizable de las firmas CAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class PKCS1ExternalizableSigner {
    
    /** Realiza una firma PKCS#1 v1.5.
     * @param signatureAlgorithm Algoritmo de firma
     * @param keyEntry Puntero a la clave de firma
     * @param data Datos a firmar
     * @return Firma PKCS#1 en binario puro no tratado
     * @throws AOException en caso de cualqioer problema durante la firma
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
