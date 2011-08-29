package es.gob.afirma.signers.cades;

import java.security.Signature;
import java.security.SignatureException;
import java.security.KeyStore.PrivateKeyEntry;

import es.gob.afirma.core.AOException;

class PKCS1ExternalizableSigner {
    
    static byte[] sign(final String signatureAlgorithm, final PrivateKeyEntry keyEntry, final byte[] data) throws AOException {
        
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
