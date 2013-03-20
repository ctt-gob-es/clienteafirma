package es.gob.afirma.core.signers;

import java.security.PrivateKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;

/** Firmador simple en formato PKCS#1.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AOPkcs1Signer implements AOSimpleSigner {

    /** Realiza una firma electr&oacute;nica PKCS#1 v1.5.
     * @param algorithm Algoritmo de firma a utilizar
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>signatureAlgorithm</code>:</p>
     * <ul>
     *  <li><i>SHA1withRSA</i></li>
     *  <li><i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li><i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li><i>SHA256withRSA</i></li>
     *  <li><i>SHA384withRSA</i></li>
     *  <li><i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada a usar para la firma
     * @param certChain Cadena de certificados del firmante
     * @param data Datos a firmar
     * @param extraParams Se ignora, esta clase no acepta par&aacute;metros adicionales
     * @return Firma PKCS#1 en binario puro no tratado
     * @throws AOException en caso de cualquier problema durante la firma
     */
	@Override
	public byte[] sign(final byte[] data, final String algorithm, final PrivateKey key, final Certificate[] certChain, final Properties extraParams) throws AOException {
        final Signature sig;
        try {
            sig = Signature.getInstance(algorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + algorithm + ": " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        try {
            sig.initSign(key);
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada: " + e, e); //$NON-NLS-1$
        }

        try {
            sig.update(data);
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar los datos a firmar: " + e, e); //$NON-NLS-1$
        }

        try {
            return sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma: " + e, e); //$NON-NLS-1$
        }
	}

}
