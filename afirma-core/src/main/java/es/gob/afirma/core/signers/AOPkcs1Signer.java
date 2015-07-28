package es.gob.afirma.core.signers;

import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
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
	 *  <li><i>SHA256withRSA</i></li>
	 *  <li><i>SHA384withRSA</i></li>
	 *  <li><i>SHA512withRSA</i></li>
	 * </ul>
	 * @param key Clave privada a usar para la firma.
	 * @param certChain Se ignora, esta clase no necesita la cadena de certificados.
	 * @param data Datos a firmar.
	 * @param extraParams Se ignora, esta clase no acepta par&aacute;metros adicionales.
	 * @return Firma PKCS#1 en binario puro no tratado.
	 * @throws AOException en caso de cualquier problema durante la firma. */
	@Override
	public byte[] sign(final byte[] data, final String algorithm, final PrivateKey key, final Certificate[] certChain, final Properties extraParams) throws AOException {
		final Signature sig;

		try {
			// En Android las capacidades de los proveedores, aunque se declaren bien, no se manejan adecuadamente
			if ("com.aet.android.providerPKCS15.SEPrivateKey".equals(key.getClass().getName())) { //$NON-NLS-1$
				sig = Signature.getInstance(algorithm, "AETProvider"); //$NON-NLS-1$
			}
			else if ("es.gob.jmulticard.jse.provider.DniePrivateKey".equals(key.getClass().getName())) { //$NON-NLS-1$
				java.util.logging.Logger.getLogger("es.gob.afirma").info("Detectada clave privada DNIe 100% Java"); //$NON-NLS-1$ //$NON-NLS-2$
				sig = Signature.getInstance(algorithm, "DNIeJCAProvider"); //$NON-NLS-1$
			}
			else if ("es.gob.jmulticard.jse.provider.ceres.CeresPrivateKey".equals(key.getClass().getName())) { //$NON-NLS-1$
				java.util.logging.Logger.getLogger("es.gob.afirma").info("Detectada clave privada CERES 100% Java"); //$NON-NLS-1$ //$NON-NLS-2$
				sig = Signature.getInstance(algorithm, "CeresJCAProvider"); //$NON-NLS-1$
			}
			else {
				sig = Signature.getInstance(algorithm);
			}
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOException("No se soporta el algoritmo de firma (" + algorithm + "): " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final NoSuchProviderException e) {
			throw new AOException("No hay un proveedor para el algoritmo '" + algorithm + "' con el tipo de clave '" + key.getAlgorithm() + "': " + e, e);  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
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
		catch (final SignatureException e) {
			throw new AOException("Error durante el proceso de firma PKCS#1: " + e, e); //$NON-NLS-1$
		}
	}
}
