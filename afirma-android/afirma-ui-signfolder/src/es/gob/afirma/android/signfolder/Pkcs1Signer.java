package es.gob.afirma.android.signfolder;

import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;

/** Firmador simple en formato PKCS#1.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface Pkcs1Signer {

	/** REaliza una firma en formato PKCS#1
	 * @param data Datos a firmar
	 * @param keyEntry Clave privada para firmar.
	 * @param algorithm Algoritmo de firma
	 * @return Datos firmados
	 * @throws SignatureException Si ocurren errores durante la firma
	 * @throws NoSuchAlgorithmException Si el algoritmo de firma no est&aacute; soportado
	 * @throws InvalidKeyException Cuando la clave de firma no es v&aacute;lida. */
	byte[] sign(final byte[] data, final PrivateKeyEntry keyEntry, final String algorithm) throws SignatureException, NoSuchAlgorithmException, InvalidKeyException;

}
