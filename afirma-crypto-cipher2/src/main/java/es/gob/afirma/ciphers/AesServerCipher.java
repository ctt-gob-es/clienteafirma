package es.gob.afirma.ciphers;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.json.JSONException;
import org.json.JSONObject;

import es.gob.afirma.core.misc.Base64;

/**
 * Cifra y descifra datos con el algoritmo AES.
 */
public class AesServerCipher implements ServerCipher {

	private final byte[] key;
	private final byte[] iv;

	private final ServerCipher legacyDecipher;

	public AesServerCipher(final String cipherConfig) throws JSONException, IOException {

		final JSONObject json = new JSONObject(cipherConfig);

		final String cipherKey = json.getString("key"); //$NON-NLS-1$
		final String cipherIv = json.getString("iv"); //$NON-NLS-1$

		this.key = cipherKey != null ? Base64.decode(cipherKey) : null;
		this.iv = cipherIv != null ? Base64.decode(cipherIv) : null;

		final String legacyDesKey = json.getString("legacydes"); //$NON-NLS-1$
		this.legacyDecipher = legacyDesKey != null
			? new DesServerCipher(legacyDesKey.getBytes())
			: null;
	}

	public AesServerCipher(final byte[] key, final byte[] iv) throws JSONException {
		this.key = key;
		this.iv = iv;
		this.legacyDecipher = null;
	}

	@Override
	public byte[] decipherData(final byte[] data) throws InvalidKeyException, NoSuchAlgorithmException,
															NoSuchPaddingException, InvalidAlgorithmParameterException,
															IllegalBlockSizeException, BadPaddingException,
															GeneralSecurityException, IOException {

		final String originalB64Data = new String(data, StandardCharsets.UTF_8).replace("_", "/").replace("-", "+");   //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		return decipherData(originalB64Data);
	}


	@Override
	public byte[] decipherData(final String dataB64) throws InvalidKeyException, NoSuchAlgorithmException,
														NoSuchPaddingException, InvalidAlgorithmParameterException,
														IllegalBlockSizeException, BadPaddingException,
														GeneralSecurityException, IOException {

		byte[] decipheredData;
		try {
			final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding"); //$NON-NLS-1$
			cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(this.key, "AES"), new IvParameterSpec(this.iv)); //$NON-NLS-1$
			decipheredData = cipher.doFinal(Base64.decode(dataB64));
		}
		catch (final Exception e) {
			// Si se definio un metodo alternativo de descifrado, se utilizara, aunque en caso de error, se devolvera
			// el error del metodo principal
			if (this.legacyDecipher != null) {
				try {
					return this.legacyDecipher.decipherData(dataB64);
				}
				catch (final Exception e2) {
					// Ignoramos este error en favor del error principal
				}
			}
			throw e;
		}

		return decipheredData;
	}

	@Override
	public String cipherData(final byte[] data) throws NoSuchAlgorithmException, NoSuchPaddingException,
														InvalidKeyException, InvalidAlgorithmParameterException,
														IllegalBlockSizeException, BadPaddingException {

		final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding"); //$NON-NLS-1$
		cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(this.key, "AES"), new IvParameterSpec(this.iv)); //$NON-NLS-1$
		final byte[] cipheredData = cipher.doFinal(data);

		return Base64.encode(cipheredData, true);
	}
}
