package es.gob.afirma.signers.multi.cades.triphase;

import java.security.MessageDigest;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSimpleSigner;

/** Sustituto del firmador PKCS#1 para firmas trif&aacute;sicas.
 * No firma realmente, sino que devuelve unos datos aleatorios del tama&ntilde;o adecuado y
 * guarda estos m&aacute;s los datos que deben ser firmados para en el cliente pueda realizarse
 * la firma y la sustituci&oacute;n de los datos aleatorios por la firma real.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CAdESFakePkcs1Signer implements AOSimpleSigner {

	private static final String MD_ALGORITHM = "SHA-512";   //$NON-NLS-1$
	
	/** Tama&ntilde;o de una firma PKCS#1 con clave RSA de 1024 bits. */
	private static final Integer PKCS1_DEFAULT_SIZE_1024 = Integer.valueOf(128);

	/** Tama&ntilde;o de una firma PKCS#1 con clave RSA de 2048 bits. */
	private static final Integer PKCS1_DEFAULT_SIZE_2048 = Integer.valueOf(256);

	/** Tama&ntilde;o de una firma PKCS#1 con clave RSA de 4096 bits. */
	private static final Integer PKCS1_DEFAULT_SIZE_4096 = Integer.valueOf(512);

	private static final Integer KEY_SIZE_1024 = Integer.valueOf(1024);
	private static final Integer KEY_SIZE_2048 = Integer.valueOf(2048);
	private static final Integer KEY_SIZE_4096 = Integer.valueOf(4096);

	private static final Map<Integer, Integer> P1_SIZES = new HashMap<Integer, Integer>(3);
	static {
		P1_SIZES.put(KEY_SIZE_1024, PKCS1_DEFAULT_SIZE_1024);
		P1_SIZES.put(KEY_SIZE_2048, PKCS1_DEFAULT_SIZE_2048);
		P1_SIZES.put(KEY_SIZE_4096, PKCS1_DEFAULT_SIZE_4096);
	}

	private final CAdESPreSignResult preResult;

	/** Construye el sustituto del firmador PKCS#1 para firmas trif&aacute;sicas.
	 * @param preCountersignResult Resultado donde ir almacenando los pares de datos a firmar
	 *                             y datos aleatorios a sustituir. */
	public CAdESFakePkcs1Signer(final CAdESPreSignResult preCountersignResult) {
		if (preCountersignResult == null) {
			throw new IllegalArgumentException(
				"Es necesario un resultado de PreContrafirma para ir almacenando las firmas" //$NON-NLS-1$
			);
		}
		this.preResult = preCountersignResult;
	}

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties extraParams) throws AOException {
		// La clave debe ser nula porque este proceso se realiza en servidor, pero el tamano de la clave
		// puede salir del Certificado

		// Obtenemos el tamano de clave y de PKCS#1
		final int keySize = ((RSAPublicKey)((X509Certificate)certChain[0]).getPublicKey()).getModulus().bitLength();
		final Integer p1Size = P1_SIZES.get(Integer.valueOf(keySize));
		if (p1Size == null) {
			throw new AOException("Tamano de clave no soportado: " + keySize); //$NON-NLS-1$
		}

		// Calculamos un valor que sera siempre el mismo para los mismos datos y de las dimensiones que
		// corresponden a un PKCS#1 del tamano de clave del certificado utilizado 
		final byte[] sha512;
		try {
			sha512 = MessageDigest.getInstance(MD_ALGORITHM).digest(data);
		}
		catch (final Exception e) {
			throw new AOException("Ocurrio un error al generar el PKCS#1 temporal de los datos", e); //$NON-NLS-1$
		}
		final byte[] dummyData = new byte[p1Size.intValue()];
		for (int i = 0; i < dummyData.length; i += sha512.length) {
			System.arraycopy(sha512, 0, dummyData, i, sha512.length);
		}
		
		// Guardamos el par de PKCS#1 falso y datos a firmar
		this.preResult.addSign(data, dummyData);

		return dummyData;
	}
}
