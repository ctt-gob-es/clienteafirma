package es.gob.afirma.signers.multi.cades.triphase;

import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Random;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSimpleSigner;

/** Sustituto del firmador PKCS#1 para firmas trif&aacute;sicas.
 * No firma realmente, sino que devuelve unos datos aleatorios del tama&ntilde;o adecuado y
 * guarda estos m&aacute;s los datos que deben ser firmados para en el cliente pueda realizarse
 * la firma y la sustituci&oacute;n de los datos aleatorios por la firma real.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CAdESFakePkcs1Signer implements AOSimpleSigner {

	private static final Random RANDOM = new Random();

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
	CAdESFakePkcs1Signer(final CAdESPreSignResult preCountersignResult) {
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

		// Miramos si ya existe una entrada para exactamente los mismos datos a firmar con PKCS#1,
		// ya que si ya esta no es necesario crear un aleatorio nuevo, y hay que reutilizar el
		// existente.
		byte[] randomDummyData = this.preResult.getRandomDummyData(data);
		if (randomDummyData == null) {

			// Creamos el PKCS#1 falso
			randomDummyData = new byte[p1Size.intValue()];
			RANDOM.nextBytes(randomDummyData);

			// Guardamos el par de PKCS#1 falso y datos a firmar
			this.preResult.addSign(data, randomDummyData);
		}

		return randomDummyData;
	}

}
