package es.gob.afirma.envelopers.cms;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.lang.reflect.Field;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreSpi;
import java.security.Provider;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherPadding;
import es.gob.afirma.core.misc.AOUtil;

/** Pruebas de sobres con tarjetas del Ministerio de Defensa.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestTemd {

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Prueba de creaci&oacute;n de sobre con PKCS#11 FNMT.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void createEnvelopePkcs11Fnmt() throws Exception {

		final byte[] content ="Hola mundo".getBytes(); //$NON-NLS-1$
		final File f = new File(
			TestTemd.class.getResource("/temd-pkcs11/x86_6_1_3/FNMT_P11.dll").toURI() //$NON-NLS-1$
		);

		final Provider p = Security.getProvider("SunPKCS11"); //$NON-NLS-1$
		p.load(new ByteArrayInputStream((
				"name=pkcs11-win_dll\n" + //$NON-NLS-1$
				"library=" + f.getAbsolutePath() + "\n" + //$NON-NLS-1$ //$NON-NLS-2$
				"disabledMechanisms={ CKM_SHA1_RSA_PKCS }\n" + //$NON-NLS-1$
				"showInfo=true" //$NON-NLS-1$
			).getBytes()));

		Security.addProvider(p);
		final KeyStore ks = KeyStore.getInstance("PKCS11", p); //$NON-NLS-1$
		ks.load(null, "A111111a".toCharArray()); //$NON-NLS-1$

		String selectedAlias = null;
		X509Certificate dest = null;
		final Enumeration<String> aliases = ks.aliases();
		while (aliases.hasMoreElements()) {
			final String alias = aliases.nextElement();
			final X509Certificate tmpCer = (X509Certificate)ks.getCertificate(alias);
			if (
				tmpCer.getIssuerX500Principal().toString().contains("O=MDEF") && //$NON-NLS-1$
				isCipherCert(tmpCer.getKeyUsage())
			) {
				selectedAlias = alias;
				dest = tmpCer;
			}
		}

		Assert.assertNotNull(dest);
		Assert.assertNotNull(selectedAlias);

		// Destinatario del sobre

		final byte[] envelope = new AOCMSEnveloper().createCMSEnvelopedData(
			content,
			null,
			new AOCipherConfig(
				AOCipherAlgorithm.AES,
				AOCipherBlockMode.ECB,
				AOCipherPadding.PKCS5PADDING
			),
			new X509Certificate[] { dest },
			Integer.valueOf(128) // <- CAMBIAR A 256 SI SE TIENE DESACTIVADA LA RESTRICCION EN EL JRE
		);

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
			selectedAlias,
			new KeyStore.PasswordProtection("A111111a".toCharArray()) //$NON-NLS-1$
		);

		// Comprobacion de que se esta intentando usar el certificado bueno
		Assert.assertEquals(
			"Prueba mal concebida, se esta intentando abrir el sobre con un certificado que no es destinatario", //$NON-NLS-1$
			dest.getSerialNumber().toString(),
			((X509Certificate)pke.getCertificate()).getSerialNumber().toString()
		);

		// Y comprobacion de que abre bien
		final byte[] recoveredData = new AOCMSEnveloper().recoverData(envelope, pke);
		Assert.assertEquals(
			"El contenido desensobrado no coincide con el ensobrado", //$NON-NLS-1$
			new String(content),
			new String(recoveredData)
		);

		System.out.println(new String(recoveredData));

	}

	/** Prueba de creaci&oacute;n de sobre con CAPI.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void createEnvelopeCapi() throws Exception {

		final byte[] content ="Hola mundo".getBytes(); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("Windows-My"); //$NON-NLS-1$
		ks.load(null, null);
		cleanCAPIDuplicateAliases(ks);

		String selectedAlias = null;
		X509Certificate dest = null;
		final Enumeration<String> aliases = ks.aliases();
		while (aliases.hasMoreElements()) {
			final String alias = aliases.nextElement();
			final X509Certificate tmpCer = (X509Certificate)ks.getCertificate(alias);
			if (
				tmpCer.getIssuerX500Principal().toString().contains("O=MDEF") && //$NON-NLS-1$
				isCipherCert(tmpCer.getKeyUsage())
			) {
				selectedAlias = alias;
				dest = tmpCer;
			}
		}

		Assert.assertNotNull(dest);
		Assert.assertNotNull(selectedAlias);

		// Destinatario del sobre

		final byte[] envelope = new AOCMSEnveloper().createCMSEnvelopedData(
			content,
			null,
			new AOCipherConfig(
				AOCipherAlgorithm.AES,
				AOCipherBlockMode.ECB,
				AOCipherPadding.PKCS5PADDING
			),
			new X509Certificate[] { dest },
			Integer.valueOf(128) // <- CAMBIAR A 256 SI SE TIENE DESACTIVADA LA RESTRICCION EN EL JRE
		);

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
			selectedAlias,
			new KeyStore.PasswordProtection("A111111a".toCharArray()) //$NON-NLS-1$
		);

		// Comprobacion de que se esta intentando usar el certificado bueno
		Assert.assertEquals(
			"Prueba mal concebida, se esta intentando abrir el sobre con un certificado que no es destinatario", //$NON-NLS-1$
			dest.getSerialNumber().toString(),
			((X509Certificate)pke.getCertificate()).getSerialNumber().toString()
		);

		// Y comprobacion de que abre bien
		final byte[] recoveredData = new AOCMSEnveloper().recoverData(envelope, pke);
		Assert.assertEquals(
			"El contenido desensobrado no coincide con el ensobrado", //$NON-NLS-1$
			new String(content),
			new String(recoveredData)
		);

		System.out.println(new String(recoveredData));

	}

	private static boolean isCipherCert(final boolean[] keyUsage) {
		return keyUsage != null     &&
			   keyUsage.length == 9 &&
			   keyUsage[2]          &&
			   keyUsage[3];
	}

	private static void cleanCAPIDuplicateAliases(final KeyStore keyStore) throws NoSuchFieldException, IllegalAccessException {
		// Java 9 no sufre el problema de Alias duplicados
		if (AOUtil.isJava9orNewer()) {
			return;
		}

		Field field = keyStore.getClass().getDeclaredField("keyStoreSpi"); //$NON-NLS-1$
		field.setAccessible(true);
		final KeyStoreSpi keyStoreVeritable = (KeyStoreSpi) field.get(keyStore);

		if ("sun.security.mscapi.KeyStore$MY".equals(keyStoreVeritable.getClass().getName())) { //$NON-NLS-1$
			String alias, hashCode;
			X509Certificate[] certificates;

			field = keyStoreVeritable.getClass().getEnclosingClass().getDeclaredField("entries"); //$NON-NLS-1$
			field.setAccessible(true);

			final Object entriesCollection = field.get(keyStoreVeritable);
			if (entriesCollection instanceof Collection<?>) {
				final Collection<?> entries = (Collection<?>) field.get(keyStoreVeritable);
				for (final Object entry : entries) {
					field = entry.getClass().getDeclaredField("certChain"); //$NON-NLS-1$
					field.setAccessible(true);
					certificates = (X509Certificate[]) field.get(entry);

					hashCode = Integer.toString(certificates[0].hashCode());

					field = entry.getClass().getDeclaredField("alias"); //$NON-NLS-1$
					field.setAccessible(true);
					alias = (String) field.get(entry);

					if (!alias.equals(hashCode)) {
						field.set(entry, alias.concat(" - ").concat(hashCode)); //$NON-NLS-1$
					}
				} // for
			}
		} // if
	}

}
