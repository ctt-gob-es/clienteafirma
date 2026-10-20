package es.gob.afirma.keystores;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.security.Key;
import java.security.KeyStore;
import java.security.KeyStoreSpi;
import java.security.PrivateKey;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;
import java.util.Locale;

import org.junit.Assert;
import org.junit.Test;

/** Pruebas de la recuperaci&oacute;n de identidades del llavero de Apple ocultas por los ajustes de
 * confianza. Se usa una implementaci&oacute;n simulada de <code>apple.security.KeychainStore</code>
 * que reproduce el comportamiento del JDK 17/21, por lo que las pruebas no necesitan un llavero
 * real ni macOS. */
public final class TestAppleKeyStoreManagerHiddenIdentities {

	private static final String ANF_P12 = "/ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final char[] ANF_PWD = "12341234".toCharArray(); //$NON-NLS-1$

	private static final String ACA_P12 = "/ACA PF Administrativo Activo.p12"; //$NON-NLS-1$
	private static final char[] ACA_PWD = "adm2013".toCharArray(); //$NON-NLS-1$

	private static final String LABEL_A = "Certificado A"; //$NON-NLS-1$
	private static final String LABEL_B = "Certificado B"; //$NON-NLS-1$

	/** Identidad (certificado con clave privada) del llavero simulado. */
	private static final class Identity {
		final String label;
		final PrivateKey key;
		final Certificate[] chain;
		Identity(final String label, final KeyStore.PrivateKeyEntry pke) {
			this.label = label;
			this.key = pke.getPrivateKey();
			this.chain = pke.getCertificateChain();
		}
		X509Certificate getCertificate() {
			return (X509Certificate) this.chain[0];
		}
	}

	/** Entrada con clave privada del almac&eacute;n simulado. */
	private static final class KeyEntry {
		final PrivateKey key;
		final Certificate[] chain;
		KeyEntry(final PrivateKey key, final Certificate[] chain) {
			this.key = key;
			this.chain = chain;
		}
	}

	/** Entrada de certificado de confianza del almac&eacute;n simulado. */
	private static final class TrustedCertEntry {
		final Certificate cert;
		TrustedCertEntry(final Certificate cert) {
			this.cert = cert;
		}
	}

	/** Simulaci&oacute;n de <code>apple.security.KeychainStore</code> de JDK 17/21, incluido el error
	 * por el que un certificado con ajustes de confianza sobrescribe a la identidad con la misma
	 * etiqueta (JDK-8320362). */
	private static final class FakeKeychainStore extends KeyStoreSpi {

		private final Hashtable<String, Object> entries = new Hashtable<>();
		private final List<Identity> identities;
		private final List<Identity> trustedCerts;
		private final boolean rescanFails;
		int scans = 0;

		FakeKeychainStore(final List<Identity> identities, final List<Identity> trustedCerts) {
			this(identities, trustedCerts, false);
		}

		FakeKeychainStore(final List<Identity> identities, final List<Identity> trustedCerts, final boolean rescanFails) {
			this.identities = identities;
			this.trustedCerts = trustedCerts;
			this.rescanFails = rescanFails;
		}

		/** Equivalente a <code>KeychainStore._scanKeychain()</code>: primero las identidades y despu&eacute;s
		 * los certificados con ajustes de confianza. */
		@SuppressWarnings("unused")
		private void _scanKeychain() {
			if (this.scans > 0 && this.rescanFails) {
				// Simula el fallo de acceso por reflexion (JVM sin --add-opens)
				throw new IllegalStateException("No se permite un nuevo recorrido del llavero"); //$NON-NLS-1$
			}
			this.scans++;
			for (final Identity id : this.identities) {
				// createKeyEntry(): si el alias existe se busca uno alternativo
				String alias = id.label;
				int uniqueVal = 1;
				while (this.entries.containsKey(alias.toLowerCase(Locale.ROOT))) {
					alias = id.label + " " + uniqueVal++; //$NON-NLS-1$
				}
				this.entries.put(alias.toLowerCase(Locale.ROOT), new KeyEntry(id.key, id.chain));
			}
			for (final Identity tc : this.trustedCerts) {
				// createTrustedCertEntry(): el JDK usa Hashtable.contains() (busca entre los valores)
				// en lugar de containsKey(), por lo que nunca detecta la colision y sobrescribe
				if (this.entries.contains(tc.label.toLowerCase(Locale.ROOT))) {
					throw new IllegalStateException("Hashtable.contains() nunca deberia encontrar un alias"); //$NON-NLS-1$
				}
				this.entries.put(tc.label.toLowerCase(Locale.ROOT), new TrustedCertEntry(tc.getCertificate()));
			}
		}

		@Override
		public void engineLoad(final InputStream stream, final char[] password) {
			this.entries.clear();
			_scanKeychain();
		}

		@Override
		public Key engineGetKey(final String alias, final char[] password) {
			final Object entry = this.entries.get(alias.toLowerCase(Locale.ROOT));
			return entry instanceof KeyEntry ? ((KeyEntry) entry).key : null;
		}

		@Override
		public Certificate[] engineGetCertificateChain(final String alias) {
			final Object entry = this.entries.get(alias.toLowerCase(Locale.ROOT));
			return entry instanceof KeyEntry ? ((KeyEntry) entry).chain.clone() : null;
		}

		@Override
		public Certificate engineGetCertificate(final String alias) {
			final Object entry = this.entries.get(alias.toLowerCase(Locale.ROOT));
			if (entry instanceof KeyEntry) {
				return ((KeyEntry) entry).chain[0];
			}
			return entry instanceof TrustedCertEntry ? ((TrustedCertEntry) entry).cert : null;
		}

		@Override
		public Date engineGetCreationDate(final String alias) {
			return new Date();
		}

		@Override
		public void engineSetKeyEntry(final String alias, final Key key, final char[] password, final Certificate[] chain) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void engineSetKeyEntry(final String alias, final byte[] key, final Certificate[] chain) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void engineSetCertificateEntry(final String alias, final Certificate cert) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void engineDeleteEntry(final String alias) {
			throw new UnsupportedOperationException();
		}

		@Override
		public Enumeration<String> engineAliases() {
			return this.entries.keys();
		}

		@Override
		public boolean engineContainsAlias(final String alias) {
			return this.entries.containsKey(alias.toLowerCase(Locale.ROOT));
		}

		@Override
		public int engineSize() {
			return this.entries.size();
		}

		@Override
		public boolean engineIsKeyEntry(final String alias) {
			return this.entries.get(alias.toLowerCase(Locale.ROOT)) instanceof KeyEntry;
		}

		@Override
		public boolean engineIsCertificateEntry(final String alias) {
			return this.entries.get(alias.toLowerCase(Locale.ROOT)) instanceof TrustedCertEntry;
		}

		@Override
		public String engineGetCertificateAlias(final Certificate cert) {
			return null;
		}

		@Override
		public void engineStore(final OutputStream stream, final char[] password) {
			throw new UnsupportedOperationException();
		}
	}

	private static Identity loadIdentity(final String label, final String resource, final char[] pwd) throws Exception {
		final KeyStore p12 = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (InputStream is = TestAppleKeyStoreManagerHiddenIdentities.class.getResourceAsStream(resource)) {
			p12.load(is, pwd);
		}
		final Enumeration<String> aliases = p12.aliases();
		while (aliases.hasMoreElements()) {
			final String alias = aliases.nextElement();
			if (p12.isKeyEntry(alias)) {
				return new Identity(
					label,
					(KeyStore.PrivateKeyEntry) p12.getEntry(alias, new KeyStore.PasswordProtection(pwd))
				);
			}
		}
		throw new IOException("El almacen " + resource + " no contiene claves privadas"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Crea el gestor sobre el almac&eacute;n simulado y calcula sus alias, tal y como hace
	 * <code>AppleKeyStoreManager.init()</code> tras cargar el almac&eacute;n. */
	private static AppleKeyStoreManager initManager(final FakeKeychainStore spi) throws Exception {
		final KeyStore ks = new KeyStore(spi, Security.getProviders()[0], "KeychainStore") { /* Acceso al constructor protegido */ }; //$NON-NLS-1$
		ks.load(null, null);

		final AppleKeyStoreManager manager = new AppleKeyStoreManager();
		manager.setKeyStore(ks);

		final Field spiField = AppleKeyStoreManager.class.getDeclaredField("keyStoreSpi"); //$NON-NLS-1$
		spiField.setAccessible(true);
		spiField.set(manager, spi);

		final Method getAliases = AppleKeyStoreManager.class.getDeclaredMethod("getAliasesWithoutDuplicates"); //$NON-NLS-1$
		getAliases.setAccessible(true);
		getAliases.invoke(manager);

		return manager;
	}

	private static String findAliasBySerial(final AppleKeyStoreManager manager, final X509Certificate cert) {
		for (final String alias : manager.getAliases()) {
			final X509Certificate c = manager.getCertificate(alias);
			if (c != null && c.getSerialNumber().equals(cert.getSerialNumber())) {
				return alias;
			}
		}
		return null;
	}

	/** Un certificado con ajustes de confianza ("Confiar siempre") oculta a su identidad; el gestor
	 * debe recuperarla forzando un nuevo recorrido del llavero.
	 * @throws Exception En cualquier error. */
	@Test
	public void testHiddenIdentityIsRecovered() throws Exception {
		final Identity a = loadIdentity(LABEL_A, ANF_P12, ANF_PWD);
		final Identity b = loadIdentity(LABEL_B, ACA_P12, ACA_PWD);

		final FakeKeychainStore spi = new FakeKeychainStore(
			new ArrayList<>(java.util.Arrays.asList(a, b)),
			Collections.singletonList(a) // El certificado A tiene ajustes de confianza
		);
		final AppleKeyStoreManager manager = initManager(spi);

		// Comprobamos que el almacen simulado reproduce el error del JDK: la entrada original de A no tiene clave
		Assert.assertTrue("El almacen simulado deberia haber convertido A en certificado de confianza", //$NON-NLS-1$
			manager.getKeyStore().isCertificateEntry(LABEL_A));
		Assert.assertEquals("Deberia haberse forzado un segundo recorrido del llavero", 2, spi.scans); //$NON-NLS-1$

		Assert.assertEquals("Deberian recuperarse las dos identidades", 2, manager.getAliases().length); //$NON-NLS-1$

		final String aliasA = findAliasBySerial(manager, a.getCertificate());
		Assert.assertNotNull("No se ha recuperado la identidad oculta por los ajustes de confianza", aliasA); //$NON-NLS-1$
		Assert.assertTrue("El alias recuperado debe ser una entrada con clave privada", manager.isKeyEntry(aliasA)); //$NON-NLS-1$
		Assert.assertEquals(LABEL_A.toLowerCase(Locale.ROOT) + " 1", aliasA); //$NON-NLS-1$
		Assert.assertEquals(a.key, manager.getKeyEntry(aliasA).getPrivateKey());
		Assert.assertEquals(a.chain.length, manager.getKeyEntry(aliasA).getCertificateChain().length);

		final String aliasB = findAliasBySerial(manager, b.getCertificate());
		Assert.assertNotNull("La identidad no afectada debe seguir disponible", aliasB); //$NON-NLS-1$
		Assert.assertEquals(b.key, manager.getKeyEntry(aliasB).getPrivateKey());
	}

	/** Sin certificados con ajustes de confianza no se hace ning&uacute;n recorrido adicional.
	 * @throws Exception En cualquier error. */
	@Test
	public void testNoRescanWithoutTrustedCertificates() throws Exception {
		final Identity a = loadIdentity(LABEL_A, ANF_P12, ANF_PWD);
		final Identity b = loadIdentity(LABEL_B, ACA_P12, ACA_PWD);

		final FakeKeychainStore spi = new FakeKeychainStore(
			new ArrayList<>(java.util.Arrays.asList(a, b)),
			Collections.<Identity>emptyList()
		);
		final AppleKeyStoreManager manager = initManager(spi);

		Assert.assertEquals(1, spi.scans);
		Assert.assertEquals(2, manager.getAliases().length);
		Assert.assertEquals(LABEL_A.toLowerCase(Locale.ROOT), findAliasBySerial(manager, a.getCertificate()));
		Assert.assertEquals(LABEL_B.toLowerCase(Locale.ROOT), findAliasBySerial(manager, b.getCertificate()));
	}

	/** Las identidades duplicadas en el llavero se reducen a una sola.
	 * @throws Exception En cualquier error. */
	@Test
	public void testDuplicatedIdentitiesAreMerged() throws Exception {
		final Identity a = loadIdentity(LABEL_A, ANF_P12, ANF_PWD);

		final FakeKeychainStore spi = new FakeKeychainStore(
			new ArrayList<>(java.util.Arrays.asList(a, a)),
			Collections.<Identity>emptyList()
		);
		final AppleKeyStoreManager manager = initManager(spi);

		Assert.assertEquals(1, manager.getAliases().length);
		Assert.assertEquals(a.key, manager.getKeyEntry(manager.getAliases()[0]).getPrivateKey());
	}

	/** Si no es posible recorrer de nuevo el llavero (por ejemplo, JVM sin <code>--add-opens</code>) se
	 * mantiene el comportamiento anterior: la identidad oculta no se recupera, pero el resto sigue
	 * disponible y no se produce ning&uacute;n error.
	 * @throws Exception En cualquier error. */
	@Test
	public void testRescanFailureFallsBackToPreviousBehaviour() throws Exception {
		final Identity a = loadIdentity(LABEL_A, ANF_P12, ANF_PWD);
		final Identity b = loadIdentity(LABEL_B, ACA_P12, ACA_PWD);

		final FakeKeychainStore spi = new FakeKeychainStore(
			new ArrayList<>(java.util.Arrays.asList(a, b)),
			Collections.singletonList(a),
			true
		);
		final AppleKeyStoreManager manager = initManager(spi);

		Assert.assertEquals(1, spi.scans);
		Assert.assertArrayEquals(new String[] { LABEL_B.toLowerCase(Locale.ROOT) }, manager.getAliases());
	}
}
