/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManagerFactory;

import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cert.jcajce.JcaX509v3CertificateBuilder;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;
import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Pruebas de la gesti&oacute;n del secreto local del canal TLS (ASVS 4.0.3 V2.5.1, V6.4.1 y
 * V8, tarea T4 de la OT Fase 2).
 */
public class LocalKeyStoreSecretsTest {

	private static final String PKCS12 = "PKCS12"; //$NON-NLS-1$
	private static final String TLS_1_2 = "TLSv1.2"; //$NON-NLS-1$
	private static final String KEYSTORE_NAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final int SECRET_BYTES = 32;

	private static final Logger APP_LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@Rule
	public TemporaryFolder folder = new TemporaryFolder();

	private CapturingHandler handler;

	@Before
	public void setUp() {
		Security.addProvider(new BouncyCastleProvider());
		this.handler = new CapturingHandler();
		APP_LOGGER.addHandler(this.handler);
	}

	@After
	public void tearDown() {
		APP_LOGGER.removeHandler(this.handler);
	}

	/**
	 * Instalaci&oacute;n limpia: el almac&eacute;n llega con la contrase&ntilde;a heredada y sin
	 * secreto local. Se genera el secreto, el fichero queda en 0600 y el socket TLS levanta.
	 */
	@Test
	public void testInstalacionLimpiaGeneraSecretoYPermiteElSocketTls() throws Exception {

		final File appDir = this.folder.newFolder("app"); //$NON-NLS-1$
		final File keyStore = createKeyStore(appDir, LocalKeyStoreSecrets.LEGACY_PASSWORD);
		final File secretFile = LocalKeyStoreSecrets.getSecretFile(keyStore);
		assertFalse("El secreto no debe existir antes del primer arranque", secretFile.exists()); //$NON-NLS-1$

		final char[] password = LocalKeyStoreSecrets.resolvePassword(keyStore);
		try {
			assertTrue("Debe generarse el fichero de secreto", secretFile.isFile()); //$NON-NLS-1$
			assertEquals("El secreto debe ser Base64 de 32 bytes", SECRET_BYTES, //$NON-NLS-1$
				Base64.getDecoder().decode(new String(password)).length);
			assertEquals("El fichero de secreto debe contener el secreto", new String(password), //$NON-NLS-1$
				new String(Files.readAllBytes(secretFile.toPath()), StandardCharsets.US_ASCII));
			assertOwnerOnly(secretFile);

			// El almacen ya no abre con la contrasena heredada y si con el secreto nuevo
			assertFalse("El almacen migrado no debe abrir con la contrasena heredada", //$NON-NLS-1$
				canOpen(keyStore, LocalKeyStoreSecrets.LEGACY_PASSWORD.toCharArray()));
			assertTrue("El almacen migrado debe abrir con el secreto local", canOpen(keyStore, password)); //$NON-NLS-1$
			assertFalse("La copia .bak debe borrarse al terminar", //$NON-NLS-1$
				new File(appDir, KEYSTORE_NAME + ".bak").exists()); //$NON-NLS-1$

			assertTlsHandshake(keyStore, password);
		}
		finally {
			LocalKeyStoreSecrets.wipe(password);
		}
	}

	/** Migraci&oacute;n: un PFX preexistente con la contrase&ntilde;a vieja se migra y el socket levanta. */
	@Test
	public void testMigracionDePfxPreexistente() throws Exception {

		final File appDir = this.folder.newFolder("app"); //$NON-NLS-1$
		final File keyStore = createKeyStore(appDir, LocalKeyStoreSecrets.LEGACY_PASSWORD);

		final char[] password = LocalKeyStoreSecrets.resolvePassword(keyStore);
		try {
			assertTrue(canOpen(keyStore, password));
			assertTrue("Debe registrarse la migracion sin el secreto", //$NON-NLS-1$
				this.handler.contains(Level.INFO, "migrado")); //$NON-NLS-1$
			assertFalse(this.handler.containsSecret(new String(password)));
			assertTlsHandshake(keyStore, password);
		}
		finally {
			LocalKeyStoreSecrets.wipe(password);
		}
	}

	/**
	 * Reinstalaci&oacute;n: el instalador vuelve a dejar un PFX con la contrase&ntilde;a heredada
	 * pero el secreto local ya existe. Se reutiliza el secreto y se vuelve a migrar.
	 */
	@Test
	public void testReinstalacionDelAlmacenConSecretoExistente() throws Exception {

		final File appDir = this.folder.newFolder("app"); //$NON-NLS-1$
		final File keyStore = createKeyStore(appDir, LocalKeyStoreSecrets.LEGACY_PASSWORD);

		final char[] first = LocalKeyStoreSecrets.resolvePassword(keyStore);
		final String secret;
		try {
			secret = new String(first);
		}
		finally {
			LocalKeyStoreSecrets.wipe(first);
		}

		// El instalador sobrescribe el almacen con la contrasena heredada
		createKeyStore(appDir, LocalKeyStoreSecrets.LEGACY_PASSWORD);

		final char[] second = LocalKeyStoreSecrets.resolvePassword(keyStore);
		try {
			assertEquals("Debe reutilizarse el secreto existente", secret, new String(second)); //$NON-NLS-1$
			assertEquals(secret, new String(Files.readAllBytes(
				LocalKeyStoreSecrets.getSecretFile(keyStore).toPath()), StandardCharsets.US_ASCII));
			assertTrue(canOpen(keyStore, second));
			assertFalse(canOpen(keyStore, LocalKeyStoreSecrets.LEGACY_PASSWORD.toCharArray()));
		}
		finally {
			LocalKeyStoreSecrets.wipe(second);
		}
	}

	/** Un secreto con permisos abiertos se corrige a 0600 y queda registrado. */
	@Test
	public void testPermisosAbiertosSeCorrigenYSeRegistran() throws Exception {

		Assume.assumeTrue("Solo aplicable a sistemas POSIX", isPosix()); //$NON-NLS-1$

		final File appDir = this.folder.newFolder("app"); //$NON-NLS-1$
		final String secret = newSecret();
		final File keyStore = createKeyStore(appDir, secret);
		final File secretFile = LocalKeyStoreSecrets.getSecretFile(keyStore);
		Files.write(secretFile.toPath(), secret.getBytes(StandardCharsets.US_ASCII));
		// Permisos abiertos a proposito: es el caso que la aplicacion debe corregir
		Files.setPosixFilePermissions(secretFile.toPath(), EnumSet.of(
			PosixFilePermission.OWNER_READ,
			PosixFilePermission.OWNER_WRITE,
			PosixFilePermission.GROUP_READ,
			PosixFilePermission.OTHERS_READ
		));

		final char[] password = LocalKeyStoreSecrets.resolvePassword(keyStore);
		try {
			assertArrayEquals(secret.toCharArray(), password);
			assertOwnerOnly(secretFile);
			assertTrue("Debe registrarse la correccion de permisos", //$NON-NLS-1$
				this.handler.contains(Level.WARNING, "permisos")); //$NON-NLS-1$
			assertFalse(this.handler.containsSecret(secret));
		}
		finally {
			LocalKeyStoreSecrets.wipe(password);
		}
	}

	/** Un fichero de secreto corrupto da un error claro y no filtra su contenido. */
	@Test
	public void testFicheroDeSecretoCorrupto() throws Exception {

		final File appDir = this.folder.newFolder("app"); //$NON-NLS-1$
		final File keyStore = createKeyStore(appDir, LocalKeyStoreSecrets.LEGACY_PASSWORD);
		final File secretFile = LocalKeyStoreSecrets.getSecretFile(keyStore);

		final String corrupt = "esto-no-es-un-secreto"; //$NON-NLS-1$
		Files.write(secretFile.toPath(), corrupt.getBytes(StandardCharsets.US_ASCII));

		try {
			LocalKeyStoreSecrets.resolvePassword(keyStore);
			fail("Un secreto corrupto debe rechazarse"); //$NON-NLS-1$
		}
		catch (final IOException e) {
			assertTrue("El error debe explicar el problema", //$NON-NLS-1$
				e.getMessage().contains("no contiene un secreto Base64 de 32 bytes")); //$NON-NLS-1$
			assertFalse("El error no debe filtrar el contenido", e.getMessage().contains(corrupt)); //$NON-NLS-1$
		}

		// Base64 valido pero de longitud incorrecta: tambien debe rechazarse
		final String shortSecret = Base64.getEncoder().encodeToString(new byte[16]);
		Files.write(secretFile.toPath(), shortSecret.getBytes(StandardCharsets.US_ASCII));
		try {
			LocalKeyStoreSecrets.resolvePassword(keyStore);
			fail("Un secreto de longitud incorrecta debe rechazarse"); //$NON-NLS-1$
		}
		catch (final IOException e) {
			assertFalse(e.getMessage().contains(shortSecret));
		}
	}

	/**
	 * Instalaci&oacute;n por maquina sin permiso de escritura (Windows en Program Files): no se
	 * puede migrar, se mantiene la contrase&ntilde;a heredada y queda un aviso registrado.
	 */
	@Test
	public void testSinEscrituraMantieneLaContrasenaHeredada() throws Exception {

		Assume.assumeTrue("Solo aplicable a sistemas POSIX", isPosix()); //$NON-NLS-1$
		Assume.assumeFalse("No aplicable a root", "root".equals(System.getProperty("user.name"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		final File appDir = this.folder.newFolder("app"); //$NON-NLS-1$
		final File keyStore = createKeyStore(appDir, LocalKeyStoreSecrets.LEGACY_PASSWORD);
		setReadOnly(appDir);

		try {
			final char[] password = LocalKeyStoreSecrets.resolvePassword(keyStore);
			try {
				assertArrayEquals(LocalKeyStoreSecrets.LEGACY_PASSWORD.toCharArray(), password);
				assertTrue("Debe avisarse de que no se puede migrar", //$NON-NLS-1$
					this.handler.contains(Level.WARNING, "no se puede migrar")); //$NON-NLS-1$
				assertFalse(LocalKeyStoreSecrets.getSecretFile(keyStore).exists());
			}
			finally {
				LocalKeyStoreSecrets.wipe(password);
			}
		}
		finally {
			setWritable(appDir);
		}
	}

	/** El fichero de secreto debe colgar del directorio del almac&eacute;n. */
	@Test
	public void testRutaDelFicheroDeSecreto() {
		final File keyStore = new File(this.folder.getRoot(), "app/" + KEYSTORE_NAME); //$NON-NLS-1$
		assertEquals(
			new File(this.folder.getRoot(), "app/" + KEYSTORE_NAME + LocalKeyStoreSecrets.SECRET_FILE_SUFFIX).getAbsoluteFile(), //$NON-NLS-1$
			LocalKeyStoreSecrets.getSecretFile(keyStore)
		);
	}

	private static String newSecret() {
		final byte[] random = new byte[SECRET_BYTES];
		new SecureRandom().nextBytes(random);
		return Base64.getEncoder().encodeToString(random);
	}

	private static void assertOwnerOnly(final File file) throws IOException {
		if (!isPosix()) {
			return;
		}
		assertEquals(
			PosixFilePermissions.fromString("rw-------"), //$NON-NLS-1$
			Files.getPosixFilePermissions(file.toPath())
		);
	}

	private static boolean isPosix() {
		return File.separatorChar == '/';
	}

	private static boolean setReadOnly(final File dir) throws IOException {
		Files.setPosixFilePermissions(dir.toPath(), PosixFilePermissions.fromString("r-x------")); //$NON-NLS-1$
		return true;
	}

	private static void setWritable(final File dir) throws IOException {
		Files.setPosixFilePermissions(dir.toPath(), PosixFilePermissions.fromString("rwx------")); //$NON-NLS-1$
	}

	private static boolean canOpen(final File keyStore, final char[] password) throws Exception {
		try (InputStream is = new FileInputStream(keyStore)) {
			final KeyStore ks = KeyStore.getInstance(PKCS12);
			ks.load(is, password);
			return true;
		}
		catch (final Exception e) {
			return false;
		}
	}

	/** Crea un PKCS#12 sint&eacute;tico con una clave RSA y un certificado autofirmado. */
	private static File createKeyStore(final File dir, final String password) throws Exception {

		final KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA"); //$NON-NLS-1$
		keyPairGenerator.initialize(2048);
		final KeyPair keyPair = keyPairGenerator.generateKeyPair();

		final X500Name subject = new X500Name("CN=127.0.0.1, O=Autofirma, C=ES"); //$NON-NLS-1$
		final long now = System.currentTimeMillis();
		final JcaX509v3CertificateBuilder certificateBuilder = new JcaX509v3CertificateBuilder(
			subject,
			BigInteger.valueOf(now),
			new Date(now - 60000L),
			new Date(now + 86400000L),
			subject,
			keyPair.getPublic()
		);
		final ContentSigner signer = new JcaContentSignerBuilder("SHA256withRSA") //$NON-NLS-1$
			.setProvider(BouncyCastleProvider.PROVIDER_NAME)
			.build(keyPair.getPrivate());
		final X509Certificate certificate = new JcaX509CertificateConverter()
			.setProvider(BouncyCastleProvider.PROVIDER_NAME)
			.getCertificate(certificateBuilder.build(signer));

		final char[] pass = password.toCharArray();
		final KeyStore keyStore = KeyStore.getInstance(PKCS12);
		keyStore.load(null, null);
		keyStore.setKeyEntry(
			"ssl", //$NON-NLS-1$
			keyPair.getPrivate(),
			pass,
			new Certificate[] { certificate }
		);

		final File file = new File(dir, KEYSTORE_NAME);
		try (OutputStream os = new FileOutputStream(file)) {
			keyStore.store(os, pass);
		}
		finally {
			Arrays.fill(pass, '\0');
		}
		return file;
	}

	/** Levanta un socket TLS con el almac&eacute;n y completa el saludo cliente-servidor. */
	private static void assertTlsHandshake(final File keyStore, final char[] password) throws Exception {

		final KeyStore ks = KeyStore.getInstance(PKCS12);
		try (InputStream is = new FileInputStream(keyStore)) {
			ks.load(is, password);
		}

		final String alias = ks.aliases().nextElement();
		final Certificate cert = ks.getCertificate(alias);
		final PrivateKey key = (PrivateKey) ks.getKey(alias, password);
		assertNotNull(key);

		final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
		kmf.init(ks, password);
		final SSLContext serverContext = SSLContext.getInstance(TLS_1_2);
		serverContext.init(kmf.getKeyManagers(), null, null);

		final KeyStore trust = KeyStore.getInstance(PKCS12);
		trust.load(null, null);
		trust.setCertificateEntry("ssl", cert); //$NON-NLS-1$
		final TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
		tmf.init(trust);
		final SSLContext clientContext = SSLContext.getInstance(TLS_1_2);
		clientContext.init(null, tmf.getTrustManagers(), null);

		final ExecutorService executor = Executors.newSingleThreadExecutor();
		try (SSLServerSocket server = (SSLServerSocket) serverContext.getServerSocketFactory()
				.createServerSocket(0, 1, InetAddress.getLoopbackAddress())) {
			server.setEnabledProtocols(new String[] { TLS_1_2 });

			final Future<?> serverTask = executor.submit(() -> {
				try (SSLSocket socket = (SSLSocket) server.accept()) {
					socket.startHandshake();
					socket.getOutputStream().write(1);
					socket.getOutputStream().flush();
				}
				return null;
			});

			try (SSLSocket client = (SSLSocket) clientContext.getSocketFactory()
					.createSocket(InetAddress.getLoopbackAddress(), server.getLocalPort())) {
				client.setEnabledProtocols(new String[] { TLS_1_2 });
				client.startHandshake();
				assertEquals("El saludo debe completarse en TLS 1.2", TLS_1_2, client.getSession().getProtocol()); //$NON-NLS-1$
			}
			serverTask.get(30, TimeUnit.SECONDS);
		}
		finally {
			executor.shutdownNow();
		}
	}

	/** Recoge los avisos de la aplicaci&oacute;n para poder comprobarlos. */
	private static final class CapturingHandler extends Handler {

		private final List<LogRecord> records = new ArrayList<>();

		CapturingHandler() {
			super();
		}

		@Override
		public void publish(final LogRecord record) {
			this.records.add(record);
		}

		@Override
		public void flush() {
			// Sin buffer
		}

		@Override
		public void close() {
			// Sin recursos
		}

		boolean contains(final Level level, final String text) {
			for (final LogRecord record : this.records) {
				if (record.getLevel().intValue() >= level.intValue()
						&& record.getMessage() != null
						&& record.getMessage().contains(text)) {
					return true;
				}
			}
			return false;
		}

		boolean containsSecret(final String secret) {
			for (final LogRecord record : this.records) {
				if (record.getMessage() != null && record.getMessage().contains(secret)) {
					return true;
				}
				if (record.getThrown() != null && String.valueOf(record.getThrown()).contains(secret)) {
					return true;
				}
			}
			return false;
		}
	}
}
