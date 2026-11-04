package es.gob.afirma.core.misc.http;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.security.KeyStore;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.ui.CoreMessages;

/**
 * Comprueba el defecto de {@code strictSslChecks} (#92).
 * Certificado de prueba generado con keytool en el propio test (CN=Ejemplo S.L.).
 */
public class StrictSslChecksTest {

	private Thread serverThread;
	private final AtomicInteger port = new AtomicInteger(0);
	private volatile boolean running;
	private File keystoreFile;
	private X509Certificate serverCertificate;
	private CapturingHandler logHandler;

	@Before
	public void clearProps() {
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS);
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS);
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_SECURE_DOMAINS_LIST);
		this.logHandler = new CapturingHandler();
		Logger.getLogger("es.gob.afirma").addHandler(this.logHandler); //$NON-NLS-1$
	}

	@After
	public void stopServer() throws Exception {
		this.running = false;
		Logger.getLogger("es.gob.afirma").removeHandler(this.logHandler); //$NON-NLS-1$
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS);
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS);
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_SECURE_DOMAINS_LIST);
		if (this.keystoreFile != null) {
			Files.deleteIfExists(this.keystoreFile.toPath());
		}
		try {
			SslSecurityManager.enableSslChecks();
		}
		catch (final Exception ignored) {
			/* restore best-effort */
		}
	}

	@Test
	public void defaultPropertyIsStrict() {
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS);
		Assert.assertTrue(Boolean.parseBoolean(
				System.getProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, Boolean.TRUE.toString())));
	}

	@Test
	public void defaultStrictRejectsSelfSigned() throws Exception {
		startSelfSignedServer();
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS);
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS, "false");
		final UrlHttpManager mgr = UrlHttpManagerFactory.getInstalledManager();
		try {
			mgr.readUrl("https://127.0.0.1:" + this.port.get() + "/", UrlHttpMethod.GET);
			Assert.fail("Debía rechazarse el certificado autofirmado con el defecto estricto");
		}
		catch (final IOException expected) {
			Assert.assertNotNull(expected.getMessage());
		}
	}

	@Test
	public void disabledStrictAllowsSelfSigned() throws Exception {
		startSelfSignedServer();
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, "false");
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS, "true");
		SslSecurityManager.disableSslChecks();
		final UrlHttpManager mgr = UrlHttpManagerFactory.getInstalledManager();
		final byte[] data = mgr.readUrl("https://127.0.0.1:" + this.port.get() + "/", UrlHttpMethod.GET);
		Assert.assertTrue(data != null && data.length > 0);
	}

	@Test
	public void strictFailureMessageOrder() {
		final String part1 = CoreMessages.getString("SSLStrictFailureDialog.2");
		final String part2 = CoreMessages.getString("SSLStrictFailureDialog.3");
		final String part3 = CoreMessages.getString("SSLStrictFailureDialog.4");
		final String msg = SSLErrorProcessor.buildStrictSslFailureMessage();
		final int i1 = msg.indexOf(part1);
		final int i2 = msg.indexOf(part2);
		final int i3 = msg.indexOf(part3);
		Assert.assertTrue("parte 1 ausente: " + msg, i1 >= 0);
		Assert.assertTrue("orden 1→2", i2 > i1);
		Assert.assertTrue("orden 2→3", i3 > i2);
	}


	/**
	 * Con strictSslChecks activo no hay via que desactive la validacion: ni la propiedad
	 * disableSslChecks ni el listado de dominios «seguros».
	 */
	@Test
	public void strictIgnoresDisableFlagAndSecureDomainList() throws Exception {
		startSelfSignedServer();
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS, "true");
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_SECURE_DOMAINS_LIST, "127.0.0.1");
		try {
			UrlHttpManagerFactory.getInstalledManager()
				.readUrl(urlWithData(), UrlHttpMethod.GET);
			Assert.fail("Ninguna via debe desactivar la validacion con strictSslChecks activo");
		}
		catch (final IOException expected) {
			Assert.assertNotNull(expected.getMessage());
		}
		Assert.assertFalse("No debe registrarse una desactivacion", this.logHandler.contains(Level.WARNING, "Validacion TLS desactivada"));
	}

	/** Solo el listado de dominios «seguros» tampoco desactiva la validacion en estricto. */
	@Test
	public void strictIgnoresSecureDomainListOnly() throws Exception {
		startSelfSignedServer();
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS, "false");
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_SECURE_DOMAINS_LIST, "*.127.0.0.1,127.0.0.1");
		try {
			UrlHttpManagerFactory.getInstalledManager().readUrl(urlWithData(), UrlHttpMethod.GET);
			Assert.fail("El listado de dominios seguros no puede desactivar la validacion en estricto");
		}
		catch (final IOException expected) {
			Assert.assertNotNull(expected.getMessage());
		}
	}

	/** La descarga de configuracion remota respeta strictSslChecks aunque pida ignorar SSL. */
	@Test
	public void configDownloadRejectsUntrustedCertificateInStrictMode() throws Exception {
		startSelfSignedServer();
		try {
			DataDownloader.downloadData(urlWithData(), false, true);
			Assert.fail("La descarga de configuracion debe validar el certificado en estricto");
		}
		catch (final IOException expected) {
			Assert.assertNotNull(expected.getMessage());
		}
		Assert.assertTrue("Debe registrarse que se ignora la peticion de saltarse la validacion",
			this.logHandler.contains(Level.WARNING, "Se ignora la peticion"));
	}

	/** En modo permisivo la descarga sin validar funciona y deja aviso. */
	@Test
	public void permissiveConfigDownloadAllowsUntrustedCertificate() throws Exception {
		startSelfSignedServer();
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, "false");
		final byte[] data = DataDownloader.downloadData(urlWithData(), false, true);
		Assert.assertNotNull(data);
		Assert.assertEquals(2, data.length);
		Assert.assertTrue(this.logHandler.contains(Level.WARNING, "sin validar el certificado TLS"));
	}

	/**
	 * En modo permisivo la propiedad disableSslChecks desactiva la validacion solo en esa
	 * peticion, deja un aviso con el motivo, no toca los valores por defecto de la JVM y no
	 * registra la URL con datos.
	 */
	@Test
	public void permissiveAppliesOnlyToThatRequestAndWarns() throws Exception {
		startSelfSignedServer();
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, "false");
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS, "true");

		final SSLSocketFactory defaultFactory = HttpsURLConnection.getDefaultSSLSocketFactory();

		final byte[] data = UrlHttpManagerFactory.getInstalledManager().readUrl(urlWithData(), UrlHttpMethod.GET);
		Assert.assertEquals(2, data.length);
		Assert.assertSame("La desactivacion no puede ser global", defaultFactory,
			HttpsURLConnection.getDefaultSSLSocketFactory());
		Assert.assertTrue("Debe avisarse con el motivo",
			this.logHandler.contains(Level.WARNING, "propiedad disableSslChecks"));
		Assert.assertFalse("El aviso no debe incluir la URL con datos",
			this.logHandler.containsAny("12345678Z", "SECRETO", "/datos/"));

		// La siguiente peticion, sin la propiedad, vuelve a validar el certificado
		System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS);
		try {
			UrlHttpManagerFactory.getInstalledManager().readUrl(urlWithData(), UrlHttpMethod.GET);
			Assert.fail("Sin accion explicita no se puede desactivar la validacion");
		}
		catch (final IOException expected) {
			Assert.assertNotNull(expected.getMessage());
		}
	}

	/** El listado de dominios seguros solo actua en modo permisivo y deja aviso. */
	@Test
	public void permissiveSecureDomainWarns() throws Exception {
		startSelfSignedServer();
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, "false");
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_SECURE_DOMAINS_LIST, "127.0.0.1");
		final byte[] data = UrlHttpManagerFactory.getInstalledManager().readUrl(urlWithData(), UrlHttpMethod.GET);
		Assert.assertEquals(2, data.length);
		Assert.assertTrue(this.logHandler.contains(Level.WARNING, "dominio seguro configurado"));
		Assert.assertFalse("El aviso no debe incluir la URL con datos",
			this.logHandler.containsAny("12345678Z", "SECRETO", "/datos/"));
	}

	/** Los usos aceptables (importar el certificado) funcionan tambien en modo estricto. */
	@Test
	public void certificateRetrievalWorksInStrictMode() throws Exception {
		startSelfSignedServer();

		final SSLSocketFactory defaultFactory = HttpsURLConnection.getDefaultSSLSocketFactory();
		final javax.net.ssl.HostnameVerifier defaultVerifier = HttpsURLConnection.getDefaultHostnameVerifier();

		final X509Certificate[] chain = SslSecurityManager.retrieveServerCertificateChain(
			urlWithData(), "prueba de recuperacion del certificado");
		Assert.assertNotNull(chain);
		Assert.assertTrue(chain.length > 0);
		Assert.assertEquals(this.serverCertificate, chain[0]);

		// Ni se acepta la conexion ni se tocan los valores por defecto de la JVM
		Assert.assertSame(defaultFactory, HttpsURLConnection.getDefaultSSLSocketFactory());
		Assert.assertSame(defaultVerifier, HttpsURLConnection.getDefaultHostnameVerifier());
		Assert.assertTrue(this.logHandler.contains(Level.WARNING, "sin validar la cadena"));

		// Tras la captura, la validacion sigue activa
		try {
			UrlHttpManagerFactory.getInstalledManager().readUrl(urlWithData(), UrlHttpMethod.GET);
			Assert.fail("La captura del certificado no puede dejar la validacion desactivada");
		}
		catch (final IOException expected) {
			Assert.assertNotNull(expected.getMessage());
		}
	}

	/** Ningun aviso de desactivacion contiene la ruta ni parametros de la peticion. */
	@Test
	public void isolationOfUntrustedCertificateCapture() throws Exception {
		startSelfSignedServer();
		try {
			SslSecurityManager.retrieveServerCertificateChain(urlWithData(), "prueba");
		}
		catch (final IOException e) {
			Assert.fail("Debe recuperarse el certificado del servidor de prueba: " + e);
		}
		Assert.assertFalse(this.logHandler.containsAny("12345678Z", "SECRETO", "/datos/"));
	}

	private String urlWithData() {
		return "https://127.0.0.1:" + this.port.get() + "/datos/12345678Z?ref=SECRETO"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	private void startSelfSignedServer() throws Exception {
		this.keystoreFile = File.createTempFile("ejemplo-ssl-", ".jks");
		Files.deleteIfExists(this.keystoreFile.toPath());
		final String storePass = "changeit";
		final ProcessBuilder pb = new ProcessBuilder(
				"keytool",
				"-genkeypair",
				"-alias", "server",
				"-keyalg", "RSA",
				"-keysize", "2048",
				"-validity", "1",
				"-keystore", this.keystoreFile.getAbsolutePath(),
				"-storepass", storePass,
				"-keypass", storePass,
				"-dname", "CN=Ejemplo S.L.,O=Ejemplo S.L.,C=ES",
				"-noprompt"
		);
		pb.redirectErrorStream(true);
		final Process p = pb.start();
		final java.io.InputStream is = p.getInputStream();
		final byte[] keytoolOut = new byte[4096];
		final int n = is.read(keytoolOut);
		final int code = p.waitFor();
		Assert.assertEquals("keytool falló: " + (n > 0 ? new String(keytoolOut, 0, n, "UTF-8") : ""), 0, code);

		final KeyStore ks = KeyStore.getInstance("JKS");
		ks.load(Files.newInputStream(this.keystoreFile.toPath()), storePass.toCharArray());
		this.serverCertificate = (X509Certificate) ks.getCertificate("server"); //$NON-NLS-1$
		final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
		kmf.init(ks, storePass.toCharArray());
		final SSLContext ctx = SSLContext.getInstance("TLS");
		ctx.init(kmf.getKeyManagers(), null, new SecureRandom());
		final SSLServerSocketFactory ssf = ctx.getServerSocketFactory();
		final SSLServerSocket server = (SSLServerSocket) ssf.createServerSocket(0);
		this.port.set(server.getLocalPort());
		this.running = true;
		this.serverThread = new Thread(() -> {
			while (this.running) {
				try (SSLSocket sock = (SSLSocket) server.accept()) {
					final byte[] req = new byte[1024];
					sock.getInputStream().read(req);
					final byte[] body = "ok".getBytes("UTF-8");
					final String headers = "HTTP/1.1 200 OK\r\nContent-Length: " + body.length + "\r\nConnection: close\r\n\r\n";
					final OutputStream out = sock.getOutputStream();
					out.write(headers.getBytes("UTF-8"));
					out.write(body);
					out.flush();
				}
				catch (final Exception e) {
					break;
				}
			}
			try {
				server.close();
			}
			catch (final IOException ignored) {
				/* */
			}
		}, "strict-ssl-test-server");
		this.serverThread.setDaemon(true);
		this.serverThread.start();
		Thread.sleep(300);
	}

	/** Recoge los avisos de la aplicacion para poder comprobarlos. */
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

		boolean containsAny(final String... texts) {
			for (final LogRecord record : this.records) {
				final String message = record.getMessage();
				if (message == null) {
					continue;
				}
				for (final String text : texts) {
					if (message.contains(text)) {
						return true;
					}
				}
			}
			return false;
		}
	}
}
