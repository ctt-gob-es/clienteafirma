/* Copyright (C) 2026 [Soluciones Alexendros]
 * This file is part of "Cliente @Firma".
 */
package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import javax.swing.Timer;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.core.misc.protocol.WebSocketSessionValidator;
import es.gob.afirma.standalone.protocol.LocalChannelTestSupport.LogCapture;

/**
 * Pruebas de integracion del canal HTTP local: se levanta el servidor real y se ejecuta
 * el {@link CommandProcessorThread} de produccion.
 */
public class HttpChannelIntegrationTest {

	private static final String SESSION_ID = "session1234567890";
	private static final String OTHER_ID = "session0987654321";
	private static final String ALLOWED_ORIGIN = "https://app.example";

	/** Reloj de prueba que solo avanza cuando se le pide. */
	private static final class MutableClock extends Clock {
		private long millis;
		MutableClock(final long millis) {
			this.millis = millis;
		}
		void advance(final long delta) {
			this.millis += delta;
		}
		@Override
		public long millis() {
			return this.millis;
		}
		@Override
		public Instant instant() {
			return Instant.ofEpochMilli(this.millis);
		}
		@Override
		public ZoneOffset getZone() {
			return ZoneOffset.UTC;
		}
		@Override
		public Clock withZone(final java.time.ZoneId zone) {
			return this;
		}
	}

	private MutableClock clock;
	private WebSocketSessionValidator validator;
	private ServerSocket serverSocket;
	private Thread acceptThread;
	private Timer timer;

	@Before
	public void setUp() throws IOException {
		LocalChannelTestSupport.restoreConfiguration();
		this.clock = new MutableClock(1000000L);
		this.validator = new WebSocketSessionValidator(this.clock, 5000L);
		// nosemgrep: java.lang.security.audit.crypto.unencrypted-socket.unencrypted-socket -- canal local loopback bajo prueba, test
		this.serverSocket = new ServerSocket(0, 10, InetAddress.getByName("127.0.0.1")); //$NON-NLS-1$
		this.timer = new Timer(600000, evt -> { /* inactividad irrelevante en la prueba */ });
		this.timer.setRepeats(false);
		this.acceptThread = new Thread(() -> {
			while (!this.serverSocket.isClosed()) {
				try {
					final Socket socket = this.serverSocket.accept();
					new CommandProcessorThread(socket, this.timer, SESSION_ID,
							ProtocolVersion.getInstance(ProtocolVersion.VERSION_4), this.validator).start();
				}
				catch (final IOException e) {
					return;
				}
			}
		});
		this.acceptThread.setDaemon(true);
		this.acceptThread.start();
	}

	@After
	public void tearDown() throws IOException {
		this.timer.stop();
		this.serverSocket.close();
		LocalChannelTestSupport.restoreConfiguration();
	}

	private int port() {
		return this.serverSocket.getLocalPort();
	}

	private String send(final String origin, final String idSession) throws IOException {
		return LocalChannelTestSupport.sendRaw(port(), LocalChannelTestSupport.rawRequest(origin, idSession));
	}

	// --- Sesion -------------------------------------------------------------

	@Test
	public void validSessionAccepted() throws IOException {
		LocalChannelTestSupport.configure(false, ALLOWED_ORIGIN);
		Assert.assertTrue(this.validator.register(SESSION_ID));
		Assert.assertTrue(LocalChannelTestSupport.isOk(send(ALLOWED_ORIGIN, SESSION_ID)));
	}

	@Test
	public void missingSessionRejected() throws IOException {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		final String response = send(ALLOWED_ORIGIN, null);
		Assert.assertFalse(LocalChannelTestSupport.isOk(response));
		Assert.assertTrue(response.contains("HTTP/1.1 200"));
	}

	@Test
	public void otherSessionRejected() throws IOException {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		Assert.assertFalse(LocalChannelTestSupport.isOk(send(ALLOWED_ORIGIN, OTHER_ID)));
	}

	@Test
	public void repeatedSessionRejected() throws IOException {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		Assert.assertTrue(LocalChannelTestSupport.isOk(send(ALLOWED_ORIGIN, SESSION_ID)));

		// Cierre del canal: consume el identificador, como hace ServiceInvocationManager
		this.validator.consume(SESSION_ID);
		Assert.assertFalse(LocalChannelTestSupport.isOk(send(ALLOWED_ORIGIN, SESSION_ID)));
	}

	@Test
	public void expiredSessionRejected() throws IOException {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		Assert.assertTrue(LocalChannelTestSupport.isOk(send(ALLOWED_ORIGIN, SESSION_ID)));

		this.clock.advance(6000L);
		Assert.assertFalse(LocalChannelTestSupport.isOk(send(ALLOWED_ORIGIN, SESSION_ID)));
	}

	// --- Origen y CORS ------------------------------------------------------

	@Test
	public void allowedOriginAcceptedWithEchoHeader() throws IOException {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		final String response = send(ALLOWED_ORIGIN, SESSION_ID);
		Assert.assertTrue(LocalChannelTestSupport.isOk(response));
		Assert.assertEquals(ALLOWED_ORIGIN, LocalChannelTestSupport.header(response, "Access-Control-Allow-Origin")); //$NON-NLS-1$
		Assert.assertEquals("Origin", LocalChannelTestSupport.header(response, "Vary")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Test
	public void unknownOriginRejectedInStrictMode() throws IOException {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		final String response = send("https://evil.example", SESSION_ID); //$NON-NLS-1$
		Assert.assertFalse(LocalChannelTestSupport.isOk(response));
		Assert.assertNull(LocalChannelTestSupport.header(response, "Access-Control-Allow-Origin")); //$NON-NLS-1$
	}

	@Test
	public void permissiveModeKeepsOpenCorsAndAcceptsUnknownOrigin() throws IOException {
		LocalChannelTestSupport.configure(false, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		final String response = send("https://evil.example", SESSION_ID); //$NON-NLS-1$
		Assert.assertTrue(LocalChannelTestSupport.isOk(response));
		Assert.assertEquals("*", LocalChannelTestSupport.header(response, "Access-Control-Allow-Origin")); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertNull(LocalChannelTestSupport.header(response, "Vary")); //$NON-NLS-1$
	}

	@Test
	public void permissiveModeWarnsAboutUnknownOrigin() throws IOException {
		LocalChannelTestSupport.configure(false, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		final LogCapture capture = new LogCapture(Logger.getLogger("es.gob.afirma")); //$NON-NLS-1$
		try {
			send("https://evil.example", SESSION_ID); //$NON-NLS-1$
		}
		finally {
			Logger.getLogger("es.gob.afirma").removeHandler(capture); //$NON-NLS-1$
		}
		Assert.assertTrue(capture.contains("Origen del canal local no configurado")); //$NON-NLS-1$
	}

	// --- Fugas en los logs ---------------------------------------------------

	@Test
	public void noSessionIdentifierLeakedInLogs() throws IOException, InterruptedException {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		final LogCapture capture = new LogCapture(Logger.getLogger("es.gob.afirma")); //$NON-NLS-1$
		try {
			Assert.assertTrue(LocalChannelTestSupport.isOk(send(ALLOWED_ORIGIN, SESSION_ID)));
			send(ALLOWED_ORIGIN, OTHER_ID);
			final CountDownLatch settle = new CountDownLatch(1);
			settle.await(200, TimeUnit.MILLISECONDS);
		}
		finally {
			Logger.getLogger("es.gob.afirma").removeHandler(capture); //$NON-NLS-1$
		}
		Assert.assertFalse("El idsession no debe aparecer en los logs", capture.contains(SESSION_ID)); //$NON-NLS-1$
		Assert.assertFalse("El idsession no debe aparecer en los logs", capture.contains(OTHER_ID)); //$NON-NLS-1$
	}
}
