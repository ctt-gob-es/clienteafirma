/* Copyright (C) 2026 [Soluciones Alexendros]
 * This file is part of "Cliente @Firma".
 */
package es.gob.afirma.standalone.protocol;

import java.net.URI;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.misc.protocol.WebSocketSessionValidator;
import es.gob.afirma.standalone.protocol.LocalChannelTestSupport.LogCapture;

/**
 * Pruebas de integracion del websocket local: se levanta el servidor real
 * ({@link AfirmaWebSocketServerV4Sup}) y se conecta un cliente websocket real con el
 * encabezado <code>Origin</code>.
 */
public class WebSocketChannelIntegrationTest {

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

	/** Cliente websocket de prueba que recoge mensajes y cierres. */
	private static final class TestClient extends WebSocketClient {

		private final List<String> messages = new ArrayList<>();
		private final CountDownLatch messageLatch = new CountDownLatch(1);
		private final CountDownLatch closeLatch = new CountDownLatch(1);
		private volatile int closeCode = -1;

		TestClient(final int port, final String origin) {
			super(URI.create("ws://127.0.0.1:" + port)); //$NON-NLS-1$
			addHeader("Origin", origin); //$NON-NLS-1$
		}

		@Override
		public void onOpen(final ServerHandshake handshake) {
			// Sin accion
		}

		@Override
		public void onMessage(final String message) {
			synchronized (this.messages) {
				this.messages.add(message);
			}
			this.messageLatch.countDown();
		}

		@Override
		public void onClose(final int code, final String reason, final boolean remote) {
			this.closeCode = code;
			this.closeLatch.countDown();
			this.messageLatch.countDown();
		}

		@Override
		public void onError(final Exception ex) {
			this.messageLatch.countDown();
		}

		String awaitMessage() throws InterruptedException {
			this.messageLatch.await(10, TimeUnit.SECONDS);
			synchronized (this.messages) {
				return this.messages.isEmpty() ? null : this.messages.get(0);
			}
		}

		boolean awaitClose() throws InterruptedException {
			return this.closeLatch.await(10, TimeUnit.SECONDS);
		}

		int getCloseCode() {
			return this.closeCode;
		}
	}

	private MutableClock clock;
	private WebSocketSessionValidator validator;
	private LocalChannelTestSupport.NonHaltingServer server;
	private int port;

	@Before
	public void setUp() throws Exception {
		LocalChannelTestSupport.restoreConfiguration();
		this.clock = new MutableClock(1000000L);
		this.validator = new WebSocketSessionValidator(this.clock, 5000L);
		this.port = LocalChannelTestSupport.freePort();
		this.server = new LocalChannelTestSupport.NonHaltingServer(this.port, SESSION_ID, this.validator);
		this.server.start();
		waitForServer();
	}

	@After
	public void tearDown() throws Exception {
		this.server.stop(1000);
		LocalChannelTestSupport.restoreConfiguration();
	}

	private void waitForServer() throws Exception {
		final long limit = System.currentTimeMillis() + 10000;
		while (System.currentTimeMillis() < limit) {
			try (java.net.Socket ignored = new java.net.Socket("127.0.0.1", this.port)) { //$NON-NLS-1$
				return;
			}
			catch (final java.io.IOException e) {
				Thread.sleep(50L);
			}
		}
		throw new IllegalStateException("El servidor websocket no arranco a tiempo"); //$NON-NLS-1$
	}

	private TestClient connect(final String origin) throws Exception {
		final TestClient client = new TestClient(this.port, origin);
		Assert.assertTrue(client.connectBlocking(10, TimeUnit.SECONDS));
		return client;
	}

	private static String echo(final String idSession) {
		return idSession == null ? "echo=@EOF" : "echo=idsession=" + idSession + "@EOF"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	private String exchange(final String origin, final String idSession) throws Exception {
		final TestClient client = connect(origin);
		try {
			client.send(echo(idSession));
			return client.awaitMessage();
		}
		finally {
			client.close();
		}
	}

	// --- Sesion -------------------------------------------------------------

	@Test
	public void validSessionAccepted() throws Exception {
		LocalChannelTestSupport.configure(false, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		Assert.assertEquals("OK", exchange(ALLOWED_ORIGIN, SESSION_ID)); //$NON-NLS-1$
	}

	@Test
	public void missingSessionRejected() throws Exception {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		final String response = exchange(ALLOWED_ORIGIN, null);
		Assert.assertNotNull(response);
		Assert.assertNotEquals("OK", response); //$NON-NLS-1$
	}

	@Test
	public void otherSessionRejected() throws Exception {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		Assert.assertNotEquals("OK", exchange(ALLOWED_ORIGIN, OTHER_ID)); //$NON-NLS-1$
	}

	@Test
	public void repeatedSessionRejectedInSecondConnection() throws Exception {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);

		final TestClient first = connect(ALLOWED_ORIGIN);
		first.send(echo(SESSION_ID));
		Assert.assertEquals("OK", first.awaitMessage()); //$NON-NLS-1$
		first.close();
		Assert.assertTrue(first.awaitClose());

		// El cierre consume la sesion: una segunda conexion con el mismo id se rechaza
		Assert.assertNotEquals("OK", exchange(ALLOWED_ORIGIN, SESSION_ID)); //$NON-NLS-1$
	}

	@Test
	public void expiredSessionRejected() throws Exception {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		this.clock.advance(6000L);
		Assert.assertNotEquals("OK", exchange(ALLOWED_ORIGIN, SESSION_ID)); //$NON-NLS-1$
	}

	// --- Origen -------------------------------------------------------------

	@Test
	public void allowedOriginAccepted() throws Exception {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		Assert.assertEquals("OK", exchange(ALLOWED_ORIGIN, SESSION_ID)); //$NON-NLS-1$
	}

	@Test
	public void unknownOriginRejectedInStrictMode() throws Exception {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);

		final TestClient client = connect("https://evil.example"); //$NON-NLS-1$
		try {
			Assert.assertTrue("El servidor debe cerrar la conexion", client.awaitClose()); //$NON-NLS-1$
			Assert.assertEquals(1008, client.getCloseCode());
		}
		finally {
			client.close();
		}
	}

	@Test
	public void permissiveModeAcceptsUnknownOriginWithWarning() throws Exception {
		LocalChannelTestSupport.configure(false, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		final LogCapture capture = new LogCapture(Logger.getLogger("es.gob.afirma")); //$NON-NLS-1$
		try {
			Assert.assertEquals("OK", exchange("https://evil.example", SESSION_ID)); //$NON-NLS-1$ //$NON-NLS-2$
		}
		finally {
			Logger.getLogger("es.gob.afirma").removeHandler(capture); //$NON-NLS-1$
		}
		Assert.assertTrue(capture.contains("Origen del canal local no configurado")); //$NON-NLS-1$
	}

	// --- Fugas en los logs ---------------------------------------------------

	@Test
	public void noSessionIdentifierLeakedInLogs() throws Exception {
		LocalChannelTestSupport.configure(true, ALLOWED_ORIGIN);
		this.validator.register(SESSION_ID);
		this.validator.register(OTHER_ID);

		final LogCapture capture = new LogCapture(Logger.getLogger("es.gob.afirma")); //$NON-NLS-1$
		try {
			Assert.assertEquals("OK", exchange(ALLOWED_ORIGIN, SESSION_ID)); //$NON-NLS-1$
			Assert.assertNotEquals("OK", exchange(ALLOWED_ORIGIN, OTHER_ID)); //$NON-NLS-1$
		}
		finally {
			Logger.getLogger("es.gob.afirma").removeHandler(capture); //$NON-NLS-1$
		}
		Assert.assertFalse("El idsession no debe aparecer en los logs", capture.contains(SESSION_ID)); //$NON-NLS-1$
		Assert.assertFalse("El idsession no debe aparecer en los logs", capture.contains(OTHER_ID)); //$NON-NLS-1$
	}
}
