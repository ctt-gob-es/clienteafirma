/* Copyright (C) 2026 [Soluciones Alexendros]
 * This file is part of "Cliente @Firma".
 */
package es.gob.afirma.core.misc.protocol;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

import org.junit.Assert;
import org.junit.Test;

/** Pruebas del registro de sesiones del canal local con reloj inyectable. */
public class WebSocketSessionValidatorTest {

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

	private static final String ID_A = "abcdefgh12345678";
	private static final String ID_B = "ijklmnop12345678";

	@Test
	public void validSessionOk() {
		final MutableClock clock = new MutableClock(1000000L);
		final WebSocketSessionValidator validator = new WebSocketSessionValidator(clock, 60000L);
		Assert.assertTrue(validator.register(ID_A));
		Assert.assertEquals(WebSocketSessionValidator.Status.OK, validator.validate(ID_A));
	}

	@Test
	public void noSessionRejected() {
		final WebSocketSessionValidator validator =
				new WebSocketSessionValidator(new MutableClock(0L), 60000L);
		validator.register(ID_A);
		Assert.assertEquals(WebSocketSessionValidator.Status.MISSING, validator.validate(null));
		Assert.assertEquals(WebSocketSessionValidator.Status.MISSING, validator.validate(""));
	}

	@Test
	public void otherSessionRejected() {
		final WebSocketSessionValidator validator =
				new WebSocketSessionValidator(new MutableClock(0L), 60000L);
		validator.register(ID_A);
		Assert.assertEquals(WebSocketSessionValidator.Status.UNKNOWN, validator.validate(ID_B));
	}

	@Test
	public void repeatedRejected() {
		final WebSocketSessionValidator validator =
				new WebSocketSessionValidator(new MutableClock(0L), 60000L);
		validator.register(ID_A);
		Assert.assertEquals(WebSocketSessionValidator.Status.OK, validator.validate(ID_A));
		validator.consume(ID_A);
		Assert.assertEquals(WebSocketSessionValidator.Status.REUSED, validator.validate(ID_A));
		Assert.assertTrue(validator.isConsumed(ID_A));
	}

	@Test
	public void concurrentUseRejected() {
		final WebSocketSessionValidator validator =
				new WebSocketSessionValidator(new MutableClock(0L), 60000L);
		validator.register(ID_A);
		validator.markInUse(ID_A);
		Assert.assertEquals(WebSocketSessionValidator.Status.REUSED, validator.validate(ID_A));
	}

	@Test
	public void expiredRejected() {
		final MutableClock clock = new MutableClock(5000L);
		final WebSocketSessionValidator validator = new WebSocketSessionValidator(clock, 2000L);
		validator.register(ID_A);
		Assert.assertEquals(WebSocketSessionValidator.Status.OK, validator.validate(ID_A));

		// Dentro del tiempo de vida: sigue siendo valida
		clock.advance(1500L);
		Assert.assertEquals(WebSocketSessionValidator.Status.OK, validator.validate(ID_A));

		// Pasado el tiempo de vida: caducada, y la entrada se purga
		clock.advance(1000L);
		Assert.assertEquals(WebSocketSessionValidator.Status.EXPIRED, validator.validate(ID_A));
		Assert.assertEquals(0, validator.activeSessions());
	}

	@Test
	public void malformedRejected() {
		final WebSocketSessionValidator validator =
				new WebSocketSessionValidator(new MutableClock(0L), 60000L);
		Assert.assertEquals(WebSocketSessionValidator.Status.MALFORMED, validator.validate("corto"));
		Assert.assertEquals(WebSocketSessionValidator.Status.MALFORMED, validator.validate("con espacios!!"));
	}

	@Test
	public void randomSessionIsGenerated() {
		final String id = WebSocketSessionValidator.generateSessionId();
		Assert.assertTrue(WebSocketSessionValidator.isValidSessionId(id));
		Assert.assertNotEquals(id, WebSocketSessionValidator.generateSessionId());
	}

	@Test
	public void memoryIsBounded() {
		final MutableClock clock = new MutableClock(0L);
		final WebSocketSessionValidator validator = new WebSocketSessionValidator(clock, 60000L);
		for (int i = 0; i < WebSocketSessionValidator.MAX_TRACKED_SESSIONS * 2; i++) {
			validator.register(String.format("session%08d", Integer.valueOf(i)));
		}
		Assert.assertTrue(validator.activeSessions() <= WebSocketSessionValidator.MAX_TRACKED_SESSIONS);

		// Las caducadas desaparecen del registro
		clock.advance(120000L);
		validator.purge();
		Assert.assertEquals(0, validator.activeSessions());
	}

	@Test
	public void originNotAllowedRejectedInStrictMode() {
		Assert.assertFalse(WebSocketOriginValidator.isAllowed("https://evil.example", "https://app.example", true));
		Assert.assertFalse(WebSocketOriginValidator.isAllowed("https://evil.example", "", true));
		Assert.assertFalse(WebSocketOriginValidator.isAllowed(null, "", true));
	}

	@Test
	public void originAllowedOkInStrictMode() {
		Assert.assertTrue(WebSocketOriginValidator.isAllowed("https://app.example", "https://app.example", true));
	}

	@Test
	public void permissiveModeAcceptsUnknownOrigin() {
		Assert.assertTrue(WebSocketOriginValidator.isAllowed("https://evil.example", "https://app.example", false));
	}

	@Test
	public void corsHeadersFollowMode() {
		Assert.assertEquals("*",
				WebSocketOriginValidator.allowOriginValue("https://evil.example", "https://app.example", false));
		Assert.assertNull(WebSocketOriginValidator.allowOriginValue("https://evil.example", "https://app.example", true));

		final String strict = WebSocketOriginValidator.corsHeaders("https://app.example", "https://app.example", true);
		Assert.assertTrue(strict.contains("Access-Control-Allow-Origin: https://app.example"));
		Assert.assertTrue(strict.contains("Vary: Origin"));

		final String permissive = WebSocketOriginValidator.corsHeaders("https://evil.example", "", false);
		Assert.assertTrue(permissive.contains("Access-Control-Allow-Origin: *"));
		Assert.assertFalse(permissive.contains("Vary: Origin"));
	}

	@Test
	public void ttlIsClamped() {
		Assert.assertEquals(WebSocketSessionValidator.MIN_TTL_MILLIS, WebSocketSessionValidator.clampTtl(-5L));
		Assert.assertEquals(WebSocketSessionValidator.MAX_TTL_MILLIS,
				WebSocketSessionValidator.clampTtl(Long.MAX_VALUE));
		Assert.assertEquals(45000L, WebSocketSessionValidator.clampTtl(45000L));
	}
}
