package es.gob.afirma.standalone.protocol;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

/**
 * Comprueba que el canal local no admite protocolos TLS obsoletos (ASVS 4.0.3 V9.1.3).
 * Tarea T3 de la OT Fase 2 · Seguridad ASVS L2.
 */
public class SecureSocketProtocolTest {

	private static final String TLS_1_2 = "TLSv1.2";

	/** Los protocolos habilitados deben ser TLS 1.2. */
	@Test
	public void testEnabledProtocolsAreTls12() {
		assertArrayEquals(new String[] { TLS_1_2 }, SecureSocketUtils.getEnabledProtocols());
	}

	/** No se debe devolver el array interno, para que nadie pueda alterarlo. */
	@Test
	public void testEnabledProtocolsAreACopy() {
		final String[] first = SecureSocketUtils.getEnabledProtocols();
		first[0] = "SSLv3";
		assertArrayEquals(new String[] { TLS_1_2 }, SecureSocketUtils.getEnabledProtocols());
	}

	/** Ni TLS 1.0 ni TLS 1.1 ni SSLv3 deben figurar como protocolo habilitado. */
	@Test
	public void testLegacyProtocolsAreNotEnabled() {
		for (final String protocol : SecureSocketUtils.getEnabledProtocols()) {
			assertFalse("SSLv3", "SSLv3".equalsIgnoreCase(protocol)); //$NON-NLS-1$ //$NON-NLS-2$
			assertFalse("TLSv1", "TLSv1".equalsIgnoreCase(protocol)); //$NON-NLS-1$ //$NON-NLS-2$
			assertFalse("TLSv1.1", "TLSv1.1".equalsIgnoreCase(protocol)); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}
}
