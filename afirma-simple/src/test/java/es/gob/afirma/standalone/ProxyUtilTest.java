package es.gob.afirma.standalone;

import java.net.ProxySelector;

import org.junit.Test;

/** Pruebas de detección de proxy sin conexiones de red. */
public final class ProxyUtilTest {

	/** La detección del sistema debe completarse sin ini4j, haya proxy o no. */
	@Test
	public void detectaProxyDelSistemaSinIni4j() {
		final ProxySelector original = ProxySelector.getDefault();
		try {
			ProxyUtil.setProxySettings();
		}
		finally {
			ProxySelector.setDefault(original);
		}
	}
}
