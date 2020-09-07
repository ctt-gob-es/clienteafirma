package es.gob.afirma.core.misc;

import java.io.InputStream;

import org.junit.Test;

import es.gob.afirma.core.signers.TriphaseData;

/** Pruebas de firmas trif&aacute;sicas.
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
public final class TestTriphaseSession {

	/** Prueba de an&aacute;lisis de XML de sesi&oacute;n trif&aacute;sica.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testTriphaseXmlParsing() throws Exception {
		final byte[] xml;
		try (
			final InputStream is = TestTriphaseSession.class.getResourceAsStream("/triphasesession.xml") //$NON-NLS-1$
		) {
			xml = AOUtil.getDataFromInputStream(
				is
			);
		}
		final TriphaseData td = TriphaseData.parser(xml);
		System.out.println(td);
	}

	/** Prueba de an&aacute;lisis de XML de sesi&oacute;n trif&aacute;sica con indicaci&oacute;n de formato.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testTriphaseXmlParsingWithFormat() throws Exception {
		final byte[] xml;
		try (
			final InputStream is = TestTriphaseSession.class.getResourceAsStream("/TriPhaseData.xml") //$NON-NLS-1$
		) {
			xml = AOUtil.getDataFromInputStream(
				is
			);
		}
		final TriphaseData td = TriphaseData.parser(xml);
		System.out.println(td);
	}

}
