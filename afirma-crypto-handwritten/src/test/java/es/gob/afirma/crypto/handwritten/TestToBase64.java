package es.gob.afirma.crypto.handwritten;

import java.io.IOException;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.TestBase64;

/** Pruebas de conversi&oacute;n a Base64. */
public class TestToBase64 {

	/** Prueba de conversi&oacute;n a Base64.
	 * @throws IOException Si hay errores en el tratamiento de datos. */
	@SuppressWarnings("static-method")
	@Test
	public void TestToBase64Conversion() throws IOException {

		final byte[] data = AOUtil.getDataFromInputStream(TestBase64.class.getResourceAsStream("ANF.pfx")); //$NON-NLS-1$

		System.out.println("DATOS en byte[]: " + data); //$NON-NLS-1$
		final String tmp = Base64.encode(data);
		System.out.println("Datos: " + tmp); //$NON-NLS-1$
	}
}
