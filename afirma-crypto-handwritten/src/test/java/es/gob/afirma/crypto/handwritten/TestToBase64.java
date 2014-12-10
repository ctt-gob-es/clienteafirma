package es.gob.afirma.crypto.handwritten;

import java.io.IOException;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.TestBase64;

public class TestToBase64 {

	@Test
	public void TestToBase64() throws IOException {

		final byte[] data = AOUtil.getDataFromInputStream(TestBase64.class.getResourceAsStream("ANF.pfx")); //$NON-NLS-1$

		System.out.println("DATOS en byte[]: " + data);
		String tmp = Base64.encode(data);
		System.out.println("Datos: " + tmp);
	}
}
