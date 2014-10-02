package es.gob.afirma.crypto.handwritten;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;

/** Pruebas de pie de firma.
 * @author Astrid Idoate. */
public class TestSignatureFooter {

	/** Prueba simple de adici&oacute;n de pie.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSimpleFooter() throws Exception {
		byte[] jpg = AOUtil.getDataFromInputStream(JseUtil.class.getResourceAsStream("/IMG_5699243969525973523.jpg")); //$NON-NLS-1$
		OutputStream os = new FileOutputStream(File.createTempFile("MODDD", ".jpg")); //$NON-NLS-1$ //$NON-NLS-2$
		os.write(JseUtil.addFooter(jpg, "Astrid Idoate Gil (12345678Z)")); //$NON-NLS-1$
		os.flush();
		os.close();
	}

}
