package es.gob.afirma.crypto.handwritten;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** Pruebas de tareas de firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestSignTask {

	/** Prueba una deserializaci&oacute;n desde un XML convertido a Base64.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDeserialBase64() throws Exception {
		final String xml = new String(AOUtil.getDataFromInputStream(TestSignTask.class.getResourceAsStream("/signTask.xml"))); //$NON-NLS-1$
		final String b64Xml = Base64.encode(xml.getBytes());
		System.out.println(
			SignTask.getInstance(b64Xml)
		);
	}

	/** Prueba una deserializaci&oacute;n desde un XML.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDeserialXml() throws Exception {
		final String xml = new String(AOUtil.getDataFromInputStream(TestSignTask.class.getResourceAsStream("/signTask.xml"))); //$NON-NLS-1$
		System.out.println(xml);
		System.out.println(
			SignTask.getInstance(xml)
		);
	}

}
