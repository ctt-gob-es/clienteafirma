package es.gob.afirma.crypto.handwritten;

import java.util.logging.Logger;

import org.junit.Test;

import com.sun.org.apache.xml.internal.security.utils.Base64;

import es.gob.afirma.core.misc.AOUtil;

/** Pruebas de tarea de firma.
 * @author Astrid Idoate. */
public class TestBioSignerRunner {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Prueba para realizar varias firmas
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testBioSignerRunner() {

			String xml;
			try {
				xml = new String(AOUtil.getDataFromInputStream(TestSignTask.class.getResourceAsStream("/signTask.xml"))); //$NON-NLS-1$

				new BioSignerRunner(xml, null).show();
			}
			catch (Exception e) {
				e.printStackTrace();
			}
			for(;;) {
				//
			}

	}

	/** Main.
	 * @param args No se usa. */
	public static void main(final String[] args) {


		String xml;
		try {
			xml = new String(AOUtil.getDataFromInputStream(TestSignTask.class.getResourceAsStream("/signTask.xml"))); //$NON-NLS-1$
			LOGGER.info("XML en base64" + Base64.encode(xml.getBytes()));
			new BioSignerRunner(xml, null).show();
		}
		catch (Exception e) {
			e.printStackTrace();
		}



	}
}
