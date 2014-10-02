package es.gob.afirma.crypto.handwritten;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;

/** Pruebas de tarea de firma.
 * @author Astrid Idoate. */
public class TestBioSignerRunner {


	/** Prueba para realizar varias firmas
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testBioSignerRunner() {

			String xml;
			try {
				xml = new String(AOUtil.getDataFromInputStream(TestSignTask.class.getResourceAsStream("/signTask.xml"))); //$NON-NLS-1$
				new BioSignerRunner(xml).show();
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
			new BioSignerRunner(xml).show();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
}
