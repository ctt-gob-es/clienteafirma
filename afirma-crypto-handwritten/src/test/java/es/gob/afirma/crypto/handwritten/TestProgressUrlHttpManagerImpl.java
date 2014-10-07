package es.gob.afirma.crypto.handwritten;

import java.awt.Frame;

import org.junit.Test;

/** Clase para probar la descarga de documentos PDF.
 * @author Astrid Idoate **/
public class TestProgressUrlHttpManagerImpl {

	/** Prueba simple de descarga de PDF.*/
	@SuppressWarnings("static-method")
	@Test
	public void testSimpleProgressUrlHttpManager () {
		try {
			ProgressUrlHttpManagerImpl puhmi = new ProgressUrlHttpManagerImpl(new Frame());
			puhmi.readUrlByGet(
				"http://technology.nasa.gov/NASA_Software_Catalog_2014.pdf"//$NON-NLS-1$
			);
		} catch (Exception e) {
			e.printStackTrace();
		}
		for(;;) {
			// Vacio
		}
	}
}
