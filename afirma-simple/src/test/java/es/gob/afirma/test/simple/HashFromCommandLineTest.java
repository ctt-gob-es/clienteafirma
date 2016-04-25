package es.gob.afirma.test.simple;

import org.junit.Test;

import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.ui.hash.HashHelper;

/** pruebas de huellas digitales desde l&iacute;nea de comandos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class HashFromCommandLineTest {

	/** Prueba de la comprobaci&oacute;n de huellas digitales. */
	@SuppressWarnings("static-method")
	@Test
	public void testHashCheck() {
		HashHelper.checkHashUI("C:\\Users\\tomas\\AppData\\Local\\Temp\\sample-facturae.xml"); //$NON-NLS-1$
	}

	/** Prueba de comprobaci&oacute;n de huellas de directorio desde l&iacute;nea de comandos. */
	@SuppressWarnings("static-method")
	@Test
	public void testHashCheckDirectory() {
		SimpleAfirma.main(
				new String[] {
					"checkdigest", "-i", "/Users/user/Desktop/AfirmaConfigurator.hashfiles"  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
			);
	}

	/** Main para pruebas.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) {
		SimpleAfirma.main(
			new String[] {
				"createdigest", "-i", "/Users/user/Desktop/"  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
		);

	}

}
