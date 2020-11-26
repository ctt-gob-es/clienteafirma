package es.gob.afirma.plugin.hash.command;

import java.io.File;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.plugin.hash.HashUIHelper;
import es.gob.afirma.standalone.plugins.PluginControlledException;

/** pruebas de huellas digitales desde l&iacute;nea de comandos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class HashFromCommandLineTest {

	/** Prueba de la comprobaci&oacute;n de huellas digitales. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testHashCheck() {
		HashUIHelper.checkHashUI(new File("C:\\Users\\tomas\\AppData\\Local\\Temp\\sample-facturae.xml"), null); //$NON-NLS-1$
	}

	/** Prueba de comprobaci&oacute;n de huellas de directorio desde l&iacute;nea de comandos.
	 * @throws IllegalArgumentException Cuando hay un error en el comando de llamada. En este caso,
	 * el mensajede la excepci&oacute;n describir&aacute; el problema.
	 * @throws PluginControlledException Cuando ha ocurrido un error durante la ejecuci&oacute;n de
	 * la operaci&oacute;n. En este caso, el mensaje de la excepci&oacute;n se mostrar&aacute; al
	 * usuario. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testHashCheckDirectory() throws IllegalArgumentException, PluginControlledException {
		new CheckHashCommand().start(
				new String[] {
					"-i", "/Users/user/Desktop/AfirmaConfigurator.hashfiles"  //$NON-NLS-1$ //$NON-NLS-2$
				}
			);
	}

	/** Main para pruebas.
	 * @param args No se usa.
	 * @throws IllegalArgumentException Cuando hay un error en el comando de llamada. En este caso,
	 * el mensajede la excepci&oacute;n describir&aacute; el problema.
	 * @throws PluginControlledException Cuando ha ocurrido un error durante la ejecuci&oacute;n de
	 * la operaci&oacute;n. En este caso, el mensaje de la excepci&oacute;n se mostrar&aacute; al
	 * usuario. */
	public static void main(final String[] args) throws IllegalArgumentException, PluginControlledException {
		new CreateHashCommand().start(
			new String[] {
				"-i", "/Users/user/Desktop/"  //$NON-NLS-1$ //$NON-NLS-2$
			}
		);

	}

}
