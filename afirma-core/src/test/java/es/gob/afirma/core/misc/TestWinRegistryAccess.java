package es.gob.afirma.core.misc;

import org.junit.Test;

import es.gob.afirma.core.util.windows.WinRegistryWrapper;

/** prueba de acceso a registro de Windows. */
public class TestWinRegistryAccess {

	/** prueba de recuperaci&oacute;n de cadena de texto desde registro de Windows.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	public void getRegistryString() throws Exception {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			System.out.println(
				WinRegistryWrapper.getString(
					WinRegistryWrapper.HKEY_LOCAL_MACHINE,
	                "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion", //$NON-NLS-1$
					"SystemRoot" //$NON-NLS-1$
	            )
	        );
		}
	}
}
