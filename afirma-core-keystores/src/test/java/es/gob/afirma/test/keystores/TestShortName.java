package es.gob.afirma.test.keystores;

import es.gob.afirma.keystores.KeyStoreUtilities;


/** Prueba de obtenci&oacute;n de nombre corto en Windows. */
public final class TestShortName {

	/** Main.
	 * @param args No se usa. */
	public static void main(final String[] args) {
		System.out.println(KeyStoreUtilities.getWindowsShortName("C:\\Program Files\\Nightly\\fnmt")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getWindowsShortName("C:")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getWindowsShortName("C:\\")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getWindowsShortName("lala")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getWindowsShortName("c:\\Users\\tomas")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getWindowsShortName("moricons.dll")); //$NON-NLS-1$
	}

}
