package es.gob.afirma.test.keystores;

import es.gob.afirma.keystores.KeyStoreUtilities;


/** Prueba de obtenci&oaucute;n de nombre corto en Windows. */
public final class TestShortName {

	/** Main.
	 * @param args No se usa. */
	public static void main(final String[] args) {
		System.out.println(KeyStoreUtilities.getShort("C:\\Program Files\\Nightly\\fnmt")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getShort("C:")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getShort("C:\\")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getShort("lala")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getShort("c:\\Users\\tomas")); //$NON-NLS-1$
		System.out.println(KeyStoreUtilities.getShort("moricons.dll")); //$NON-NLS-1$
	}

}
