package es.gob.afirma.ui.core.jse;

import java.io.IOException;

import es.gob.afirma.core.ui.AOUIFactory;

/** Pruebas de di&aacute;logos sin JUnit.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class FileDialogsTest {

	/** Prueba de selecci&oacute;n simple y m&uacute;ltiples de fichero.
	 * @param args
	 * @throws IOException */
	public static void main(final String[] args) throws IOException {
		for (final String s : AOUIFactory.getLoadFileName("Selecciona", new String[] {"txt"}, "Ficheros de texto", true, null)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			System.out.println(s);
		}
		for (final String s : AOUIFactory.getLoadFileName("Selecciona", new String[] {"txt"}, "Ficheros de texto", false, null)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			System.out.println(s);
		}
		System.out.println(AOUIFactory.getSaveDataToFile(
			"Hola".getBytes(), //$NON-NLS-1$
			"Titulo del dialogo", //$NON-NLS-1$
			null,
			new String[] { "txt" }, //$NON-NLS-1$
			"Ficheros de texto", //$NON-NLS-1$
			null
		));
	}
}

