package es.gob.afirma.ui.core.jse;

import java.io.File;
import java.io.IOException;
import java.util.Collections;

import es.gob.afirma.core.ui.AOUIManager;
import es.gob.afirma.core.ui.GenericFileFilter;

/** Pruebas del di&aacute;logo de guardado.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SaveTest {

	/** Main para pruebas.
	 * @param args No se usa.
	 * @throws IOException En cualquier error. */
	public static void main(final String[] args) throws IOException {
		final AOUIManager m = new JSEUIManager();
		m.saveDataToFile(
			"HOLA".getBytes(), //$NON-NLS-1$
			"Prueba", //$NON-NLS-1$
			File.createTempFile("NNN", "OOO").getParent(), //$NON-NLS-1$ //$NON-NLS-2$
			"hola.txt", //$NON-NLS-1$
			Collections.singletonList(
				new GenericFileFilter(
					new String[] { ".TXT" }, //$NON-NLS-1$
					"Ficheros de texto (*.txt)" //$NON-NLS-1$
				)
			),
			null
		);
	}

}
