package es.gob.afirma.plugin.hash;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;

/** Pruebas de generaci&oacute;n de huellas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestHash {

	/** Prueba de generaci&oacute;n de huellas de directorio con GUI.
	 * @param args No se usa. */
	public static void main(final String[] args) {
		try {
			CreateHashDirDialog.doHashProcess(
				null,
				new File("c://temp"), //$NON-NLS-1$
				null,
				null,
				"SHA-512", //$NON-NLS-1$
				true
			);
		}
		catch (final AOCancelledOperationException e) {
			Logger.getLogger("es.gob.afirma").info("Operacion cancelada"); //$NON-NLS-1$//$NON-NLS-2$
		}
		catch (final Exception e) {
			AOUIFactory.showErrorMessage(
				Messages.getString("CreateHashDialog.13"), //$NON-NLS-1$
				Messages.getString("CreateHashDialog.14"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				e
			);
			Logger.getLogger("es.gob.afirma").log( //$NON-NLS-1$
				Level.SEVERE, "Error generando o guardando la huella digital", e//$NON-NLS-1$
			);
		}
	}

}
