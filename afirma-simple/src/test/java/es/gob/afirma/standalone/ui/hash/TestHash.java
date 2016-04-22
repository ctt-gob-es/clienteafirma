package es.gob.afirma.standalone.ui.hash;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import org.junit.Test;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Pruebas de generaci&oacute;n de huellas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestHash {

	/** Prueba de generaci&oacute;n de huellas de directorio.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDirHashGeneration() throws Exception {
		System.out.println(
			CreateHashFiles.getHashReport("c://temp", true, "SHA-512") //$NON-NLS-1$ //$NON-NLS-2$
		);
	}

	/** Prueba de generaci&oacute;n de huellas de directorio con GUI.
	 * @param args No se usa.
	 * @throws Exception en cualquier error. */
	public static void main(final String[] args) {
		try {
			CreateHashFiles.doHashProcess(
				null,
				"c://temp", //$NON-NLS-1$
				"SHA-512", //$NON-NLS-1$
				true
			);
		}
		catch (final AOCancelledOperationException e) {
			Logger.getLogger("es.gob.afirma").info("Operacion cancelada"); //$NON-NLS-1$//$NON-NLS-2$
		}
		catch (final Exception e) {
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("CreateHashDialog.13"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			Logger.getLogger("es.gob.afirma").log( //$NON-NLS-1$
				Level.SEVERE, "Error generando o guardando la huella digital", e//$NON-NLS-1$
			);
		}
	}

}
