package es.gob.afirma.standalone.ws;

import javax.swing.JOptionPane;

import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.configurator.AutoFirmaConfiguratorJNLP;

/**
 * Aplicacion WebStart encargada de gestionar la instalaci&oacute;n
 * de AutoFirma y su ejecuci&oacute;n.
 */
public class AutoFirmaProtocolWebStart {

	/**
	 * Inicia la carga JNLP de AutoFirma en modo invocaci&oacute;n por protocolo.
	 * @param args Argumentos de la operaci&oacute;n que se debe ejecutar.
	 */
	public static void main(final String[] args) {

		// Nos aseguramos de que la configuracion es correcta
		try {
			AutoFirmaConfiguratorJNLP.configure(args);
		} catch (final Exception e) {
			JOptionPane.showMessageDialog(
					null,
					"No se pudo habilitar la comunicacion entre la p\u00E1gina y AutoFirma", //$NON-NLS-1$
					"Error", //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE);
			return;
		}

		// Ejecutamos la operacion con AutoFirma (desactivando las actualizaciones)
		SimpleAfirma.setUpdatesEnabled(false);
		SimpleAfirma.main(args);
	}
}
