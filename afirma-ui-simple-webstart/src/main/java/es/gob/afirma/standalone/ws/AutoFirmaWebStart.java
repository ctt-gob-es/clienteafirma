package es.gob.afirma.standalone.ws;

import java.io.File;

import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.VisorFirma;

/**
 * Aplicacion WebStart encargada de gestionar la instalaci&oacute;n
 * de AutoFirma y su ejecuci&oacute;n.
 */
public class AutoFirmaWebStart {

	/** Comando usado para la apertura de ficheros. */
	private static final String PARAM_OPEN   = "-open"; //$NON-NLS-1$

	/**
	 * Inicia la carga JNLP de AutoFirma para su ejecuci&oacute;n como aplicaci&oacute;n de escritorio.
	 * @param args Argumentos de la operaci&oacute;n que se debe ejecutar.
	 */
	public static void main(final String[] args) {


		// Se ha hecho doble-clic sobre un fichero asociado a AutoFirma
		if (args != null && args.length == 2 && PARAM_OPEN.equalsIgnoreCase(args[0])) {
			openFile(args);
		}
		// Uso corriente de AutoFirma
		else {
			openApp(args);
		}
	}

	/**
	 * Abrimos AutoFirma para mostrar los datos de un fichero de firma.
	 * @param args Argumentos de la operaci&oacute;n.
	 */
	private static void openFile(final String[] args) {
		new VisorFirma(true, null).initialize(false, new File(args[1]));
	}

	/**
	 * Abrimos AutoFirma normalmente. Se usar&aacute; como aplicaci&oacute;n de
	 * escritorio, por consola o protocolo seg&uacute;n los par&aacute;metros
	 * proporcionados).
	 * @param args Argumentos de la operaci&oacute;n.
	 */
	private static void openApp(final String[] args) {
		SimpleAfirma.main(args);
	}
}
