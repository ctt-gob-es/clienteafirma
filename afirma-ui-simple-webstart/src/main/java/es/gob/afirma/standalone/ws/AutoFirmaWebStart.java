package es.gob.afirma.standalone.ws;

import java.io.File;

import javax.jnlp.BasicService;
import javax.jnlp.ServiceManager;

import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.VisorFirma;

/**
 * Aplicacion WebStart encargada de gestionar la instalaci&oacute;n
 * de AutoFirma y su ejecuci&oacute;n.
 */
public class AutoFirmaWebStart {

	/** Comando usado para la apertura de ficheros. */
	private static final String PARAM_OPEN   = "-open"; //$NON-NLS-1$

	public static void main(final String[] args) {

		// Si detectamos que se ejecuta esta aplicacion como parte del proceso de instalacion,
		// no hacemos nada
		if (isInstallationProcess(args)) {
			System.exit(0);
		}

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
	 * Indica si nos encontramos durante el proceso de instalacion JNLP de AutoFirma
	 * o si se trata de una ejecuci&oacute;n corriente.
	 * @return {@code true} si estamos en la instalaci&oacute;n de AutoFirma, {@code false}
	 * en caso contrario.
	 */
	private static boolean isInstallationProcess(final String[] args) {

		if (args != null && args.length > 0) {
			return false;
		}

		try {
			return !((BasicService) ServiceManager.lookup("javax.jnlp.BasicService")).isOffline(); //$NON-NLS-1$
		} catch (final Throwable e) {
			// No se ha podido cargar el servicio JNLP. Entendemos que estamos en
			// una ejecucion corriente
		}

		return false;
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
