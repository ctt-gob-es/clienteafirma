package es.gob.afirma.standalone.configurator;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;

final class ConsoleManager {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static boolean getHeadLess() {
        if (GraphicsEnvironment.isHeadless()) {
            return true;
        }
        if (Platform.OS.LINUX.equals(Platform.getOS())) {
            return true;
        }
        return false;
    }

	private static final boolean headless = getHeadLess();
	private static final java.io.Console con = System.console();

	/**
	 * Recupera una consola para la notificaci&oacute;n del estado del proceso
	 * de configuraci&oacute;n. La selecci&oacute;n se realizar&aacute; en base
	 * al entorno de ejecuci&oacute;n.
	 * @param cl Escuchador que reaccionar&aacute; ante los mensajes de la consola.
	 * Si se indica {@code null} se usar&aacute; un consola sin entorno gr&aacute;fico.
	 * @return
	 */
	static Console getConsole(final ConsoleListener cl) {
		if (headless || cl == null) {
			if (con != null) {
			    LOGGER.info("Se utilizara la consola de tipo I/O"); //$NON-NLS-1$
				return new IoConsole(con);
			}
			LOGGER.info("Se utilizara la consola del sistema"); //$NON-NLS-1$
			return new PrintConsole();
		}
		LOGGER.info("Se utilizara la consola grafica"); //$NON-NLS-1$
		return new GraphicConfiguratorConsole(cl);
	}

	static void showErrorMessage(final Component parent, final String errorText) {
		if (!headless) {
			JOptionPane.showMessageDialog(
				parent,
				errorText,
				Messages.getString("AutoFirmaConfigurator.2"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
		}
		else if (con != null) {
			con.printf(errorText);
		}
		else {
			System.out.println(errorText);
		}
	}

}
