package es.gob.afirma.standalone.ui.restoreconfig;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.JTextArea;

import es.gob.afirma.core.misc.Platform;

/**
 * Clase que contiene la l&oacute;gica para iniciar el proceso de restauraci&oacute;n decidiendo
 * el sistema operativo objetivo.
 *
 */
public class RestoreConfigManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private RestoreConfig configurator;

	/**
	 * Constructor del restaurador de configuración
	 * de Autofirma
	 */
	public RestoreConfigManager() {

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			this.configurator = new RestoreConfigWindows();

		}
		else if (Platform.OS.LINUX == Platform.getOS()){

		    this.configurator = new RestoreConfigLinux();
		}
		else if (Platform.OS.MACOSX == Platform.getOS()){
            this.configurator = new RestoreConfigMacOSX();
        }
		else {
			LOGGER.warning(
				"El sistema operativo '" + Platform.getOS() + "' no tiene definida una secuencia de configuracion/desinstalacion" //$NON-NLS-1$ //$NON-NLS-2$
			);
			this.configurator = null;
		}
	}

	/** Repara la configuraci&oacute;n de navegadores para permitir la correcta ejecuci&oacute;n de AutoFirma.
	 * @param taskOutput Cuadro de texto done se informa de las operaciones ejecutadas.
	 * @throws GeneralSecurityException Cuando se produce un error al manipular los almacenes de certificados.
	 * @throws ConfigurationException Cuando falla la generacion del certificados SSL.
	 * @throws IOException Cuando no es posible cargar o manipular alg&uacute;n fichero de configuraci&oacute;n o recursos. */
	public void restoreConfigAutoFirma(final JTextArea taskOutput) throws GeneralSecurityException, ConfigurationException, IOException {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		// Creamos el almacen para la configuracion del SSL
		try {
			this.configurator.restore(taskOutput);
		}
		catch (final IOException e) {
			LOGGER.severe("Error al copiar, leer o eliminar alguno de los ficheros de configuracion. El configurador se detendra: " + e); //$NON-NLS-1$
			JOptionPane.showMessageDialog(null, Messages.getString("RestoreAutoFirma.3")); //$NON-NLS-1$
			throw e;
		}
		catch (final ConfigurationException e) {
			LOGGER.severe("Error al generar las claves de cifrado SSL. El configurador se detendra: " + e); //$NON-NLS-1$
			JOptionPane.showMessageDialog(null, Messages.getString("RestoreAutoFirma.4")); //$NON-NLS-1$
			throw e;
		}
		catch (final GeneralSecurityException e) {
			LOGGER.severe("Error en la importacion de la CA de confianza o la limpieza del almacen: " + e); //$NON-NLS-1$
			JOptionPane.showMessageDialog(null, Messages.getString("RestoreAutoFirma.5")); //$NON-NLS-1$
			throw e;
		}
	}

}
