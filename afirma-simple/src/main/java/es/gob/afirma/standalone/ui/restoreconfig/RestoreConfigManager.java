package es.gob.afirma.standalone.ui.restoreconfig;

import java.util.logging.Logger;

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
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n. */
	public void restoreConfigAutoFirma(final RestoreConfigPanel configPanel) {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}
		this.configurator.restore(configPanel);
	}

}
