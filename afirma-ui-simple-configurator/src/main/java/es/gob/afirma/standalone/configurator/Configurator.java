package es.gob.afirma.standalone.configurator;

import java.io.IOException;
import java.security.GeneralSecurityException;

/**
 * Interfaz que define los m&eacute;todos necesarios para configurar el sistema operativo tras la
 * instalaci&oacute;n de AutoFirma.
 */
public interface Configurator {

	/**
	 * Configura el entorno para la ejecuci&oacute;n de AutoFirma.
	 * @param window Ventana padre con consola.
	 * @throws IOException Cuando no es posible cargar o manipular alg&uacute;n fichero de configuraci&oacute;n o recursos.
	 * @throws ConfigurationException Cuando falla la generacion del certificados SSL.
	 * @throws GeneralSecurityException Cuando se produce un error al manipular los almacenes de certificados.
	 */
	void configure(ConfiguratorConsole window) throws IOException, ConfigurationException, GeneralSecurityException;

	/**
	 * Desinstala los componentes necesarios del sistema.
	 */
	void uninstall();
}
