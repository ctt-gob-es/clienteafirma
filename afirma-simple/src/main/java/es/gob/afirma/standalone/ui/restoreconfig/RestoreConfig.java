package es.gob.afirma.standalone.ui.restoreconfig;

import java.io.IOException;
import java.security.GeneralSecurityException;

import javax.swing.JTextArea;

/** Interfaz que define los m&eacute;todos necesarios para realizar la restauraci&oacute;n
 *  de la configuraci&oacute;n de navegadores de AutoFirma.
 **/
interface RestoreConfig {

	/** Repara la configuraci&oacute;n del entorno para la ejecuci&oacute;n de AutoFirma.
	 * @param taskOutput Objeto que representa el &aacute;rea de texto de la consola de mensajes de proceso
	 * de restauraci&oacute;n.
	 * @throws IOException Cuando no es posible cargar o manipular alg&uacute;n fichero de configuraci&oacute;n o recursos.
	 * @throws ConfigurationException Cuando falla la generacion del certificados SSL.
	 * @throws GeneralSecurityException Cuando se produce un error al manipular los almacenes de certificados. */
	void restore(JTextArea taskOutput) throws IOException, ConfigurationException, GeneralSecurityException;
	
}
