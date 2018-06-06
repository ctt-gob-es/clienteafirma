package es.gob.afirma.standalone.plugins;

import java.util.Properties;

import javax.swing.JPanel;

/**
 * Panel con las opciones de configuraci&oacute;n de un plugin.
 */
public abstract class ConfigurationPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = -1281422505557803637L;

	/**
	 * Este m&eacute;todo proporciona al panel la configuraci&oacute;n almacenada previamente.
	 * Se ejecuta justo despu&eacute;s de construir el di&aacute;logo de configuraci&oacute;n
	 * y debe cargar los valores proporcionados en los distintos componentes del panel.
	 * Es la propia aplicaci&oacute;n la que recupera estos datos para el plugin.
	 * @param config Configuraci&oacute;n anteriormente guardada.
	 */
	public abstract void init(Properties config);

	/**
	 * Este m&eacute;todo debe devolver la configuraci&oacute; establecida en el panel
	 * para permitir a la aplicaci&oacute;n guardarla. Se ejecuta cuando el usuario pulsa
	 * el bot&oacute;n "Aceptar" del di&aacute;logo de configuraci&oacute;n.
	 * Es la propia aplicaci&oacute;n la que guarda estos datos para el plugin.
	 * @return Configuraci&oacute;n que se desea guardar.
	 */
	public abstract Properties getConfiguration();
}
