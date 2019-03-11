package es.gob.afirma.standalone.plugins;

import java.awt.Window;

/**
 * Acci&oacute;n para el procesado de los datos de entrada cargados en la aplicaci&oacute;n.
 * Este proceso no puede implicar ningun tipo de cambios sobre los propios datos.
 */
public class DataProcessAction extends PluginAction {

	/**
	 * Procesa los datos de entrada cargados en la interfaz.
	 * @param data Informaci&oacute;n de los ficheros cargados o {@code null} si
	 * no se ha seleccionado ning&uacute;n fichero.
	 * @param parent Venta padre sobre la que poder mostrar los di&aacute;logos
	 * gr&aacute;ficos.
	 */
	public void processData(final InputData[] data, final Window parent) {
		super.start(parent);
	}
}
