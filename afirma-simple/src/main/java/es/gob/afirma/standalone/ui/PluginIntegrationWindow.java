package es.gob.afirma.standalone.ui;

/**
 * Ventana de la aplicacion en la que pueden integrarse un plugin.
 */
public enum PluginIntegrationWindow {
	/** Ventana de carga de datos. */
	INPUT_DATA,
	/** Ventana de visualizaci&oacute;n del resultado de una firma. */
	SINGLE_RESULT,
	/** Ventana de visualizaci&oacute;n del resultado de m&uacute;ltiples firmas. */
	MULTI_RESULT,
	/** Ventana con la informaci&oacute;n de firma. */
	VISOR;

	/**
	 * Obtiene la ventana a la que se refiere una cadena de texto.
	 * @param window Ventana en la que se desea integrar un plugin.
	 * @return Identificador de la que se desea integrar el plugin o {@code null}
	 * si no se identifica el tipo.
	 */
	public static PluginIntegrationWindow getWindow(final String window) {

		switch (window.toLowerCase()) {
		case "input": //$NON-NLS-1$
		case "input_data": //$NON-NLS-1$
			return INPUT_DATA;
		case "single_result": //$NON-NLS-1$
			return SINGLE_RESULT;
		case "multi_result": //$NON-NLS-1$
			return MULTI_RESULT;
		case "visor": //$NON-NLS-1$
			return VISOR;
		default:
			return null;
		}
	}
}