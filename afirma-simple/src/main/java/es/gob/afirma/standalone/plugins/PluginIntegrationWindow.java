package es.gob.afirma.standalone.plugins;

/**
 * Ventana de la aplicacion en la que pueden integrarse un plugin.
 */
public enum PluginIntegrationWindow {
	/** Ventana de carga de datos. */
	PREPROCESS,
	/** Ventana de visualizacion del resultado de la firma. */
	POSTPROCESS,
	/** Ventana con la informaci&oacute;n de firma firma. */
	VISOR;

	/**
	 * Obtiene la ventana a la que se refiere una cadena de texto.
	 * @param window Ventana en la que se desea integrar un plugin.
	 * @return Identificador de la que se desea integrar el plugin o {@code null}
	 * si no se identifica el tipo.
	 */
	public static PluginIntegrationWindow getWindow(String window) {

		switch (window) {
		case "preprocess": //$NON-NLS-1$
		case "PREPROCESS": //$NON-NLS-1$
			return PREPROCESS;
		case "postprocess": //$NON-NLS-1$
		case "POSTPROCESS": //$NON-NLS-1$
			return POSTPROCESS;
		case "visor": //$NON-NLS-1$
		case "VISOR": //$NON-NLS-1$
			return VISOR;
		default:
			return null;
		}
	}
}