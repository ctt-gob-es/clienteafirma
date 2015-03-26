package es.gob.afirma.core.misc.http;


/** Factor&iacute;a para la obtenci&oacute;n de un manejador para la lectura y env&iacute;o de datos a URL remotas.
 * @author Carlos Gamuci */
public abstract class UrlHttpManagerFactory {

	private static UrlHttpManager staticUrlManager = null;
	
	/**
	 * Instala el manejador que se encargar&aacute; de realizar las conexiones con las URL indicadas.
	 * @param urlManager Manejador de conexicion para la lectura y env&iacute;o de datos. */
	public static void install(final UrlHttpManager urlManager) {
		staticUrlManager = urlManager;
	}
	
	/**
	 * Recupera el manejador instalado o, en su defecto, el manejador por defecto.
	 * @return Manejador de conexi&oacute;nes.
	 */
	public static UrlHttpManager getInstalledManager() {
		if (staticUrlManager == null) {
			staticUrlManager = new UrlHttpManagerImpl();
		}
		return staticUrlManager;
	}
}
