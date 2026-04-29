package es.gob.afirma.standalone.protocol;

/**
 * Listener para procesar los errores de conexi&oacute;n del websocket.
 */
public interface AfirmaWebSocketBindingErrorListener {

	/**
	 * Procesa un error de conexion del websocket.
	 */
	void onErrorBinding();
}
