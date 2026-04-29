package es.gob.afirma.standalone.protocol;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.java_websocket.WebSocket;

import es.gob.afirma.core.misc.protocol.ProtocolVersion;

public class WebSocketServerOperationHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static Map<String, ActiveWebSocketOperationThread> waitingThreadMap = new HashMap<>();

	private static final String WAIT_RESPONSE = "#wait"; //$NON-NLS-1$

	private static final String GET_RESULT_URL = "getresult?"; //$NON-NLS-1$

	/**
	 * Gestiona la operaci&oacute;n indicada por par&aacute;metro.
	 * @param protocol Versi&oacute;n de protocolo.
	 * @param operation Operaci&oacute;n a realizar.
	 * @param sessionId Identificador de la sesi&oacute;n.
	 * @param server Servidor donde realizar la operaci&oacute;n.
	 * @param ws WebSocket donde mandar el resultado.
	 */
	public static void handleOperation(final ProtocolVersion protocol,
									   final String operation,
									   final String sessionId,
									   final AfirmaWebSocketServerV4Sup server,
									   final WebSocket ws) {

		if (operation.startsWith(GET_RESULT_URL)) {
			getResultAndSendResponse(server, ws, sessionId);
		} else {

			try {
				final ActiveWebSocketOperationThread activeWebSocketWaitingThread
				= new ActiveWebSocketOperationThread(protocol, operation, sessionId);

	    		activeWebSocketWaitingThread.start();
	    		waitingThreadMap.put(sessionId, activeWebSocketWaitingThread);
	    		server.broadcast(WAIT_RESPONSE, Collections.singletonList(ws));
			} catch (final Exception e) {
				LOGGER.warning("Error al procesar la operacion: " + operation + "\n" + e); //$NON-NLS-1$ //$NON-NLS-2$
			}

		}
	}

	/**
	 * Envia el resultado o una se&ntilde;al de espera en caso de que no haya terminado la operaci&oacute;n.
	 * @param server Servidor a utilizar.
	 * @param ws Websocket donde enviar mensaje.
	 * @param sessionId Identificador de la sesi&oacute;n.
	 */
	private static void getResultAndSendResponse(final AfirmaWebSocketServerV4Sup server,
												final WebSocket ws,
												final String sessionId) {


		// Comprobamos si el hilo de espera activa sigue en ejecucion o ya ha terminado la operacion
		final ActiveWebSocketOperationThread websocketOperationThread = waitingThreadMap.get(sessionId);

		if (websocketOperationThread == null) {
			LOGGER.warning("No se ha iniciado una sesion para el id: " + sessionId); //$NON-NLS-1$
			server.broadcast(WAIT_RESPONSE, Collections.singletonList(ws));
		} else if (websocketOperationThread.isAlive()) {
			// Se devuelve respuesta de espera al socket, la operacion no ha terminado
			LOGGER.info("El hilo con la operacion sigue ejecutandose para la sesion: " + sessionId); //$NON-NLS-1$
			server.broadcast(WAIT_RESPONSE, Collections.singletonList(ws));
		} else {
			// La operacion ha terminado y devolvemos su resultado
			LOGGER.info("Se devuelve el resultado para la sesion: " + sessionId); //$NON-NLS-1$
			final String result = websocketOperationThread.getOperationResult();
			server.broadcast(result, Collections.singletonList(ws));
			waitingThreadMap.remove(sessionId);
		}
	}


}
