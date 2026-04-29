package es.gob.afirma.standalone.protocol;

import java.util.logging.Logger;

import es.gob.afirma.core.misc.protocol.ProtocolVersion;

/**
 * Hilo que envia peri&oacute;dicamente una señal "#WAIT" al cliente conectado por WebSocket.
 */
public class ActiveWebSocketOperationThread extends Thread {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final ProtocolVersion protocol;
    private final String operation;
    private final String idSession;

    private String operationResult;

    /**
     * Crea el hilo para realizar la operaci&oacute;n indicada por par&aacute;metro.
     *
     * @param protocol Versi&oacute;n de protocolo.
     * @param operation Cadena con la operaci&oacute;n a realizar.
     * @param idSession Identificador de la sesi&oacute;n.
     */
    public ActiveWebSocketOperationThread(
    		final ProtocolVersion protocol,
    		final String operation,
            final String idSession) {

    	this.protocol = protocol;
    	this.operation = operation;
        this.idSession = idSession;
    }

    @Override
    public void run() {

    	LOGGER.info("Iniciando hilo de operacion para la sesion: " + this.idSession); //$NON-NLS-1$

    	try {

    		final String result = ProtocolInvocationLauncher.launch(this.operation, this.protocol, true);
    		this.operationResult = result;

    	} catch (final Exception e) {
    		LOGGER.severe("Error en la ejecucion de la operacion para la sesion " + this.idSession + " :" + e); //$NON-NLS-1$ //$NON-NLS-2$
    	}

    	LOGGER.info("Finalizado hilo de operacion activa para la sesion: " + this.idSession); //$NON-NLS-1$
    }

	public String getOperationResult() {
		return this.operationResult;
	}

	public void setOperationResult(final String operationResult) {
		this.operationResult = operationResult;
	}

}
