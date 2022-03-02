package es.gob.afirma.standalone.protocol;

/**
 * Informaci&oacute;n para el establecimiento del canal de comunicaci&oacute;n
 * a trav&eacute;s de socket/websocket.
 */
class ChannelInfo {

	private final String idSession;
	private int[] ports;

	/**
	 * Crea la informaci&oacute;n del canal.
	 * @param idSession Identificador de la sesi&oacute;n que se asignar&aacute; al canal.
	 * @param ports Listado de puertos entre los que buscar uno en el que abrir el canal.
	 */
	public ChannelInfo(final String idSession, final int[] ports) {
		this.idSession = idSession;
		this.ports = ports;
	}

	/**
	 * Obtiene el identificador de sesi&oacute;n.
	 * @return Identificador de sesi&oacute;n.
	 */
	public String getIdSession() {
		return this.idSession;
	}

	/**
	 * Obtiene el listado de puertos.
	 * @return Listado de puertos.
	 */
	public int[] getPorts() {
		return this.ports;
	}

	/**
	 * Establece el listado de puertos.
	 * @param ports Listado de puertos.
	 */
	public void setPorts(final int[] ports) {
		this.ports = ports;
	}
}