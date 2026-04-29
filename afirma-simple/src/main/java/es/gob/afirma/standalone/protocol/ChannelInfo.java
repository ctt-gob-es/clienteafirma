package es.gob.afirma.standalone.protocol;

/**
 * Informaci&oacute;n para el establecimiento del canal de comunicaci&oacute;n
 * a trav&eacute;s de socket/websocket.
 */
class ChannelInfo {

	private final String idSession;

	/** Listado de puertos en los que se deber&iacute;a intentar abrir le canal. */
	private int[] ports;

	/** &Iacute;ndice del siguiente puerto disponible. */
	private int portIndex = 0;

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

	/**
	 * Devuelve el siguiente puerto disponible del listado que no se haya
	 * devuelto anteriormente y marca cualquier puerto anterior como no disponible.
	 * @return El siguiente puerto disponible o {@code -1} si no hay m&aacute;s.
	 */
	public int nextPortAvailable() {
		if (this.portIndex >= this.ports.length) {
			return -1;
		}
		return this.ports[this.portIndex++];
	}
}