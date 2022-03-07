package es.gob.afirma.plugin.hash.command;

public enum HashCommands {
	CREATEHASH("createdigest", true), //$NON-NLS-1$
	CHECKHASH("checkdigest", false); //$NON-NLS-1$

	private String op;
	private boolean mainFileNeeded;

	private HashCommands(final String op, final boolean mainFileNeeded) {
		this.op = op;
		this.mainFileNeeded = mainFileNeeded;
	}

	public String getOp() {
		return this.op;
	}

	public boolean isMainFileNeeded() {
		return this.mainFileNeeded;
	}

	/** Obtiene el comando que se responde a la cadena indicada.
	 * @param op Texto identificador del comando.
	 * @return Comando correspondiente o {@code null} si no hay ninguno. */
	public static HashCommands parse(final String op) {
		for (final HashCommands c : HashCommands.values()) {
			if (c.getOp().equals(op)) {
				return c;
			}
		}
		return null;
	}
}
