package es.gob.afirma.standalone.configurator;

import java.awt.Component;

final class IoConsole implements Console {

	private final java.io.Console console;

	IoConsole(final java.io.Console con) {
		if (con == null) {
			throw new IllegalArgumentException(
				"La consola de Java no puede ser nula" //$NON-NLS-1$
			);
		}
		this.console = con;
	}

	@Override public void showConsole() { /* Vacio */ }
	@Override public void dispose() { /* Vacio */ }

	@Override
	public void print(final String s) {
		this.console.printf(s);
	}

	@Override public Component getParentComponent() {
		return null;
	}
}
