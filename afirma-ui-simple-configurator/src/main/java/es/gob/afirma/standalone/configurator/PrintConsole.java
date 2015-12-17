package es.gob.afirma.standalone.configurator;

import java.awt.Component;

final class PrintConsole implements Console {

	@Override public void showConsole() { /* Vacio */ }
	@Override public void dispose() { /* Vacio */ }

	@Override
	public void print(final String s) {
		System.out.println(s);
	}

	@Override
	public Component getParentComponent() {
		return null;
	}

}
