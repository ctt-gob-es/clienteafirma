package es.gob.afirma.standalone.configurator;

import java.awt.Component;

interface Console {
	void showConsole();
	void print(final String s);
	void dispose();
	Component getParentComponent();
}
