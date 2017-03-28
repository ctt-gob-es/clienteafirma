package es.gob.afirma.standalone.ui.restoreconfig;

import java.awt.Component;

interface Console {
	void showConsole();
	void print(final String s);
	void dispose();
	Component getParentComponent();
}
