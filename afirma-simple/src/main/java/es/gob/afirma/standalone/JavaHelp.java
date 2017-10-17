/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.net.URL;
import java.util.logging.Logger;

import javax.help.DefaultHelpBroker;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.WindowPresentation;
import javax.swing.JFrame;

/**
 * Clase con utilidades relacionadas con la ayuda de la aplicacion
 */
final class JavaHelp {

	private JavaHelp() {
		// No permitimos la instanciacion
	}

    private static HelpBroker helpBroker;

//	private static final Hashtable<String, Component> components = new Hashtable<String, Component>();
//	private static HelpSet helpset = null;

	static {
        try {
            // Cargamos el archivo de datos de la ayuda
        	final ClassLoader classLoader = HelpBroker.class.getClassLoader();
            final URL hsURL = classLoader.getResource("help/JavaHelp/help_set-es_ES.hs"); //$NON-NLS-1$

            // Creamos la ventana de ayuda
            final HelpSet hset = new HelpSet(classLoader, hsURL);
            helpBroker = hset.createHelpBroker();
            helpBroker.initPresentation();
            final WindowPresentation wp = ((DefaultHelpBroker)helpBroker).getWindowPresentation();
            final JFrame helpwindow = (JFrame) wp.getHelpWindow();

            // Introducimos el icono en la ventana
            helpwindow.setIconImage(
        		AutoFirmaUtil.getDefaultDialogsIcon()
    		);
        }
        catch (final Exception ex) {
            Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar la ayuda: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
        }
	}

//	/**
//	 * Cambia el idioma de la ayuda
//	 * @param language	Idioma al que se debe cambiar
//	 */
//	static void change(final String language) {
//		try {
//			// Carga el nuevo archivos de datos para ese idioma
//			final URL hsURL = HelpBroker.class.getResource("/help/help_set-" + language + ".hs");  //$NON-NLS-1$//$NON-NLS-2$
//			helpset = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
//			helpBroker = helpset.createHelpBroker();
//			for (final String key : components.keySet().toArray(new String[0])) {
//				helpBroker.enableHelpKey(components.get(key), key, helpset);
//			}
//
//		}
//		catch (final Exception ex) {
//		}
//	}

//	/**
//	 * Activa el acceso a la ventana de ayuda por la pulsacion de una tecla
//	 * @param component	Componente que se va a mostrar al pulsar la tecla
//	 * @param key		Tecla que se debe pulsar para mostrar la ventana
//	 */
//    static void enableHelpKey(final Component component, final String key) {
//		components.put(key, component);
//		helpBroker.enableHelpKey(component, key, helpset);
//	}

	/**
	 * Visualiza la ayuda por la p&aacute;gina principal.
	 */
	static void showHelp() {
		helpBroker.setDisplayed(true);
		helpBroker.setCurrentID("SimpleAfirma"); //$NON-NLS-1$
	}

}
