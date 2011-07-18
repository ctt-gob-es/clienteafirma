/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.standalone;

import java.awt.Component;
import java.awt.Toolkit;
import java.net.URL;
import java.util.Hashtable;

import javax.help.DefaultHelpBroker;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.WindowPresentation;
import javax.swing.JFrame;

/**
 * Clase con utilidades relacionadas con la ayuda de la aplicacion
 */
public class JavaHelp {
	
	private static final Hashtable<String, Component> components = new Hashtable<String, Component>();
	private static HelpBroker helpBroker;
	private static HelpSet helpset = null;
 
	static {
        try {
            // Cargamos el archivo de datos de la ayuda
            final URL hsURL = HelpBroker.class.getResource("/help/help_set-es_ES.hs"); //$NON-NLS-1$
            
            // Creamos la ventana de ayuda
            final HelpSet hset = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
            helpBroker = hset.createHelpBroker();
            helpBroker.initPresentation();
            final WindowPresentation wp = ((DefaultHelpBroker)helpBroker).getWindowPresentation();
            final JFrame helpwindow = (JFrame) wp.getHelpWindow();
            
            // Introducimos el icono en la ventana
            helpwindow.setIconImage(
                    Toolkit.getDefaultToolkit().createImage(
                            JavaHelp.class.getClassLoader().getResource("resources/afirma_ico.png") //$NON-NLS-1$
                    )
            ); 
        } 
        catch (final Exception ex) {
            ex.printStackTrace();
        }
	}

	/**
	 * Cambia el idioma de la ayuda
	 * @param language	Idioma al que se debe cambiar
	 */
	public static void change(String language) {
		try {
			// Carga el nuevo archivos de datos para ese idioma
			final URL hsURL = HelpBroker.class.getResource("/help/help_set-" + language + ".hs");  //$NON-NLS-1$//$NON-NLS-2$
			helpset = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
			helpBroker = helpset.createHelpBroker();
			for (final String key : components.keySet().toArray(new String[0])) {
				helpBroker.enableHelpKey(components.get(key), key, helpset);
			}

		} 
		catch (final Exception ex) {
			ex.printStackTrace();
		}
	}

	/**
	 * Activa el acceso a la ventana de ayuda por la pulsacion de una tecla
	 * @param component	Componente que se va a mostrar al pulsar la tecla
	 * @param key		Tecla que se debe pulsar para mostrar la ventana
	 */
	public static void enableHelpKey(final Component component, final String key) {
		components.put(key, component);
		helpBroker.enableHelpKey(component, key, helpset);
	}
	
	/**
	 * Visualiza la ayuda por la p&aacute;gina principal.
	 */
	public static void showHelp() {
		helpBroker.setDisplayed(true);
		helpBroker.setCurrentID("SimpleAfirma"); //$NON-NLS-1$
	}
}
