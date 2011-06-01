/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;

import javax.help.DefaultHelpBroker;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.WindowPresentation;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

/**
 * Clase con utilidades relacionadas con la ayuda de la aplicacion
 */
public class HelpUtils {
	
	private static Hashtable<String, Component> components = new Hashtable<String, Component>();
	private static HelpBroker helpBroker = null;
	private static HelpSet helpset = null;
 
	static {
		getHelp();
	}
	
	/**
	 * Genera la ayuda
	 * @return	Ventana con el panel de ayuda
	 */
	private static HelpBroker getHelp() {
		if (helpBroker == null) {
			try {
				// Cargamos el archivo de datos de la ayuda
				URL hsURL = HelpBroker.class.getResource("/help/help_set-es_ES.hs");
				
				// Creamos la ventana de ayuda
				HelpSet helpset = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
				helpBroker = helpset.createHelpBroker();
				helpBroker.initPresentation();
				WindowPresentation wp = ((DefaultHelpBroker)helpBroker).getWindowPresentation();
				JFrame helpwindow = (JFrame) wp.getHelpWindow();
				
				// Introducimos el icono en la ventana
				Image icon = Toolkit.getDefaultToolkit().createImage(HelpUtils.class.getClassLoader().getResource("images/afirma_ico.png"));
				helpwindow.setIconImage(icon);	
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}

		return helpBroker;
	}

	/**
	 * Cambia el idioma de la ayuda
	 * @param language	Idioma al que se debe cambiar
	 */
	public static void change(String language) {
		try {
			// Carga el nuevo archivos de datos para ese idioma
			URL hsURL = HelpBroker.class.getResource("/help/help_set-" + language + ".hs");
			helpset = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
			helpBroker = helpset.createHelpBroker();
			Enumeration<String> enumeration = components.keys();
			while(enumeration.hasMoreElements()) {
				String key = enumeration.nextElement();
				Component component = components.get(key);
				helpBroker.enableHelpKey(component, key, helpset);
			}

		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	/**
	 * Activa el acceso a la ventana de ayuda por la pulsacion de una tecla
	 * @param component	Componente que se va a mostrar al pulsar la tecla
	 * @param key		Tecla que se debe pulsar para mostrar la ventana
	 */
	public static void enableHelpKey(Component component, String key) {
		components.put(key, component);
		getHelp().enableHelpKey(component, key, helpset);
	}	
	
	/**
	 * Visualiza la ayuda en la pagina de "Introduccion"
	 */
	public static void visualize() {
		getHelp().setDisplayed(true);
		getHelp().setCurrentID("introduccion");		
	}
	
	/**
	 * Genera una etiqueta con el icono de ayuda y que apunta a la p&aacute;gina dada.
	 * @param pagina	P&aacute;gina a mostrar cuando se pulse el bot&oacute;n
	 * @return			Bot&oacute;n de ayuda
	 */
	public static JLabel fechButton(final String pagina) { 
		JLabel botonAyuda = new JLabel(new ImageIcon(HelpUtils.class.getResource("/images/help.png")));
		botonAyuda.setToolTipText(Messages.getString("ayudaHTML.contenido"));
		botonAyuda.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		botonAyuda.addMouseListener(new MouseListener() {
			public void mousePressed(MouseEvent e) {}
			public void mouseReleased(MouseEvent e) {}
			public void mouseEntered(MouseEvent e) {}
			public void mouseExited(MouseEvent e) {}

			public void mouseClicked(MouseEvent e) {
				getHelp().setDisplayed(true);
				getHelp().setCurrentID(pagina);	
			}
		});
		
		return botonAyuda;
	}
}
