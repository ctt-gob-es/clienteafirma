/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dialog.ModalExclusionType;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
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
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;

import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.principal.PrincipalGUI;

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
				//La ventana de ayuda no debe ser bloqueada por ninguna ventana de la aplicaciï¿½n
				helpwindow.setModalExclusionType(ModalExclusionType.APPLICATION_EXCLUDE);
				
				// Introducimos el icono en la ventana
				Image icon = Toolkit.getDefaultToolkit().createImage(HelpUtils.class.getClassLoader().getResource("resources/images/afirma_ico.png"));
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
	 * @param id Identificador de la entrada de la ayuda a la que se desea acceder.
	 */
	public static void enableHelpKey(Component component, String id) {
		components.put(id, component);
		getHelp().enableHelpKey(component, id, helpset);
	}	
	
	/**
	 * Visualiza la ayuda en la pagina de "Introduccion"
	 */
	public static void visualize() {
		getHelp().setDisplayed(true);
		getHelp().setCurrentID("introduccion");
	}

	//TODO: el siguiente mï¿½todo se podrï¿½a borrar
	/**
	 * Genera una etiqueta con el icono de ayuda y que apunta a la p&aacute;gina dada.
	 * @param pagina	P&aacute;gina a mostrar cuando se pulse el bot&oacute;n
	 * @return			Bot&oacute;n de ayuda
	 */
	public static JLabel fechButton(final String pagina) {

		JLabel botonAyuda = new JLabel(new ImageIcon(HelpUtils.class.getResource("/resources/images/help.png")));
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

	/**
	 * Genera el boton de ayuda que apuntara a la pagina dada.
	 * @param pagina Pagina a mostrar cuando se puelse el boton de ayuda.
	 * @return boton de ayuda
	 */
	/**
	 * @param pagina
	 * @return
	 */
	public static JButton helpButton(final String pagina) {

		JButton botonAyuda = new JButton(new ImageIcon(HelpUtils.class.getResource("/resources/images/help.png")));
		botonAyuda.setToolTipText(Messages.getString("ayudaHTML.contenido"));
		botonAyuda.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		botonAyuda.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("ayudaHTML.contenido")));
		botonAyuda.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("ayudaHTML.contenido")));
		botonAyuda.setMnemonic(KeyEvent.VK_H); //Se le asigna un mnemónico al botón de ayuda
		botonAyuda.getAccessibleContext().setAccessibleName(botonAyuda.getToolTipText());
		//Se asigna una dimension al boton segun su icono
		Dimension dimension = new Dimension(12,27);
		botonAyuda.setPreferredSize(dimension);
		
		botonAyuda.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
		botonAyuda.setContentAreaFilled(false); //area del boton invisible
		
		//Acción para desplegar la pantalla de ayuda
		botonAyuda.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				getHelp().setDisplayed(true);
				getHelp().setCurrentID(pagina);
			}
		});
		Utils.remarcar(botonAyuda);
        
		return botonAyuda;
	}

	/**
	 * Muestra la ayuda por la p&aacute;gina indicada. Si no se indica, se
	 * abrir&aacute;a por la p&aacute;gina principal. 
	 * @param pagina Identificador de p&aacute;gina.
	 */
	public static void showHelp(final String pagina) {
	    getHelp().setDisplayed(true);
	    if (pagina != null) {
	        try {
	            getHelp().setCurrentID(pagina);
	        } catch (Exception e) {
	            /* No hacemos nada para que se abra por la pagina principal */
	        }
	    }
	}
}


