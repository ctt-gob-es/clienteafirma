/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.listeners;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import es.gob.afirma.ui.utils.JStatusBar;

/**
 * Manejador para que se muestre un texto en la barra de estado cada vez que se pase el rat&oacute;n
 * por encima de &eacute;l. Cuando se quite el rat&oacute;n de encima de elemento, se eliminar&aacute;
 * el texto de la barra de estado.   
 */
public class ElementDescriptionMouseListener implements MouseListener {

	/** Barra de estado. */
	private JStatusBar statusBar;
	
	/** Texto descriptivo. */
	private String textDescription;
	
	/**
	 * Crea un manejador que muestra un texto en la barra de estado indicada cuando se situa
	 * el puntero el rat&oacute;n sobre el componente y lo borra cuando se sale del mismo.
	 * @param statusBar Barra de estado en la que mostrar el texto.
	 * @param description Texto que se desea mostrar.
	 */
	public ElementDescriptionMouseListener(JStatusBar statusBar, String description) {
		this.statusBar = statusBar;
		this.textDescription = description;
	}
	
	public void mouseClicked(MouseEvent e) {}

	public void mouseEntered(MouseEvent e) {
		statusBar.setStatus(this.textDescription);
	}

	public void mouseExited(MouseEvent e) {
		statusBar.setStatus("");
	}

	public void mousePressed(MouseEvent e) {}

	public void mouseReleased(MouseEvent e) {}

}
