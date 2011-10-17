/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.listeners;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import es.gob.afirma.ui.utils.JStatusBar;

/**
 * Manejador para que se muestre un texto en la barra de estado cada vez que se enfoque
 * el componente. Cuando se piera el foco se eliminar&aacute; el texto de la barra de estado.   
 */
public class ElementDescriptionFocusListener implements FocusListener {

	/** Barra de estado. */
	private JStatusBar statusBar;
	
	/** Texto descriptivo. */
	private String textDescription;
	
	/**
	 * Crea un manejador que muestra un texto en la barra de estado indicada cuando se
	 * enfoca el componente y lo borra cuando el foco sale del mismo.
	 * @param statusBar Barra de estado en la que mostrar el texto.
	 * @param description Texto que se desea mostrar.
	 */
	public ElementDescriptionFocusListener(JStatusBar statusBar, String description) {
		this.statusBar = statusBar;
		this.textDescription = description;
	}

	@Override
    public void focusGained(FocusEvent e) {
		this.statusBar.setStatus(this.textDescription);
	}

	@Override
    public void focusLost(FocusEvent e) {
		this.statusBar.setStatus(""); //$NON-NLS-1$
	}

}
