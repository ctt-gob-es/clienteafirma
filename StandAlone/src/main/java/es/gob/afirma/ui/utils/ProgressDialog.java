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

import java.awt.Container;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JProgressBar;

/**
 * Di&aacute;logo con barra de progreso.
 */
public class ProgressDialog {

	/** Componente padre del di&aacute;logo. */ 
	private JDialog parent = null;
	
	/** Basrra de progreso. */
	private JProgressBar progressBar = null;
	
	/** Texto que se muestra sobre la barra de progreso. */
	private JLabel ltexto = null;
	
	/** Di&aacute;logo con la barra de progreso. */
	private JDialog dialog = null;
	
	/** valor actual de la barra de progreso. */
	private int currentValue = 0;
	
	/**
	 * Crea un di&aacute;logo pero no lo muestra.
	 * @param parent Componente padre sobre el que se mostrara el di&aacute;logo.
	 * @param maxValue Valor m&aacute;ximo de la barra de progres (maxValue > 0).
	 * @param title T&iacute;tulo del di&aacute;logo.
	 */
	public ProgressDialog(JDialog parent, int maxValue, String title) {
		
		if(maxValue <= 0)
			throw new IllegalArgumentException("El valor maximo de la barra de progreso no puede ser nulo");
		
		// Barra de progreso
		this.parent = parent; 
		this.progressBar = new JProgressBar(0, maxValue);
		this.ltexto = new JLabel("");
	}

	/** Muestra el di&aacute;logo con al barra de progreso. */
	public void show() {
		Container panel = new Container();
		progressBar.setValue(0);
	    progressBar.setStringPainted(true);
	    panel.setLayout(null);
	    panel.add(ltexto);
	    ltexto.setBounds(20, 15, 250, 20);
	    panel.add(progressBar);
	    progressBar.setBounds(20, 35, 260, 19);

		dialog = new JDialog(this.parent, Messages.getString("Wizard.multifirma.progress.titulo"), false);
		dialog.setBounds(parent.getX()+(parent.getWidth()-300)/2, parent.getY()+(parent.getHeight()-100)/2, 300, 100);
		dialog.setResizable(false);
		dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		dialog.setVisible(true);
	}
	
	/** Cierra el di&aacute;logo. */
	public void close() {
		if (dialog != null)
			dialog.setVisible(false);
	}
	
	/**
	 * Aumenta en una unidad el valor de la barra de progreso e indica estar procesando el elemento indicado.
	 * @param elementName Nombre del elemento que se esta procesando.
	 */
	public synchronized void processElement(String elementName) {
		if(elementName == null) 
			this.ltexto.setText(Messages.getString("Procesando"));
		else 
			this.ltexto.setText(Messages.getString("Procesando.elemento")+" \""+elementName+"\".");
		
		this.progressBar.setValue(++currentValue);
	}
}
