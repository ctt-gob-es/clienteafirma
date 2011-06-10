/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.ui;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import javax.swing.JPanel;

final class ResizingTextPanel extends JPanel {

	private static final long serialVersionUID = -5496697696047898537L;

	private static final int MARGIN = 30;
	
	private final String text;
	private Font font;

	private void changeInternalFont() {
		FontMetrics fm = getFontMetrics(this.font);
		while (true) {
			if (getWidth() <= fm.stringWidth(this.text) + (MARGIN * 3)) break;
			this.font = this.font.deriveFont((float)this.font.getSize()+1);
			fm = getFontMetrics(this.font);
		}
		while (true) {
			if (getWidth() > fm.stringWidth(this.text) + (MARGIN * 3)) break;
			this.font = this.font.deriveFont((float)this.font.getSize()-1);
			fm = getFontMetrics(this.font);
		}
	}
	
	ResizingTextPanel(final String txt) {
		super(true);
		this.text = txt;
		this.font = this.getFont();
	}
	
	@Override
	public void paintComponent(final Graphics g) {
		super.paintComponent(g);
		changeInternalFont();
		g.setFont(this.font);
		g.drawString(this.text, MARGIN, getSize().height/2);
	}

}
