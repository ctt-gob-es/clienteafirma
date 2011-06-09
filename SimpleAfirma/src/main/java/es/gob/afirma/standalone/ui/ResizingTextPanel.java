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
