package es.gob.afirma.standalone.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

class RoundedPanel extends JPanel {

	private static final long serialVersionUID = 1L;
	
	private final int arc;

    public RoundedPanel(int arc) {
        this.arc = arc;
        setOpaque(false);
        setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
        int w = getWidth();
        int h = getHeight();

        g2.setColor(getBackground());
        g2.fillRoundRect(0, 0, w, h, arc, arc);

        g2.setColor(Color.GRAY);
        g2.setStroke(new BasicStroke(5.5f)); 
        g2.drawRoundRect(0, 0, w - 1, h - 1, arc, arc);

        g2.dispose();
    }
}


