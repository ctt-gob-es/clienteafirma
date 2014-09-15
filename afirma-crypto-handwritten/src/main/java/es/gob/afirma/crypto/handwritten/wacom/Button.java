package es.gob.afirma.crypto.handwritten.wacom;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionListener;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

/** Bot&oacute;n en la pantalla de un dispositivo de captura de firmas Wacom. */
final class Button {

	/** Posici&oacute;n en pantalla del bot&oacute;n. */
    private java.awt.Rectangle bounds = new Rectangle(0, 0, 0, 0);

    private final boolean useColor;

    private final String text;

    private final List<ActionListener> clicks = new ArrayList<ActionListener>();

    Button(final String txt, final boolean color) {
    	this.text = txt != null ? txt : ""; //$NON-NLS-1$
    	this.useColor = color;
    }

    private double getX() {
    	return this.bounds.getX();
    }

    private double getY() {
    	return this.bounds.getY();
    }

    private String getText() {
    	return this.text;
    }

    boolean contains(final Point p) {
    	return this.bounds.contains(p);
    }

    boolean contains(final Point2D p) {
    	return this.bounds.contains(p);
    }

    private double getHeight() {
    	return this.bounds.getHeight();
    }

    private double getWidth() {
    	return this.bounds.getWidth();
    }

    void setBounds(final java.awt.Rectangle bnds) {
    	this.bounds = bnds;
    }

    void addActionListener(final ActionListener al) {
    	if (al != null) {
    		this.clicks.add(al);
    	}
    }

	void performClick() {
		for (final ActionListener al : this.clicks) {
			al.actionPerformed(null);
		}
    }

	void paint(final Graphics gfx) {
		if (this.useColor) {
			gfx.setColor(Color.LIGHT_GRAY);
			gfx.fillRect(
				(int) getX(),
				(int) getY(),
				(int) getWidth(),
				(int) getHeight()
			);
		}
		gfx.setColor(Color.BLACK);
		gfx.drawRect(
			(int) getX(),
			(int) getY(),
			(int) getWidth(),
			(int) getHeight()
		);
		final double fontSize = getHeight() / 2.0; // pixels
		gfx.setFont(new Font("Arial", Font.PLAIN, (int) fontSize)); //$NON-NLS-1$

		drawCenteredString(
			gfx,
			getText(),
			(int) getX(),
			(int) getY(),
			(int) getWidth(),
			(int) getHeight()
		);
	}

	private static void drawCenteredString(final Graphics gfx,
			                               final String text,
			                               final int x,
			                               final int y,
			                               final int width,
			                               final int height) {

		final FontMetrics fm = gfx.getFontMetrics(gfx.getFont());
		final int textHeight = fm.getHeight();
		final int textWidth = fm.stringWidth(text);

		final int textX = x + (width - textWidth) / 2;
		final int textY = y + (height - textHeight) / 2 + fm.getAscent();

		gfx.drawString(text, textX, textY);
	}

}
