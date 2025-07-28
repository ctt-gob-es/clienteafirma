/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Area;
import java.util.EventListener;

import javax.swing.ImageIcon;
import javax.swing.JLabel;

final class PageLabel extends JLabel {

	interface PageLabelListener extends EventListener {
		void setX(String x);
		void setY(String y);
		void setWidth(String width);
		void setHeight(String height);
	}

	private static final long serialVersionUID = 4917110251831788580L;

	private static final int TRANSPARENCY_LEVEL = 3;

	private final PageLabelListener listener;
	PageLabelListener getPageLabelListener() {
		return this.listener;
	}

	private final float scale;

    private Rectangle selectionBounds = null;
    void setSelectionBounds(final Rectangle r) {
    	this.selectionBounds = r;
    }
    Rectangle getSelectionBounds() {
    	return this.selectionBounds = null;
    }

    private Point clickPoint;
    Point getClickPoint() {
    	return this.clickPoint;
    }
    void setClickPoint(final Point p) {
    	this.clickPoint = p;
    }

	PageLabel(final Image image,
			  final int width,
			  final int height,
			  final PageLabelListener pll,
			  final float scaleFactor) {

		super(new ImageIcon(image));

		if (pll == null) {
			throw new IllegalArgumentException();
		}
		this.listener = pll;
		this.scale = scaleFactor;

		setSize(width, height);
		setCursor(new java.awt.Cursor(java.awt.Cursor.CROSSHAIR_CURSOR));

		final MouseAdapter ma = new MouseAdapter() {

            @Override
            public void mouseClicked(final MouseEvent e) {
            	// No hacemos nada
            }

            @Override
            public void mousePressed(final MouseEvent e) {
            	clearAreaValues();
            	setClickPoint(e.getPoint());
                setSelectionBounds(null);
                repaint();
            }

            @Override
            public void mouseReleased(final MouseEvent e) {
            	final Point initialPoint = getClickPoint();
            	final Point finalPoint = e.getPoint();

            	// Si el area no tiene superficie, seleccionamos el area
            	if (initialPoint.getX() == finalPoint.getX() ||
            			initialPoint.getY() == finalPoint.getY()) {
            		clearAreaValues();
            	}
            	else {

            		// Ajustamos los puntos a las dimensiones reales del campo
            		final int lWidth = getWidth();
            		final int lHeight = getHeight();

            		int pX = (int) initialPoint.getX();
            		final int pointOneX = pX < 0 ? 0 : pX > lWidth ? lWidth : pX;
            		int pY = (int) initialPoint.getY();
            		final int pointOneY = pY < 0 ? 0 : pY > lHeight ? lHeight : pY;

            		pX = (int) finalPoint.getX();
            		final int pointTwoX = pX < 0 ? 0 : pX > lWidth ? lWidth : pX;
            		pY = (int) finalPoint.getY();
            		final int pointTwoY = pY < 0 ? 0 : pY > lHeight ? lHeight : pY;

            		setAreaValues(
            				Math.min(pointOneX, pointTwoX),
            				Math.min(pointOneY, pointTwoY),
            				Math.abs(pointOneX - pointTwoX),
            				Math.abs(pointOneY - pointTwoY));
            	}
            	setClickPoint(null);
            }

            @Override
            public void mouseDragged(final MouseEvent e) {
            	if (getClickPoint() != null) {
	                final Point dragPoint = e.getPoint();
	                final int x = Math.min(getClickPoint().x, dragPoint.x);
	                final int y = Math.min(getClickPoint().y, dragPoint.y);
	                final int rWidth = Math.max(getClickPoint().x - dragPoint.x, dragPoint.x - getClickPoint().x);
	                final int rHeight = Math.max(getClickPoint().y - dragPoint.y, dragPoint.y - getClickPoint().y);
	                setSelectionBounds(new Rectangle(x, y, rWidth, rHeight));
	                repaint();
            	}

            }

            @Override
			public void mouseMoved(final MouseEvent e) {
            	// No hacemos nada
            }

            @Override
			public void mouseExited(final MouseEvent e) {
            	// No hacemos nada
            }
		};

		addMouseListener(ma);
		addMouseMotionListener(ma);
	}

	void clearAreaValues() {
		getPageLabelListener().setX(""); //$NON-NLS-1$
		getPageLabelListener().setY(""); //$NON-NLS-1$
		getPageLabelListener().setWidth(""); //$NON-NLS-1$
		getPageLabelListener().setHeight(""); //$NON-NLS-1$
	}

	void setAreaValues(final int x, final int y, final int width, final int height) {
    	getPageLabelListener().setX(
			Integer.toString(
				Math.round(
					Math.min(x, getWidth()) / this.scale
				)
			)
		);
    	getPageLabelListener().setY(
			Integer.toString(
				Math.round(
						Math.min(y, getHeight()) / this.scale
				)
			)
		);
    	getPageLabelListener().setWidth(
			Integer.toString(
				Math.round(
						width / this.scale
				)
			)
		);
    	getPageLabelListener().setHeight(
			Integer.toString(
				Math.round(
						height / this.scale
				)
			)
		);
	}

	public float getScale() {
		return this.scale;
	}

	@Override
	public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        final Graphics2D g2d = (Graphics2D) g.create();
        g2d.setColor(new Color(255, 255, 255, 128));

        final Area fill = new Area(new Rectangle(new Point(0, 0), getSize()));
        if (this.selectionBounds != null) {
            fill.subtract(new Area(this.selectionBounds));
        }
        g2d.fill(fill);
        if (this.selectionBounds != null) {
	        g2d.setColor(Color.BLUE);
	        g2d.setComposite(AlphaComposite.getInstance(
	        		AlphaComposite.SRC_OVER,TRANSPARENCY_LEVEL * 0.1f));
            g2d.fill(this.selectionBounds);
        }
        g2d.dispose();
	}

}
