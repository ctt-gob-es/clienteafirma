/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.font.TextAttribute;
import java.util.Map;

import javax.swing.JLabel;

import es.gob.afirma.standalone.LookAndFeelManager;

/**
 * Gestor de foco y rat&oacute;n en JLabel con enlaces.
 * @author Jos&eacute; Montero Rivero.
 * */
public final class LabelLinkManager extends KeyAdapter implements FocusListener,
                                          							MouseListener{

    private final JLabel label;

    private LabelLinkListener labelLinkListener;

    /**
     * Crea un nuevo gestor de foco y rat&oacute;n.
     * @param label Etiqueta a la que agregar el gestor.
     */
    public LabelLinkManager(final JLabel label) {

    	this.label = label;
    	final Font font = this.label.getFont();
    	final Map attributes = font.getAttributes();
    	attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
    	this.label.setFont(font.deriveFont(attributes));
    	if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        	this.label.setForeground(Color.BLUE);
    	} else {
        	this.label.setForeground(Color.YELLOW);
    	}
    	this.label.setBackground(null);
    	this.label.setCursor(new Cursor(Cursor.HAND_CURSOR));
    	this.label.setOpaque(true);

        this.label.setFocusable(true);
        this.label.addMouseListener(this);
        this.label.addFocusListener(this);
        this.label.addKeyListener(this);
    }

    public void setLabelLinkListener(final LabelLinkListener linkListener) {
    	this.labelLinkListener = linkListener;
    }

    @Override
    public void focusGained(final FocusEvent e) {
    	this.label.setOpaque(true);
    	if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        	this.label.setForeground(Color.WHITE);
    	} else {
        	this.label.setForeground(Color.YELLOW);
    	}
    	this.label.setBackground(Color.blue);
    	this.label.repaint();
    }

    @Override
    public void focusLost(final FocusEvent e) {
    	this.label.setOpaque(false);
    	if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        	this.label.setForeground(Color.BLUE);
    	} else {
        	this.label.setForeground(Color.YELLOW);
    	}
    	this.label.setBackground(null);
    	this.label.repaint();
    }

	@Override public void mousePressed(final MouseEvent e) {/* No implementado */}

	@Override public void mouseReleased(final MouseEvent e) {/* No implementado */}

	@Override public void mouseEntered(final MouseEvent e) {/* No implementado */}

	@Override public void mouseExited(final MouseEvent e) {/* No implementado */}

	@Override
	public void mouseClicked(final MouseEvent e) {

		this.label.requestFocus();
		focusGained(null);
		if (this.labelLinkListener != null) {
			this.labelLinkListener.openLink();
		}
	}

	@Override
	public void keyPressed(final KeyEvent e) {
		switch (e.getKeyCode()) {
		case KeyEvent.VK_SPACE:
		case KeyEvent.VK_ENTER:
			if (this.labelLinkListener != null) {
				this.labelLinkListener.openLink();
			}
			break;
		default:
			break;
		}
	}

}
