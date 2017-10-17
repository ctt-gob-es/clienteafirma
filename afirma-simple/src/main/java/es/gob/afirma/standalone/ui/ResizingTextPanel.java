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
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

import es.gob.afirma.core.misc.Platform;

final class ResizingTextPanel extends JPanel {

    private static final long serialVersionUID = -5496697696047898537L;

    private static final int MARGIN = 30;

    private final String text;
    private Font font;

    private void changeInternalFont() {
        FontMetrics fm = getFontMetrics(this.font);
        while (true) {
            if (getWidth() <= fm.stringWidth(this.text) + MARGIN * 3) {
                break;
            }
            this.font = this.font.deriveFont((float) this.font.getSize() + 1);
            fm = getFontMetrics(this.font);
        }
        while (true) {
            if (getWidth() > fm.stringWidth(this.text) + MARGIN * 3) {
                break;
            }
            this.font = this.font.deriveFont((float) this.font.getSize() - 1);
            fm = getFontMetrics(this.font);
        }
    }

    ResizingTextPanel(final String txt) {
        super(true);
        this.text = txt != null ? txt : ""; //$NON-NLS-1$
        this.font = this.getFont();
        // En Linux ponemos un borde al panel
        if (Platform.OS.LINUX.equals(Platform.getOS())) {
        	this.setBorder(BorderFactory.createLineBorder(Color.BLACK));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        changeInternalFont();
        g.setFont(this.font);
        g.drawString(this.text, MARGIN, getSize().height / 2);
    }

}
