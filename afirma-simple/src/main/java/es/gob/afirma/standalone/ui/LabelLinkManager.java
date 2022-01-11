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
import java.awt.Desktop;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.font.TextAttribute;
import java.net.URI;
import java.security.cert.X509Certificate;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.JOptionPane;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Gestor de foco y rat&oacute;n en JLabel con enlaces.
 * @author Jos&eacute;s Montero Rivero.
 * */
public final class LabelLinkManager extends KeyAdapter implements FocusListener,
                                          							MouseListener{

    private final JLabel label;

    private final boolean isValideURL;

    private final X509Certificate cert;

    /**
     * Crea un nuevo gestor de foco y rat&oacute;n.
     * @param label Etiqueta a la que agregar el gestor.
     * @param isValideURL Indica si redirigir a la URL de VALIDe o a la informaci&oacute;n de un certificado
     * @param cert Informacion sobre el certificado a redirigir, en caso de se quiera redirigir a VALIDe ser&aacute; null.
     */
    public LabelLinkManager (final JLabel label, final boolean isValideURL, final X509Certificate cert) {

    	this.label = label;
    	final Font font = this.label.getFont();
    	final Map attributes = font.getAttributes();
    	attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
    	this.label.setFont(font.deriveFont(attributes));
    	this.label.setForeground(Color.blue);
    	this.label.setCursor(new Cursor(Cursor.HAND_CURSOR));
    	this.label.setOpaque(true);

        this.isValideURL = isValideURL;
        this.cert = cert;
        this.label.setFocusable(true);
        this.label.addMouseListener(this);
        this.label.addFocusListener(this);
        this.label.addKeyListener(this);
    }

    @Override
    public void focusGained(final FocusEvent e) {
    	this.label.setOpaque(true);
    	this.label.setForeground(Color.white);
    	this.label.setBackground(Color.blue);
    	this.label.repaint();
    }

    @Override
    public void focusLost(final FocusEvent e) {
    	this.label.setOpaque(false);
    	this.label.setForeground(Color.blue);
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
		openLink();
	}

	@Override
	public void keyPressed(final KeyEvent e) {

		switch (e.getKeyCode()) {
			case KeyEvent.VK_SPACE:
			case KeyEvent.VK_ENTER:

				openLink();
					break;

	            default:
	            	break;
	        }
	    }

	/**
	 * Abre el enlace seg&uacute;n el tipo indicado en la variable isValideURL.
	 */
	private void openLink() {
		if (this.isValideURL) {
	    	try {
				Desktop.getDesktop().browse(new URI(SimpleAfirmaMessages.getString("SignResultPanel.33"))); //$NON-NLS-1$
			} catch (final Exception ex) {
		    	AOUIFactory.showErrorMessage(
		            SimpleAfirmaMessages.getString("SignResultPanel.0") + SimpleAfirmaMessages.getString("SignResultPanel.33"), //$NON-NLS-1$ //$NON-NLS-2$
		            SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
		            JOptionPane.ERROR_MESSAGE,
		            ex
		        );
			}
		} else {
			SignDataPanel.openCertificate(this.cert);
		}
	}

}
