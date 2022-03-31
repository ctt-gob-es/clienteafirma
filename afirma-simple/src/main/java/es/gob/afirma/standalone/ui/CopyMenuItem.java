/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;

import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.text.JTextComponent;

final class CopyMenuItem extends JPopupMenu implements ClipboardOwner {

	private static final long serialVersionUID = 1750985678317829383L;

	private final JTextComponent textComponent;
	JTextComponent getTextComponent() {
		return this.textComponent;
	}

	private void createUI(final String text) {
		final JMenuItem copyItem = new JMenuItem(text);
		copyItem.addActionListener(ae -> {
			if (CopyMenuItem.this.getTextComponent() != null) {
				Toolkit.getDefaultToolkit().getSystemClipboard().setContents(
					new StringSelection(
						CopyMenuItem.this.getTextComponent().getText()
					),
					CopyMenuItem.this
				);
			}
		});
		add(copyItem);
	}

	CopyMenuItem(final JTextComponent tc, final String text) {
		this.textComponent = tc;
        createUI(text);
	}

	CopyMenuItem(final JLabel label, final String text) {
		this.textComponent = new JTextField(label.getText());
        createUI(text);
	}

	@Override
	public void lostOwnership(final Clipboard clipboard, final Transferable contents) {
	    // No implementado
	}

}
