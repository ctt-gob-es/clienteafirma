/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.standalone.ui;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.text.JTextComponent;

final class CopyMenuItem extends JPopupMenu implements ClipboardOwner {

	private static final long serialVersionUID = 1750985678317829383L;

	final JTextComponent textComponent;

	private void createUI(final String text) {
		final JMenuItem copyItem = new JMenuItem(text);
		copyItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (CopyMenuItem.this.textComponent != null) {
					Toolkit.getDefaultToolkit().getSystemClipboard().setContents(
						new StringSelection((CopyMenuItem.this.textComponent.getSelectedText() != null) ? CopyMenuItem.this.textComponent.getSelectedText() : CopyMenuItem.this.textComponent.getText()), CopyMenuItem.this
					);
				}
			}
		});
		add(copyItem);
	}

	CopyMenuItem(final JTextComponent tc, final String text) {
		this.textComponent = tc;
        createUI(text);
	}

	@Override
	public void lostOwnership(final Clipboard clipboard, final Transferable contents) {
	    // No implementado
	}

}
