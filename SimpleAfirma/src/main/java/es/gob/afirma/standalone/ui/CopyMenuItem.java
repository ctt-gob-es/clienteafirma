package es.gob.afirma.standalone.ui;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.text.JTextComponent;

final class CopyMenuItem extends JMenuItem implements ClipboardOwner {

	private static final long serialVersionUID = 1750985678317829383L;

	private final JTextComponent textComponent;
	
	CopyMenuItem(final JTextComponent tc) {
		this.textComponent = tc;
		this.setText("Copiar");
		this.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (CopyMenuItem.this.textComponent != null) {
					Toolkit.getDefaultToolkit().getSystemClipboard().setContents(CopyMenuItem.this.textComponent.getSelectedText(), CopyMenuItem.this);
				}
			}
		});
	}

	@Override
	public void lostOwnership(Clipboard clipboard, Transferable contents) {}
	
}
