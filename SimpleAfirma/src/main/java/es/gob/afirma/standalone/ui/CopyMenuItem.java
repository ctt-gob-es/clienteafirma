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

import es.gob.afirma.standalone.Messages;

final class CopyMenuItem extends JPopupMenu implements ClipboardOwner {

	private static final long serialVersionUID = 1750985678317829383L;

	private final JTextComponent textComponent;

	private void createUI() {
		final JMenuItem copyItem = new JMenuItem(Messages.getString("CopyMenuItem.0")); //$NON-NLS-1$
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

	CopyMenuItem(final JTextComponent tc) {
		this.textComponent = tc;
        createUI();
	}

	@Override
	public void lostOwnership(final Clipboard clipboard, final Transferable contents) {}

}
